# Score units using prompts and response schemas supplied by a dataset module.
# Each arm writes scores_<arm>.jsonl; reruns skip unit IDs already saved there.
# Errors are retried, and missing IDs are reported after all arms finish.
# Run from code/:
#   ../.venv/bin/python judge/run_judge.py --units ../data/prism/census_units.csv \
#       --prompts ../data/prism --out ../data/prism/judge_census \
#       --unit-block prism.build_prompts --arms sat sat_type \
#       --model gpt-5-nano --reasoning-effort low

import argparse, asyncio, csv, importlib, itertools, json, os, pathlib, random, statistics, sys, time

from openai import AsyncOpenAI

sys.path.insert(0, os.getcwd())  # --unit-block is resolved relative to code/

PRICE = {  # assumed dollars per million input/output tokens for cost estimates
    "gpt-5-nano": (.05, .40), "gpt-4o-mini": (.15, .60),
    "gpt-4.1-mini": (.40, 1.60), "gpt-5-mini": (.25, 2.00),
    "gemini-3.5-flash-lite": (.30, 2.50), "gemini-3.1-flash-lite": (.25, 1.50),
}

# Select the endpoint and API-key variable by model name.
# None keeps the OpenAI client's default endpoint.
GEMINI_BASE = "https://generativelanguage.googleapis.com/v1beta/openai/"
provider = lambda m: ((GEMINI_BASE, "GEMINI_API_KEY") if m.startswith("gemini-")
                      else (None, "OPENAI_API_KEY"))
TIMEOUT = 90
RETRIES = 4          # transient errors: timeouts, dropped connections
RL_RETRIES = 10      # allow more attempts for rate limits
RL_WAIT = (45, 75)   # seconds, jittered

ap = argparse.ArgumentParser()
ap.add_argument("--units", required=True)
ap.add_argument("--prompts", required=True, help="dir holding prompt_<arm>.txt")
ap.add_argument("--out", required=True)
ap.add_argument("--arms", nargs="+", required=True)
ap.add_argument("--unit-block", required=True, help="corpus module: ID, FIELDS, schema(arm), unit_block(row, arm)")
ap.add_argument("--model", default="gpt-5-nano", choices=sorted(PRICE))
ap.add_argument("--reasoning-effort", default=None, help="reasoning models only; omit = API default")
ap.add_argument("--temperature", type=float, default=None, help="omit = API default (1.0 for chat completions); 0 to cut run-to-run nondeterminism")
ap.add_argument("--concurrency", type=int, default=128)
ap.add_argument("--limit", type=int, default=0, help="score only the first N units (smoke test)")
args = ap.parse_args()

# The dataset module supplies the unit ID, answer fields, schema, and prompt renderer.
# For PRISM, FIELDS contains score and confidence; the local names binary/prob
# below are also used for these integer scores. CENSUS_N optionally projects costs.
_m = importlib.import_module(args.unit_block)
ID, FIELDS, schema, unit_block = _m.ID, _m.FIELDS, _m.schema, _m.unit_block
CENSUS_N = getattr(_m, "CENSUS_N", None)
PROMPT_DIR = pathlib.Path(args.prompts)

BASE_URL, KEY_VAR = provider(args.model)
for line in open("../.env"):
    if line.startswith(KEY_VAR):
        os.environ[KEY_VAR] = line.split("=", 1)[1].strip()
client = AsyncOpenAI(api_key=os.environ[KEY_VAR], base_url=BASE_URL,
                     timeout=TIMEOUT, max_retries=0)  # retry is ours, so it is counted

rows = list(csv.DictReader(open(args.units)))
if args.limit:
    rows = rows[: args.limit]
out_dir = pathlib.Path(args.out)
out_dir.mkdir(parents=True, exist_ok=True)


async def score(arm, system, r, sem, stat):
    """Score one unit, returning None if all attempts fail.

    Rate-limit errors get longer waits and more attempts than other errors.
    Randomize waits so concurrent requests do not all retry together.
    """
    binary, prob = FIELDS[arm]
    for attempt in range(max(RETRIES, RL_RETRIES)):
        async with sem:
            t0 = time.monotonic()
            try:
                resp = await client.chat.completions.create(
                    model=args.model,
                    messages=[{"role": "system", "content": system},
                              {"role": "user", "content": unit_block(r, arm)}],
                    response_format=schema(arm),
                    **({"reasoning_effort": args.reasoning_effort}
                       if args.reasoning_effort else {}),
                    **({"temperature": args.temperature}
                       if args.temperature is not None else {}),
                )
                ans = json.loads(resp.choices[0].message.content)
                us = resp.usage
                cached = getattr(us.prompt_tokens_details, "cached_tokens", 0) or 0
                stat["lat"].append(time.monotonic() - t0)
                stat["in"] += us.prompt_tokens
                stat["out"] += us.completion_tokens
                stat["cached"] += cached
                return {ID: r[ID], binary: int(ans[binary]), prob: ans[prob],
                        "in_tok": us.prompt_tokens, "out_tok": us.completion_tokens,
                        "cached_tok": cached, "lat": round(time.monotonic() - t0, 2),
                        "attempt": attempt}
            except Exception as e:
                name = type(e).__name__
                stat["err"][name] = stat["err"].get(name, 0) + 1
                # Count API and response-parsing errors rather than invent a score.
                # Rate limits use a separate retry budget and longer wait.
                rate_limited = name == "RateLimitError"
                if attempt >= (RL_RETRIES if rate_limited else RETRIES) - 1:
                    stat["lost"].append((r[ID], repr(e)[:200]))
                    return None
        # backoff OUTSIDE the semaphore, so a waiting request frees its slot
        await asyncio.sleep(random.uniform(*RL_WAIT) if rate_limited else 2 ** attempt)


async def run_arm(arm):
    system = (PROMPT_DIR / f"prompt_{arm}.txt").read_text()
    dest = out_dir / f"scores_{arm}.jsonl"
    done = set()
    if dest.exists():
        done = {json.loads(l)[ID] for l in open(dest) if l.strip()}
    todo = [r for r in rows if r[ID] not in done]
    print(f"\n=== {arm}: {len(todo)} to score ({len(done)} already landed), "
          f"concurrency {args.concurrency}", flush=True)
    if not todo:
        return

    # Start another unit whenever one finishes, so slow retries do not hold up a batch.
    # Limit pending tasks to bound memory while allowing some to wait between retries.
    sem = asyncio.Semaphore(args.concurrency)
    stat = {"lat": [], "in": 0, "out": 0, "cached": 0, "err": {}, "lost": []}
    t0, n = time.monotonic(), 0
    queue = iter(todo)
    launch = lambda r: asyncio.create_task(score(arm, system, r, sem, stat))

    def progress():
        el = max(time.monotonic() - t0, 1e-9)
        lat = f"{statistics.median(stat['lat']):.1f}s" if stat["lat"] else "n/a"
        print(f"  {n}/{len(todo)} | {n/el*60:.0f} req/min | "
              f"{(stat['in']+stat['out'])/el*60/1e6:.2f}M tok/min | p50 {lat} | "
              f"cache {stat['cached']/max(stat['in'],1):.0%} | "
              f"lost {len(stat['lost'])}", flush=True)

    with open(dest, "a") as f:
        pending = {launch(r) for r in itertools.islice(queue, args.concurrency * 4)}
        while pending:
            done, pending = await asyncio.wait(pending, return_when=asyncio.FIRST_COMPLETED)
            for t in done:
                rec = t.result()
                n += 1
                if rec:
                    f.write(json.dumps(rec) + "\n")
                nxt = next(queue, None)
                if nxt is not None:
                    pending.add(launch(nxt))
                if n % 1000 == 0:
                    f.flush()
                    progress()
        f.flush()
    if n % 1000:
        progress()

    el = time.monotonic() - t0
    lat = sorted(stat["lat"])
    print(f"--- {arm} done in {el/60:.1f} min")
    print(f"    throughput {n/el*60:.0f} req/min | "
          f"{(stat['in']+stat['out'])/el*60/1e6:.2f}M tok/min (ceiling 2.00)")
    if lat:
        print(f"    latency p50 {lat[len(lat)//2]:.1f}s  p90 {lat[int(.9*len(lat))]:.1f}s  "
              f"p99 {lat[int(.99*len(lat))]:.1f}s")
    print(f"    tokens in {stat['in']/1e6:.2f}M (cached {stat['cached']/max(stat['in'],1):.0%}) "
          f"out {stat['out']/1e6:.2f}M")
    p_in, p_out = PRICE[args.model]
    cost = stat["in"] / 1e6 * p_in + stat["out"] / 1e6 * p_out
    proj = (f" -> census x{CENSUS_N/max(n,1):.1f} = ${cost*CENSUS_N/max(n,1):.0f}, "
            f"{el/60*CENSUS_N/max(n,1)/60:.1f} h") if CENSUS_N else ""
    print(f"    cost ${cost:.2f}{proj}")
    if stat["err"]:
        print(f"    retried errors: {stat['err']}")
    if stat["lost"]:
        print(f"    LOST {len(stat['lost'])} after {RETRIES}/{RL_RETRIES} attempts "
              f"(transient/rate-limited): {stat['lost'][:3]}")


async def main():
    for arm in args.arms:
        await run_arm(arm)
    # Report every submitted unit still missing, including exhausted retries.
    print()
    submitted = {r[ID] for r in rows}
    for arm in args.arms:
        dest = out_dir / f"scores_{arm}.jsonl"
        got = {json.loads(l)[ID] for l in open(dest) if l.strip()}
        gap = submitted - got
        print(f"{arm}: {len(got)}/{len(submitted)} scored, {len(gap)} missing"
              + (f" -- rerun to fill: {sorted(gap)[:5]}" if gap else ""))


asyncio.run(main())

# Build PRISM judge inputs from each response and its prior conversation history.
# sat shows conversation text; sat_type also shows the conversation-type label.
# Neither includes participant attributes. Both request satisfaction and confidence.
# Run from code/: ../.venv/bin/python prism/build_prompts.py
# Writes ../data/prism/census_units.csv for judge/run_judge.py.

import pathlib

D = pathlib.Path("../data/prism")
ID = "unit"
CENSUS_N = 68371

FIELDS = {"sat": ("score", "confidence"), "sat_type": ("score", "confidence")}


def schema(arm):
    return {
        "type": "json_schema",
        "json_schema": {
            "name": "satisfaction_score",
            "strict": True,
            "schema": {
                "type": "object",
                "properties": {"score": {"type": "integer"},
                               "confidence": {"type": "integer"}},
                "required": ["score", "confidence"],
                "additionalProperties": False,
            },
        },
    }


def unit_block(r, arm):
    parts = []
    if arm == "sat_type":
        parts.append(f"Conversation type: {r['conversation_type']}")
    if r["history"]:
        parts.append(f"Conversation so far:\n{r['history']}")
    parts.append(f"User: {r['user_prompt']}")
    parts.append(f"Assistant response to score: {r['model_response']}")
    return "\n\n".join(parts)


if __name__ == "__main__":
    # Export from population.rds (via R), then
    # build the history column here and rewrite census_units.csv.
    import subprocess
    subprocess.run(["Rscript", "-e", """
        p <- readRDS('../data/prism/population.rds')
        write.csv(p[order(p$unit), c('unit', 'utterance_id', 'conversation_id',
                                     'turn', 'if_chosen', 'conversation_type',
                                     'model_name', 'user_prompt', 'model_response')],
                  '../data/prism/census_units_raw.csv', row.names = FALSE, na = '')
    """], check=True)
    import csv, collections
    rows = list(csv.DictReader(open(D / "census_units_raw.csv")))
    assert len(rows) == CENSUS_N and rows[0]["unit"] == "1"
    # History follows the responses the participant chose at earlier turns.
    # Other candidates at those turns must not appear in the conversation.
    chosen = {}  # (conversation_id, turn) -> (prompt, chosen response)
    for r in rows:
        if r["if_chosen"] == "TRUE":
            chosen[(r["conversation_id"], int(r["turn"]))] = (r["user_prompt"], r["model_response"])
    for r in rows:
        hist = []
        for t in range(int(r["turn"])):
            pr = chosen.get((r["conversation_id"], t))
            if pr:
                hist.append(f"User: {pr[0]}\nAssistant: {pr[1]}")
        r["history"] = "\n\n".join(hist)
    cols = ["unit", "utterance_id", "conversation_id", "turn", "conversation_type",
            "model_name", "history", "user_prompt", "model_response"]
    with open(D / "census_units.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)
    (D / "census_units_raw.csv").unlink()
    chars = sum(len(r["history"]) + len(r["user_prompt"]) + len(r["model_response"]) for r in rows)
    print(f"{len(rows)} units; ~{chars/4/1e6:.1f}M unit-text tokens with history (chars/4); "
          f"units with history: {sum(1 for r in rows if r['history'])}")

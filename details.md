# Details

Setup, data provenance, and how each figure and table is produced. See [`README.md`](README.md) for the layout.

## Setup

R 4.5.1, with the packages pinned in `renv.lock`:

```r
install.packages("renv")
renv::activate()
renv::restore()
```

`renv::activate()` matters: it writes the startup files that put the pinned library on the path. `code/.Rprofile` extends that to scripts run from `code/`, which is where they all run. Without activation, R silently uses whatever versions are installed.

Python 3.14, from the repository root:

```sh
python3.14 -m venv .venv
.venv/bin/pip install -r requirements.txt
```

Only the two ingest scripts need Python, plus the judge runner if you rerun the judge. Indirect dependencies (numpy and so on) are not pinned.

The simulations parallelize with `parallel::mclapply`, which forks, so they run serially on Windows.

## Data

### PRISM

PRISM (Kirk et al. 2024, arXiv:2404.16019) is not redistributed here. Download the two parquet files from HuggingFace (`HannahRoseKirk/prism-alignment`, CC BY-NC 4.0):

```sh
cd data/prism
curl -L -o prism_utterances.parquet https://huggingface.co/api/datasets/HannahRoseKirk/prism-alignment/parquet/utterances/train/0.parquet
curl -L -o prism_survey.parquet https://huggingface.co/api/datasets/HannahRoseKirk/prism-alignment/parquet/survey/train/0.parquet
cd ../../code
```

Then, from `code/`:

```sh
../.venv/bin/python prism/ingest.py   # -> data/prism/survey_flat.csv
Rscript prism/ingest.R                # -> data/prism/population.rds
```

This gives 68,371 rated responses from 1,396 participants in 63 domains.

The judge scores used in the paper ship in `data/prism/judge_census/scores_sat.jsonl` and `scores_sat_type.jsonl`, one record per response. They came from gpt-5-nano at low reasoning effort, with the system prompts in `data/prism/prompt_sat.txt` and `prompt_sat_type.txt`. Rerunning the judge is optional and needs an OpenAI API key in `.env` at the repository root (`OPENAI_API_KEY=...`):

```sh
../.venv/bin/python prism/build_prompts.py   # -> data/prism/census_units.csv
../.venv/bin/python judge/run_judge.py --units ../data/prism/census_units.csv \
    --prompts ../data/prism --out ../data/prism/judge_census \
    --unit-block prism.build_prompts --arms sat sat_type \
    --model gpt-5-nano --reasoning-effort low
```

A rerun will not reproduce the shipped scores exactly, because the API is not deterministic.

### Open LLM Leaderboard

The per-question correctness matrices come from Wu, Nair & Candès (2026, arXiv:2601.20251), released in [skbwu/efficiently-evaluating-llms](https://github.com/skbwu/efficiently-evaluating-llms) under Apache-2.0. `data/ollb/mixed.zip` holds `M1.csv` and `M2.csv` from their `data/processed/bbh+gpqa+ifeval+math+musr.zip` at commit `10f05ca`, unmodified. SHA-256:

```
c2e32da48f4a2ed5eed4b5c274ecffaefe511109f30d03eb5aa5abe0e19e371d  M1.csv
9c71b51ee15ed24d70232e2a9d60fafde379cb41eeb27868d0813afc6be6a3fc  M2.csv
```

From the repository root:

```sh
unzip data/ollb/mixed.zip -d data/ollb/mixed
cd code && ../.venv/bin/python ollb/ingest.py   # -> data/ollb/units_mixed.csv
```

## Reproducing the paper

Run the simulations first. Each writes `.rds` caches to `data/`, which the figure and table scripts read. Every script's header documents its environment variables; `CORES` sets parallelism.

```sh
cd code
Rscript paper_smoothing.R       # benchmark study       -> data/paper_smoothing_{10,20,30}_{math,weak,strong}.rds
Rscript paper_informative.R     # hidden selection      -> data/paper_informative_{05,10,20,30}.rds
Rscript paper_selection.R       # traffic replications  -> data/paper_selection_{10,20}.rds
Rscript paper_single_sample.R   # traffic single sample -> data/paper_single_sample_10.rds
```

Then:

```sh
Rscript paper_figures_selection.R
Rscript paper_figures_bench.R
Rscript paper_figures_traffic.R
Rscript paper_tables_bench.R
Rscript paper_tables_traffic.R
```

| Paper output | Script | Reads |
|---|---|---|
| Figure `fig:hidden-selection` (`informative_prism.pdf`) | `paper_figures_selection.R` | `paper_informative_*.rds` |
| Figure `fig:taxonomy` (`taxonomy_tree.pdf`) | `pdflatex paper/figures/taxonomy_tree.tex` | — |
| Table `tab:bench-main` (`bench_main.tex`) | `paper_tables_bench.R` | `paper_smoothing_*.rds` |
| Figure `fig:bench-waterfall` (`bench_waterfall.pdf`) | `paper_figures_bench.R` | `paper_smoothing_*.rds` |
| Appendix tables `bench_rmse.tex`, `bench_is.tex`, `bench_coverage.tex` | `paper_tables_bench.R` | `paper_smoothing_*.rds` |
| Figure `fig:traffic-single` (`traffic_single_sample.pdf`) | `paper_figures_traffic.R` | `paper_single_sample_10.rds` |
| Appendix table `traffic_truth.tex` | `paper_tables_traffic.R` | `paper_selection_*.rds`, `paper_single_sample_10.rds` |
| Tables `tab:traffic-field`, `tab:traffic-validators`, `tab:traffic-validators-app` | typeset by hand | `paper_single_sample_10.rds`, `paper_selection_*.rds` |

Figures are written to `paper/figures/`, overwriting the shipped PDFs, and tables to `paper/tables/`.

Each replicate is seeded (`seed 100 + r`), so a shorter run reproduces the first replicates of a long one exactly.

### Shorter runs

`REPS=3` runs three replicates instead of 100. Two wrinkles:

- Runs with `REPS` under 100 write caches with a `smoke_` prefix, which the figure and table scripts do not read. Rename or copy them to compare against the full results.
- `paper_selection.R` and `paper_single_sample.R` always write to `../data`, ignoring `OUT`, and `paper_single_sample.R` has no shortened mode.

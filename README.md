# disagg-ai-eval

Code for *Prediction-Powered Smoothing and Validation for Disaggregated AI Evaluation*.

The paper estimates AI evaluation metrics within many small domains from a limited set of labels. It combines direct estimators (Horvitz–Thompson, prediction-powered inference, GREG) with Fay–Herriot smoothing and taxonomy smoothing, then selects among them by design-based cross-validation. Two studies: a curated benchmark (the Open LLM Leaderboard) and deployed agent traffic (PRISM).

[`details.md`](details.md) has the full setup, data provenance, and the map from each figure and table to the script that makes it.

## Layout

All scripts run from `code/`, reading and writing through `../data` and `../paper`.

| Path | Contents |
|---|---|
| `code/R/` | Estimators and validation: populations and domains, designs, direct/PPI/GREG, Fay–Herriot and taxonomy smoothing, scoring, design-based cross-validation. `run.R` sources the rest. |
| `code/prism/`, `code/judge/` | PRISM ingest, population loader, judge prompts and runner. |
| `code/ollb/`, `code/mixedbench/` | Open LLM Leaderboard ingest and benchmark population loader. |
| `code/tests/` | Tests for estimators, sampling designs, covariance and CV, taxonomy smoothing, and the driver. |
| `code/paper_*.R` | Simulations (write caches to `data/`) and the figure and table scripts that read them. |
| `data/` | Judge prompts and scores; the benchmark matrices, zipped. |
| `paper/figures/` | The paper's four figures, and the taxonomy figure's TikZ source. |

## Quick start

```sh
Rscript -e 'install.packages("renv"); renv::activate(); renv::restore()'
python3.14 -m venv .venv && .venv/bin/pip install -r requirements.txt
```

Then follow the data and reproduction steps in [`details.md`](details.md). PRISM is downloaded from HuggingFace; the benchmark matrices ship here.

## Tests

After restoring the R environment, run the tests on synthetic data:

```sh
cd code
Rscript tests/run_all.R
```

No dataset download or API key is required.

## License

Apache-2.0 (`LICENSE`). The judge scores in `data/prism/judge_census/` derive from PRISM and stay under CC BY-NC 4.0, so they may not be used commercially. `data/ollb/mixed.zip` is redistributed under Apache-2.0 (`data/ollb/LICENSE`).

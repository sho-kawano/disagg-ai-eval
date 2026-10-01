# PRISM Alignment survey table -> flat CSV. Run once, before ingest.R.
#
# The survey parquet has struct columns (ethnicity, location, ...) that
# nanoparquet cannot read, so this one step is Python. Everything downstream
# is R.
#
# Source (CC-BY-NC 4.0, no auth): HuggingFace HannahRoseKirk/prism-alignment,
#   https://huggingface.co/api/datasets/HannahRoseKirk/prism-alignment/parquet/utterances/train/0.parquet
#   https://huggingface.co/api/datasets/HannahRoseKirk/prism-alignment/parquet/survey/train/0.parquet
# Save them as data/prism/prism_{utterances,survey}.parquet.
#
# Struct handling: ethnicity -> the 'simplified' key; location -> reside_country
# and special_region. Raw self-described strings are dropped.
#
# Run from code/:  ../.venv/bin/python prism/ingest.py

import pyarrow.parquet as pq

s = pq.read_table("../data/prism/prism_survey.parquet").to_pandas()
flat = s[["user_id", "age", "gender", "education", "study_locale",
          "english_proficiency", "lm_familiarity",
          "num_completed_conversations"]].copy()
flat["ethnicity"] = s.ethnicity.apply(lambda d: d["simplified"])
flat["reside_country"] = s.location.apply(lambda d: d["reside_country"])
flat["special_region"] = s.location.apply(lambda d: d["special_region"])

assert len(flat) == 1500 and flat.user_id.is_unique
flat.to_csv("../data/prism/survey_flat.csv", index=False)
print(len(flat), "participants ->", "../data/prism/survey_flat.csv")

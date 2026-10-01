# Open LLM Leaderboard corpus (Wu, Nair & Candes, arXiv 2601.20251,
# github.com/skbwu/efficiently-evaluating-llms) -> unit table.
# Run once from code/: .venv python ollb/ingest.py. Reads data/ollb/mixed/M*.csv
# (their processed M1/M2 matrices, unzipped from data/ollb/mixed.zip),
# writes data/ollb/units_mixed.csv consumed by ollb/load.R.
#
# Column order: columns are named {subtask}_{i}.

import pandas as pd

DATA = "../data/ollb"

# truth candidates (M2 = newer half) and cheap auxiliary models (M1 = historical half).
TRUTH = ["microsoft__phi-4",
         "deepseek-ai__DeepSeek-R1-Distill-Llama-70B",
         "meta-llama__Llama-3.1-8B-Instruct"]
AUX = ["meta-llama__Llama-3.2-1B-Instruct",
       "Qwen__Qwen2.5-1.5B-Instruct"]

META = ("model", "created_date", "sha")

def rows(path, models):
    df = pd.read_csv(path)
    df = df[df.model.isin(models)].set_index("model")
    return df[[c for c in df.columns if c not in META[1:]]].astype(int)

m1 = pd.read_csv(f"{DATA}/mixed/M1.csv")
qcols = [c for c in m1.columns if c not in META]
out = pd.DataFrame({"unit": qcols})
sub = out.unit.str.rsplit("_", n=1).str[0]
out["benchmark"] = sub.str.split("_").str[0]
out["subtask"] = sub
out["difficulty"] = m1[qcols].mean().values          # historical item difficulty
tr = rows(f"{DATA}/mixed/M2.csv", TRUTH)
ax = m1[m1.model.isin(AUX)].set_index("model")[qcols].astype(int)
for m in TRUTH:
    out["y__" + m] = tr.loc[m].values
for m in AUX:
    out["aux__" + m] = ax.loc[m].values
out.to_csv(f"{DATA}/units_mixed.csv", index=False)
print("mixed", out.shape)

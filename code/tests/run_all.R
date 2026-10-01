# Run the test suite from code/: Rscript tests/run_all.R
# 1. Load the pipeline and test helpers.
# 2. Run each test file, stopping at the first failure.

# ---- 1. Load the pipeline and test helpers. ----
source("R/run.R")
source("tests/helper.R")

# ---- 2. Run each test file, stopping at the first failure. ----
tests <- c("test_estimators.R", "test_designs.R", "test_validation.R",
           "test_ts.R", "test_run.R")
for (t in tests) {
  cat("\n== ", t, " ==\n", sep = "")
  source(file.path("tests", t))
}
cat("\nALL TESTS PASSED\n")

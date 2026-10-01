# Load Open LLM Leaderboard questions with one model's correctness as truth.
# Domains are benchmark/subtask combinations; judge selects a population auxiliary
# column such as difficulty or aux__<model>. Run ollb/ingest.py to build the CSV.
# Source: Wu, Nair & Candes, arXiv 2601.20251.
# The paper uses phi-4; the R1 distill's MATH results require checking answer extraction.

source("R/population.R")

ollb_population <- function(truth = "microsoft__phi-4", judge = NULL,
                            csv = "../data/ollb/units_mixed.csv") {
  units <- read.csv(csv, check.names = FALSE)
  stopifnot(paste0("y__", truth) %in% names(units))
  units$y <- units[[paste0("y__", truth)]]
  if (!is.null(judge)) units$judge <- units[[judge]]   # "difficulty" or an aux__ column
  new_population(units, domain_vars = c("benchmark", "subtask"))
}

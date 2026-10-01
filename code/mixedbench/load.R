# Apply the paper's domain definitions to the Open LLM Leaderboard population.
# The default rule excludes temporal sequences and merges advanced MATH subtasks.
# rule = "raw" keeps the original domains from ollb_population().

source("ollb/load.R")

MATH_ADV <- c("math_geometry_hard", "math_precalculus_hard",
              "math_intermediate_algebra_hard", "math_counting_and_prob_hard")

mixedbench_population <- function(..., rule = "v2") {
  pop <- ollb_population(...)
  if (rule == "raw") return(pop)
  # Temporal sequences has constant outcomes for the paper's evaluated model.
  u <- pop$units[pop$units$subtask != "bbh_temporal_sequences", ]
  # Pool related advanced MATH topics to reduce sparse positive labels per domain.
  u$subtask[u$subtask %in% MATH_ADV] <- "math_advanced_hard"
  new_population(u, domain_vars = c("benchmark", "subtask"))
}

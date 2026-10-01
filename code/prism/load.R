# Load rated PRISM responses, with domains defined by model and conversation type.
# y is the participant's satisfaction score on its original 1-100 scale.
# Optional scores supplies a judge JSONL file joined by unit ID; otherwise judge is NA.
# Prepare population.rds with prism/ingest.py followed by prism/ingest.R.

source("R/population.R")

prism_population <- function(rds = "../data/prism/population.rds", floor_alt = 200,
                             scores = NULL) {
  units <- readRDS(rds)
  units$model <- units$model_name
  units$conv_type <- units$conversation_type
  vars <- c("model", "conv_type")

  if (!is.null(scores)) {
    sc <- jsonlite::stream_in(file(scores), verbose = FALSE)
    units$judge <- sc$score[match(units$unit, sc$unit)]
    stopifnot(!anyNA(units$judge))
  }
  # Store an alternative locale/age grouping without changing the reporting domains.
  # Merge small age groups within locales, then merge remaining small locales.
  if (!is.null(floor_alt)) {
    loc <- units$study_locale
    age <- units$age
    for (step in 1:2) {
      key <- paste(loc, age, sep = " / ")
      small <- key %in% names(which(table(key) < floor_alt))
      age[small] <- "OTHER ages"
      if (step == 2) loc[small] <- "OTHER"
    }
    units$domain_alt <- paste(loc, age, sep = " / ")
  }
  new_population(units, domain_vars = vars)
}

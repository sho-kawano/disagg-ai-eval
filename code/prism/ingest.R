# Join PRISM response ratings to participant covariates, keeping one row per response.
# y is the participant's satisfaction score; if_chosen marks the response continued.
# Prior conversation history is added separately by prism/build_prompts.py.
# Run from code/ after prism/ingest.py; writes ../data/prism/population.rds.

library(data.table)

u <- as.data.table(nanoparquet::read_parquet("../data/prism/prism_utterances.parquet"))
s <- fread("../data/prism/survey_flat.csv")

# Group education categories while retaining the original answers in education_raw.
edu_map <- c("Some Primary" = "Secondary or less",
             "Completed Primary School" = "Secondary or less",
             "Some Secondary" = "Secondary or less",
             "Completed Secondary School" = "Secondary or less",
             "Vocational" = "Vocational",
             "Some University but no degree" = "Some university",
             "University Bachelors Degree" = "Bachelors",
             "Graduate / Professional degree" = "Graduate",
             "Prefer not to say" = "Prefer not to say")
s[, education_raw := education]
s[, education := edu_map[education_raw]]
stopifnot(!anyNA(s$education))

pop <- merge(u, s, by = "user_id", all.x = TRUE)
stopifnot(nrow(pop) == 68371, !anyNA(pop$age), !anyNA(pop$score))

pop[, resp_chars := nchar(model_response)]
pop[, resp_words := lengths(gregexpr("\\S+", model_response))]
setnames(pop, "score", "y")
pop[, unit := .I]

cat(nrow(pop), "utterances;", uniqueN(pop$user_id), "participants;",
    uniqueN(pop$conversation_id), "conversations\n")
cat("primary domains (model_name x conversation_type):",
    uniqueN(pop[, paste(model_name, conversation_type)]), "\n")
saveRDS(as.data.frame(pop), "../data/prism/population.rds")

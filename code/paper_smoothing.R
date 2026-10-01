# Compare direct, PPI/GREG, FH, and taxonomy estimates on the benchmark.
# 1. Set sampling budgets, MCMC controls, judges, and estimators.
# 2. Load each judge and build intercept-only and judge-mean linking models.
# 3. Define one stratified-sample replication and run it at each budget.
# 4. Save results and summarize error, coverage, and excluded domains.
#    4.1. Save results and simulation settings.
#    4.2. Report mean error and coverage across replications.
#    4.3. Report excluded-domain diagnostics.
# Run from code/: Rscript paper_smoothing.R
# Writes ../data/paper_smoothing_<fraction>_<arm>.rds; REPS < 100 adds smoke_.
# Environment settings: REPS, CORES, FRACS, OUT, NDESIRED, NBURN.


library(dplyr)
library(parallel)
source("R/run.R")
source("mixedbench/load.R")

# ---- 1. Set sampling budgets, MCMC controls, judges, and estimators. ----
REPS  <- as.integer(Sys.getenv("REPS", "100"))
SMOKE <- REPS < 100
CORES <- as.integer(Sys.getenv("CORES", if (SMOKE) "2" else "10"))
CTRL  <- fh_control(ndesired = as.integer(Sys.getenv("NDESIRED", 2000)),
                    nburn    = as.integer(Sys.getenv("NBURN", 1000)))
FRACS <- as.numeric(strsplit(Sys.getenv("FRACS", "0.10,0.20,0.30"), ",")[[1]])
OUT   <- Sys.getenv("OUT", "../data")
# The math judge is a model row in M1.csv, with questions in columns.
# Its answers are attached below by question ID; the other judges are already
# columns in the units table. NA in ARMS marks this separate loading path.
M1_ARMS <- c(math = "Qwen__Qwen2.5-Math-7B-Instruct")
# Compare historical mean correctness (strong), the math specialist (math),
# and the general-purpose small model (weak).
ARMS <- c(strong = "difficulty", math = NA,
          weak = "aux__Qwen__Qwen2.5-1.5B-Instruct")
LADDER <- c("naive_judge", "direct", "ppi", "ppi_tuned", "fh", "pp_s",
            "ts", "ts_greg")

# ---- 2. Load each judge and build intercept-only and judge-mean linking models. ----
m1 <- NULL
for (arm in names(ARMS)) {
  if (arm %in% names(M1_ARMS)) {
    pop <- mixedbench_population(truth = "microsoft__phi-4", judge = "difficulty")
    if (is.null(m1)) m1 <- read.csv("../data/ollb/mixed/M1.csv", check.names = FALSE)
    judge_predictions <- m1 |>
      filter(model == M1_ARMS[[arm]]) |>
      dplyr::select(all_of(pop$units$unit))
    pop$units <- pop$units |>
      mutate(judge = as.numeric(judge_predictions))
    stopifnot(!anyNA(pop$units$judge))
  } else {
    pop <- mixedbench_population(truth = "microsoft__phi-4", judge = ARMS[[arm]])
  }
  X <- list(intercept = domain_design_matrix(pop, means = NULL),
            judge     = domain_design_matrix(pop, means = "judge"))

  cat(sprintf("mixedbench [%s]: %d units, %d domains | corr(judge, y) %.3f\n", arm,
              nrow(pop$units), nrow(pop$domains), cor(pop$units$judge, pop$units$y)))
  th <- pop$domains$theta_true
  for (sp in names(X))
    cat(sprintf("linking R^2 [%-9s] p = %2d  R2 = %.3f\n", sp, ncol(X[[sp]]),
                1 - sum(resid(lm(th ~ X[[sp]] - 1))^2) / sum((th - mean(th))^2)))
  # Report the population regression slope as a reference for PPI's fixed slope of one.
  lam_pop <- coef(lm(y ~ judge, data = pop$units))["judge"]
  cat(sprintf("population tuned slope %.3f\n", lam_pop))

  # ---- 3. Define one stratified-sample replication and run it at each budget. ----
  one_rep <- function(r, budget) {
    tr <- Sys.time()
    s <- draw_sample(pop, stratified(budget, n_min = 10), seed = 100 + r)
    res <- run_one(pop, s, CTRL, estimators = LADDER, fh_X = X, ts_X = X)
    res <- res |> mutate(rep = r)
    list(metrics = res,
         secs = as.numeric(difftime(Sys.time(), tr, units = "secs")))
  }
  for (f in FRACS) {
    budget <- round(f * nrow(pop$units))
    t0 <- Sys.time()
    reps <- mclapply(seq_len(REPS), one_rep, budget = budget, mc.cores = CORES)
    bad <- vapply(reps, inherits, logical(1), "try-error")
    if (any(bad)) stop(f, ": reps failed: ", paste(which(bad), collapse = ", "),
                       " -- first error: ", reps[[which(bad)[1]]])

    # ---- 4. Save results and summarize error, coverage, and excluded domains. ----
    # ---- 4.1. Save results and simulation settings. ----
    out <- list(metrics = bind_rows(lapply(reps, `[[`, "metrics")),
                frac = f, budget = budget, reps = REPS, control = CTRL,
                corpus = "mixedbench", arm = arm,
                judge_col = if (arm %in% names(M1_ARMS)) M1_ARMS[[arm]] else ARMS[[arm]],
                specs = lapply(X, colnames),
                judge_corr = cor(pop$units$judge, pop$units$y),
                lambda_pop = as.numeric(lam_pop),
                rep_secs = vapply(reps, `[[`, numeric(1), "secs"),
                total_secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
    saveRDS(out, sprintf("%s/%spaper_smoothing_%02d_%s.rds", OUT,
                         if (SMOKE) "smoke_" else "", round(100 * f), arm))
    cat(sprintf("[paper-smooth %s %2d%%] budget %5d | %5.1f min | per-rep %5.1f s\n",
                arm, round(100 * f), budget, out$total_secs / 60,
                mean(out$rep_secs)))

    # ---- 4.2. Report mean error and coverage across replications. ----
    m <- out$metrics
    agg <- function(metric) {
      m |>
        filter(.data$metric == .env$metric) |>
        group_by(estimator) |>
        summarise(value = mean(value), .groups = "drop") |>
        pull(value, name = estimator)
    }
    for (metric in c("rmse", "IS")) {
      v <- agg(metric)
      cat(sprintf("%s ratio to direct:\n", metric))
      print(round(v / v["direct"], 3))
    }
    cat("coverage:\n")
    print(round(agg("coverage"), 3))

    # ---- 4.3. Report excluded-domain diagnostics. ----
    est <- agg("estimable")
    masked_reps <- m |>
      filter(metric == "estimable", estimator == "fh_intercept") |>
      summarise(fraction = mean(value < 1)) |>
      pull(fraction)
    cat(sprintf("masking: mean masked share %.3f | reps with any mask %.2f\n",
                1 - est["fh_intercept"], masked_reps))
  }
}
cat("PAPER SMOOTHING DONE\n")

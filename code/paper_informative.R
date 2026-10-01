# Simulate labels arriving more often for rejected PRISM responses.
# 1. Set up the population, sampling designs, and estimators.
# 2. Define one replication comparing stratified and informative samples.
#    2.1. Draw stratified and informative samples.
#    2.2. Fit and score estimators under each analysis.
#    2.3. Measure selection bias and prediction calibration.
# 3. Run each budget, save results, and summarize error and selection bias.
# Run from code/: Rscript paper_informative.R
# Writes ../data/paper_informative_<fraction>.rds; REPS < 100 adds smoke_.
# Environment settings: REPS, CORES, FRACS, OUT.


library(dplyr)
library(parallel)
source("R/run.R")
source("prism/load.R")

# ---- 1. Set up the population, sampling designs, and estimators. ----
REPS  <- as.integer(Sys.getenv("REPS", "100"))
SMOKE <- REPS < 100
CORES <- as.integer(Sys.getenv("CORES", if (SMOKE) "2" else "10"))
CTRL  <- fh_control(ndesired = 100, nburn = 50)   # unused (no FH); run_one arg
FRACS <- as.numeric(strsplit(Sys.getenv("FRACS", "0.05,0.10,0.20,0.30"), ",")[[1]])
OUT   <- Sys.getenv("OUT", "../data")

# A negative tilt makes rejected responses more likely to be labelled.
TILT <- list(col = "chosen", t = -0.30)
BASE <- c("direct", "ppi", "ppi_tuned")
INF  <- c("direct", "ppi_unweighted", "ppi_tuned_unweighted")
pop <- prism_population(scores = "../data/prism/judge_census/scores_sat.jsonl")
pop$units <- pop$units |> mutate(chosen = as.numeric(if_chosen))
ybar <- mean(pop$units$y); fbar <- mean(pop$units$judge)
fbar_i <- tapply(pop$units$judge, pop$units$domain, mean)
theta  <- setNames(pop$domains$theta_true, as.character(pop$domains$domain))

cat(sprintf("PRISM: %d units, %d domains | corr(judge,y) %.3f | chosen %.3f (mean y %.1f vs %.1f)\n",
            nrow(pop$units), nrow(pop$domains),
            cor(pop$units$judge, pop$units$y), mean(pop$units$chosen),
            mean(pop$units$y[pop$units$chosen == 1]),
            mean(pop$units$y[pop$units$chosen == 0])))

# ---- 2. Define one replication comparing stratified and informative samples. ----
one_rep <- function(r, budget) {
  tr <- Sys.time()
  # ---- 2.1. Draw stratified and informative samples. ----
  samples <- list(
    baseline = draw_sample(pop, stratified(budget, n_min = 10), seed = 100 + r),
    informative = draw_sample(pop, informative(budget, tilt = TILT$col,
                                               form = "exp", t = TILT$t),
                              seed = 100 + r))
  metrics <- list()
  design <- list()
  for (arm in names(samples)) {
    # ---- 2.2. Fit and score estimators under each analysis. ----
    drawn_sample <- samples[[arm]]
    effective_n <- sum(drawn_sample$w)^2 / sum(drawn_sample$w^2)

    # The baseline uses design weights; the informative analysis ignores selection.
    analysis_sample <- if (arm == "baseline") drawn_sample else design_naive(drawn_sample)
    estimator_tags <- if (arm == "baseline") BASE else INF
    metrics[[arm]] <- run_one(pop, analysis_sample, CTRL, estimators = estimator_tags) |>
      mutate(rep = r, arm = .env$arm)

    # ---- 2.3. Measure selection bias and prediction calibration. ----
    domain_ids <- as.character(pop$domains$domain)
    # The fitted slope measures how the analysis calibrates judge predictions.
    fitted_slope <- coef(lm(y ~ judge, data = analysis_sample, weights = w))["judge"]
    sample_counts <- table(analysis_sample$domain)[domain_ids]
    usable_domains <- !is.na(sample_counts) & sample_counts >= 2
    sample_outcome_means <- tapply(analysis_sample$y, analysis_sample$domain, mean)[domain_ids]
    sample_judge_means <- tapply(analysis_sample$judge, analysis_sample$domain, mean)[domain_ids]
    # Inclusion-outcome correlation and mean gaps diagnose sample selection.
    included <- as.integer(pop$units$unit %in% analysis_sample$unit)
    # Domain mean gaps use only domains with at least two sampled units.
    design[[arm]] <- data.frame(
      rep = r, arm = arm, n = nrow(analysis_sample), n_w = effective_n,
      ddc = cor(included, pop$units$y),
      bias_raw = mean(analysis_sample$y) - ybar,
      jgap = mean(analysis_sample$judge) - fbar,
      bias_domain = mean(sample_outcome_means[usable_domains] - theta[domain_ids][usable_domains]),
      jgap_domain = mean(sample_judge_means[usable_domains] - fbar_i[domain_ids][usable_domains]),
      lambda_hat = as.numeric(fitted_slope),
      domains_empty = sum(!(domain_ids %in% as.character(analysis_sample$domain))))
  }
  list(metrics = bind_rows(unname(metrics)),
       design  = bind_rows(unname(design)),
       secs = as.numeric(difftime(Sys.time(), tr, units = "secs")))
}

# ---- 3. Run each budget, save results, and summarize error and selection bias. ----
for (f in FRACS) {
  budget <- round(f * nrow(pop$units))
  t0 <- Sys.time()
  reps <- mclapply(seq_len(REPS), one_rep, budget = budget, mc.cores = CORES)
  bad <- vapply(reps, inherits, logical(1), "try-error")
  if (any(bad)) stop(f, ": reps failed: ", paste(which(bad), collapse = ", "),
                     " -- first error: ", reps[[which(bad)[1]]])
  out <- list(metrics = bind_rows(lapply(reps, `[[`, "metrics")),
              design  = bind_rows(lapply(reps, `[[`, "design")),
              frac = f, budget = budget, reps = REPS, corpus = "prism",
              tilt = TILT,
              judge_corr = cor(pop$units$judge, pop$units$y),
              rep_secs = vapply(reps, `[[`, numeric(1), "secs"),
              total_secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  saveRDS(out, sprintf("%s/%spaper_informative_%02d.rds", OUT,
                       if (SMOKE) "smoke_" else "", round(100 * f)))
  cat(sprintf("[paper-inf %2d%%] budget %5d | %5.1f min | per-rep %4.1f s\n",
              round(100 * f), budget, out$total_secs / 60, mean(out$rep_secs)))
  m <- out$metrics; d <- out$design
  agg <- function(a, met) {
    m |>
      filter(arm == .env$a, metric == .env$met) |>
      group_by(estimator) |>
      summarise(value = mean(value), .groups = "drop") |>
      pull(value, name = estimator)
  }
  informative_summary <- d |>
    filter(arm == "informative") |>
    summarise(across(
      c(ddc, n_w, n, bias_raw, jgap, bias_domain, jgap_domain, lambda_hat), mean))
  baseline_summary <- d |>
    filter(arm == "baseline") |>
    summarise(across(c(ddc, lambda_hat), mean))
  cat(sprintf("informative DDC %+.4f | n_w %4.0f of n %5.0f | y-bias %+.3f | jgap %+.3f | domain: y %+.3f f %+.3f | lam-hat %.3f\n",
              informative_summary$ddc, informative_summary$n_w,
              informative_summary$n, informative_summary$bias_raw,
              informative_summary$jgap, informative_summary$bias_domain,
              informative_summary$jgap_domain,
              informative_summary$lambda_hat))
  cat(sprintf("baseline DDC %+.4f | lam-hat %.3f\n",
              baseline_summary$ddc,
              baseline_summary$lambda_hat))
  for (met in c("bias", "rmse", "coverage"))
    for (a in c("informative", "baseline")) {
      cat(sprintf("%-11s %-8s: ", a, met)); print(round(agg(a, met), 4))
    }
}
cat("PAPER INFORMATIVE DONE\n")

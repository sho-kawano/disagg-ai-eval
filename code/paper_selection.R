# Compare debiased CV, naive CV, and separate hold-out samples on PRISM.
# 1. Set up the population, covariates, and seven candidate estimators.
# 2. Define one replication comparing CV and hold-out scores.
# 3. Define summaries of selection accuracy and interval performance.
# 4. Run each budget, save results, and report performance against truth.
#    4.1. Run replications for the sampling budget.
#    4.2. Save results and simulation settings.
#    4.3. Report candidate estimation and interval performance.
#    4.4. Compare CV and hold-out selection performance.
# Run from code/: Rscript paper_selection.R
# Writes ../data/paper_selection_<fraction>.rds; REPS < 100 adds smoke_.
# Environment settings: REPS, FRACS, CORES.

library(dplyr)
library(parallel)
source("R/run.R")
source("prism/load.R")

# ---- 1. Set up the population, covariates, and seven candidate estimators. ----
CORES  <- as.integer(Sys.getenv("CORES", "8"))
REPS   <- as.integer(Sys.getenv("REPS", "100"))
SMOKE  <- REPS < 100
FRACS  <- as.numeric(strsplit(Sys.getenv("FRACS", "0.10,0.20"), ",")[[1]])
SPLITS <- c(0.8, 0.5)
K      <- 5
CTRL   <- fh_control(ndesired = 2000, nburn = 1000)
pop <- prism_population(scores = "../data/prism/judge_census/scores_sat.jsonl")
pop$units <- pop$units |>
  mutate(
    log_resp_chars = log1p(resp_chars),
    turn0 = as.integer(turn == 0),
    chosen = as.integer(if_chosen)
  )
domains <- pop$domains$domain
q <- pop$domains$N / sum(pop$domains$N)
theta_true <- pop$domains$theta_true
CONTENT <- c("log_resp_chars", "turn0", "chosen")
XS <- list(judge = domain_design_matrix(pop, means = "judge"),
           full  = domain_design_matrix(pop, means = c("judge", CONTENT)))
F_JUDGE <- ~ judge
F_FULL  <- as.formula(paste("~ judge +", paste(CONTENT, collapse = " + ")))
tsf <- make_ts_fitter(pop)
# Build a candidate that smooths GREG estimates from the unit-level formula fml.
# Return posterior means and intervals, plus the covariance needed for CV debiasing.
greg_model <- function(fitter, X, fml = F_FULL) function(pop, sub, control) {
  inp <- greg_estimates(pop, sub, fml) |>
    mask_by_direct(direct_estimates(sub), domains) |>
    align_to_domains(domains)
  f <- fitter(X, inp$y, inp$d, control)
  v <- apply(f$theta, 2, var)
  wj <- lm(update(fml, y ~ .), data = sub, weights = w)
  cc <- as.numeric(cov_direct_residual(sub, predict(wj, newdata = sub), domains))
  # v / inp$d approximates the smoother's weight on its own domain input.
  # Multiplying by cc gives its covariance with the direct estimate.
  list(point = f$point, var = v, cov = v * cc / inp$d,
       lo = apply(f$theta, 2, quantile, .025), hi = apply(f$theta, 2, quantile, .975))
}
MODELS <- c(
  cv_models("direct"),
  list(greg_judge = cv_models("greg", greg_formula = F_JUDGE)$greg,
       greg_full  = cv_models("greg", greg_formula = F_FULL)$greg,
       pps_judge  = greg_model(fit_fh_xy, XS$judge, F_JUDGE),
       pps_full   = greg_model(fit_fh_xy, XS$full,  F_FULL),
       ppts_judge = greg_model(tsf,       XS$judge, F_JUDGE),
       ppts_full  = greg_model(tsf,       XS$full,  F_FULL)))

# Use posterior quantiles when the fit supplies them. Otherwise construct
# normal 95% intervals from the candidate's point estimate and variance.
bounds <- function(f) if (!is.null(f$lo)) cbind(lo = f$lo, hi = f$hi) else
  cbind(lo = f$point - 1.96 * sqrt(f$var), hi = f$point + 1.96 * sqrt(f$var))

# ---- 2. Define one replication comparing CV and hold-out scores. ----
one_rep <- function(r, budget) {
  s <- draw_sample(pop, stratified(budget, n_min = 10), seed = 100 + r)
  cv <- cv_compare(pop, s, MODELS, K = K, control = CTRL, seed = r)
  debiased_scores <- cv_debias(cv)

  # Compare all candidates on the same domains, with valid scores and direct variances.
  all_scores_finite <- apply(is.finite(debiased_scores), 1, all)
  scorable_domains <- all_scores_finite & is.finite(cv$d_full)
  domain_weights <- q[scorable_domains]
  weighted_score <- function(domain_scores) {
    scores <- domain_scores[scorable_domains]
    sum(domain_weights * scores) / sum(domain_weights)
  }

  # Each column is one candidate; average its domain scores using traffic shares.
  score <- apply(debiased_scores, 2, weighted_score)
  score_naive <- apply(cv$domain_naive, 2, weighted_score)
  score_adj <- apply(cv$domain_score, 2, weighted_score)  # Dong-Li, before debiasing

  # Assess full-sample estimates against known truth over all population domains.
  oracle_error <- function(estimates) {
    squared_errors <- (estimates - theta_true)^2
    sum(q * squared_errors)
  }
  orc_all <- apply(cv$point_full, 2, oracle_error)
  # per-domain full-sample estimates, 95% bounds and sd (design sd for the direct
  # candidates, posterior sd for the smoothers), every candidate (domains x models)
  B <- lapply(cv$fullfit, bounds)
  lo <- sapply(B, `[`, , "lo"); hi <- sapply(B, `[`, , "hi"); sd <- sqrt(cv$post_var)
  dimnames(lo) <- dimnames(hi) <- dimnames(sd) <- list(as.character(domains), names(MODELS))
  split <- list()
  for (sp in SPLITS) {
    # Draw training and validation samples separately; they may share units.
    s1 <- draw_sample(pop, stratified(round(sp * budget), n_min = 10), seed = 100 + r)
    s2 <- draw_sample(pop, stratified(round((1 - sp) * budget), n_min = 10),
                      seed = 7000 + r)
    est1 <- sapply(MODELS, function(f) f(pop, s1, CTRL)$point)
    estp <- sapply(MODELS, function(f)
      f(pop, pool_stratified_samples(pop, s1, s2), CTRL)$point)
    stopifnot(all(is.finite(est1)), all(is.finite(estp)))
    yh <- align_to_domains(direct_estimates(s2), domains)$y
    ok <- is.finite(yh)
    split[[sprintf("%02d", round(100 * sp))]] <- list(
      hold = apply(est1, 2, function(p) sum(q[ok] * (p[ok] - yh[ok])^2) / sum(q[ok])),
      oracle_pooled = apply(estp, 2, function(p) sum(q * (p - theta_true)^2)),
      n_hold_domains = sum(ok), n_overlap = sum(s2$unit %in% s1$unit))
  }
  list(rep = r, score = score, score_naive = score_naive, score_adj = score_adj,
       oracle = orc_all, selected = names(which.min(score)), n_domains = sum(scorable_domains),
       point = cv$point_full, lo = lo, hi = hi, sd = sd, split = split)
}

# ---- 3. Define summaries of selection accuracy and interval performance. ----
grade_arm <- function(label, score, oracle) {
  sel <- colnames(score)[apply(score, 1, which.min)]
  best <- names(which.min(colMeans(oracle)))
  r_sel <- sqrt(oracle[cbind(seq_len(nrow(oracle)), match(sel, colnames(oracle)))])
  cat(sprintf("  %-26s hit %.2f | selected RMSE %.4f | best (%s) %.4f\n",
              label, mean(sel == best), mean(r_sel), best,
              mean(sqrt(oracle[, best]))))
  sel
}

# traffic-share-weighted interval score, coverage and width of one rep's bounds
grade_intervals <- function(x, alpha = 0.05) {
  is_ <- (x$hi - x$lo) + (2 / alpha) * pmax(x$lo - theta_true, 0) +
    (2 / alpha) * pmax(theta_true - x$hi, 0)
  cv_ <- (theta_true >= x$lo & theta_true <= x$hi)
  rbind(IS = colSums(q * is_), coverage = colSums(q * cv_), width = colSums(q * (x$hi - x$lo)))
}

# ---- 4. Run each budget, save results, and report performance against truth. ----
for (f in FRACS) {
  # ---- 4.1. Run replications for the sampling budget. ----
  budget <- round(f * nrow(pop$units))
  t0 <- Sys.time()
  done <- mclapply(seq_len(REPS), one_rep, budget = budget, mc.cores = CORES)
  bad <- vapply(done, inherits, logical(1), "try-error")
  if (any(bad)) stop(f, ": reps failed ", paste(which(bad), collapse = ","),
                     " -- ", done[[which(bad)[1]]])
  mins <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

  # ---- 4.2. Save results and simulation settings. ----
  CACHE <- sprintf("../data/%spaper_selection_%02d.rds",
                   if (SMOKE) "smoke_" else "", round(100 * f))
  saveRDS(list(done = done, frac = f, reps = REPS, K = K, splits = SPLITS,
               control = CTRL, models = names(MODELS), minutes = mins, cores = CORES,
               inputs = list(judge = deparse(F_JUDGE), full = deparse(F_FULL)),
               specs = lapply(XS, colnames)), CACHE)
  cat(sprintf("\n==== PRISM %d%% | %d reps | K = %d | budget %d | %.1f min ====\n",
              round(100 * f), REPS, K, budget, mins))

  # ---- 4.3. Report candidate estimation and interval performance. ----
  SC <- t(sapply(done, `[[`, "score"))
  OR <- t(sapply(done, `[[`, "oracle"))
  cat("mean oracle RMSE, full-n fits:\n")
  print(round(sqrt(colMeans(OR)), 4))

  IV <- Reduce(`+`, lapply(done, grade_intervals)) / length(done)
  cat("mean interval score / coverage / width, full-n fits (traffic-share weighted):\n")
  print(round(IV, 3))

  # ---- 4.4. Compare CV and hold-out selection performance. ----
  s_d <- grade_arm("DBCV:", SC, OR)
  cat("  DBCV selections: ")
  print(table(factor(s_d, levels = colnames(SC))))

  NV <- t(sapply(done, `[[`, "score_naive"))
  s_n <- grade_arm("naive CV (no correction):", NV, OR)
  cat("  naive CV selections: ")
  print(table(factor(s_n, levels = colnames(NV))))

  for (sp in sprintf("%02d", round(100 * SPLITS))) {
    HO <- t(sapply(done, function(x) x$split[[sp]]$hold))
    OP <- t(sapply(done, function(x) x$split[[sp]]$oracle_pooled))
    grade_arm(sprintf("hold-out %s/%s:", sp, sprintf("%02d", 100 - as.integer(sp))),
              HO, OP)
  }
}
cat("PAPER SELECTION DONE\n")

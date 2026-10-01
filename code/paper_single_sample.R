# Fit and compare seven PRISM candidates on one stratified sample.
# 1. Set up the population, covariates, and seven candidate estimators.
# 2. Draw one sample and select by population-weighted debiased CV.
# 3. Compute full-sample estimates and intervals for each candidate.
# 4. Compare estimates with truth, print summaries, and save results.
# The default seed matches the first replication of paper_selection.R.
# Run from code/: Rscript paper_single_sample.R
# Writes ../data/paper_single_sample_<fraction>.rds. Environment settings: FRAC, SEED.


library(dplyr)
source("R/run.R")
source("prism/load.R")

# ---- 1. Set up the population, covariates, and seven candidate estimators. ----
FRAC <- as.numeric(Sys.getenv("FRAC", "0.10"))
SEED <- as.integer(Sys.getenv("SEED", "101"))
K    <- 5
CTRL <- fh_control(ndesired = 2000, nburn = 1000)
pop <- prism_population(scores = "../data/prism/judge_census/scores_sat.jsonl")
pop$units <- pop$units |>
  mutate(
    log_resp_chars = log1p(resp_chars),
    turn0 = as.integer(turn == 0),
    chosen = as.integer(if_chosen)
  )

domains <- pop$domains$domain
q <- pop$domains$N / sum(pop$domains$N)
CONTENT <- c("log_resp_chars", "turn0", "chosen")
XS <- list(judge = domain_design_matrix(pop, means = "judge"),
           full  = domain_design_matrix(pop, means = c("judge", CONTENT)))
F_JUDGE <- ~ judge
F_FULL  <- as.formula(paste("~ judge +", paste(CONTENT, collapse = " + ")))
tsf <- make_ts_fitter(pop)
# Smooth GREG inputs and compute the covariance needed for CV debiasing.
# keep_draws retains posterior draws for constructing quantile intervals.
greg_fit <- function(fitter, X, fml) function(pop, sub, control,
                                              keep_draws = FALSE) {
  inp <- greg_estimates(pop, sub, fml) |>
    mask_by_direct(direct_estimates(sub), domains) |>
    align_to_domains(domains)
  f <- fitter(X, inp$y, inp$d, control)
  v <- apply(f$theta, 2, var)
  wj <- lm(update(fml, y ~ .), data = sub, weights = w)
  cc <- as.numeric(cov_direct_residual(sub, predict(wj, newdata = sub), domains))
  out <- list(point = f$point, var = v, cov = v * cc / inp$d)
  if (keep_draws) out$theta <- f$theta
  out
}
MODELS <- list(
  direct     = cv_models("direct")$direct,
  greg_judge = cv_models("greg", greg_formula = F_JUDGE)$greg,
  greg_full  = cv_models("greg", greg_formula = F_FULL)$greg,
  pps_judge  = greg_fit(fit_fh_xy, XS$judge, F_JUDGE),
  pps_full   = greg_fit(fit_fh_xy, XS$full,  F_FULL),
  ppts_judge = greg_fit(tsf,       XS$judge, F_JUDGE),
  ppts_full  = greg_fit(tsf,       XS$full,  F_FULL))

# ---- 2. Draw one sample and select by population-weighted debiased CV. ----
budget <- round(FRAC * nrow(pop$units))
s <- draw_sample(pop, stratified(budget, n_min = 10), seed = SEED)
cat(sprintf("one sample: n = %d over %d domains (%.0f%%), seed %d\n",
            nrow(s), length(domains), 100 * FRAC, SEED))

res <- cv_compare(pop, s, MODELS, K = K, control = CTRL, seed = 1)
deb <- cv_debias(res)
ok  <- rowSums(!is.finite(deb)) == 0 & is.finite(res$d_full)
score <- apply(deb, 2, function(v) sum(q[ok] * v[ok]) / sum(q[ok]))
sel <- names(which.min(score))

# ---- 3. Compute full-sample estimates and intervals for each candidate. ----
# Only the smoother fitting functions accept keep_draws.
fits <- lapply(names(MODELS), function(m)
  if ("keep_draws" %in% names(formals(MODELS[[m]]))) MODELS[[m]](pop, s, CTRL, keep_draws = TRUE)
  else MODELS[[m]](pop, s, CTRL))
names(fits) <- names(MODELS)
est <- data.frame(domain = as.character(domains),
                  n = as.integer(table(s$domain)[as.character(domains)]),
                  N = pop$domains$N)
for (m in names(fits)) {
  f <- fits[[m]]
  if (is.null(f$theta)) {
    lower <- f$point - 1.96 * sqrt(f$var)
    upper <- f$point + 1.96 * sqrt(f$var)
  } else {
    lower <- apply(f$theta, 2, quantile, .025)
    upper <- apply(f$theta, 2, quantile, .975)
  }
  est <- est |>
    mutate(
      "{m}" := .env$f$point,
      "{m}_lo" := .env$lower,
      "{m}_hi" := .env$upper
    )
}
# ---- 4. Compare estimates with truth, print summaries, and save results. ----
gm <- sum(s$w * s$y) / sum(s$w)   # estimated traffic-wide mean
est <- est |> mutate(truth = pop$domains$theta_true)

w_ <- function(m) est[[paste0(m, "_hi")]] - est[[paste0(m, "_lo")]]
sep <- function(m) sprintf("%d below / %d above",
                           sum(est[[paste0(m, "_hi")]] < gm),
                           sum(est[[paste0(m, "_lo")]] > gm))
cat(sprintf("selected: %s | estimated traffic-wide mean %.1f\n", sel, gm))
print(data.frame(
  candidate = names(MODELS),
  `DB-CV score (RMSE scale)` = sprintf("%.3f", sqrt(score[names(MODELS)])),
  `mean width` = sprintf("%.2f", sapply(names(MODELS), function(m) mean(w_(m)))),
  separated = sapply(names(MODELS), sep),
  `RMSE (truth)` = sprintf("%.3f", sapply(names(MODELS), function(m)
    sqrt(mean((est[[m]] - est$truth)^2)))),
  coverage = sprintf("%.2f", sapply(names(MODELS), function(m)
    mean(est$truth >= est[[paste0(m, "_lo")]] &
         est$truth <= est[[paste0(m, "_hi")]]))),
  check.names = FALSE, row.names = NULL))
saveRDS(list(est = est, selected = sel, score = score, gm = gm, seed = SEED,
             frac = FRAC, budget = budget, n = nrow(s), K = K, control = CTRL,
             models = names(MODELS), specs = lapply(XS, colnames)),
        sprintf("../data/paper_single_sample_%02d.rds", round(100 * FRAC)))
cat("SINGLE SAMPLE DONE\n")

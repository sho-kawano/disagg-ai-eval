# Design-based simulation: repeatedly sample a fixed finite population and
# evaluate estimators against its known domain means.
# - run_one() fits and scores estimators on one sample.
# - run_replications() in designs.R repeats this across samples and designs.
# Source this file from code/.

source("R/population.R")     # also sources domains.R
source("R/designs.R")
source("R/direct.R")
source("R/ppi.R")
source("R/greg.R")
source("R/fh.R")             # also sources mcmc_helper.R
source("R/ts.R")             # taxonomy smoothing
source("R/scoring.R")
source("R/cv.R")

#' Estimation and evaluation for one sampled dataset.
#'
#' @param pop Population from new_population().
#' @param sample Sample from draw_sample().
#' @param control MCMC settings from fh_control().
#' @param estimators Candidate tags: direct (HT), ppi, ppi_tuned or greg (GREG), fh,
#'   pp_s, ts (TFH), ts_greg (PP-TS), or naive_judge. PPI/GREG tags also accept
#'   _unweighted.
#' @param greg_formula One-sided formula for greg; ppi_tuned and the GREG-input
#'   smoothers use ~ judge.
#' @param fh_X Named list of domain-aligned matrices for FH and PP-S. NULL uses
#'   domain_design_matrix(pop); names are appended to estimator tags.
#' @param ts_X Corresponding matrix list for taxonomy smoothers; defaults to fh_X.
#' @param fpc Apply available finite-population corrections to direct and residual
#'   variances.
#' @return A data frame with estimator, metric, and value. Smoothers include
#'   coverage for observed and predicted domains separately.
run_one <- function(pop, sample, control = fh_control(),
                    estimators = c("direct", "ppi", "greg", "fh"),
                    greg_formula = ~ judge, fh_X = NULL, ts_X = fh_X, fpc = TRUE) {
  theta_true <- pop$domains$theta_true
  domains <- pop$domains$domain
  W <- pop$domains$N / sum(pop$domains$N)
  # Exclude degenerate sampled domains from every estimator's scores.
  # Unsampled domains remain eligible for prediction scoring.
  dir_tab <- direct_estimates(sample, fpc = fpc)
  aligned_dir <- align_to_domains(dir_tab, domains)
  sampled <- as.character(domains) %in% as.character(dir_tab$domain)
  keep <- !is.na(aligned_dir$y) | !sampled
  # Score a direct, PPI, or GREG estimate table using normal intervals.
  db_rows <- function(tag, est) {
    i <- match(domains, est$domain)
    y <- est$y[i]
    d <- est$d[i]
    y[!keep] <- NA
    d[!keep] <- NA
    rbind(est_rows(tag, y, theta_true, W),
          interval_rows(tag, y, d, theta_true))
  }

  out <- list()
  if ("naive_judge" %in% estimators) {
    jm <- tapply(pop$units$judge, pop$units$domain, mean)
    nj <- as.numeric(jm[as.character(domains)])
    nj[!keep] <- NA
    out$naive_judge <- est_rows("naive_judge", nj, theta_true, W)
  }
  if ("direct" %in% estimators)
    out$direct <- db_rows("direct", dir_tab)
  if ("ppi" %in% estimators)
    out$ppi <- db_rows("ppi", ppi_estimates(pop, sample, fpc = fpc))
  if ("ppi_unweighted" %in% estimators)
    out$ppi_unw <- db_rows("ppi_unweighted",
                           ppi_estimates(pop, sample, weighted = FALSE))
  if ("ppi_tuned" %in% estimators)
    out$ppi_tuned <- db_rows("ppi_tuned", greg_estimates(pop, sample, ~ judge, fpc = fpc))
  if ("ppi_tuned_unweighted" %in% estimators)
    out$ppi_tuned_unw <- db_rows("ppi_tuned_unweighted",
                                 greg_estimates(pop, sample, ~ judge,
                                                weighted = FALSE))
  if ("greg" %in% estimators)
    out$greg <- db_rows("greg", greg_estimates(pop, sample, greg_formula, fpc = fpc))
  if ("greg_unweighted" %in% estimators)
    out$greg_unw <- db_rows("greg_unweighted",
                            greg_estimates(pop, sample, greg_formula,
                                           weighted = FALSE))

  # Fit one linking-model specification and score its posterior draws.
  # Coverage is split by whether the domain supplied a usable input estimate.
  fh_rows <- function(tag, X, fitter, inp) {
    d <- align_to_domains(inp, domains)
    fit <- fitter(X, d$y, d$d, control)
    # The fit covers all domains; scoring uses the common keep set.
    pt <- fit$point
    pt[!keep] <- NA
    th <- fit$theta[, keep, drop = FALSE]
    tt <- theta_true[keep]
    obs <- is.finite(d$y)[keep]
    cov <- c(coverage = calc_coverage(th, tt))
    if (any(obs))
      cov["coverage_obs"] <- calc_coverage(th[, obs, drop = FALSE], tt[obs])
    if (any(!obs))
      cov["coverage_pred"] <- calc_coverage(th[, !obs, drop = FALSE], tt[!obs])
    rbind(est_rows(tag, pt, theta_true, W),
          data.frame(estimator = tag, metric = names(cov), value = as.numeric(cov)),
          data.frame(estimator = tag, metric = "IS", value = calc_IS(th, tt)))
  }
  ts_fitter <- NULL
  for (model in intersect(c("fh", "pp_s", "ts", "ts_greg"), estimators)) {
    fitter <- if (model %in% c("ts", "ts_greg")) {
        if (is.null(ts_fitter)) ts_fitter <- make_ts_fitter(pop)
        ts_fitter
      } else fit_fh_xy
    # PP-S and PP-TS smooth GREG inputs; FH and TFH smooth direct inputs.
    inp <- if (model %in% c("pp_s", "ts_greg"))
      mask_by_direct(greg_estimates(pop, sample, ~ judge, fpc = fpc),
                     dir_tab, domains) else dir_tab
    # Fit each named covariate matrix separately; its name becomes part of the
    # estimator tag. With no matrix list, use the default domain covariates.
    X_list <- if (model %in% c("ts", "ts_greg")) ts_X else fh_X
    specs <- if (is.null(X_list)) setNames(list(domain_design_matrix(pop)), model) else
      setNames(X_list, paste0(model, "_", names(X_list)))
    for (tag in names(specs)) out[[tag]] <- fh_rows(tag, specs[[tag]], fitter, inp)
  }
  do.call(rbind, c(out, make.row.names = FALSE))
}

# Point-estimate accuracy against known domain truth.
# point and theta_true follow the same domain order. Return estimable fraction,
# RMSE, and bias; optional population shares W add weighted RMSE.
oracle_point <- function(point, theta_true, W = NULL) {
  ok <- is.finite(point)
  out <- data.frame(estimable = mean(ok),
                    rmse = sqrt(mean((point[ok] - theta_true[ok])^2)),
                    bias = mean(point[ok] - theta_true[ok]))
  if (!is.null(W))
    out$rmse_wtd <- sqrt(sum(W[ok] * (point[ok] - theta_true[ok])^2) / sum(W[ok]))
  out
}

# Point-estimate metrics in long format, labelled by estimator.
# Inputs point, theta_true, and optional W follow oracle_point(); return one row per metric.
est_rows <- function(estimator, point, theta_true, W = NULL) {
  op <- oracle_point(point, theta_true, W)
  data.frame(estimator = estimator, metric = names(op),
             value = as.numeric(op[1, ]), row.names = NULL)
}

# Normal-interval score and coverage against known domain truth.
# y, d, and theta_true follow the same domain order; alpha is the tail probability.
# Return mean interval score and coverage over domains with finite inputs.
interval_rows <- function(estimator, y, d, theta_true, alpha = 0.05) {
  ok <- is.finite(y) & is.finite(d)
  z <- qnorm(1 - alpha / 2)
  lo <- y[ok] - z * sqrt(d[ok])
  hi <- y[ok] + z * sqrt(d[ok])
  th <- theta_true[ok]
  data.frame(estimator = estimator, metric = c("IS", "coverage"),
             value = c(mean((hi - lo) + (2 / alpha) * pmax(lo - th, 0) +
                              (2 / alpha) * pmax(th - hi, 0)),
                       mean(th >= lo & th <= hi)))
}

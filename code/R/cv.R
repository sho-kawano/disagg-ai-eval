# Design-based K-fold CV (Dong & Li, arXiv 2604.23464, Algorithm 1).
# - cv_compare(): fit candidates on common folds and return domain-level scores.
# - cv_debias(): apply this paper's additional correction to those scores.
# - cv_models(): supply candidate fitting functions for cv_compare().
# - cv_decision(): compare two candidates using Dong & Li's bound on adjusted
#   scores, before the additional correction from cv_debias().

#' Compare candidate estimators on common folds, then refit on the full sample.
#'
#' @param pop,sample Population and sample from new_population() and draw_sample().
#' @param models Named fitting functions from cv_models() or the same interface.
#' @param K Number of within-domain folds.
#' @param control MCMC settings passed to candidates.
#' @param q Domain weights in population order for cv_decision(); default: population shares.
#' @param seed Fold-assignment seed.
#' @param fpc Correct direct variances for finite populations; match the models setting.
#' @return Domain-by-model naive/adjusted scores, fold predictions, and full-sample
#'   fits with direct variance d_full, candidate variance post_var, and their cov_full.
cv_compare <- function(pop, sample, models, K = 5, control = fh_control(),
                       q = NULL, seed = 1, fpc = TRUE) {
  domains <- pop$domains$domain
  M <- length(domains)
  if (is.null(q)) q <- pop$domains$N / sum(pop$domains$N)
  sample <- cv_folds(sample, K, seed)

  heldout <- matrix(NA_real_, M, K, dimnames = list(as.character(domains), NULL))
  train <- lapply(models, function(f)
    matrix(NA_real_, M, K, dimnames = list(as.character(domains), NULL)))
  for (k in seq_len(K)) {
    # Adjust weights for fold inclusion probabilities (Algorithm 1).
    test <- transform(sample[sample$fold == k, ], w = w * K)
    heldout[, k] <- subset_direct_points(test, domains)
    sub <- transform(sample[sample$fold != k, ], w = w * K / (K - 1))
    for (m in names(models)) train[[m]][, k] <- models[[m]](pop, sub, control)$point
  }
  domain_score <- sapply(names(models), function(m) adjusted_score(heldout, train[[m]]))
  domain_naive <- sapply(names(models), function(m) naive_score(heldout, train[[m]]))

  # Debiasing and the pairwise bound need full-sample variances and covariances.
  full <- direct_estimates(sample, fpc = fpc)
  d_full <- setNames(rep(NA_real_, M), as.character(domains))
  d_full[as.character(full$domain)] <- full$d
  d_full[d_full <= VAR_FLOOR] <- NA   # degenerate domains leave the scorable set
  fullfit <- lapply(models, function(f) f(pop, sample, control))
  post_var <- sapply(fullfit, `[[`, "var")
  # Flag variance substituted for missing covariance so cv_debias() can warn.
  cov_full <- sapply(fullfit, function(f) if (is.null(f$cov)) f$var else f$cov)
  missing_cov <- vapply(fullfit, function(f) is.null(f$cov), logical(1))
  attr(cov_full, "assumed") <- names(models)[missing_cov]

  y_full <- setNames(rep(NA_real_, M), as.character(domains))
  y_full[as.character(full$domain)] <- full$y

  list(domain_score = domain_score, domain_naive = domain_naive,
       q = q, d_full = d_full, post_var = post_var,
       cov_full = cov_full, y_full = y_full,
       point_full = sapply(fullfit, `[[`, "point"),
       domains = domains, K = K, heldout = heldout, train = train,
       fullfit = fullfit)
}

# Apply this paper's full-sample bias correction to a cv_compare() result.
# cov is direct-candidate sampling covariance; return domain-by-model scores.
# See the paper appendix, "The Debiased Score".
cv_debias <- function(res, cov = res$cov_full) {
  guessed <- attr(cov, "assumed")
  if (length(guessed))
    warning("cv_debias: no cov supplied for ", paste(guessed, collapse = ", "),
            "; using var. Correct for FH-type smoothers, wrong for PPI/GREG.")
  res$domain_score - res$d_full + 2 * cov
}

#' Build candidate fitting functions for cv_compare().
#'
#' @param estimators Tags: direct, ppi, ppi_tuned, greg, fh, or naive_judge.
#' @param greg_formula One-sided unit-level formula for GREG.
#' @param fpc Use finite-population corrections; match cv_compare().
#' @return Named functions f(pop, sub, control) fitting sample sub. Each returns
#'   point estimates, var (their variance), and cov (their sampling covariance
#'   with direct estimates), all in population domain order.
cv_models <- function(estimators = c("direct", "ppi", "ppi_tuned", "greg", "fh"),
                      greg_formula = ~ judge, fpc = TRUE) {
  # Preserve domain order even when a training fold has no units in some domains.
  aligned <- function(est, domains, cov) {
    i <- match(domains, est$domain)
    list(point = est$y[i], var = est$d[i], cov = cov)
  }
  # With population predictions treated as fixed, only residuals contribute covariance.
  resid_type <- function(est, aux, pop, sub) {
    cov <- cov_direct_residual(sub, aux, pop$domains$domain, fpc = fpc)
    aligned(est, pop$domains$domain, cov)
  }
  mk <- list(
    direct    = function(pop, sub, control) {
      e <- direct_estimates(sub, fpc = fpc)
      aligned(e, pop$domains$domain, e$d[match(pop$domains$domain, e$domain)])
    },
    ppi       = function(pop, sub, control)
      resid_type(ppi_estimates(pop, sub, fpc = fpc), sub$judge, pop, sub),
    ppi_tuned = function(pop, sub, control) {
      f <- lm(y ~ judge, data = sub, weights = w)
      resid_type(greg_estimates(pop, sub, ~ judge, fpc = fpc), predict(f, newdata = sub), pop, sub)
    },
    greg      = function(pop, sub, control) {
      f <- lm(update(greg_formula, y ~ .), data = sub, weights = w)
      resid_type(greg_estimates(pop, sub, greg_formula, fpc = fpc), predict(f, newdata = sub), pop, sub)
    },
    naive_judge = function(pop, sub, control) {
      # This candidate uses no sampled labels, so it is fixed across samples and folds.
      jm <- tapply(pop$units$judge, pop$units$domain, mean)
      list(point = as.numeric(jm[as.character(pop$domains$domain)]),
           var = rep(0, nrow(pop$domains)), cov = rep(0, nrow(pop$domains)))
    },
    fh        = function(pop, sub, control) {
      f <- fit_fh(pop, direct_estimates(sub, fpc = fpc), control)
      # Posterior variance approximates direct-model covariance for FH.
      # The identity assumes independent domain errors and fixed variance components.
      v <- apply(f$theta, 2, var)
      list(point = f$point, var = v, cov = v)
    })
  mk[estimators]
}

# Compare candidates m1 and m2 in a cv_compare() result using Dong & Li's bound.
# Uses adjusted scores before cv_debias(); returns weighted scores, gap, bound,
# verdict (winner or inconclusive), and covered (fraction of domains scored).
cv_decision <- function(res, m1, m2) {
  s1 <- res$domain_score[, m1]
  s2 <- res$domain_score[, m2]

  # Bound the remaining bias in the score difference (Dong & Li, Theorem 3).
  t_i <- 2 * sqrt(2 * res$d_full * (res$post_var[, m1] + res$post_var[, m2]))

  ok <- is.finite(s1) & is.finite(s2) & is.finite(t_i)   # domains scorable by both models
  q <- res$q[ok] / sum(res$q[ok])

  agg1 <- sum(q * s1[ok])
  agg2 <- sum(q * s2[ok])
  t_q  <- sum(q * t_i[ok])
  diff <- agg1 - agg2

  verdict <- if (abs(diff) <= t_q) "inconclusive" else if (diff < 0) m1 else m2
  list(score = setNames(c(agg1, agg2), c(m1, m2)),
       diff = diff, t_q = t_q, verdict = verdict, covered = mean(ok))
}

# Add a fold column, distributing each domain's sampled units evenly across K folds.
cv_folds <- function(sample, K, seed = 1) {
  set.seed(seed)
  sample$fold <- ave(seq_len(nrow(sample)), sample$domain,
                     FUN = function(i) sample(rep_len(seq_len(K), length(i))))
  sample
}

# Uncorrected held-out mean squared error by domain.
# heldout (validation means) and train (candidate estimates) are domain-by-fold matrices.
naive_score <- function(heldout, train) {
  vapply(seq_len(nrow(heldout)), function(i) {
    ok <- is.finite(heldout[i, ]) & is.finite(train[i, ])
    if (sum(ok) < 2) return(NA_real_)
    mean((train[i, ok] - heldout[i, ok])^2)
  }, numeric(1))
}

# Dong & Li's adjusted cross-validation score by domain (Theorem 2).
# Inputs are the same domain-by-fold matrices as naive_score().
adjusted_score <- function(heldout, train) {
  vapply(seq_len(nrow(heldout)), function(i) {
    w <- heldout[i, ]
    m <- train[i, ]
    ok <- is.finite(w) & is.finite(m)
    if (sum(ok) < 2) return(NA_real_)
    w <- w[ok]
    m <- m[ok]
    # Use divisor K (usable folds), not the sample-variance divisor K - 1.
    naive <- mean((m - w)^2)
    v_hat <- mean((w - mean(w))^2)
    c_hat <- mean((w - mean(w)) * (m - mean(m)))
    naive - v_hat + 2 * c_hat
  }, numeric(1))
}

# Held-out direct means from sub, in domains order; absent domains get NA.
subset_direct_points <- function(sub, domains) {
  est <- direct_estimates(sub)
  out <- setNames(rep(NA_real_, length(domains)), as.character(domains))
  out[as.character(est$domain)] <- est$y
  out
}

# Direct-residual sampling covariance for PPI/GREG debiasing, in domains order.
# aux holds predictions in sample row order, treated as fixed.
cov_direct_residual <- function(sample, aux, domains, fpc = TRUE) {
  e <- hajek_domain(sample, sample$y, sample$y - aux, fpc = fpc)
  setNames(e$d[match(domains, e$domain)], as.character(domains))
}

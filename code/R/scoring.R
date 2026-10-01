# Scores for predictions and posterior intervals against known domain truth.
# theta_samples has draws in rows and domains in columns. Losses are smaller
# when predictions are better; coverage is compared with its nominal level.

# ---- Gaussian predictive scores (closed form) -------------------------------

# Sum the negative log densities of observations under normal predictions.
gaussian_nll <- function(obs, mean, sd) {
  -sum(dnorm(obs, mean, sd, log = TRUE))
}

# Mean continuous ranked probability score (CRPS) for normal predictions.
# It measures distributional accuracy on the same scale as the observations.
gaussian_crps <- function(obs, mean, sd) {
  zs <- (obs - mean) / sd
  mean(sd * (zs * (2 * pnorm(zs) - 1) + 2 * dnorm(zs) - 1 / sqrt(pi)))
}

# Mean squared error corrected for noise in the comparison outcome.
# obs is an unbiased target estimate independent of est; obs_var gives its variance.
# Setting obs_var to zero leaves the uncorrected MSE.
mse_debiased <- function(est, obs, obs_var = 0) {
  mean((est - obs)^2) - mean(obs_var)
}

# ---- Sample-based scores against a known truth (oracle only) -----------------

# Mean interval score for central 1 - alpha posterior intervals.
# Penalize interval width and misses, with larger penalties farther outside.
calc_IS <- function(theta_samples, theta_true, alpha = 0.05) {
  q_lo <- apply(theta_samples, 2, quantile, alpha / 2)
  q_hi <- apply(theta_samples, 2, quantile, 1 - alpha / 2)
  width <- q_hi - q_lo
  penalty_lo <- (2 / alpha) * pmax(q_lo - theta_true, 0)
  penalty_hi <- (2 / alpha) * pmax(theta_true - q_hi, 0)
  mean(width + penalty_lo + penalty_hi)
}

# Fraction of domains whose central 1 - alpha interval contains the true mean.
calc_coverage <- function(theta_samples, theta_true, alpha = 0.05) {
  q_lo <- apply(theta_samples, 2, quantile, alpha / 2)
  q_hi <- apply(theta_samples, 2, quantile, 1 - alpha / 2)
  mean(theta_true >= q_lo & theta_true <= q_hi)
}

# Mean continuous ranked probability score from posterior draws.
# theta_samples has draws in rows and domains in columns; theta_true follows
# column order. Lower scores indicate better predictive distributions.
calc_CRPS <- function(theta_samples, theta_true) {
  crps_by_area <- sapply(seq_along(theta_true), function(i) {
    x <- theta_samples[, i]
    y <- theta_true[i]
    m <- length(x)
    # Sorting avoids computing every pairwise distance between draws.
    x_s <- sort(x)
    term1 <- mean(abs(x - y))
    term2 <- (1 / m^2) * sum((2 * seq_len(m) - m - 1) * x_s)
    term1 - term2
  })
  mean(crps_by_area)
}

# Predictive loss for one held-out data-thinning split.
# loss selects the score; theta_hat and theta_var describe the training posterior.
# y_test has mean test_frac * theta and variance test_frac * d.
dt_loss <- function(loss, theta_hat, theta_var, y_test, test_frac, d) {
  c0 <- test_frac
  switch(loss,
    plugin_NLL     = gaussian_nll(y_test, c0 * theta_hat, sqrt(c0 * d)),
    predictive_NLL = gaussian_nll(y_test, c0 * theta_hat,
                                  sqrt(c0 * d + c0^2 * theta_var)),
    CRPS           = gaussian_crps(y_test, c0 * theta_hat,
                                   sqrt(c0 * d + c0^2 * theta_var)),
    MSE            = mse_debiased(theta_hat, y_test / c0, d / c0),
    naive_MSE      = mse_debiased(theta_hat, y_test / c0, 0),
    stop("Unknown loss: ", loss)
  )
}

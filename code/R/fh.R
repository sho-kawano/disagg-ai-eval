# Bayesian Fay-Herriot smoothing (Fay & Herriot 1979).
# Inputs y and d.var are the paper's z_i and d_i; see sampler_conditionals.tex.

source("R/mcmc_helper.R")

#' One MCMC chain for the Fay-Herriot model with IID domain effects.
#'
#' @param X Covariate matrix, including an intercept, with one row per domain.
#' @param y Domain estimates in X row order: direct inputs for FH, GREG for PP-S.
#' @param d.var Sampling variances of y, treated as known during fitting.
#' @param ndesired Number of MCMC draws to retain.
#' @param nburn Number of burn-in iterations.
#' @param nthin Number of iterations between retained draws.
#' @param var.prior Optional variance prior: "improper" (flat) or "inverse-gamma".
#' @param hyp Optional inverse-gamma shape a and scale b; both default to 0.5.
#' @param ini Optional list of initial values for beta, sigma.sq, and v.
#' @param verbose Print progress and computation time if TRUE.
#' @return MCMC matrices for beta, v, sigma.sq, and theta (domain means), plus
#'   prior_details. Rows are retained draws; columns are coefficients or domains.
fh_fit <- function(X, y, d.var, ndesired, nburn, nthin, var.prior = "improper",
                   hyp = list(), ini = list(), verbose = TRUE) {
  nsim <- nthin * ndesired
  # Domains with missing estimates or unusable variances are predicted from the model.
  obs <- is.finite(y) & is.finite(d.var) & d.var > 0
  if (sum(obs) < 2L) stop("fh_fit: fewer than 2 usable finest areas")
  y[!obs] <- 0                        # finite placeholder; these rows get zero weight
  m <- nrow(X)
  j <- ncol(X)

  # Matrices for retained draws.
  Res_beta     <- mcmc_mat(ndesired, j, colnames(X))
  Res_v        <- mcmc_mat(ndesired, m, paste0("v_", 1:m))
  Res_sigma.sq <- mcmc_mat(ndesired, 1, "sigma sq.")
  Res_theta    <- mcmc_mat(ndesired, m, paste0("theta_", 1:m))

  # Variance prior hyperparameters; the regression prior is flat.
  if (var.prior == "inverse-gamma") {
    a_sig <- if (is.null(hyp$a)) 0.5 else hyp$a
    b_sig <- if (is.null(hyp$b)) 0.5 else hyp$b
    details <- paste0("prior: sigma.sq ~ IG(a, b): ", paste(a_sig, b_sig, sep = ", "))
  } else if (var.prior == "improper") {
    a_sig <- -1                       # IG(-1, 0) is the flat prior on sigma^2
    b_sig <- 0
    details <- "Improper prior for beta & sigma.sq are assumed."
  } else {
    stop("Invalid variance prior. Implemented priors: inverse-gamma or improper")
  }

  # Initial values: use the observed-domain regression unless ini supplies them.
  ls_fit <- lm(y[obs] ~ X[obs, , drop = FALSE] - 1)
  beta <- if (is.null(ini$beta)) ls_fit$coefficients else ini$beta
  sigma.sq <- if (is.null(ini$sigma.sq)) mean(ls_fit$residuals^2) else ini$sigma.sq
  v <- if (is.null(ini$v)) rnorm(m, sd = sqrt(sigma.sq)) else ini$v

  # X and the sampling variances are fixed, so compute this Cholesky factor once.
  Xstd <- X / sqrt(d.var)
  Xstd[!obs, ] <- 0
  U <- chol(t(Xstd) %*% Xstd)

  if (verbose) {
    ptm <- start_chain(nsim, nthin, nburn)
    pb <- txtProgressBar(min = 0, max = nsim + nburn, style = 3)
  }

  for (index in 1:(nsim + nburn)) {
    # Update beta (regression coefficients).
    weighted_residuals <- X * (y - v) / d.var
    weighted_residuals[!obs, ] <- 0
    h_beta <- apply(weighted_residuals, 2, sum)
    e <- rnorm(j)
    beta <- backsolve(U, backsolve(U, h_beta, transpose = TRUE) + e)

    # Update v (domain random effects).
    mean.v <- sigma.sq * (y - X %*% beta) / (sigma.sq + d.var)
    var.v  <- sigma.sq * d.var / (sigma.sq + d.var)
    # Domains without usable inputs retain the random-effect prior.
    mean.v[!obs] <- 0
    var.v[!obs] <- sigma.sq
    v <- rnorm(m, mean = mean.v, sd = sqrt(var.v))

    # Update sigma.sq (random-effect variance).
    shape_sig <- 0.5 * m + a_sig
    rate_sig <- 0.5 * sum(v^2) + b_sig
    sigma.sq <- 1 / rgamma(1, shape = shape_sig, rate = rate_sig)

    if (index > nburn && (index - nburn) %% nthin == 0) {
      k <- (index - nburn) / nthin
      Res_beta[k, ] <- beta
      Res_v[k, ] <- v
      Res_sigma.sq[k, ] <- sigma.sq
      Res_theta[k, ] <- X %*% beta + v
    }
    if (verbose) setTxtProgressBar(pb, index)
  }
  if (verbose) {
    writeLines("")
    print(proc.time() - ptm)
  }

  list(beta = Res_beta, v = Res_v, sigma.sq = Res_sigma.sq, theta = Res_theta,
       prior_details = details)
}

# MCMC settings shared by FH and taxonomy smoothing.
# Retain ndesired draws, discard nburn iterations, and save every nthin iterations.
fh_control <- function(ndesired = 1000, nburn = 500, nthin = 1)
  list(ndesired = ndesired, nburn = nburn, nthin = nthin)

# Fit FH to estimates y and variances d in X row order, using fh_control() settings.
# Return theta draws and point means in the shared FH/TS format used by the driver and CV.
fit_fh_xy <- function(X, y, d, control = fh_control()) {
  fit <- fh_fit(X, y, d, control$ndesired, control$nburn, control$nthin, verbose = FALSE)
  list(theta = fit$theta, point = colMeans(fit$theta))
}

# Treat variances at or below this threshold as unusable, not as precise data.
VAR_FLOOR <- 1e-8

# Align a domain/y/d/n estimate table to domains order for smoothing.
# Set inputs to NA for missing domains, n < 2, or non-finite/near-zero variances.
align_to_domains <- function(direct, domains) {
  d <- direct[match(domains, direct$domain), c("y", "d", "n")]
  d$domain <- domains
  # Identical sampled binary labels give zero estimated variance,
  # even though the domain mean is still uncertain.
  unusable <- is.na(d$n) | d$n < 2 | !is.finite(d$d) | d$d <= VAR_FLOOR
  d$y[unusable] <- NA
  d$d[unusable] <- NA
  d
}

# Remove rows of inp whose direct estimates are unusable, matching by domain ID.
# Uses population domain IDs in domains to apply the same exclusions to GREG inputs.
mask_by_direct <- function(inp, direct, domains) {
  bad <- as.character(domains)[is.na(align_to_domains(direct, domains)$y)]
  bad <- bad[bad %in% as.character(inp$domain)]
  if (length(bad))
    warning("mask_by_direct: dropping ", length(bad),
            " degenerate domain(s): ", paste(bad, collapse = ", "))
  inp[!(as.character(inp$domain) %in% bad), ]
}

# Fit FH to a domain/y/d/n estimate table; X follows pop$domains order.
# Return draws and point means with domain IDs, aligned inputs, and X.
fit_fh <- function(pop, direct, control = fh_control(), X = domain_design_matrix(pop)) {
  d <- align_to_domains(direct, pop$domains$domain)
  fit <- fit_fh_xy(X, d$y, d$d, control)
  c(fit, list(domains = pop$domains$domain, direct = d, X = X))
}

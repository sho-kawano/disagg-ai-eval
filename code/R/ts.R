# Taxonomy smoothing with one additive random effect per nested level.
# Inputs y and d.var are standardized before sampling; see sampler_conditionals.tex.

# Nested taxonomy membership for the population domains.
# Return one membership factor per level in pop$domains order; pop$domain_vars must
# run from coarse to fine.
domain_tree_levels <- function(pop) {
  u <- pop$units[!duplicated(pop$units$domain), c(pop$domain_vars, "domain")]
  u <- u[match(pop$domains$domain, u$domain), ]
  # Include ancestors so equal node names under different parents stay distinct.
  lapply(seq_along(pop$domain_vars), function(r)
    factor(do.call(paste, c(u[pop$domain_vars[1:r]], sep = " // "))))
}

# Domain-to-node membership matrix for one taxonomy level.
# Input factor f assigns domains to nodes; return one indicator column per node.
level_matrix <- function(f) {
  Z <- matrix(0, length(f), nlevels(f))
  Z[cbind(seq_along(f), as.integer(f))] <- 1
  Z
}

#' Gibbs sampler for taxonomy smoothing
#'
#' @param X Numeric domain-by-coefficient matrix, including any intercept.
#' @param y,d.var Numeric domain estimates and sampling variances in X row order.
#'   Non-finite inputs or nonpositive variances omit a domain from the likelihood.
#' @param levels List of membership factors, ordered coarse to fine. Each factor has
#'   one entry per row of X.
#' @param ndesired Number of draws to retain.
#' @param nburn Initial iterations to discard.
#' @param nthin Iterations between retained draws.
#' @param hyp Optional prior overrides (tau_beta, a_v, b_v); defaults are defined below.
#' @param verbose Print progress dots if TRUE.
#' @return A list with standardized theta draws (draws by domains), sigma.sq draws
#'   (draws by levels), center, scale, and obs (usable-input indicator). Priors
#'   apply on the standardized scale.
ts_fit <- function(X, y, d.var, levels, ndesired, nburn, nthin,
                   hyp = list(), verbose = FALSE) {
  nsim <- nthin * ndesired
  obs <- is.finite(y) & is.finite(d.var) & d.var > 0
  if (sum(obs) < 2L) stop("ts_fit: fewer than 2 usable domains")
  m <- nrow(X)
  R <- length(levels)

  # Standardize before applying the priors; make_ts_fitter restores the input scale.
  center <- mean(y[obs])
  scale <- sd(y[obs])
  y <- (y - center) / scale
  d.var <- d.var / scale^2
  Dinv <- 1 / d.var
  y[!obs] <- 0
  Dinv[!obs] <- 0                        # no likelihood contribution

  hy <- function(nm, default) if (is.null(hyp[[nm]])) default else hyp[[nm]]
  tau_beta <- hy("tau_beta", 1e-4)         # beta ~ N(0, tau_beta^-1 I)
  a_v <- rep_len(hy("a_v", 3), R)          # tau_r ~ Gamma(shape = a_v, rate = b_v)
  b_v <- rep_len(hy("b_v", 0.6), R)

  Zr <- lapply(levels, level_matrix)
  n <- vapply(Zr, ncol, integer(1))
  Z <- cbind(X, do.call(cbind, Zr))
  j <- ncol(X)
  # Record which columns of Z belong to each level so its effects can be
  # extracted from the joint draw when updating that level's variance.
  blk <- lapply(seq_len(R), function(r) {
    offset <- j + sum(n[seq_len(r - 1)])
    offset + seq_len(n[r])
  })

  tau_v <- rep(1, R)
  ZtDy <- as.numeric(crossprod(Z, Dinv * y))
  ZtDZ <- crossprod(Z * sqrt(Dinv))

  Res_theta <- matrix(NA_real_, ndesired, m)
  Res_sigma <- matrix(NA_real_, ndesired, R)

  for (index in 1:(nsim + nburn)) {
    # Draw regression and taxonomy effects together, using the current level variances.
    prior <- c(rep(tau_beta, j), rep(tau_v, n))
    U <- chol(ZtDZ + diag(prior, ncol(Z)))
    mu <- backsolve(U, backsolve(U, ZtDy, transpose = TRUE))
    gamma <- mu + backsolve(U, rnorm(ncol(Z)))

    # Update each level's variance from its effects; tau_v stores inverse variances.
    for (r in seq_len(R)) {
      v <- gamma[blk[[r]]]
      shape_v <- n[r] / 2 + a_v[r]
      rate_v <- sum(v^2) / 2 + b_v[r]
      tau_v[r] <- rgamma(1, shape = shape_v, rate = rate_v)
    }

    if (index > nburn && (index - nburn) %% nthin == 0) {
      k <- (index - nburn) / nthin
      Res_theta[k, ] <- as.numeric(Z %*% gamma)
      Res_sigma[k, ] <- 1 / tau_v
    }
    if (verbose && index %% 100 == 0) cat(".")
  }
  list(theta = Res_theta, sigma.sq = Res_sigma,
       center = center, scale = scale, obs = obs)
}

# Capture pop's taxonomy so driver/CV callers can use the same interface as FH.
# Return a fitter giving theta draws and point means on the original scale.
make_ts_fitter <- function(pop, hyp = list()) {
  levels <- domain_tree_levels(pop)
  function(X, y, d, control = fh_control()) {
    fit <- ts_fit(X, y, d, levels, control$ndesired, control$nburn,
                  control$nthin, hyp = hyp)
    theta <- fit$theta * fit$scale + fit$center
    list(theta = theta, point = colMeans(theta))
  }
}

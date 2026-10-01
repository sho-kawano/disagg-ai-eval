# Minimal MCMC container helpers used by fh_fit().

# Storage for retained MCMC draws, with named parameter columns.
# iters and params set the dimensions; names labels the columns. Entries start as NA.
mcmc_mat <- function(iters, params, names) {
  result <- matrix(NA_real_, nrow = iters, ncol = params)
  dimnames(result) <- list(iterations = NULL, names)
  result
}

# Chain progress reporting and timing.
# Print nsim post-burn-in iterations, nthin spacing, and nburn discarded iterations;
# return the starting process time.
start_chain <- function(nsim, nthin, nburn) {
  message("Running ", nsim, " simulations (excl. burn-in), thinning by ", nthin,
          "; burn-in: ", nburn)
  proc.time()
}

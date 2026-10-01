#' Prediction-powered (PPI) estimates of domain means.
#' For each domain, average judge predictions over all population units,
#' then add the Hajek estimate of the average error (y - judge) from sampled units.
#'
#' @param pop Population with judge values available for every unit.
#' @param sample Sample with domain, y, judge, w, and optional Npop.
#' @param weighted Use sampling weights if TRUE; FALSE uses unweighted means and
#'   disables the finite-population correction.
#' @param fpc Apply the finite-population correction when available.
#' @return A domain, y, d, n table for sampled domains; d is the estimated variance of
#'   the residual mean.
ppi_estimates <- function(pop, sample, weighted = TRUE, fpc = TRUE) {
  stopifnot(!all(is.na(sample$judge)))
  # The design-ignoring analysis uses unweighted means and iid variances.
  if (!weighted) {
    sample$w <- 1
    fpc <- FALSE
  }

  # Average predictions over all population units in each domain.
  Jbar <- tapply(pop$units$judge, pop$units$domain, mean)

  # Estimate each domain's mean prediction error from sampled units.
  residuals <- sample$y - sample$judge
  residual_estimates <- hajek_domain(sample, residuals, fpc = fpc)
  prediction_means <- as.numeric(Jbar[as.character(residual_estimates$domain)])

  # Add the Hajek residual correction to the population prediction mean.
  estimates <- residual_estimates
  estimates$y <- prediction_means + residual_estimates$y
  estimates
}

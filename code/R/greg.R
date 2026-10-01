#' Generalized regression (GREG) estimates of domain means.
#' For each domain, average regression predictions over all population units,
#' then add the Hajek estimate of the average error (y - prediction) from sampled units.
#'
#' @param pop Population containing all predictors used in formula.
#' @param sample Sample with y, domain, w, optional Npop, and the predictors.
#' @param formula One-sided regression formula. The default ~ judge gives tuned PPI;
#'   one regression is fitted across all sampled domains.
#' @param weighted Use weights in both regression and residual means if TRUE; FALSE
#'   also disables the finite-population correction.
#' @param fpc Apply the finite-population correction when available.
#' @return A domain, y, d, n table for sampled domains; d uses residual variation,
#'   treating fitted coefficients as fixed.
greg_estimates <- function(pop, sample, formula = ~ judge, weighted = TRUE,
                           fpc = TRUE) {
  # Ignore the design in both the regression and residual correction.
  if (!weighted) {
    sample$w <- 1
    fpc <- FALSE
  }
  # With ~ judge, the slope tunes the PPI correction (PPI++).
  fit <- lm(update(formula, y ~ .), data = sample, weights = w)

  # Average predictions over all population units in each domain.
  Gbar <- tapply(predict(fit, newdata = pop$units), pop$units$domain, mean)

  # Estimate each domain's mean prediction error from sampled units.
  residuals <- sample$y - predict(fit, newdata = sample)
  residual_estimates <- hajek_domain(sample, residuals, fpc = fpc)
  prediction_means <- as.numeric(Gbar[as.character(residual_estimates$domain)])

  # Add the Hajek residual correction to the population prediction mean.
  estimates <- residual_estimates
  estimates$y <- prediction_means + residual_estimates$y
  estimates
}

# Hajek domain estimates; equal to the paper's HT estimates under stratified SRSWOR.

library(survey)

# Allow singleton-domain means; hajek_domain() marks their variances as NA.
options(survey.lonely.psu = "certainty")

# Calculate the Hajek estimator of the population mean for each domain.
# sample supplies domain IDs (domain), w = 1/pi, and optional Npop for the FPC.
# u contains unit outcomes or PPI/GREG residuals in sample row order.
# v defaults to u for variance; a second vector gives covariance of the estimates.
# Return domain (domain ID), y (estimate), d (variance/covariance), and n (sample count).
hajek_domain <- function(sample, u, v = u, fpc = TRUE) {
  sample <- droplevels(sample)
  two <- !identical(u, v)
  sample$u_ <- u
  if (two) {
    sample$v_ <- v
    sample$dif_ <- u - v
  }
  if (fpc && !(!is.null(sample$Npop) && all(is.finite(sample$Npop)))) fpc <- FALSE
  # Domain strata make variances and FPCs use the sample size within each domain.
  des <- if (fpc)
    svydesign(ids = ~1, strata = ~domain, fpc = ~Npop, weights = ~w, data = sample)
  else svydesign(ids = ~1, strata = ~domain, weights = ~w, data = sample)
  f <- if (two) ~ u_ + v_ + dif_ else ~ u_
  est <- svyby(f, ~domain, des, svymean, vartype = "var")
  d <- if (two) (est$var.u_ + est$var.v_ - est$var.dif_) / 2 else est$var
  n <- as.integer(table(sample$domain)[as.character(est$domain)])
  if (any(n < 2)) {
    warning("hajek_domain: n_i < 2, d_i = NA in ", sum(n < 2), " domain(s): ",
            paste(as.character(est$domain)[n < 2], collapse = ", "))
    d[n < 2] <- NA_real_
  }
  data.frame(domain = est$domain, y = est$u_, d = d, n = n, row.names = NULL)
}

direct_estimates <- function(sample, fpc = TRUE)
  hajek_domain(sample, sample$y, fpc = fpc)

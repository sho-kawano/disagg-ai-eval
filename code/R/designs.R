# Specify sampling designs and draw unit records with inclusion probabilities.
# Stratified designs sample within domains; informative designs tilt selection
# toward a chosen variable and have random sample sizes.

# Simple random sampling of n units without replacement.
srs <- function(n) list(type = "srs", n = n)

#' Stratified simple random sampling within reporting domains.
#'
#' @param n Total target sample count; must be feasible under domain floors and
#'   sizes.
#' @param alloc "proportional", "equal", or a numeric vector of target domain
#'   counts. Named counts are matched by domain ID.
#' @param pi0 Optional minimum sampling fraction per domain.
#' @param n_min Optional minimum count per domain, capped at its population size.
#' @return A design specification for draw_sample().
stratified <- function(n, alloc = "proportional", pi0 = NULL, n_min = NULL)
  list(type = "stratified", n = n, alloc = alloc, pi0 = pi0, n_min = n_min)

#' Informative Poisson sampling driven by a population variable.
#'
#' @param n Target expected sample size before probabilities are capped at one.
#' @param tilt Name of the population column driving selection.
#' @param direction "high" or "low" for numeric ramp selection.
#' @param odds For numeric ramps, controls the high-to-low selection ratio; for
#'   categories, a named vector of relative weights.
#' @param form "ramp" uses numeric ranks or category weights; "exp" uses exp(t * z)
#'   for standardized numeric tilt values.
#' @param t Tilt coefficient required for form = "exp"; negative values favor
#'   smaller tilt values.
#' @return A design specification for draw_sample(); the realized count is random.
informative <- function(n, tilt = "judge", direction = "high", odds = 5,
                        form = "ramp", t = NULL)
  list(type = "informative", n = n, tilt = tilt, direction = direction,
       odds = odds, form = form, t = t)

# Sample for an analysis that ignores the selection design.
# Return sample with unit weights and no population-size information, so estimators
# use unweighted means without a finite-population correction.
design_naive <- function(sample) {
  sample$w <- 1
  sample$Npop <- NA_real_
  sample
}

# Integer sample allocation across domains.
# design supplies the budget, allocation, and floors; N contains named population
# sizes. Return counts in N order; the requested budget must be feasible.
stratified_counts <- function(design, N) {
  m <- length(N)
  n <- design$n
  tgt <- if (is.numeric(design$alloc)) {
    a <- design$alloc
    as.numeric(if (is.null(names(a))) a else a[names(N)])
  } else switch(design$alloc,
                proportional = n * N / sum(N),
                equal        = rep(n / m, m),
                stop("unknown allocation: ", design$alloc))
  fl <- rep(1, m)
  if (!is.null(design$pi0)) fl <- pmax(fl, design$pi0 * N)
  if (!is.null(design$n_min)) fl <- pmax(fl, pmin(design$n_min, N))
  for (k in 1:50) {
    tgt <- pmin(N, pmax(tgt, fl))
    if (abs(sum(tgt) - n) < 1e-9) break
    # Redistribute the remaining labels among domains that have reached
    # neither their minimum allocation nor their population size.
    free <- tgt > fl + 1e-9 & tgt < N - 1e-9
    if (!any(free)) break
    tgt[free] <- tgt[free] * (n - sum(tgt[!free])) / sum(tgt[free])
    tgt[free] <- pmax(tgt[free], fl[free])
  }
  # Give leftover labels to the largest fractional remainders.
  cnt <- pmax(1, floor(tgt))
  short <- round(min(n, sum(pmin(N, ceiling(tgt)))) - sum(cnt))
  if (short > 0) {
    ord <- order(tgt - cnt, decreasing = TRUE)
    ord <- ord[cnt[ord] < N[ord]][seq_len(short)]
    cnt[ord] <- cnt[ord] + 1
  }
  setNames(pmin(N, cnt), names(N))
}

#' Probability sample with inclusion probabilities and design weights.
#'
#' @param pop Population from new_population().
#' @param design Specification from srs(), stratified(), or informative().
#' @param seed Optional random seed; NULL uses the current random-number state.
#' @return Sampled unit records with pi, w = 1/pi, and domain size Npop. Poisson
#'   samples set Npop to NA to disable the SRSWOR variance correction.
draw_sample <- function(pop, design, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  units <- pop$units
  N <- nrow(units)
  if (design$type == "srs") {
    pi <- rep(design$n / N, N)
    sel <- logical(N)
    sel[sample.int(N, design$n)] <- TRUE
  } else if (design$type == "stratified") {
    N_i <- table(units$domain)
    cnt <- stratified_counts(design, setNames(as.numeric(N_i), names(N_i)))
    sel <- logical(N)
    for (ci in names(cnt)) {
      idx <- which(units$domain == ci)
      sel[idx[sample.int(length(idx), cnt[[ci]])]] <- TRUE
    }
    pi <- (cnt / setNames(as.numeric(N_i), names(N_i)))[as.character(units$domain)]
  } else if (design$type == "informative" && design$form == "exp") {
    # Poisson inclusion with exponential tilt; capping can reduce the expected size.
    z <- as.numeric(scale(units[[design$tilt]]))
    z[is.na(z)] <- 0                       # mean-impute (standardized mean = 0)
    size <- exp(design$t * z)
    pi <- pmin(design$n * size / sum(size), 1)
    sel <- runif(N) < pi
  } else if (design$type == "informative") {
    x <- units[[design$tilt]]
    if (is.numeric(x)) {
      u <- (rank(x, ties.method = "average") - 0.5) / N
      if (design$direction == "low") u <- 1 - u
      size <- 1 + (design$odds - 1) * u
    } else {
      size <- as.numeric(design$odds[as.character(x)])
      stopifnot(!anyNA(size))
    }
    pi <- pmin(design$n * size / sum(size), 1)   # E[#included] ~= n
    sel <- runif(N) < pi
  } else {
    stop("unknown design type: ", design$type)
  }
  s <- units[sel, ]
  s$pi <- pi[sel]
  s$w <- 1 / s$pi
  # Retain domain population sizes when subsetting, so CV folds can compute
  # their own sampling fractions for the finite-population correction.
  # Poisson samples do not use the stratified SRSWOR correction.
  Ndomain <- table(units$domain)
  s$Npop <- if (design$type == "informative") NA_real_ else
    as.numeric(Ndomain[as.character(s$domain)])
  s
}

#' Pooled sample from two independent stratified draws.
#'
#' @param pop Population from which both samples were drawn.
#' @param s1,s2 Stratified samples from draw_sample(), with unit IDs and the same
#'   domain factor levels.
#' @return Unique sampled units with union inclusion probabilities and weights. Npop
#'   is retained for the realized-size finite-population correction.
pool_stratified_samples <- function(pop, s1, s2) {
  Nc <- table(pop$units$domain)
  f <- function(s) as.numeric(table(s$domain)[names(Nc)]) / as.numeric(Nc)
  pi_u <- setNames(1 - (1 - f(s1)) * (1 - f(s2)), names(Nc))
  s <- rbind(s1, s2)
  s <- s[!duplicated(s$unit), ]
  s$pi <- pi_u[as.character(s$domain)]
  s$w <- 1 / s$pi
  s
}

# Repeated-sampling comparison of named designs on one population.
# Use seed0 + r for replication r. Pass control and ... to run_one(); return its
# results with design and rep columns.
run_replications <- function(pop, designs, reps, seed0 = 1, control = fh_control(), ...) {
  rows <- list()
  for (d in names(designs)) for (r in seq_len(reps)) {
    s <- draw_sample(pop, designs[[d]], seed = seed0 + r)
    res <- run_one(pop, s, control, ...)
    res$design <- d
    res$rep <- r
    rows[[length(rows) + 1]] <- res
  }
  do.call(rbind, rows)
}

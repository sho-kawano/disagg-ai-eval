# Finite population with unit IDs, domain variables, truth y, and auxiliary judge.
# Sampling reveals y on a subset; judge remains available across the population.

source("R/domains.R")

#' Finite population for domain estimation and simulation.
#'
#' @param units Data frame with one row per unit; unit IDs are added if absent.
#' @param domain_vars Names of reporting variables whose combinations define domains.
#'   Order coarse to fine for taxonomy smoothing.
#' @param truth Name of the numeric outcome column, copied to y.
#' @param judge Name of the auxiliary column, copied to judge; absent columns become
#'   NA.
#' @return A finite_population list with units, domain_vars, and domains (domain IDs,
#'   sizes, and true means).
new_population <- function(units, domain_vars, truth = "y", judge = "judge") {
  stopifnot(is.data.frame(units), all(domain_vars %in% names(units)), truth %in% names(units))
  if (is.null(units$unit)) units$unit <- seq_len(nrow(units))
  units$y <- units[[truth]]                                     # canonical names
  units$judge <- if (judge %in% names(units)) units[[judge]] else NA_real_
  units$domain <- assign_domains(units, domain_vars)
  structure(list(units = units, domain_vars = domain_vars,
                 domains = summarize_domains(units)),
            class = "finite_population")
}

# Generate a test population with unequal domain sizes and binary outcomes.
# Judge scores track the outcomes but have a different bias in each domain.
synthetic_population <- function(n = 4000, n_domain = 4, n_intent = 4, n_channel = 2,
                                 seed = 1) {
  set.seed(seed)
  # skewed domain sizes: draw each unit's domain from a heavily unequal multinomial
  domains <- expand.grid(topic = paste0("D", seq_len(n_domain)),
                       intent = paste0("I", seq_len(n_intent)),
                       channel = paste0("C", seq_len(n_channel)))
  size_wt <- rexp(nrow(domains))^2                                # long-tailed
  idx <- sample(nrow(domains), n, replace = TRUE, prob = size_wt)
  units <- domains[idx, ]

  # per-domain true satisfaction rate and per-domain judge miscalibration bias
  p_domain <- setNames(plogis(rnorm(nrow(domains), 0.3, 0.8)), seq_len(nrow(domains)))
  bias_domain <- setNames(rnorm(nrow(domains), 0, 0.6), seq_len(nrow(domains)))
  units$y <- rbinom(n, 1, p_domain[as.character(idx)])
  # judge score in (0,1): tracks truth but with domain-varying bias + noise
  units$judge <- plogis(qlogis(0.5 + 0.35 * (units$y - 0.5)) +
                        bias_domain[as.character(idx)] + rnorm(n, 0, 0.4))

  new_population(units, domain_vars = c("topic", "intent", "channel"))
}

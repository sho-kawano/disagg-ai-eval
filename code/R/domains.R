# Define reporting domains and summarize their population values for the models.

# Domain membership from the reporting columns named in domain_vars.
# Return one factor entry per row of units, omitting unobserved combinations.
assign_domains <- function(units, domain_vars) {
  factor(interaction(units[domain_vars], drop = TRUE, sep = " / "))
}

# Population sizes and true means by domain.
# units contains the domain factor domain and outcome y; return domain, N, and theta_true.
summarize_domains <- function(units) {
  N <- tapply(units$y, units$domain, length)
  theta_true <- tapply(units$y, units$domain, mean)
  data.frame(domain = factor(names(N), levels = levels(units$domain)),
             N = as.integer(N), theta_true = as.numeric(theta_true),
             row.names = NULL)
}

#' Domain-level design matrix for the linking model.
#'
#' Summarize unit covariates over the full population, with one row per domain.
#' This makes X available even for domains with no sampled labels.
#'
#' @param pop Population from new_population().
#' @param means Names of numeric unit columns to average within domains; NULL omits
#'   them. All-NA columns are skipped.
#' @param comps Names of categorical unit columns to summarize as domain
#'   proportions; NULL omits them.
#' @return A numeric matrix in pop$domains order, containing an intercept, domain
#'   means, and category proportions with the first category omitted.
domain_design_matrix <- function(pop, means = "judge", comps = NULL) {
  domains <- as.character(pop$domains$domain)
  X <- matrix(1, length(domains), 1, dimnames = list(domains, "(Intercept)"))
  for (v in means) {
    col <- pop$units[[v]]
    if (all(is.na(col))) next
    cm <- tapply(col, pop$units$domain, mean)
    X <- cbind(X, as.numeric(cm[domains]))
    colnames(X)[ncol(X)] <- paste0(v, "_mean")
  }
  for (v in comps) {
    # Drop one composition category because X already contains an intercept.
    P <- prop.table(table(pop$units$domain, pop$units[[v]]), 1)[domains, -1, drop = FALSE]
    colnames(P) <- paste0(v, ":", colnames(P))
    X <- cbind(X, P)
  }
  X
}

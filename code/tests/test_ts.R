# Check taxonomy construction and smoothing predictions.
# 1. Check nested taxonomy levels and domain membership.
# 2. Check predictions for sampled and unobserved domains.

cat("taxonomy smoothing\n")

# ---- 1. Check nested taxonomy levels and domain membership. ----
pop_t <- synthetic_population(n = 4000, seed = 7)
lv <- domain_tree_levels(pop_t)
check("tree: one factor per domain_var, finest = the domains themselves",
      length(lv) == length(pop_t$domain_vars) &&
        nlevels(lv[[length(lv)]]) == nrow(pop_t$domains))
check("tree: levels are nested (each fine category has one parent)",
      all(tapply(as.integer(lv[[1]]), lv[[2]], function(z) length(unique(z))) == 1))
check("tree: membership matrix has exactly one open column per domain",
      all(rowSums(level_matrix(lv[[1]])) == 1))

# ---- 2. Check predictions for sampled and unobserved domains. ----
s <- draw_sample(pop_t, srs(400), seed = 2)
d <- align_to_domains(direct_estimates(s), pop_t$domains$domain)
X <- domain_design_matrix(pop_t)
fitter <- make_ts_fitter(pop_t)
fit <- fitter(X, d$y, d$d, fh_control(ndesired = 400, nburn = 200))
check("tree: a point estimate and draws for EVERY domain, sampled or not",
      all(is.finite(fit$point)) && ncol(fit$theta) == nrow(pop_t$domains))
check("tree: unobserved domains are predicted with wider posteriors",
      mean(apply(fit$theta[, is.na(d$y), drop = FALSE], 2, sd)) >
        mean(apply(fit$theta[, !is.na(d$y), drop = FALSE], 2, sd)))
flat <- fit_fh_xy(X, d$y, d$d, fh_control(ndesired = 400, nburn = 200))
check("tree: on OBSERVED domains, points track the flat model (both follow y there)",
      cor(fit$point[!is.na(d$y)], flat$point[!is.na(d$y)]) > 0.95)

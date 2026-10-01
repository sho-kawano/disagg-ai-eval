# Check covariance estimation, CV scores, and debiasing.
# 1. Check direct-residual covariance against hand calculations.
# 2. Check computed covariance feeds the PPI bias correction.
# 3. Check cross-validation folds and adjusted scores.
# 4. Check covariance passes through cross-validation to debiasing.

cat("covariance + design-based CV\n")

# ---- 1. Check direct-residual covariance against hand calculations. ----
u <- data.frame(topic = rep(c("a", "b"), each = 4),
                y = rep(c(0, 1), 4),
                judge = c(0, -1, 0, -1, -1, 2, -1, 2))
pop_cov <- new_population(u, domain_vars = "topic")
s_cov <- pop_cov$units[c(1, 2, 5, 6), ]
s_cov$w <- 2
s_cov$Npop <- 4
residual <- s_cov$y - s_cov$judge
# In a, outcomes (0,1) and residuals (0,2) give covariance of means 0.5.
# In b, residuals (1,-1) give -0.5. Sampling half the domain halves both.
check_equal("Hajek: positive and negative covariance without FPC",
            hajek_domain(s_cov, s_cov$y, residual, fpc = FALSE)$d, c(0.5, -0.5))
check_equal("Hajek: FPC scales covariance by 1 - n/N",
            hajek_domain(s_cov, s_cov$y, residual)$d, c(0.25, -0.25))
check_equal("Hajek: covariance path preserves the outcome means",
            hajek_domain(s_cov, s_cov$y, residual)$y, c(0.5, 0.5))
ordered_domains <- c("b", "a", "missing")
covariance <- cov_direct_residual(s_cov, s_cov$judge, ordered_domains)
check_equal("direct-residual: population order and missing domains",
            covariance, c(-0.25, 0.25, NA_real_))
check("direct-residual: names follow requested domain order",
      identical(names(covariance), ordered_domains))
check_equal("direct-residual: forwards the FPC setting",
            cov_direct_residual(s_cov, s_cov$judge, c("a", "b"), fpc = FALSE),
            c(0.5, -0.5))

# Unequal weights (1,3): centered outcomes (-3/4,1/4), residuals (-3/2,1/2).
# The covariance of weighted means is 2 * (9/8 + 9/8) / 16 = 9/32.
s_weighted <- s_cov[1:2, ]
s_weighted$w <- c(1, 3)
check_equal("Hajek: covariance uses unequal weights",
            hajek_domain(s_weighted, c(0, 1), c(0, 2), fpc = FALSE)$d, 9 / 32)

# ---- 2. Check computed covariance feeds the PPI bias correction. ----
ppi_model <- cv_models("ppi")$ppi
ppi_fit <- ppi_model(pop_cov, s_cov, fh_control())
check_equal("PPI candidate: returns direct-residual covariance",
            ppi_fit$cov, c(0.25, -0.25))
# Direct variance is 1/8 in each domain. Starting at score 1 gives
# 1 - 1/8 + 2*(1/4) = 11/8 and 1 - 1/8 - 2*(1/4) = 3/8.
res_cov <- list(domain_score = matrix(1, 2, 1),
                d_full = direct_estimates(s_cov)$d,
                cov_full = matrix(ppi_fit$cov, 2, 1))
check_equal("cv_debias: uses estimated covariance with its sign and factor of two",
            as.numeric(cv_debias(res_cov)), c(11 / 8, 3 / 8))

# ---- 3. Check cross-validation folds and adjusted scores. ----
sf <- cv_folds(data.frame(domain = factor(rep(c("a", "b"), c(10, 7))), y = 0, w = 1),
               K = 5, seed = 1)
sizes_a <- table(sf$fold[sf$domain == "a"])
check("cv_folds: every unit folded 1..K, balanced within domain",
      all(sf$fold %in% 1:5) && nrow(sf) == 17 && max(sizes_a) - min(sizes_a) <= 1)

# heldout w = (0.4, 0.6), train m = (0.5, 0.5): naive 0.01, v_hat 0.01, c_hat 0.
check_equal("adjusted_score: constant model -> 0 here",
            adjusted_score(matrix(c(0.4, 0.6), 1), matrix(c(0.5, 0.5), 1)), 0)
# train tracks held-out exactly: naive 0, v_hat 0.01, c_hat 0.01 -> +0.01.
check_equal("adjusted_score: model tracking held-out -> +0.01 (cov penalty)",
            adjusted_score(matrix(c(0.4, 0.6), 1), matrix(c(0.4, 0.6), 1)), 0.01)
check("adjusted_score: < 2 usable folds -> NA",
      is.na(adjusted_score(matrix(c(0.5, NA), 1), matrix(c(0.5, 0.5), 1))))

# ---- 4. Check covariance passes through cross-validation to debiasing. ----
# Duplicate each unit so both folds have two observations per domain.
pop_cv <- new_population(u[rep(seq_len(nrow(u)), each = 2), ], domain_vars = "topic")
s_cv <- pop_cv$units[c(1:4, 9:12), ]
s_cv$w <- 2
s_cv$Npop <- 8
res <- cv_compare(pop_cv, s_cv, cv_models("ppi"), K = 2, seed = 1)
check_equal("cv_compare: retains computed positive and negative covariance",
            as.numeric(res$cov_full), c(1 / 12, -1 / 12))
# Direct variance is 1/24; the two covariance corrections give +1/8 and -5/24.
check("cv_debias: cross-validation produces finite corrected scores",
      all(is.finite(cv_debias(res))))
check_equal("cv_debias: uses covariance returned by cv_compare",
            as.numeric(cv_debias(res) - res$domain_score), c(1 / 8, -5 / 24))

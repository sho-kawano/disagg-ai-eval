# Check population summaries and the simulation pipeline.
# 1. Check domain summaries and covariates against hand calculations.
# 2. Check the fraction of domains with finite estimates.
# 3. Check estimator outputs and linking models on one sample.
# 4. Check the domains used for scoring and their error metrics.

cat("population + driver\n")

# ---- 1. Check domain summaries and covariates against hand calculations. ----
u <- data.frame(topic = c("A", "A", "B", "B", "B"),
                intent = c("x", "y", "x", "x", "y"),
                y      = c(1,   0,   1,   1,   0),
                judge  = c(.9,  .1,  .8,  .7,  .2),
                resp   = c("p", "q", "p", "q", "q"))
pop0 <- new_population(u, domain_vars = c("topic", "intent"))
check("domains: one level per non-empty combination, sizes sum to N",
      nlevels(pop0$units$domain) == 4 && sum(pop0$domains$N) == nrow(u))
check_equal("domains: theta_true for B/x = mean(1,1) = 1",
            pop0$domains$theta_true[pop0$domains$domain == "B / x"], 1)
X <- domain_design_matrix(pop0)
check("covariates: default = intercept + judge_mean",
      identical(colnames(X), c("(Intercept)", "judge_mean")))
check_equal("covariates: judge_mean for B/x = mean(.8,.7) = 0.75",
            X["B / x", "judge_mean"], 0.75)
xc <- domain_design_matrix(pop0, means = NULL, comps = "resp")
check_equal("covariates: composition share resp=q for B/x = 0.5",
            xc["B / x", "resp:q"], 0.5)
pop_nj <- new_population(u[c("topic", "intent", "y")], domain_vars = c("topic", "intent"))
check("covariates: intercept-only when judge deferred",
      ncol(domain_design_matrix(pop_nj)) == 1)

# ---- 2. Check the fraction of domains with finite estimates. ----
check_equal("oracle_point: two of three domains have finite estimates",
            oracle_point(c(0.2, NA, 0.8), c(0.3, 0.5, 0.7))$estimable, 2 / 3)

# ---- 3. Check estimator outputs and linking models on one sample. ----
pop <- synthetic_population(n = 4000, seed = 7)
res <- run_one(pop, draw_sample(pop, srs(400), seed = 2),
               fh_control(ndesired = 200, nburn = 100),
               estimators = c("naive_judge", "direct", "ppi", "ppi_tuned", "greg",
                              "fh", "pp_s"),
               fh_X = list(intercept = domain_design_matrix(pop, means = NULL),
                           judge     = domain_design_matrix(pop)))
check("run_one: every metric value finite", all(is.finite(res$value)))
check("run_one: fh_<spec> rows for each spec",
      all(c("fh_intercept", "fh_judge") %in% res$estimator))
check("run_one: pp_s_<spec> rows present",
      all(c("pp_s_intercept", "pp_s_judge") %in% res$estimator))

# ---- 4. Check the domains used for scoring and their error metrics. ----
# All estimators use the same scoring domains within a replication.
s2 <- draw_sample(pop, srs(400), seed = 2)
dtab <- direct_estimates(s2)
keep <- !is.na(align_to_domains(dtab, pop$domains$domain)$y) |
        !(as.character(pop$domains$domain) %in% as.character(dtab$domain))
check("run_one: FH estimable = kept fraction; both RMSE weightings present",
      all(res$value[grepl("^(fh|pp_s)_", res$estimator) & res$metric == "estimable"] ==
            mean(keep)) &&
        all(c("rmse", "rmse_wtd") %in% res$metric))
jm <- tapply(pop$units$judge, pop$units$domain, mean)[as.character(pop$domains$domain)]
check_equal("run_one: naive_judge rmse = census judge domain mean vs truth (kept domains)",
            res$value[res$estimator == "naive_judge" & res$metric == "rmse"],
            sqrt(mean((jm[keep] - pop$domains$theta_true[keep])^2)))

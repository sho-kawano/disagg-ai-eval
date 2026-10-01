# Check estimator identities and behavior on known examples.
# 1. Check direct domain means against hand calculations.
# 2. Check the finite population correction, including subsets.
# 3. Check PPI with perfect and zero predictions.
# 4. Check weighted and unweighted PPI under equal weights.
# 5. Check GREG recovers an exact linear relationship.
# 6. Check FH masks missing or unusable direct estimates.

cat("estimators\n")

# ---- 1. Check direct domain means against hand calculations. ----
s <- data.frame(domain = factor(c("g", "g", "g")), y = c(1, 0, 1), w = c(1, 1, 2))
check_equal("direct: Hajek mean (1+0+2)/(1+1+2) = 0.75", direct_estimates(s)$y, 0.75)
s3 <- data.frame(domain = factor(c("a", "a", "b", "b")), y = c(1, 0, 1, 1), w = rep(1, 4))
check_equal("direct: per-domain means (0.5, 1)", direct_estimates(s3)$y, c(0.5, 1))

# ---- 2. Check the finite population correction, including subsets. ----
# Constant pi within a domain (stratified/SRS), so the linearization variance must
# reduce to the textbook stratum-mean forms: (1-f) s^2/n with the fpc, s^2/n
# without. n = 4 of N = 20 -> f = 0.2. Hand values, not a round-trip.
sf <- data.frame(domain = factor(rep("a", 4)), y = c(1, 0, 1, 1),
                 w = rep(5, 4), Npop = rep(20, 4))
s2_n <- var(sf$y) / 4                                     # s^2/n = 0.25/4
check_equal("direct: no fpc -> s^2/n", direct_estimates(sf, fpc = FALSE)$d, s2_n)
check_equal("direct: fpc -> (1 - n/N) s^2/n", direct_estimates(sf)$d, 0.8 * s2_n)
check_equal("direct: fpc is a no-op without Npop",
            direct_estimates(sf[setdiff(names(sf), "Npop")])$d, s2_n)
# A subset gets the correction ITS OWN size implies -- what makes cv.R's folds
# come out right with no weight bookkeeping: 2 of 20 -> (1 - 0.1).
check_equal("direct: fpc recomputed from the rows supplied (subset)",
            direct_estimates(sf[1:2, ])$d, 0.9 * var(sf$y[1:2]) / 2)

# ---- 3. Check PPI with perfect and zero predictions. ----
u <- data.frame(topic = rep("A", 6), intent = rep("x", 6), y = c(1, 1, 0, 0, 1, 0))
u$judge <- u$y
pop1 <- new_population(u, domain_vars = c("topic", "intent"))
sp <- pop1$units[1:4, ]
sp$pi <- 0.5
sp$w <- 2
p <- ppi_estimates(pop1, sp)
check_equal("ppi: judge == truth -> census judge mean with zero variance",
            c(p$y, p$d), c(0.5, 0))
u2 <- u
u2$judge <- 0
pop2 <- new_population(u2, domain_vars = c("topic", "intent"))
sp2 <- pop2$units[1:4, ]
sp2$pi <- 0.5
sp2$w <- 2
check_equal("ppi: zero-signal judge -> the direct Hajek mean",
            ppi_estimates(pop2, sp2)$y, direct_estimates(sp2)$y)

# ---- 4. Check weighted and unweighted PPI under equal weights. ----
pop3 <- synthetic_population(n = 1000, seed = 5)
ss <- draw_sample(pop3, srs(150), seed = 1)
check_equal("ppi: unweighted == weighted under SRS",
            ppi_estimates(pop3, ss, weighted = FALSE)$y, ppi_estimates(pop3, ss)$y)

# ---- 5. Check GREG recovers an exact linear relationship. ----
set.seed(11)
ug <- data.frame(topic = rep(paste0("D", 1:4), each = 50), intent = "x")
ug$judge <- runif(200)
ug$y <- 2 + 3 * ug$judge
popc <- new_population(ug, domain_vars = c("topic", "intent"))
sg <- draw_sample(popc, stratified(60), seed = 1)
g <- greg_estimates(popc, sg)
check_equal("greg: exact linear truth recovered in every domain",
            g$y[match(popc$domains$domain, g$domain)], popc$domains$theta_true)
check_equal("greg: zero variance when residuals vanish", g$d, rep(0, 4), tol = 1e-8)
check_equal("greg: unweighted == weighted under equal-pi stratified",
            greg_estimates(popc, sg, weighted = FALSE)$y, g$y, tol = 1e-6)

# ---- 6. Check FH masks missing or unusable direct estimates. ----
domains <- factor(c("a", "b", "c"))
al <- align_to_domains(data.frame(domain = c("a", "b"), y = c(0.5, 0.9),
                                d = c(0.01, 0.02), n = c(10, 1)), domains)
check("fh align: missing and n<2 domains masked, usable kept",
      is.na(al$y[al$domain == "c"]) && is.na(al$y[al$domain == "b"]) &&
        is.finite(al$y[al$domain == "a"]))
check("fh align: degenerate near-zero variance masked",
      is.na(align_to_domains(data.frame(domain = "a", y = 1, d = 1e-30, n = 3),
                           factor("a"))$y))

# Check sampling allocations, inclusion probabilities, and combined samples.
# 1. Build a population with known domain sizes.
# 2. Check simple random sampling.
# 3. Check stratified budgets, minimum sizes, and allocations.
# 4. Check informative sampling with numeric and categorical tilts.
# 5. Check pooled samples and union inclusion probabilities.

cat("sampling designs\n")

# ---- 1. Build a population with known domain sizes. ----
pop <- synthetic_population(n = 2000, seed = 3)
N <- nrow(pop$units)
N_i <- table(pop$units$domain)

# ---- 2. Check simple random sampling. ----
s <- draw_sample(pop, srs(200), seed = 1)
check("srs: n exact, pi = n/N, y revealed unrelabelled",
      nrow(s) == 200 && all(s$pi == 200 / N) &&
        all(s$y == pop$units$y[match(s$unit, pop$units$unit)]))

# ---- 3. Check stratified budgets, minimum sizes, and allocations. ----
st <- draw_sample(pop, stratified(300, n_min = 10), seed = 4)
n_i <- table(st$domain)[levels(pop$units$domain)]
check("stratified: exact budget, pi = n_i/N_i, weights sum to N",
      nrow(st) == 300 &&
        all(abs(st$pi - (as.numeric(n_i) / as.numeric(N_i))[match(st$domain, names(n_i))]) < 1e-12) &&
        abs(sum(st$w) - N) < 1e-6)
check("stratified: n_min floor holds in every domain",
      all(as.numeric(n_i) >= pmin(10, as.numeric(N_i))))
cnt <- stratified_counts(stratified(300), setNames(as.numeric(N_i), names(N_i)))
sv <- draw_sample(pop, stratified(300, alloc = cnt), seed = 6)
check("stratified: named allocation vector reproduced exactly",
      all(table(sv$domain)[names(cnt)] == cnt))

# ---- 4. Check informative sampling with numeric and categorical tilts. ----
si <- draw_sample(pop, informative(200, tilt = "judge"), seed = 1)
slo <- draw_sample(pop, informative(200, tilt = "judge", direction = "low"), seed = 1)
check("informative: pi in (0,1]; high tilt raises, low tilt lowers sampled judge",
      all(si$pi > 0 & si$pi <= 1) &&
        mean(si$judge) > mean(pop$units$judge) &&
        mean(slo$judge) < mean(pop$units$judge))
sc <- draw_sample(pop, informative(200, tilt = "topic",
                                   odds = c(D1 = 4, D2 = 1, D3 = 1, D4 = 1)), seed = 2)
pi_by <- tapply(sc$pi, droplevels(factor(sc$topic)), unique)
check_equal("informative: categorical pi ratio = odds ratio",
            pi_by[["D1"]] / pi_by[["D2"]], 4)

# ---- 5. Check pooled samples and union inclusion probabilities. ----
pa <- draw_sample(pop, stratified(240, n_min = 10), seed = 11)
pb <- draw_sample(pop, stratified(60,  n_min = 10), seed = 12)
pu <- pool_stratified_samples(pop, pa, pb)
na_i <- table(pa$domain); nb_i <- table(pb$domain)
piu_i <- 1 - (1 - as.numeric(na_i) / as.numeric(N_i)) *
             (1 - as.numeric(nb_i) / as.numeric(N_i))
check("pool: no duplicate units; union size = n1 + n2 - overlap",
      !any(duplicated(pu$unit)) &&
        nrow(pu) == nrow(pa) + nrow(pb) - sum(pb$unit %in% pa$unit))
check("pool: pi = union formula per domain; w = 1/pi; Npop intact",
      all(abs(pu$pi - piu_i[match(pu$domain, names(na_i))]) < 1e-12) &&
        all(abs(pu$w - 1 / pu$pi) < 1e-12) &&
        all(pu$Npop == as.numeric(N_i)[match(pu$domain, names(N_i))]))
check("pool: pi exceeds both marginals and is below their sum",
      all(pu$pi >= pmax(pa$pi[match(pu$unit, pa$unit)], 0, na.rm = TRUE)) &&
        all(piu_i <= as.numeric(na_i) / as.numeric(N_i) +
                     as.numeric(nb_i) / as.numeric(N_i)))
# Monte-Carlo sanity on one domain: empirical inclusion rate matches pi_u.
domain1 <- names(N_i)[1]
hits <- replicate(400, {
  d1 <- draw_sample(pop, stratified(240, n_min = 10), seed = NULL)
  d2 <- draw_sample(pop, stratified(60,  n_min = 10), seed = NULL)
  u1 <- pop$units$unit[pop$units$domain == domain1][1]
  u1 %in% d1$unit || u1 %in% d2$unit
})
check("pool: empirical union inclusion within 4 MC SEs of pi_u (one unit)",
      abs(mean(hits) - piu_i[1]) <
        4 * sqrt(piu_i[1] * (1 - piu_i[1]) / 400))

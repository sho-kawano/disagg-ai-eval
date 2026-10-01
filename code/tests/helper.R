# Report named test failures and numerical comparisons.
# 1. Check a logical claim and stop on failure.
# 2. Compare numerical results within a tolerance.

# ---- 1. Check a logical claim and stop on failure. ----
check <- function(desc, ok) {
  if (isTRUE(ok)) cat(sprintf("  PASS  %s\n", desc))
  else stop(sprintf("FAIL  %s", desc), call. = FALSE)
}

# ---- 2. Compare numerical results within a tolerance. ----
check_equal <- function(desc, actual, expected, tol = 1e-8) {
  ok <- isTRUE(all.equal(unname(actual), unname(expected), tolerance = tol))
  check(sprintf("%s  [got %s, want %s]", desc,
                paste(signif(actual, 6), collapse = ","),
                paste(signif(expected, 6), collapse = ",")), ok)
}

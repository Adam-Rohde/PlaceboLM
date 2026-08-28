# Verify the package against the numbers published in the paper.
#
# This cannot run in an environment without access to CRAN and the Harvard
# Dataverse, which is why the applications vignette quotes the paper's figures
# rather than computing them. Run this on a networked machine to check them:
#
#   Rscript data-raw/verify-paper-numbers.R
#
# It installs nothing silently: if `qte` is missing it says so and stops.

suppressMessages(library(PlaceboLM))

ok <- function(label, got, want, tol) {
  pass <- is.finite(got) && abs(got - want) <= tol
  cat(sprintf("  [%s] %-42s got %10.3f   paper %10.3f   tol %g\n",
              if (pass) "PASS" else "FAIL", label, got, want, tol))
  pass
}

if (!requireNamespace("qte", quietly = TRUE))
  stop("Package 'qte' is required for the NSW checks.\n",
       "  install.packages('qte')", call. = FALSE)

data(lalonde, package = "qte")
covs <- c("age", "education", "black", "hispanic", "married", "nodegree")
results <- logical(0)

# ---- Section 3.2: NSW, 1975 earnings as the placebo outcome -----------------
cat("\nNSW, placebo = re75 (paper Section 3.2)\n")

fit75 <- placebo_lm(lalonde.psid, "re78", "treat", "re75",
                    covariates = covs, structure = "placebo_outcome")
bm <- plm_benchmarks(fit75)

results <- c(results,
  ok("scale factor SF",                    fit75$SF,                        1.167,  0.01),
  ok("adjusting for observables (k = 0)",  bm$adjusted_coefficient[1],     -5000,   1500),
  ok("DID, m = 1",                         bm$adjusted_coefficient[2],      2087,    60),
  ok("equiconfounding, k = 1",             bm$adjusted_coefficient[3],      3428,    60),
  ok("k reproducing the benchmark 1671",   plm_solve(fit75, 1671)$k,        0.812,  0.02),
  ok("implied m = k * SF",                 plm_solve(fit75, 1671)$m,        0.948,  0.02))

b75 <- plm_bounds(fit75, k = c(0.5, 1), n_boot = 0)
results <- c(results,
  ok("bound lower, 0.5 < k < 1",           b75$lower,                      -1249,    60),
  ok("bound upper, 0.5 < k < 1",           b75$upper,                       3428,    60))

# ---- Appendix variant: 1974 earnings, and the triangulated intersection -----
cat("\nNSW, placebo = re74, and the intersection (paper Section 3.2)\n")

fit74 <- placebo_lm(lalonde.psid, "re78", "treat", "re74",
                    covariates = covs, structure = "placebo_outcome")
b74 <- plm_bounds(fit74, k = c(0.5, 1), n_boot = 0)
results <- c(results,
  ok("re74 bound lower",                   b74$lower,                      -1406,    60),
  ok("re74 bound upper",                   b74$upper,                       3115,    60))

tri <- plm_triangulate(list(re75 = fit75, re74 = fit74), k = c(0.5, 1))
inter <- tri[tri$placebo == "(intersection)", ]
results <- c(results,
  ok("intersection lower",                 inter$lower,                    -1249,    60),
  ok("intersection upper",                 inter$upper,                     3115,    60))

# ---- 1975 employment: a deliberately badly scaled placebo -------------------
if ("u75" %in% names(lalonde.psid)) {
  cat("\nNSW, placebo = 1975 employment (paper Section 3.2)\n")
  d <- lalonde.psid
  d$e75 <- 1 - d$u75
  fite <- placebo_lm(d, "re78", "treat", "e75", covariates = covs,
                     structure = "placebo_outcome")
  cat(sprintf("  scale factor SF = %.0f  (paper: exceeds 40,000)\n", fite$SF))
  results <- c(results,
    ok("k reproducing the benchmark",      plm_solve(fite, 1671)$k,         0.33,   0.05))
}

# ---- Zika ------------------------------------------------------------------
cat("\nZika (paper Section 3.3)\n")
zika_path <- "data-raw/zika.rds"
if (!file.exists(zika_path)) {
  cat("  SKIPPED: ", zika_path, " not found.\n",
      "  See data-raw/zika.R for acquisition from the Harvard Dataverse\n",
      "  (Amorim 2022, doi:10.7910/DVN/ENG0IY).\n", sep = "")
} else {
  zika <- readRDS(zika_path)
  fitz <- placebo_lm(zika, "birth_rate_2016", "treated", "birth_rate_2014",
                     structure = "placebo_outcome")
  bz <- plm_benchmarks(fitz)
  results <- c(results,
    ok("naive difference in means",        bz$adjusted_coefficient[1],       3.4,   0.2),
    ok("DID, m = 1",                       bz$adjusted_coefficient[2],      -1.25,  0.2),
    ok("k at which the sign flips",        plm_solve(fitz, 0)$k,             0.7,   0.1),
    ok("estimate at k = 1.2",              plm_estimate(fitz, k = 1.2),     -2.0,   0.2))
}

cat(sprintf("\n%d of %d checks passed.\n", sum(results), length(results)))
if (!all(results)) {
  cat("\nA failure here means the package and the paper disagree. Investigate\n",
      "before treating either as correct -- the tolerances above are set to the\n",
      "paper's own rounding, so a near-miss is a real difference.\n", sep = "")
  quit(status = 1)
}

# Data-generating processes matched to each causal structure.
#
# The older `plm_test_data()` uses a single DGP for all five structures, which
# is why the oracle-recovery test there had to be weakened to "the adjustment
# moves the estimate in the right direction" for four of them. These fixtures
# generate data that actually satisfies each structure's assumed graph, so the
# strong claim becomes testable.
#
# In every DGP:
#   Z  is a single unobserved confounder, returned in the frame so that "oracle"
#      long regressions can be run, but never passed to placebo_lm().
#   X  is an observed covariate.
#   The causal effect of D on Y is `beta_D`.
#
# The graphs, following Tables 2 and 3 of the paper:
#
#   placebo_outcome        Z->D, Z->P, Z->Y, D->Y, (weak D->P for imperfection)
#   placebo_treatment      Z->D, Z->P, Z->Y, D->Y, (weak P->Y for imperfection)
#   observed_confounder_1  Z->D, Z->P, Z->Y, D->Y, P->Y
#   observed_confounder_2  Z->P, Z->Y, P->D, D->Y
#   post_outcome           Z->D, Z->Y, D->Y, Y->P

plm_dgp <- function(structure, n = 4000, seed = 1, beta_D = 2) {
  set.seed(seed)
  Z <- stats::rnorm(n)
  X <- stats::rnorm(n)
  e <- function() stats::rnorm(n)

  d <- switch(structure,

    placebo_outcome = {
      D <- X + Z + e()
      P <- X + Z + e() + 0.3 * D          # imperfect placebo: D -> P
      Y <- beta_D * D + X + Z + e()
      data.frame(Y = Y, D = D, P = P, X = X, Z = Z)
    },

    placebo_treatment = {
      D <- X + Z + e()
      P <- X + Z + e()                    # P not a descendant of D, nor D of P
      Y <- beta_D * D + 0.4 * P + X + Z + e()   # imperfect: P -> Y
      data.frame(Y = Y, D = D, P = P, X = X, Z = Z)
    },

    observed_confounder_1 = {
      D <- X + Z + e()
      P <- X + Z + e()
      Y <- beta_D * D + 0.7 * P + X + Z + e()   # P -> Y
      data.frame(Y = Y, D = D, P = P, X = X, Z = Z)
    },

    observed_confounder_2 = {
      P <- X + Z + e()
      D <- 0.6 * P + X + Z + e()          # P -> D
      Y <- beta_D * D + X + Z + e()
      data.frame(Y = Y, D = D, P = P, X = X, Z = Z)
    },

    post_outcome = {
      D <- X + Z + e()
      Y <- beta_D * D + X + Z + e()
      P <- 0.8 * Y + X + Z + e()          # Y -> P
      data.frame(Y = Y, D = D, P = P, X = X, Z = Z)
    },

    stop("no DGP defined for structure '", structure, "'")
  )
  d
}


# Refit one of a fit's regressions with the unobserved confounder Z included,
# and return the coefficient the structure reads from it. This is the "long
# regression" the method is trying to recover.
plm_long_coef <- function(fit, data, which = c("target", "sens")) {
  which <- match.arg(which)
  loc <- if (which == "target") fit$spec$target_coef(fit$vars)
         else                   fit$spec$sens_coef(fit$vars)
  f <- stats::update(fit$formulas[[loc$reg]], . ~ . + Z)
  stats::coef(stats::lm(f, data = data))[[loc$coef]]
}


# The true sensitivity parameters for a given fit and DGP:
#   imperfection = the sensitivity coefficient in the long regression
#   m            = ratio of the two omitted-variable biases
#   k            = m / SF
plm_true_params <- function(fit, data) {
  bt_long <- plm_long_coef(fit, data, "target")
  bs_long <- plm_long_coef(fit, data, "sens")
  m <- (fit$coefs$target$estimate - bt_long) /
       (fit$coefs$sens$estimate   - bs_long)
  list(target_long = bt_long, imperfection = bs_long,
       m = m, k = m / fit$SF)
}


# Partial correlation of a and b given `cond`.
plm_pcor <- function(a, b, cond, data) {
  ra <- stats::resid(stats::lm(stats::reformulate(cond, a), data = data))
  rb <- stats::resid(stats::lm(stats::reformulate(cond, b), data = data))
  stats::cor(ra, rb)
}

# Cohen's f from a partial correlation.
plm_cohen_f <- function(a, b, cond, data) {
  r <- plm_pcor(a, b, cond, data)
  r / sqrt(1 - r^2)
}


# --- Population parameters, for calibration tests -----------------------------
#
# Coverage must be measured against a target that is FIXED across simulations.
# Recomputing the "truth" from each sample is vacuous here, because the recovery
# identity in test-recovery.R is exact in sample: the estimator would hit a
# sample-specific target every single draw and coverage would be 1 by
# construction.
#
# So the population target and the population sensitivity parameters are taken
# once, from a single very large draw, and reused. For the placebo_outcome DGP
# these recover the values built into the generator (effect 2, imperfection 0.3),
# which is a useful check that the machinery is pointed at the right quantity.

.plm_pop_cache <- new.env(parent = emptyenv())

plm_population <- function(structure, n = 400000, seed = 987) {
  key <- paste(structure, n, seed, sep = "/")
  if (!is.null(.plm_pop_cache[[key]])) return(.plm_pop_cache[[key]])
  d   <- plm_dgp(structure, n = n, seed = seed)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = structure)
  out <- plm_true_params(fit, d)
  .plm_pop_cache[[key]] <- out
  out
}


# Clustered variants of the placebo-outcome DGP.
#
# Whether clustering threatens inference here turns out to depend on WHERE the
# dependence lives, because of the Moulton condition: a coefficient's variance
# is inflated only when the regressor AND the residual are both cluster
# correlated. Two cases are therefore generated.
#
#   shock = "confounder"
#     All cluster structure sits in the unobserved confounder Z. The treatment
#     inherits it, so the regressor is cluster correlated -- but the placebo
#     adjustment exists precisely to remove Z, and it removes the dependence
#     along with it. Measured on one draw, the ratio of clustered to classical
#     standard errors falls from 1.66 before adjustment to 0.76 after.
#
#   shock = "outcome"
#     Z still has a cluster component, so the treatment is cluster correlated,
#     but Y also carries a separate cluster shock that P does not share. That
#     shock survives the adjustment, both Moulton conditions hold, and the
#     ratio stays high: 2.39 before adjustment, 1.84 after.
#
# In both cases the cluster shock is independent of D given Z, so the population
# target remains the DGP effect.
plm_dgp_clustered <- function(n_clust = 40, per = 20, seed = 1, beta_D = 2,
                              shock = c("confounder", "outcome")) {
  shock <- match.arg(shock)
  set.seed(seed)
  cl <- rep(seq_len(n_clust), each = per)
  n  <- length(cl)

  Z <- stats::rnorm(n_clust, sd = 1.2)[cl] + stats::rnorm(n, sd = 0.5)
  X <- stats::rnorm(n)
  u <- if (shock == "outcome") stats::rnorm(n_clust, sd = 1.5)[cl] else 0

  D <- X + Z + stats::rnorm(n)
  P <- X + Z + stats::rnorm(n) + 0.3 * D
  Y <- beta_D * D + X + Z + u + stats::rnorm(n)

  data.frame(Y = Y, D = D, P = P, X = X, Z = Z, cl = factor(cl))
}

plm_population_clustered <- function(n_clust = 4000, per = 20, seed = 987,
                                     shock = c("confounder", "outcome")) {
  shock <- match.arg(shock)
  key <- paste("clustered", shock, n_clust, per, seed, sep = "/")
  if (!is.null(.plm_pop_cache[[key]])) return(.plm_pop_cache[[key]])
  d   <- plm_dgp_clustered(n_clust = n_clust, per = per, seed = seed,
                           shock = shock)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  out <- plm_true_params(fit, d)
  .plm_pop_cache[[key]] <- out
  out
}

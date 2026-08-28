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


# Closed-form population values, where the DGP admits them.
#
# For the linear placebo_outcome generator every quantity is exact:
#
#   target       = beta_D. The long regression Y ~ D + X + Z recovers the
#                  equation that generated Y, because its error is independent
#                  of all three regressors. Nothing is estimated.
#   imperfection = 0.3. Same argument applied to P ~ D + X + Z; the parameter
#                  IS the direct D -> P arrow written into the generator.
#   m            = 1, exactly, and for ANY error variance. Omitted-variable bias
#                  factors as (loading on Z) x gamma, where gamma is the
#                  coefficient of Z on D given X. Y and P both load 1.0 on Z, so
#                  m = gamma/gamma = 1 and gamma cancels. Changing the noise in D
#                  changes gamma but cancels identically top and bottom.
#
# Substantively that is equiconfounding: the generator builds Y and P to be
# equally confounded, so m = 1 is not a coincidence but the DGP doing as told.
#
# Using these instead of a large draw removes a real problem. An estimated
# population value carries error of order 1/sqrt(N), and that error is the SAME
# in every simulation -- so it acts as a bias, not noise. As n grows the
# estimator's own standard error shrinks while the bias does not, so coverage
# falls with n. That artefact was visible (0.950 -> 0.927 -> 0.907) before these
# closed forms replaced the sampled values.
plm_population_exact <- function(structure, beta_D = 2) {
  if (!identical(structure, "placebo_outcome")) return(NULL)
  list(target_long = beta_D, imperfection = 0.3, m = 1, k = NA_real_,
       exact = TRUE)
}


# Population parameters, with a convergence guard.
#
# `m` is a RATIO of two omitted-variable biases. When both are small it is
# poorly determined, and for some generators it does not settle at all: the
# spread across seeds can grow with N rather than shrink. Building a coverage
# test on such a target measures nothing, so this refuses to return one.
#
# Set `check = FALSE` only when you already know the target is well behaved and
# want the cheaper single draw.
plm_population <- function(structure, n = 400000, seed = 987, check = TRUE,
                           n_seeds = 4, tol_cv = 1.0) {
  exact <- plm_population_exact(structure)
  if (!is.null(exact)) return(exact)

  key <- paste(structure, n, seed, check, sep = "/")
  if (!is.null(.plm_pop_cache[[key]])) return(.plm_pop_cache[[key]])

  draw <- function(N, sd) {
    d   <- plm_dgp(structure, n = N, seed = sd)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = structure)
    plm_true_params(fit, d)
  }

  out <- draw(n, seed)

  if (check) {
    cv_at <- function(N) {
      ms <- vapply(seq_len(n_seeds), function(i) draw(N, seed + i)$m, numeric(1))
      100 * stats::sd(ms) / abs(mean(ms))
    }
    cv_small <- cv_at(n %/% 4)
    cv_big   <- cv_at(n)
    if (!(cv_big < tol_cv && cv_big <= cv_small * 1.5))
      stop("The population value of m does not converge for structure '",
           structure, "'.\n",
           sprintf("  spread across seeds: %.2f%% at N=%d, %.2f%% at N=%d\n",
                   cv_small, n %/% 4, cv_big, n),
           "  m is a ratio of two omitted-variable biases; when both are small ",
           "it is\n  poorly determined. A coverage test built on this target ",
           "would measure nothing.",
           call. = FALSE)
    out$cv_m <- cv_big
  }

  out$exact <- FALSE
  .plm_pop_cache[[key]] <- out
  out
}


# Clustered variants of the placebo-outcome DGP, used to exercise the inference
# code under dependent sampling.
#
#   shock = "confounder"  all cluster structure sits in the unobserved
#                         confounder Z, which the treatment inherits.
#   shock = "outcome"     Z still has a cluster component, and Y additionally
#                         carries a cluster shock that P does not share.
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


# --- Non-linear generators ----------------------------------------------------
#
# Every other fixture here is linear, additive, Gaussian and constant-effect.
# That is the friendliest possible regime, and an all-linear suite silently
# implies the method was checked more broadly than it was.
#
# These three depart from linearity in different places while keeping the
# population target well determined (verified: spread of m across seeds is
# ~0.15% at N = 800,000, and shrinking). They exist to check that INFERENCE
# survives non-linearity -- the point estimator's recovery identity is pure OLS
# algebra and cannot be broken by any DGP, so it is not what is at stake.
#
#   nl_confounding  Z enters D, P and Y through Z^2 as well as Z, and Y and P
#                   load on it differently. Note the population target is then
#                   2.534, NOT the structural coefficient 2, and m is 1.451
#                   rather than 1 -- the projection is simply a different
#                   quantity once the world is non-linear.
#   interaction     the confounder interacts with the observed covariate.
#   nl_treatment    treatment assignment is exponential in the confounder.
#
# A deliberately hostile generator (gamma confounder, exp treatment,
# Z^2 + sin(3Z) placebo, t3 errors scaled by 1+|Z|) was tried and REJECTED as a
# fixture: its m does not converge, with the spread across seeds growing from
# 9% at N = 200,000 to 14% at N = 800,000. plm_population()'s guard now refuses
# such targets rather than letting a meaningless coverage test be built on one.
plm_dgp_nonlinear <- function(kind = c("nl_confounding", "interaction",
                                       "nl_treatment"),
                              n = 2000, seed = 1, beta_D = 2) {
  kind <- match.arg(kind)
  set.seed(seed)
  Z <- stats::rnorm(n); X <- stats::rnorm(n)
  e <- function() stats::rnorm(n)

  switch(kind,
    nl_confounding = {
      D <- X + Z + 0.5 * Z^2 + e()
      data.frame(Y = beta_D * D + X + 1.5 * Z + 0.8 * Z^2 + e(),
                 D = D, P = X + Z + 0.5 * Z^2 + e() + 0.3 * D, X = X, Z = Z)
    },
    interaction = {
      D <- X + Z + e()
      data.frame(Y = beta_D * D + X + Z + 0.8 * Z * X + e(),
                 D = D, P = X + Z + 0.6 * Z * X + e() + 0.3 * D, X = X, Z = Z)
    },
    nl_treatment = {
      D <- exp(0.5 * Z) + X + e()
      data.frame(Y = beta_D * D + X + Z + e(),
                 D = D, P = X + Z + e() + 0.3 * D, X = X, Z = Z)
    })
}


# --- Heterogeneous effects: the projection is NOT the ATE ---------------------
#
# The package targets a coefficient in a linear projection, which equals an
# average causal effect only under further assumptions. Every other fixture
# makes the two coincide, so nothing could tell them apart.
#
# Two natural attempts at heterogeneity FAILED to separate them -- an effect
# linear in X, with X in the regression, leaves the projection equal to the ATE
# (2.00 vs 2.00, then 3.502 vs 3.50). What does separate them is the textbook
# variance-weighting case: OLS on a binary treatment recovers a
# conditional-variance-weighted average of effects, so when the effect AND the
# propensity both vary with a covariate, strata with propensity nearer 0.5 are
# over-weighted relative to their share of the population.
#
# Here the effect is 1 when X = 0 and 5 when X = 1, so the ATE is 3, while
# propensity is about 0.1 and 0.5 in those strata. The X = 1 stratum carries far
# more treatment variance, so the projection lands near 3.86.
plm_dgp_heterogeneous <- function(n = 2000, seed = 1) {
  set.seed(seed)
  Z  <- stats::rnorm(n)
  X  <- stats::rbinom(n, 1, 0.5)
  D  <- stats::rbinom(n, 1, stats::plogis(-2.2 + 2.2 * X + 0.6 * Z))
  tau <- 1 + 4 * X
  data.frame(Y = tau * D + X + Z + stats::rnorm(n),
             D = D, P = X + Z + stats::rnorm(n) + 0.3 * D, X = X, Z = Z)
}

# The ATE of plm_dgp_heterogeneous(): E[1 + 4X] with X ~ Bernoulli(0.5).
PLM_HET_ATE <- 3

# Population parameters for a generator supplied as a function, with the same
# convergence guard as plm_population().
plm_population_of <- function(gen, label, n = 800000, seeds = 1:4,
                              tol_cv = 1.0) {
  # `label` is REQUIRED, and deliberately so. An earlier version derived the
  # cache key from deparse(body(gen)), which silently collided: three
  # generators built as local({ kk <- k; function(n, seed) f(kk, ...) }) all
  # have the identical body, because kk lives in the closure environment rather
  # than the body. One cached entry then served all three, two DGPs were scored
  # against the wrong population target, and their measured coverage was
  # exactly 0.
  #
  # Coverage of exactly 0 is a bug signature, not a statistical finding, and it
  # is worth noticing that the collision defeated the convergence guard this
  # function exists to provide: the target was well determined, just not the
  # target for that generator. Naming the generator explicitly removes the
  # failure mode rather than making the key cleverer.
  if (missing(label) || !is.character(label) || length(label) != 1L)
    stop("`label` is required: give this generator a unique name so its ",
         "population\nvalues cannot be confused with another's in the cache.",
         call. = FALSE)
  key <- paste(label, n, sep = "/")
  if (!is.null(.plm_pop_cache[[key]])) return(.plm_pop_cache[[key]])

  one <- function(N, sd) {
    d   <- gen(n = N, seed = sd)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    plm_true_params(fit, d)
  }
  cv_of <- function(N) {
    ms <- vapply(seeds, function(s) one(N, 1000 + s)$m, numeric(1))
    100 * stats::sd(ms) / abs(mean(ms))
  }
  cv_small <- cv_of(n %/% 4); cv_big <- cv_of(n)
  if (!(cv_big < tol_cv && cv_big <= cv_small * 1.5))
    stop("Population m does not converge for this generator: ",
         sprintf("%.2f%% at N=%d vs %.2f%% at N=%d.", cv_small, n %/% 4,
                 cv_big, n), call. = FALSE)

  ps <- lapply(seeds, function(s) one(n, 1000 + s))
  out <- list(
    target_long  = mean(vapply(ps, `[[`, numeric(1), "target_long")),
    imperfection = mean(vapply(ps, `[[`, numeric(1), "imperfection")),
    m            = mean(vapply(ps, `[[`, numeric(1), "m")),
    cv_m         = cv_big
  )
  .plm_pop_cache[[key]] <- out
  out
}

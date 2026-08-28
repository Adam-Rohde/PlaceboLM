# PlaceboLM (development)

## Deprecation notice is a message, not a warning

`.plm_deprecate()` now uses `message()`. R's convention for deprecation is
`warning()`, but that convention assumes no published, citable code depends on
the call succeeding. Here it does: the paper prints the pre-0.2.0 interface and
directs readers to install this package.

Under `options(warn = 2)` -- common in CI and among users who run strictly --
a warning is promoted to an error, and the code printed in the paper's appendix
failed on its first call with something that looked like a broken package. A
message is still shown by default and still says the interface is deprecated,
but no `warn` setting can turn it into an error. The notice is informational; it
is not a signal that results are wrong.

`test-legacy-api.R` now runs the paper's code under `options(warn = 2)` and
requires it to complete.

## Vignettes

Everything shipped now runs. The applications vignette (NSW and Zika) has been
removed: its data comes from CRAN and the Harvard Dataverse, neither of which
was reachable where the package was developed, so six of its eight chunks were
never executed and its figures were quoted from the paper rather than computed.
Shipping unexecuted analysis invites the assumption that it was checked.

Removed with it: `data-raw/verify-paper-numbers.R` and `data-raw/zika.R`, which
existed only to support that vignette, and `qte` from `Suggests`.

The two remaining vignettes execute in full, and the two illustrative chunks in
`structures.Rmd` that were `eval = FALSE` are now live -- they had been gated
only because they appeared before the chunk creating their data.

One practical consequence: `R CMD check` no longer needs
`_R_CHECK_FORCE_SUGGESTS_=false` to pass, and the dependency NOTE is gone.

## Correctness and labelling

* **`m = 1` is no longer labelled "DID" for every structure.** Difference-in-
  differences is the special case where the placebo is a pre-treatment measure of
  the outcome. `plm_benchmarks()` previously labelled the `m = 1` row
  `"DID (m = 1)"` for a placebo treatment and a post-outcome placebo too, where
  no such reading exists. The registry now carries `did_equivalent`, and other
  structures read `"Equiconfounding, raw scale (m = 1)"`.
* **The estimand is stated explicitly.** What is partially identified is the
  treatment coefficient in the infeasible long regression -- a linear projection,
  not automatically an average treatment effect. `fit$estimand` and
  `fit$assumptions` record this, `print()` shows both, and the output column
  `estimate` is renamed `adjusted_coefficient`.
* **`plm_bounds()` no longer calls its bootstrap columns a confidence interval.**
  `ci_lower`/`ci_upper` are renamed `lower_boot_q`/`upper_boot_q`. They summarise
  sampling variability in the bounds; they are not a confidence region for the
  identified set, and `?plm_bounds` now distinguishes the four objects that are
  easy to confuse here.
* Bootstrap documentation states the i.i.d. sampling assumption and points to
  cluster-robust `plm_analytic()` for dependent data.

Both renames are clean breaks without alias columns.

## Specifying a structure by causal assumption

* `placebo_lm()` gains `edges`: state the directed edges among the roles `D`
  (treatment), `P` (placebo) and `Y` (outcome) and the package resolves them to
  a structure, reporting which row of the paper's taxonomy it landed on. Naming
  a structure with `structure=` still works; supplying both is an error.
* `plm_edge_table()` documents the mapping, and a test asserts every row of it
  actually resolves as advertised.
* Two edge sets are genuinely ambiguous and require `placebo_role`: no direct
  edges (`P` readable as a placebo outcome or a placebo treatment), and `P->Y`,
  where the paper itself gives two readings of the same graph -- an imperfect
  placebo treatment, or an observed confounder handled on the placebo-outcome
  side. Omitting the role is an error rather than a silent default.
* Contradictory edges (`D->P` with `P->D`), unknown edges, and edges written
  with variable names instead of roles all produce errors that say what is
  wrong. `D->P` plus `P->Y` is the mediator case and is refused through this
  path too.
* Arrows may be written `->`, `<-`, or with unicode arrows, in any case, with
  or without spaces, in any order.

## Faster bootstrap (opt-in)

* `plm_grid()` and `plm_bounds()` gain `engine`. The new `"matrix"` engine
  builds each model matrix once and solves by QR directly, instead of
  re-running `lm()` and `summary.lm()` on a freshly subset data frame every
  replicate. Where two regressions share a right-hand side -- the
  `placebo_outcome` case, which covers both of the paper's applications -- a
  single QR serves both responses.
* Measured, single core, 400 replicates:

  | case | lm | matrix | speedup |
  |---|---|---|---|
  | n = 2000, placebo_outcome | 1.77s | 0.44s | 4.0x |
  | n = 20000, placebo_outcome | 5.06s | 2.33s | 2.2x |
  | n = 2000, post_outcome | 1.21s | 0.42s | 2.9x |

  The gain shrinks with `n`: the overhead removed is fixed, while the QR still
  performed grows. An earlier projection of ~7x came from summing component
  microbenchmarks and overstated the end-to-end result.
* `"lm"` remains the default. It is the path whose numbers back the published
  results, and the fast path is opt-in until it has accrued mileage.
* `test-engine.R` requires agreement to 1e-10 on the coefficient, standard
  error, residual df and scale factor -- replicate by replicate, not just on
  summaries -- for all five structures, across `k` and imperfection grids
  including negatives, at n = 30 and n = 20000, with factor covariates, with no
  covariates, and with a placebo of near-zero residual variance.
* One deliberate behavioural match: `lm()` refuses to fit when a resample leaves
  a factor with one level, so the matrix engine reproduces that refusal rather
  than using the rank-deficient design. Its behaviour is arguably better, but
  changing which replicates are usable is a statistical decision and should not
  arrive via an engine switch.

## Inference calibration

`test-calibration.R` adds Monte Carlo validation, skipped unless
`PLACEBOLM_SLOW_TESTS` is set. Coverage is judged against its Monte Carlo
standard error rather than an arbitrary tolerance. All 20 assertions pass.

* Empirical coverage of percentile-bootstrap, normal-approximation and analytic
  intervals, for both engines, at several sample sizes.
* Targets and sensitivity parameters are **population** quantities, taken once
  from a very large draw and held fixed across simulations. This matters: the
  first version of the suite recomputed the "truth" from each sample, and since
  the recovery identity is exact in sample, the estimator hit that target to
  machine precision on every draw and coverage was 1.0 by construction. The
  tests passed and measured nothing. `plm_population()` in `helper-dgp.R` now
  supplies fixed targets, and a guard test checks they still recover the values
  built into the generator.
* The variance-direction prediction from the submitted draft's Section 2.3
  (`1 + m^2 s2_N/s2_Y - 2 m s_YN/s2_Y`) checked against empirical sampling
  variances -- the sharpest available test of that newly added section.
* The paper's stated reason for recommending the bootstrap when reasoning with
  `k` is demonstrated rather than quoted: intervals that hold `SF` fixed are
  narrower than those that resample it.
* A placebo with little residual variation produces wider intervals, not falsely
  tight ones -- the failure mode the paper's volatility warning implies.
* Behaviour under clustered sampling is covered by tests, so that a future
  change to the resampling scheme cannot alter it unnoticed.

## Statistical validation

* **DGPs matched to each causal structure** (`helper-dgp.R`). Previously one
  shared DGP was used for all five, which forced the oracle-recovery test to
  assert only that the adjustment moved the estimate in the right direction.
* **`test-recovery.R`** verifies that feeding the true sensitivity parameters
  into the estimator returns the long-regression coefficient. This identity is
  exact in sample rather than asymptotic, so it holds to machine precision, for
  every structure, across seeds and effect sizes.
* **The scale factor is validated substantively**, not just algebraically,
  against the paper's own definitions of `k` -- a ratio of partial correlations
  for a placebo outcome, of Cohen's *f* values for a placebo treatment. The
  recovery tests cannot do this, because `SF` cancels within them.
* Invariance properties: `k` is scale-free while `m` is not; `k = 0` returns the
  short-regression coefficient exactly; the adjustment vanishes when the
  postulated imperfection equals the observed sensitivity coefficient; the
  estimate is monotone in `k`.
* **`test-reproducibility.R`** pins bootstrap reproducibility under
  `parallel::mclapply`. Indices are drawn in the parent before forking, so
  `set.seed()` behaves identically at any core count.

Ground-up rewrite around the structure of the method rather than the history of
the code. The arithmetic is unchanged: `plm_estimate()` reproduces the previous
`estimate_PLM()` exactly for every supported structure, and this is enforced by
a regression test (`test-legacy-equivalence.R`).

## New interface

* `placebo_lm()` replaces `placeboLM()`. It takes a **data frame** rather than
  the name of one, and an explicit `structure` argument rather than inferring
  the causal structure from `DP`/`PY` string arguments.
* The regressions are fitted **once**, at construction. Previously every
  user-facing function refit them; `placeboLM_table()` refit them `3 + n_grid`
  times.
* `print()`, `summary()`, `plot()`, `coef()`, and `as.data.frame()` methods.
  Printing a fit no longer dumps the dataset.

## New capabilities

* `plm_bounds()` returns the range of estimates implied by a *range* of
  assumptions — the paper's headline output, previously only readable off a
  plot. Computed exactly rather than by grid search.
* `plm_solve()` inverts the estimator: tipping points ("the estimate is
  negative once k > 0.7") and backing out relative confounding from an external
  benchmark. Closed-form.
* `plm_benchmarks()` returns the three reference estimates as data.
* Both parameterizations are first-class. Supply either `k` (scale-free) or `m`
  (raw bias ratio); conventional difference-in-differences is now expressible
  directly as `m = 1` instead of a derived `k = 1/SF` label.
* `ci_type = "normal"` alongside the percentile bootstrap.
* `plm_structure_table()` documents the supported causal structures.

## Behaviour changes

* **Mediator structures are refused.** The paper states that where `D -> P -> Y`
  "the placebo approach becomes very complicated, and we do not recommend our
  approach"; the previous version computed a number anyway.
* Bootstrap resampling happens **once per call** and is reused across the
  sensitivity grid, rather than being redrawn at every grid point.
* A mistyped sensitivity-parameter name is now an error. Previously it resolved
  to `NULL` and propagated silently as `numeric(0)`.
* A degenerate regression is caught with an informative error. A *large* scale
  factor is explicitly allowed — the paper's own NSW analysis uses a placebo
  with `SF > 40,000`.
* Bootstrap parallelism falls back correctly on Windows.
* Double-placebo support remains out of scope (Appendix B of the paper).

## Analytic and cluster-robust inference

Following the revised paper, the `m`-parameterized adjustment is exactly an
ordinary least squares fit on the pseudo-outcome `Y - m * P`, by
Frisch-Waugh-Lovell. Conditional on the postulated sensitivity parameters, the
standard error of the adjusted estimate is therefore the ordinary standard
error of the treatment coefficient in that regression, under any variance
estimator.

* `plm_regression()` returns that fit, so it can be handed to `sandwich`,
  `lmtest`, or any other variance machinery — which is how cluster-robust
  inference is now available.
* `plm_analytic()` wraps it, returning the estimate, standard error, and
  interval in closed form, with an optional `vcov` argument.

When reasoning with `k` the scale factor is itself estimated; the paper
recommends the nonparametric bootstrap there, which remains the default
(`plm_grid()`).

## Deprecated, not removed

The pre-0.2.0 interface — `placeboLM()`, `estimate_regs()`, `estimate_PLM()`,
`boot_funk()`, `bootstrap_regs()`, `placeboLM_point_estimate()`,
`placeboLM_table()`, `placeboLM_contour_plot()`, `placeboLM_line_plot()`,
`beta_expression_convert()` — is retained in `R/legacy.R` with its arithmetic
unchanged. The appendix of the paper prints example code written against it and
directs readers to install this package from GitHub, so that code must continue
to run. Each entry point warns once per session. `test-legacy-api.R` runs the
paper's published code and checks it reproduces the published figures.

Double placebos raise an error in the legacy interface too.

## Dependencies

Dropped `knitr` from `Imports`. `boot`, `stringr`, and `tidyr` are retained
solely for the deprecated interface; the new code paths use only `stats`,
`graphics`, and `parallel`.

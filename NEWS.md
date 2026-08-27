# PlaceboLM 0.2.0

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

## Removed

`placeboLM()`, `estimate_regs()`, `estimate_PLM()`, `boot_funk()`,
`bootstrap_regs()`, `placeboLM_point_estimate()`, `placeboLM_table()`,
`placeboLM_contour_plot()`, `placeboLM_line_plot()`,
`beta_expression_convert()`.

## Dependencies

Dropped `boot`, `tidyr`, `stringr`, and `knitr` from `Imports`; the bootstrap is
implemented directly against `parallel`.

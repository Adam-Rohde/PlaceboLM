# PlaceboLM

<!-- badges: start -->
<!-- badges: end -->

Partial identification of causal effects using imperfect placebos, implementing
the omitted-variable-bias framework of Rohde and Hazlett, *Causal progress with
imperfect placebo treatments and outcomes*.

Existing placebo-based methods generally require two point assumptions: that
the placebo is **perfect** (zero effect of the placebo treatment on the real
outcome, or of the real treatment on the placebo outcome), and
**equiconfounding** (the placebo relationship suffers exactly the same
confounding as the relationship of interest). Both are often indefensible.

This package replaces them with *ranges*. You postulate an interval for how
imperfect the placebo might be, and an interval for how the confounding
compares, and get back the range of estimates consistent with those
assumptions.

When the placebo is a pre-treatment measure of the outcome, conventional
difference-in-differences is the single point assumption `m = 1`, so the
framework is a reasoned relaxation of parallel trends. In other placebo
settings `m = 1` is still equiconfounding on the raw scale, but it has no
difference-in-differences reading.

## What is being estimated

What the method partially identifies is the coefficient on the treatment in the
*long regression* — the one that would include the unobserved confounders if you
could observe them. That is a linear projection, not automatically an average
treatment effect: the two coincide only under further assumptions, such as no
effect heterogeneity. The package therefore names its output
`adjusted_coefficient` rather than `estimate`, and `fit$estimand` states the
target in words.

## Installation

```r
# install.packages("devtools")
devtools::install_github("Adam-Rohde/PlaceboLM")
```

## Usage

```r
library(PlaceboLM)

fit <- placebo_lm(
  data       = lalonde.psid,
  outcome    = "re78",         # 1978 earnings
  treatment  = "treat",        # job training programme
  placebo    = "re75",         # 1975 earnings: a pre-treatment placebo outcome
  covariates = c("age", "education", "black", "hispanic",
                 "married", "nodegree"),
  structure  = "placebo_outcome"
)
```

**Benchmarks.** The three reference points, as data:

```r
plm_benchmarks(fit)
#>                            benchmark     k      m  imperfection  adjusted_coefficient
#> 1          No unobserved confounding  0.00  0.000            0                 -5928
#> 2                        DID (m = 1)  0.857 1.000            0                  2087
#> 3  Equiconfounding, rescaled (k = 1)  1.00  1.167            0                  3428
```

The middle row is labelled `DID (m = 1)` only for the `placebo_outcome`
structure, where the difference-in-differences reading exists. Elsewhere it
reads `Equiconfounding, raw scale (m = 1)`.

**Bounds** over a range of assumptions — the method's headline output:

```r
plm_bounds(fit, k = c(0.5, 1), n_boot = 1000)
#>   k_low k_high  lower  upper  lower_boot_q  upper_boot_q
#>     0.5      1  -1249   3428          ...           ...
```

`lower`/`upper` are the point bounds. `lower_boot_q`/`upper_boot_q` summarise
bootstrap variability *in those bounds* — they are deliberately not called a
confidence interval, because they carry no coverage guarantee for the
identified set. See `?plm_bounds`.

**Tipping points**, and backing out relative confounding from a benchmark:

```r
plm_solve(fit, target = 0)      # which k flips the sign of the conclusion?
plm_solve(fit, target = 1671)   # which k reproduces the experimental estimate?
```

**Visualisation:**

```r
plm_line_plot(fit, k = c(0, 2), n_boot = 1000)
plm_contour_plot(fit, k = c(0, 2))
```

## Two ways to state relative confounding

| | Meaning | Use when |
|---|---|---|
| `m` | Raw ratio of biases. Conventional DID is exactly `m = 1`. | Placebo and outcome are on the same scale — e.g. pre- and post-treatment measures of the same variable. |
| `k` | Scale-free: a ratio of partial correlations, `m = k * SF`. | They are not. The paper uses binary 1975 employment as a placebo for 1978 earnings in dollars, where `SF > 40,000` and `m` is meaningless. |

Supply whichever you can defend; the package converts.

## Analytic and cluster-robust standard errors

With the `m` parameterization the adjusted estimate is exactly a regression
coefficient on the pseudo-outcome `Y - m * P`, so inference is available in
closed form:

```r
plm_analytic(fit, m = 1)

# any variance estimator you like, including cluster-robust
plm_analytic(fit, m = 1,
             vcov = function(mod) sandwich::vcovCL(mod, cluster = dat$state))
```

Reasoning with `k` instead makes the scale factor an estimated quantity; the
paper recommends the bootstrap there, which is what `plm_grid()` does.

For a faster bootstrap, `engine = "matrix"` builds each model matrix once and
solves by QR rather than re-running `lm()` per replicate — measured 2–4x, with
the larger gains at smaller `n`. It is opt-in and agrees with the default to
1e-10.

```r
plm_grid(fit, k = c(0, 1), n_boot = 2000, engine = "matrix")
```

Note the bootstrap resamples rows independently and the theory assumes i.i.d.
sampling. Under clustered, panel, or spatially dependent sampling those
intervals will generally be too narrow; use the cluster-robust `vcov` route
above on the `m` path instead.

## Supported causal structures

You can name a structure directly, or — usually easier — state your causal
assumptions as edges among the roles `D`, `P` and `Y` and let the package report
which row of the paper's taxonomy that is:

```r
placebo_lm(dat, "Y", "D", "P", covariates = "X", edges = "P->D")
#> Placebo role: Observed Confounder 2  (paper Table 3[e], [f])
#>   Sensitivity parameter: beta_D_P_given_ZX

plm_edge_table()   # the full mapping
```

Two edge sets are genuinely ambiguous — no direct edges, and `P->Y` — and there
`placebo_role` selects between the readings rather than a default being assumed.

```r
plm_structure_table()
```

| `structure` | Paper | Placebo's role |
|---|---|---|
| `placebo_outcome` | Table 2[a],[b] | Shares confounders with `Y`, not caused by `D` |
| `placebo_treatment` | Table 2[a],[c] | Shares confounders with `D`, does not cause `Y` |
| `observed_confounder_1` | Table 2[c] | Causes `Y`, shares confounders with `D` |
| `observed_confounder_2` | Table 3[e],[f] | Causes `D` |
| `post_outcome` | Table 3[g],[h] | A descendant of `Y` (reverse causation) |

Mediator structures (`D -> P -> Y`) are deliberately **not** supported: the
paper states that there "the placebo approach becomes very complicated, and we
do not recommend our approach." Specifying one raises an error pointing to
Zhang and Ding (2022) instead.

## Vignettes

```r
vignette("getting-started", package = "PlaceboLM")  # worked simulation
vignette("structures",      package = "PlaceboLM")  # choosing a structure
vignette("applications",    package = "PlaceboLM")  # NSW and Zika
```

## Reproducing the paper

The example code printed in the paper's appendix uses the pre-0.2.0 interface
(`placeboLM()`, `estimate_regs()`, `estimate_PLM()`, and the
`placeboLM_*_plot()` functions). That interface still works and still produces
the published figures; it warns that it is deprecated. See
`?"PlaceboLM-deprecated"`.

## Reference

Rohde, A. and Hazlett, C. Causal progress with imperfect placebo treatments and
outcomes.

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
compares, and get back the range of effect estimates consistent with those
assumptions. Conventional difference-in-differences falls out as the single
point assumption `m = 1`, so the framework is a reasoned relaxation of parallel
trends.

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
#>                   benchmark     k   m  imperfection  estimate
#> 1 No unobserved confounding  0.00  0.00           0     -5928
#> 2               DID (m = 1)  0.857 1.00           0      2087
#> 3   Equiconfounding (k = 1)  1.00  1.167          0      3428
```

**Bounds** over a range of assumptions — the method's headline output:

```r
plm_bounds(fit, k = c(0.5, 1), n_boot = 1000)
#>   k_low k_high  lower  upper  ci_lower  ci_upper
#>     0.5      1  -1249   3428     ...        ...
```

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
| `k` | Scale-free: a ratio of partial correlations, `m = k * SF`. | They are not. The paper uses binary 1975 unemployment as a placebo for 1978 earnings in dollars, where `SF > 40,000` and `m` is meaningless. |

Supply whichever you can defend; the package converts.

## Supported causal structures

```r
plm_structure_table()
```

| `structure` | Paper | Placebo's role |
|---|---|---|
| `placebo_outcome` | Table 1[a],[b] | Shares confounders with `Y`, not caused by `D` |
| `placebo_treatment` | Table 1[a],[c] | Shares confounders with `D`, does not cause `Y` |
| `observed_confounder_1` | Table 1[c] | Causes `Y`, shares confounders with `D` |
| `observed_confounder_2` | Table 2[e],[f] | Causes `D` |
| `post_outcome` | Table 2[g],[h] | A descendant of `Y` (reverse causation) |

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

## Reference

Rohde, A. and Hazlett, C. Causal progress with imperfect placebo treatments and
outcomes.

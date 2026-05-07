
# PlaceboLM: Causal Progress with Imperfect Placebos

<!-- badges: start -->
<!-- badges: end -->

An R package implementing the partial-identification framework of
Rohde and Hazlett (2025) for making causal progress with imperfect placebo
variables.  Given a placebo outcome or placebo treatment that shares
confounders with the treatment–outcome relationship, the package produces
estimates, bootstrap confidence intervals, summary tables, and plots over
a user-specified range of sensitivity parameters.

## Installation

```r
# install.packages("devtools")
devtools::install_github("Adam-Rohde/PlaceboLM")
```

## Quick start: LaLonde (1986) job-training example

```r
library(PlaceboLM)

data(lalonde, package = "qte")

plm <- placeboLM(
  data                  = lalonde.psid,          # data frame, not a string
  outcome               = "re78",
  treatment             = "treat",
  placebo_outcome       = "re74",                # pre-treatment earnings
  observed_covariates   = c("age", "education", "black",
                             "hispanic", "married", "nodegree"),
  partialIDparam_minmax = list(
    k                 = c(-2, 2),
    coef_P_D_given_XZ = c(-15000, 15000)
  )
)

set.seed(0)
placeboLM_table(plm, n_boot = 200, ptiles = c(0.25, 0.5, 0.75), decimals = 0)

placeboLM_contour_plot(plm, gran = 60)

placeboLM_line_plot(
  plm,
  bootstrap   = TRUE,
  n_boot      = 200,
  ptiles      = c(0, 0.5, 1),
  focus_param = "k",
  ptile_param = "coef_P_D_given_XZ",
  gran        = 15
)
```

## Vignettes

- **Getting Started** — reproduces the LaLonde analysis step by step.
- **Placebo Configurations** — demonstrates all six supported causal structures
  with a synthetic DGP where the oracle answer is known.

```r
vignette("getting-started",       package = "PlaceboLM")
vignette("placebo-configurations", package = "PlaceboLM")
```

## Supported placebo configurations

| `DP` | `PY` | Placebo kind | Interpretation |
|---|---|---|---|
| `""` | `""` | outcome or treatment | No direct path between placebo and treatment/outcome (DiD-like) |
| `"->"` | `""` | outcome | Treatment directly causes the placebo outcome |
| `""` | `"->"` | outcome or treatment | Placebo directly causes the outcome |
| `"->"` | `"->"` | outcome or treatment | Placebo is a mediator; total effect targeted |
| `"<-"` | — | treatment | Placebo is an observed confounder of D |
| — | `"<-"` | outcome | Outcome directly causes the placebo (reverse causation) |

## Reference

Rohde, A. and Hazlett, C. (2025). Causal progress with imperfect placebo
treatments and outcomes. *Journal of the Royal Statistical Society: Series A*.
<https://doi.org/10.1093/jrsssa/qnaf001>

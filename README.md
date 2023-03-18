
# PlaceboLM: Causal Progress with Imperfect Placebos

<!-- badges: start -->
<!-- badges: end -->

Initial R package for making causal progress with imperfect placebos.
See Rohde and Hazlett (20XX) for details.

## Installation

You can install the development version of ‘ripp’ from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Adam-Rohde/ripp")
```

## LaLonde (1986) Example

``` r
library(ripp)

data(lalonde,package = "qte")

plm = placeboLM(
  data = "lalonde.psid",
  placebo_data = NULL,
  outcome = "re78",
  treatment = "treat",
  placebo_outcome = "re74",
  placebo_treatment = "",
  DP = "",
  PY = "",
  observed_covariates = c("age", "education", "black", "hispanic", "married", "nodegree"),
  partialIDparam_minmax = list(k = c(-2,2), coef_P_D_given_XZ = c(-15000,15000))
  )
#> Placebo assumed to have no direct relationship with either treatment or outcome.
#> 
#> Placebo Type: Single Placebo, No Direct Relationships, Placebo Outcome
#> 
#> Regression 1 : lm(re78~treat + age + education + black + hispanic + married + nodegree , data = plm$dta)
#> 
#> Regression 2 : lm(re74~treat + age + education + black + hispanic + married + nodegree , data = plm$dta)
#> 

placeboLM_table(plm,
                n_boot = 100,
                ptiles = c(0.25,0.5,0.75),
                alpha = 0.05)
```

|   k | coef_P\_D_given_XZ |   Estimate | Std. Error |     CI Low |    CI High |
|----:|-------------------:|-----------:|-----------:|-----------:|-----------:|
|  -1 |              -7500 |  -6100.922 |  1177.4214 |  -8239.923 |  -3951.543 |
|  -1 |                  0 | -14971.526 |  1353.4532 | -17675.895 | -12731.882 |
|  -1 |               7500 | -23842.130 |  1496.6576 | -26538.782 | -21412.673 |
|   0 |              -7500 |  -5928.110 |   762.0732 |  -7344.621 |  -4409.371 |
|   0 |                  0 |  -5928.110 |   806.4220 |  -7430.376 |  -4433.818 |
|   0 |               7500 |  -5928.110 |   937.5700 |  -8004.585 |  -4210.544 |
|   1 |              -7500 |  -5755.298 |   868.4238 |  -7418.883 |  -4066.237 |
|   1 |                  0 |   3115.306 |   879.1249 |   1491.171 |   4758.044 |
|   1 |               7500 |  11985.909 |   968.4343 |   9730.748 |  13441.627 |

``` r
placeboLM_contour_plot(plm,
                       gran= 100)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
placeboLM_line_plot(plm,
                    bootstrap=TRUE,
                    n_boot=100,
                    ptiles = c(0.5,0.75),
                    focus_param = "k",
                    ptile_param = "coef_P_D_given_XZ",
                    gran= 10,
                    alpha = 0.05)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" /><img src="man/figures/README-unnamed-chunk-2-3.png" width="100%" />

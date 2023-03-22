
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

set.seed(0)
placeboLM_table(plm,
                n_boot = 1000,
                ptiles = c(0.25,0.5,0.75),
                alpha = 0.05)
```

|              | k       | coef_P\_D_given_XZ | Estimate  | Std. Error | CI Low    | CI High   |
|:-------------|:--------|:-------------------|:----------|:-----------|:----------|:----------|
| SOO          | 0       | 0                  | -5928.11  | 822.57     | -7563.28  | -4337.09  |
| Standard DID | 0.84549 | 0                  | 1718.01   | 852.98     | 27.35     | 3393.96   |
| k=1 DID      | 1       | 0                  | 3115.31   | 882.42     | 1337.4    | 4783.3    |
| Grid         | -1      | -7500              | -6100.92  | 1375.32    | -8843.01  | -3539.27  |
| Grid         | -1      | 0                  | -14971.53 | 1379.99    | -17615.87 | -12160.46 |
| Grid         | -1      | 7500               | -23842.13 | 1484.07    | -26566.69 | -20663    |
| Grid         | 0       | -7500              | -5928.11  | 809.4      | -7473.88  | -4304.65  |
| Grid         | 0       | 0                  | -5928.11  | 842.29     | -7549.07  | -4215.56  |
| Grid         | 0       | 7500               | -5928.11  | 832.39     | -7523.88  | -4262.12  |
| Grid         | 1       | -7500              | -5755.3   | 871.06     | -7441.71  | -4051.86  |
| Grid         | 1       | 0                  | 3115.31   | 926.64     | 1389.42   | 4907.9    |
| Grid         | 1       | 7500               | 11985.91  | 984.87     | 10176.39  | 13963.14  |

``` r
placeboLM_contour_plot(plm,
                       gran= 100)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
placeboLM_line_plot(plm,
                    bootstrap=TRUE,
                    n_boot=1000,
                    ptiles = c(0.5),
                    focus_param = "k",
                    ptile_param = "coef_P_D_given_XZ",
                    gran= 10,
                    alpha = 0.05)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

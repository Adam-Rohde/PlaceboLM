
# PlaceboLM: Causal Progress with Imperfect Placebos

<!-- badges: start -->
<!-- badges: end -->

Initial R package for making causal progress with imperfect placebos.
See Rohde and Hazlett (20XX) for details.

## Installation

You can install the development version of ‘PlaceboLM’ from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Adam-Rohde/PlaceboLM")
```

## LaLonde (1986) Example

``` r
library(PlaceboLM) 

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
                n_boot = 100,
                ptiles = c(0.25,0.5,0.75),
                alpha = 0.05)
#>                           k       coef_P_D_given_XZ Estimate  Std. Error
#> No Unobserved Confounding 0       0                 -5928.11  950.028   
#> DID (m=1)                 0.84549 0                 1718.007  860.272   
#> Perfect Placebo, k=1      1       0                 3115.306  879.816   
#> Grid                      -1      -7500             -6100.922 1454.268  
#> Grid                      -1      0                 -14971.53 1403.223  
#> Grid                      -1      7500              -23842.13 1449.186  
#> Grid                      0       -7500             -5928.11  840.724   
#> Grid                      0       0                 -5928.11  830.05    
#> Grid                      0       7500              -5928.11  866.055   
#> Grid                      1       -7500             -5755.298 927.376   
#> Grid                      1       0                 3115.306  840.085   
#> Grid                      1       7500              11985.91  1066.668  
#>                           CI Low    CI High  
#> No Unobserved Confounding -7735.1   -3952.261
#> DID (m=1)                 161.412   3385.71  
#> Perfect Placebo, k=1      1380.355  4826.311 
#> Grid                      -9012.438 -2726.811
#> Grid                      -17561.15 -11937.26
#> Grid                      -26163.16 -21245.12
#> Grid                      -7597.789 -4408.788
#> Grid                      -7363.709 -4237.757
#> Grid                      -7514.027 -4113.043
#> Grid                      -7375.714 -3950.692
#> Grid                      1555.221  4694.273 
#> Grid                      9964.502  13943.65

placeboLM_contour_plot(plm,
                       gran= 100)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

placeboLM_line_plot(plm,
                    bootstrap=TRUE,
                    n_boot=100,
                    ptiles = c(0,0.5,1),
                    focus_param = "k",
                    ptile_param = "coef_P_D_given_XZ",
                    gran= 10,
                    alpha = 0.05)
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" /><img src="man/figures/README-unnamed-chunk-2-3.png" width="100%" /><img src="man/figures/README-unnamed-chunk-2-4.png" width="100%" />

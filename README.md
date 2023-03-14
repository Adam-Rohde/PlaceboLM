
# ripp: R ImPerfect Placebos

<!-- badges: start -->
<!-- badges: end -->

Initial R package for making causal progress with imperfect placebos.
See Rohde and Hazlett (20XX) for details.

## Installation

You can install the development version of ripp from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Adam-Rohde/ripp")
```

## Example

Here is a simple simulated example of how to use a placebo outcome `N`
to inform partial identification of the effect of `D` on `Y`. We start
by simulating some data. `Z` is an unobserved confounder. Then we fit
`lm(Y ~ D)` and `lm(N ~ D)`. Next, we call the `ripp` function, followed
by `ripp_summary`, `ripp_contour_plot`, and `ripp_line_plot`. We call
`ripp_line_plot` under the assumption that `beta.nd.pxz=0` as well as
under the assumption that `beta.nd.pxz=2`, which is the true value in
the simulation.

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
  partialIDparam_minmax = list(k = c(-2,2), coef_P_D_given_XZ = c(-15000,15000)))
#> Placebo assumed to have no direct relationship with either treatment or outcome.


placeboLM_table(plm,n_boot = 100,ptiles = c(0,0.25,0.5,0.75,1))
#>        k coef_P_D_given_XZ   Estimate Std. Error   95% CI Low 95% CI High
#>  [1,] -2            -15000  11467.475  2003.7434   7467.90535   15537.509
#>  [2,] -2             -7500  -6273.733  2234.1901 -10854.26934   -2167.356
#>  [3,] -2                 0 -24014.941  2337.9918 -28246.09397  -19665.237
#>  [4,] -2              7500 -41756.149  2370.6163 -45581.90209  -37470.883
#>  [5,] -2             15000 -59497.357  2781.5184 -65163.27536  -53896.374
#>  [6,] -1            -15000   2769.682  1329.3477    -21.15387    5277.692
#>  [7,] -1             -7500  -6100.922  1424.9961  -8637.51395   -3495.754
#>  [8,] -1                 0 -14971.526  1321.2365 -17302.31797  -12461.930
#>  [9,] -1              7500 -23842.130  1458.3443 -26520.75818  -21093.144
#> [10,] -1             15000 -32712.733  1759.2001 -36456.19550  -29832.750
#> [11,]  0            -15000  -5928.110   869.2138  -7691.89967   -4193.108
#> [12,]  0             -7500  -5928.110   846.2486  -7641.13490   -4189.406
#> [13,]  0                 0  -5928.110   893.7540  -7754.50942   -4323.066
#> [14,]  0              7500  -5928.110   875.1516  -7512.34142   -4075.087
#> [15,]  0             15000  -5928.110   861.2122  -7651.73878   -4378.338
#> [16,]  1            -15000 -14625.902   812.7556 -16198.08470  -13066.715
#> [17,]  1             -7500  -5755.298   849.2327  -7134.25191   -4153.810
#> [18,]  1                 0   3115.306   831.7695   1758.01043    4674.124
#> [19,]  1              7500  11985.909   980.9997   9822.73064   13776.589
#> [20,]  1             15000  20856.513  1153.9846  18647.07433   22798.447
#> [21,]  2            -15000 -23323.695  1506.3512 -25942.92682  -20141.262
#> [22,]  2             -7500  -5582.487  1432.6348  -8046.45381   -2612.456
#> [23,]  2                 0  12158.721  1521.4297   9457.73904   15487.028
#> [24,]  2              7500  29899.929  1566.5889  26517.02634   32384.101
#> [25,]  2             15000  47641.137  2134.5922  43213.15036   51978.183

placeboLM_contour_plot(plm)
```

<img src="man/figures/README-example-1.png" width="100%" />

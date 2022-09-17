
# Data Overview

Similar to type I error rates, type II error rates are estimated. The
main changes are the alternative hypothesis is true, the null hypothesis
is held constant and effect size varies. The first five data points look
like

    #> # A tibble: 96,000 x 5
    #>   test                   effectSize  stat    pvalue alt      
    #>   <chr>                       <dbl> <dbl>     <dbl> <chr>    
    #> 1 gaussian_mu_one_sample       -0.3  15.7 0.0000759 two.sided
    #> 2 gaussian_mu_one_sample       -0.3  15.2 0.0000983 two.sided
    #> 3 gaussian_mu_one_sample       -0.3  16.2 0.0000557 two.sided
    #> 4 gaussian_mu_one_sample       -0.3  17.3 0.0000319 two.sided
    #> 5 gaussian_mu_one_sample       -0.3  14.9 0.000113  two.sided
    #> # ... with 95,995 more rows
    #> # i Use `print(n = ...)` to see more rows

Multiple experiments are aggregated to calculate type II error rates.

    #> # A tibble: 24 x 3
    #> # Groups:   test [2]
    #>   test                   effectSize Type_II_Error
    #>   <chr>                       <dbl>         <dbl>
    #> 1 gaussian_mu_one_sample      -0.3           0.01
    #> 2 gaussian_mu_one_sample      -0.25          0.04
    #> 3 gaussian_mu_one_sample      -0.2           0.15
    #> 4 gaussian_mu_one_sample      -0.15          0.37
    #> 5 gaussian_mu_one_sample      -0.1           0.64
    #> # ... with 19 more rows
    #> # i Use `print(n = ...)` to see more rows

Like type I calculations, each simulated experiment is based on a sample
size of 200. Each combination of effect size and test are repeated 2,000
times. Where possible, exact tests are included for comparison.

# Overall Type II Error Rate

All tests achieve near 0% type II error for a large enough effect size.

<img src="man/figures/README-typeTwoSummary-1.png" width="100%" />

# Analysis Criteria

For a distribution, the likelihood ratio test works well if

-   Type II error rates are near zero for large effect sizes.
-   When exact tests are implemented in R, type II error rates are
    similar to the exact test.

To check the above, one graph is shown per test. When the effect size is
near 0, type II error rates are near 100%. As effect size grows, type II
error rates decrease. All tests achieve near 0% type II for large enough
effect sizes.

## Gaussian

<img src="man/figures/README-gaussainTypeII-1.png" width="100%" /><img src="man/figures/README-gaussainTypeII-2.png" width="100%" />

## Gamma

<img src="man/figures/README-gammaTypeII-1.png" width="100%" />

## Poisson

<img src="man/figures/README-poissonTypeII-1.png" width="100%" />

## Beta

<img src="man/figures/README-betaTypeII-1.png" width="100%" />

## Exponential

<img src="man/figures/README-exponentialTypeII-1.png" width="100%" />

## Binomial

<img src="man/figures/README-binomTypeII-1.png" width="100%" />

## Negative Binomial

<img src="man/figures/README-negativeBonimialTypeII-1.png" width="100%" />

## Cauchy

<img src="man/figures/README-CauchyTypeII-1.png" width="100%" />

## Inverse Gaussian

<img src="man/figures/README-InvGaussII-1.png" width="100%" />

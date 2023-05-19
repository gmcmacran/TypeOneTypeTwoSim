
# Data Overview

Similar to type I error rates, type II error rates are estimated. The
main changes are the alternative hypothesis is true, the null hypothesis
is held constant and effect size varies. The first five data points look
like

    #> # A tibble: 180,000 × 5
    #>   test                   effectSize  stat      pvalue alt      
    #>   <chr>                       <dbl> <dbl>       <dbl> <chr>    
    #> 1 gaussian_mu_one_sample       -0.2 15.2  0.0000989   two.sided
    #> 2 gaussian_mu_one_sample       -0.2  8.91 0.00283     two.sided
    #> 3 gaussian_mu_one_sample       -0.2 10.1  0.00145     two.sided
    #> 4 gaussian_mu_one_sample       -0.2 27.3  0.000000177 two.sided
    #> 5 gaussian_mu_one_sample       -0.2 20.5  0.00000611  two.sided
    #> # … with 179,995 more rows
    #> # ℹ Use `print(n = ...)` to see more rows

Multiple experiments are aggregated to calculate type II error rates.

    #> # A tibble: 18 × 3
    #> # Groups:   test [2]
    #>   test                   effectSize Type_II_Error
    #>   <chr>                       <dbl>         <dbl>
    #> 1 gaussian_mu_one_sample      -0.2           0   
    #> 2 gaussian_mu_one_sample      -0.15          0.06
    #> 3 gaussian_mu_one_sample      -0.1           0.34
    #> 4 gaussian_mu_one_sample      -0.05          0.75
    #> 5 gaussian_mu_one_sample       0.05          0.75
    #> # … with 13 more rows
    #> # ℹ Use `print(n = ...)` to see more rows

Like type I calculations, each simulated experiment is based on a sample
size of 500. Each combination of effect size and test is repeated 5,000
times. Where possible, exact tests are included for comparison.

# Overall Type II Error Rate

All tests achieve near 0% type II error for a large enough effect size.

<img src="man/figures/README-typeTwoSummary-1.png" width="100%" />

# Analysis Criteria

For a distribution, the likelihood ratio test works well if both of the
following are true.

- Type II error rates are near zero for large effect sizes.
- When exact tests are implemented in R, type II error rates are similar
  to the exact test.

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

## Empirical Likelihood For Mu

<img src="man/figures/README-empTypeII-1.png" width="100%" />

## Empirical Likelihood For Quantile

<img src="man/figures/README-empQuantTypeII-1.png" width="100%" />

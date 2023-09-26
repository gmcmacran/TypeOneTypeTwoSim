
# Repo Overview

This repo is a simulation study of statistical properties for hypothesis
tests in LRTesteR. Each row is an experiment where data are generated
from random number generators and hypothesis test are done. The first
five rows look like

    #> # A tibble: 450,000 × 8
    #>   test                      mu variance   stat pvalue alt       CI_LB CI_UB
    #>   <chr>                  <dbl>    <dbl>  <dbl>  <dbl> <chr>     <dbl> <dbl>
    #> 1 gaussian_mu_one_sample    -4        1 0.251   0.617 two.sided -4.07 -3.89
    #> 2 gaussian_mu_one_sample    -4        1 1.79    0.181 two.sided -4.03 -3.85
    #> 3 gaussian_mu_one_sample    -4        1 1.30    0.255 two.sided -4.04 -3.86
    #> 4 gaussian_mu_one_sample    -4        1 0.453   0.501 two.sided -4.11 -3.94
    #> 5 gaussian_mu_one_sample    -4        1 0.0143  0.905 two.sided -4.09 -3.92
    #> # ℹ 449,995 more rows

For each simulated experiment, both the true hypothesis and the outcome
of the test are known. Multiple experiments are aggregated to calculate
type I error rates.

    #> # A tibble: 30 × 4
    #> # Groups:   test, mu [10]
    #>   test                      mu variance Type_I_Error
    #>   <chr>                  <dbl>    <dbl>        <dbl>
    #> 1 gaussian_mu_one_sample    -4        1         0.05
    #> 2 gaussian_mu_one_sample    -4        3         0.05
    #> 3 gaussian_mu_one_sample    -4        5         0.05
    #> 4 gaussian_mu_one_sample    -2        1         0.05
    #> 5 gaussian_mu_one_sample    -2        3         0.05
    #> # ℹ 25 more rows

Each simulated experiment is based on a sample size of 500. Each
combination of true hypothesis, parameter value, and test is repeated
5,000 times.

Detailed statistical analysis can be found in the type I and type II
folders. Code to run the simulation is in the R Code folder.

# One Sample Type I Error Rate

Most tests have a type I error rate of 5%. Likelihood ratio tests have
similar error rates to exact tests.

<img src="man/figures/README-typeOneSummary-1.png" width="100%" />

# One Way Type I Error Rate

Compared to the one sample tests, type I error rates are further from
the .05 target. The worst performing tests are within half a percentage
point.

<img src="man/figures/README-typeOneSummary2-1.png" width="100%" />

# One Sample Type II Error Rate

All tests achieve near 0% type II error for a large enough effect size.

<img src="man/figures/README-typeTwoSummary-1.png" width="100%" />

# One Way Type II Error Rate

Similar to above, all one way tests have near 0% type II error rate for
large effect sizes.

<img src="man/figures/README-typeTwoSummary2-1.png" width="100%" />

# Confidence Interval Coverage

Most functions have a coverage rate of 95%. The worst performing
confidence intervals are within one percentage point.

<img src="man/figures/README-CISummary-1.png" width="100%" />

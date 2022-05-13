
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overall Type II Error Rate

Asymptotic type II error rates for LRTesteR’s likelihood ratio tests are
estimated via simulation. Calculations are based on 5,000 iterations and
a sample size of 200. Simulation holds the null hypothesis constant
across all alternatives and effect sizes. Where possible, exact tests
are included for comparison.

Most tests can achieve near 0% type II error for a large enough effect
size. The test for a geometric distribution’s p stands out as not
getting close to zero. Nearly the entire range of effect size was tested
for the geometric test, and it did not get close to zero.

<img src="man/figures/README-typeTwoSummary-1.png" width="100%" />

# Successful Tests

For a distribution, the likelihood ratio test works well if

-   Type II error rates decrease quickly.
-   When exact tests are implemented in R, type II error rates are
    similar to the exact test.

To check the above, one graph is shown per test.

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

# Failures

For a distribution, the likelihood ratio test is considered bad if

-   Type II error rate never hits zero.

## Geometric

For the greater than and two sided alternatives, this test has 100% type
II error when the null is .50. It will never reject the null regardless
of the data. For all alternatives it never does better than .20%. This
test unusable.

It is interesting to note the likelihood test approximation worked very
well. The test it is approximating is unusable.

<img src="man/figures/README-geometricTypeII-1.png" width="100%" />

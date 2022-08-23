
# Repo Overview

This repo estimates asymptotic type I error rates, asymptotic type II
error rates, and asymptotic coverage rates for confidence intervals for
likelihood ratio tests in LRTesteR. Detailed analysis can be found in
the type I and type II folders. Calculations based on 2,000 iterations
and a sample size of 200.

# One Sample Type I Error Rate

Most tests have a type I error rate of .05. Likelihood ratio tests match
exact tests.

<img src="man/figures/README-typeOneSummary-1.png" width="100%" />

# One Way Type I Error Rate

Most tests are within half a percentage point of 5%. The one way test
for binomial p stands out as having as having a 6% type I error rate
instead of 5%.

<img src="man/figures/README-typeOneSummary2-1.png" width="100%" />

# One Sample Type II Error Rate

All tests can achieve near 0% type II error for a large enough effect
size.

<img src="man/figures/README-typeTwoSummary-1.png" width="100%" />

# One Way Type II Error Rate

Similar to above, all tests can achieve near 0% type II for a large
effect size.

<img src="man/figures/README-typeTwoSummary2-1.png" width="100%" />

# Confidence Interval Coverage

Most functions have a coverage rate of 95%. The worst performing
confidence intervals are within half a percentage point.

<img src="man/figures/README-CISummary-1.png" width="100%" />

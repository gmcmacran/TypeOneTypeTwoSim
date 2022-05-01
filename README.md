
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Repo Overview

This repo estimates asymptotic type I and type II error rates for
likelihood ratio tests in LRTesteR. Detailed analysis can be found in
the type I and type II folders. Calculations based on 5,000 iterations
and a sample size of 200.

## Summary of Type I Error Rate

Most tests have a type I error rate of .05. Likelihood ratio tests match
exact tests.

<img src="man/figures/README-typeOneSummary-1.png" width="100%" />

## Summary of Type II Error Rate

Most tests can achieve near 0% type II error for a large enough effect
size. The test for a geometric distribution’s p test does not.

<img src="man/figures/README-typeTwoSummary-1.png" width="100%" />

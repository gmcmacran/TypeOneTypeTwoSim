
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Code Overview

Code is split between distribution. Within each distribution (and R
file), each parameter has a test. Each test gets a for loop that
generates data and runs the test for both type I and type II error. All
code ultimately saves a tibble where one row is a one experiment.The
gaussian sim file is a good example of how all R files are organized.

Each script is self contained and should be ran as a job within RStudio
in parallel. If the code takes a long time to execute, type I and type
II error simulations are split. The beta distribution’s type I error is
split between parameters.

## Compute Time

R code should be ran in jobs. The source of long compute time for beta,
gamma, and cauchy simulations is MLEs don’t have closed forms and rely
on numerical searches. These numeral searches are a part of larger
numerical searches to calculate confidence intervals.

Run times are based on a Intel 6700k overclocked to 4.4 gigs.

-   beta sim type II: 11 hours 14 minutes.
-   beta sim type I shape 1: 3 hours 50 minutes.
-   beta sim type I shape 2: 3 hours 50 minutes.
-   cauchy sim type I: 2 hours and 35 minutes.
-   cauchy sim type II: 38 minutes.
-   gamma sim type I: 1 hour 44 minutes.
-   gamma sim type II: 53 minutes.
-   gaussian sim: 12 minutes.
-   binomial sim: 1 minute.
-   exponential sim: 4 minutes.
-   negative binomial sim: 5 minutes.
-   poisson sim: 5 minutes.

There should be 4 jobs running concurrently. Beyond 4, stability becomes
an issue. There were namespace errors, cache value errors and job
failures without error. This may or may not be associated with the fact
the Intel 6700k has 4 cores or the 200 megahertz overclock.

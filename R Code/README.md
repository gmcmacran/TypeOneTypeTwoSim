
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Code Overview

Code is split between distribution. Within each distribution (and R
file), each parameter has a test. Each test gets a for loop that
generates data and runs the test for both type I and type II error. All
code ultimately saves a tibble where one row is a one experiment.The
gaussian sim file is a good example of how all R files are organized.

If the code takes a long time to execute, type I and type II error
simulations are split. The beta distribution’s type I error is split
between parameters.

Each script is self contained and should be ran as a job within RStudio
in parallel. Working directory needs to be the folder containing the R
project file.

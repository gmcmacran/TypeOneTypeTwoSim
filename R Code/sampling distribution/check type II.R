# Simple script to explore type II results and run a few sanity checks.

library(LRTesteR)
library(tidyverse)

###############
# Load data
###############
load_df <- function(fn) {
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat)
  return(DF)
}

fns <- c(
  "gaussian_type_two.rds",
  "gaussian_type_two_one_way.rds",
  "gamma_type_two_rate.rds",
  "gamma_type_two_rate_one_way.rds",
  "gamma_type_two_scale.rds",
  "gamma_type_two_scale_one_way.rds",
  "gamma_type_two_shape.rds",
  "gamma_type_two_shape_one_way.rds",
  "poisson_type_two.rds",
  "poisson_type_two_one_way.rds",
  "beta_type_two.rds",
  "beta_type_two_one_way.rds",
  "negative_binomial_type_two.rds",
  "negative_binomial_type_two_one_way.rds",
  "exponential_type_two.rds",
  "exponential_type_two_one_way.rds",
  "binomail_type_two.rds",
  "binomail_type_two_one_way.rds",
  "cauchy_type_two.rds",
  "cauchy_type_two_one_way.rds"
)

typeII <- map_dfr(fns, load_df)

typeII %>%
  drop_na() %>%
  nrow() == typeII %>%
  nrow()

typeII %>%
  distinct(test) %>%
  nrow() == 26

typeII %>%
  distinct(alt) %>%
  nrow() == 3

typeII %>%
  filter(alt == "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeII %>%
  filter(alt == "two.sided", stat < 0) %>%
  nrow()

library(LRTesteR)
library(tidyverse)

###############
# one sample
###############
load_df <- function(fn) {
  if (str_detect(fn, "one") & !str_detect(fn, "_one_way") & !str_detect(fn, "exponential")) {
    print(str_c("Possible Error. fn: ", fn))
  }
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat, pvalue, effectSize)
  return(DF)
}

fns <- c(
  "gaussian_type_two.rds",
  "gamma_type_two_rate.rds",
  "gamma_type_two_scale.rds",
  "gamma_type_two_shape.rds",
  "poisson_type_two.rds",
  "beta_type_two.rds",
  "negative_binomial_type_two.rds",
  "exponential_type_two.rds",
  "binomail_type_two.rds",
  "cauchy_type_two.rds",
  "inverse_gaussian_type_two.rds",
  "empirical_type_two.rds"
)

typeII <- map_dfr(fns, load_df)

typeII %>%
  drop_na() %>%
  nrow() == typeII %>%
  nrow()

typeII %>%
  distinct(test) %>%
  nrow() == 17

typeII %>%
  distinct(alt) %>%
  nrow() == 3

typeII %>%
  filter(alt == "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeII %>%
  filter(alt != "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeII %>%
  filter(alt != "two.sided") %>%
  group_by(test) %>%
  summarise(minStat = min(stat), maxStat = max(stat)) %>%
  arrange(test) %>%
  print(n = Inf)

typeII %>%
  summarise(
    P_LB = all(pvalue >= 0),
    P_UB = all(pvalue <= 1)
  )

rm(list = ls())

###############
# two sample
###############
load_df <- function(fn) {
  if (str_detect(fn, "one") & !str_detect(fn, "_one_way") & !str_detect(fn, "exponential")) {
    print(str_c("Possible Error. fn: ", fn))
  }
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat, pvalue, effectSize)
  return(DF)
}

fns <- c(
  "gaussian_type_two_one_way.rds",
  "gamma_type_two_rate_one_way.rds",
  "gamma_type_two_scale_one_way.rds",
  "gamma_type_two_shape_one_way.rds",
  "poisson_type_two_one_way.rds",
  "beta_type_two_one_way.rds",
  "negative_binomial_type_two_one_way.rds",
  "exponential_type_two_one_way.rds",
  "binomail_type_two_one_way.rds",
  "cauchy_type_two_one_way.rds",
  "inverse_gaussian_type_two_one_way.rds",
  "empirical_type_two_one_way.rds"
)

typeII <- map_dfr(fns, load_df)

typeII %>%
  drop_na() %>%
  nrow() == typeII %>%
  nrow()

typeII %>%
  distinct(test) %>%
  nrow() == 17

typeII %>%
  distinct(alt) %>%
  nrow() == 1

typeII %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeII %>%
  summarise(
    P_LB = all(pvalue >= 0),
    P_UB = all(pvalue <= 1)
  )

rm(list = ls())

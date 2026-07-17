library(LRTesteR)
library(tidyverse)
library(sn)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 10) # Only 10 values of skew are tested
B <- 5000
N <- 500

calc_alpha <- function(skew) {
  stopifnot(all(abs(skew) <= .99))
  b <- sqrt(2 / pi)
  r <- (2 * abs(skew) / (4 - pi))^(2 / 3)
  delta <- sign(skew) * sqrt(r / (b^2 * (1 + r)))
  alpha <- delta / sqrt(1 - delta^2)
  return(alpha)
}

################
# Type I
################
skews <- seq(-.9, .9, .2)

run_sim <- function(skews) {
  sim_results <- tibble()
  for (skew in skews) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "empirical_skewness_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- rsn(n = N, xi = 0, omega = 1, alpha = calc_alpha(skew))
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- empirical_skewness_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, skew = skew, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
  }
  return(sim_results)
}
sim_results <- future_map_dfr(skews, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(skew) %>%
  nrow() == length(skews)

sim_results %>%
  distinct(alt) %>%
  nrow() == 1

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_skewness_type_one_one_way.rds")

rm(sim_results, run_sim, skews)

################
# Type II
################
skew <- 0
skewEffectSizes <- seq(-.9, .9, .2) %>%
  round(2)
skewEffectSizes <- skewEffectSizes[skewEffectSizes < 0 | skewEffectSizes > 0]

run_sim <- function(skewEffectSizes) {
  sim_results <- tibble()
  for (skewEffectSize in skewEffectSizes) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "empirical_skewness_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- c(rsn(n = N / 2, xi = 0, omega = 1, alpha = calc_alpha(skew)), rsn(n = N / 2, xi = 0, omega = 1, alpha = calc_alpha(skew + skewEffectSize)))
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- empirical_skewness_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, effectSize = skewEffectSize, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, skewEffectSize, test)
  }
  return(sim_results)
}
sim_results <- future_map_dfr(skewEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(alt) %>%
  nrow() == 1

sim_results %>%
  distinct(alt, test) %>%
  nrow() == 1

sim_results %>%
  filter(test == "empirical_skewness_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(skewEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_skewness_type_two_one_way.rds")

rm(list = ls())

library(LRTesteR)
library(tidyverse)
library(stringr)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 4)
compiler::enableJIT(3)
B <- 5000
N <- 500

calc_shape <- function(skew) {
  shape <- (2 / skew)^2
  return(shape)
}

################
# Type I
################
skews <- seq(1, 4, 1)

run_sim <- function(skews) {
  sim_results <- tibble()
  for (skew in skews) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "empirical_skewness_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rgamma(n = N, shape = calc_shape(skew))
        test <- empirical_skewness_one_sample(x, skew, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, skew = skew, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
    }
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
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  filter(alt == "two.sided") %>%
  summarise(CICheck = all(CI_LB < CI_UB))

# save
sim_results %>%
  saveRDS("results/empirical_skewness_type_one.rds")

rm(skews, sim_results)

################
# Type II
################
skewEffectSizes <- round(seq(-.40, .40, .10), 2)
skewEffectSizes <- skewEffectSizes[skewEffectSizes != 0]

run_sim <- function(skewEffectSizes) {
  sim_results <- tibble()
  for (skewEffectSize in skewEffectSizes) {
    if (skewEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "empirical_skewness_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(n = N, shape = calc_shape(2 + skewEffectSize))
          test <- empirical_skewness_one_sample(x, 2, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = skewEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    } else {
      for (alt in c("two.sided", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "empirical_skewness_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(n = N, shape = calc_shape(2 + skewEffectSize))
          test <- empirical_skewness_one_sample(x, 2, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = skewEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    }
  }
  rm(alt, skewEffectSize, x, test)
  return(sim_results)
}

sim_results <- future_map_dfr(skewEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  distinct(alt, test) %>%
  nrow() == 3

sim_results %>%
  filter(test == "empirical_skewness_one_sample") %>%
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
  saveRDS("results/empirical_skewness_type_two.rds")

rm(list = ls())

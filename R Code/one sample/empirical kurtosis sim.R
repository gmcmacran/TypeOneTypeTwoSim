library(LRTesteR)
library(tidyverse)
library(stringr)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 8)
compiler::enableJIT(3)
B <- 5000
N <- 500

calc_df <- function(kurtosis) {
  v <- 4 + (6 / kurtosis)
  return(v)
}

################
# Type I
################
kurtosis_es <- seq(.1, 1.4, .1)
all(calc_df(kurtosis_es) > 8) # Need all degrees of freedom above 8 so the 8th moment is finite.

run_sim <- function(kurtosis_es) {
  sim_results <- tibble()
  for (kurtosis in kurtosis_es) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "empirical_kurtosis_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rt(n = N, df = calc_df(kurtosis))
        test <- empirical_kurtosis_one_sample(x, kurtosis, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, kurtosis = kurtosis, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
    }
  }
  return(sim_results)
}

sim_results <- future_map_dfr(kurtosis_es, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(kurtosis) %>%
  nrow() == length(kurtosis_es)

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
  saveRDS("results/empirical_kurtosis_type_one.rds")

rm(kurtosis_es, sim_results, run_sim)

################
# Type II
################
kurtosisEffectSizes <- round(seq(-.30, .30, .10), 2)
kurtosisEffectSizes <- kurtosisEffectSizes[kurtosisEffectSizes != 0]

run_sim <- function(kurtosisEffectSizes) {
  sim_results <- tibble()
  for (kurtosisEffectSize in kurtosisEffectSizes) {
    if (kurtosisEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "empirical_kurtosis_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rt(n = N, df = calc_df(1 + kurtosisEffectSize))
          test <- empirical_kurtosis_one_sample(x, 1, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = kurtosisEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    } else {
      for (alt in c("two.sided", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "empirical_kurtosis_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rt(n = N, df = calc_df(1 + kurtosisEffectSize))
          test <- empirical_kurtosis_one_sample(x, 1, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = kurtosisEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    }
  }
  rm(alt, kurtosisEffectSize, x, test)
  return(sim_results)
}

sim_results <- future_map_dfr(kurtosisEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

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
  filter(test == "empirical_kurtosis_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(kurtosisEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_kurtosis_type_two.rds")

rm(list = ls())

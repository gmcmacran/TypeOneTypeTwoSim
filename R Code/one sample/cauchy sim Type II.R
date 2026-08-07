library(LRTesteR)
library(tidyverse)
library(stringr)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 5)
B <- 5000
N <- 500

################
# Type II
################

location0 <- 10
scale0 <- 10
locationEffectSizes <- seq(-3, 3, .5) %>%
  round(2) %>%
  setdiff(0)

run_sim <- function(locationEffectSizes) {
  sim_results <- tibble()
  for (locationEffectSize in locationEffectSizes) {
    if (locationEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "cauchy_location_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rcauchy(n = N, location = location0 + locationEffectSize, scale = scale0)
          test <- cauchy_location_test(x, location0, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = locationEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    } else {
      for (alt in c("two.sided", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "cauchy_location_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rcauchy(n = N, location = location0 + locationEffectSize, scale = scale0)
          test <- cauchy_location_test(x, location0, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = locationEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    }
  }
  return(sim_results)
}

sim_results_part_one <- future_map_dfr(locationEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

location0 <- 10
scale0 <- 10
scaleEffectSizes <- seq(-3, 3, .5) %>%
  setdiff(0)

run_sim <- function(scaleEffectSizes) {
  sim_results <- tibble()
  for (scaleEffectSize in scaleEffectSizes) {
    if (scaleEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "cauchy_scale_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rcauchy(n = N, location = location0, scale = scale0 + scaleEffectSize)
          test <- cauchy_scale_test(x, scale0, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = scaleEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    } else {
      for (alt in c("two.sided", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "cauchy_scale_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rcauchy(n = N, location = location0, scale = scale0 + scaleEffectSize)
          test <- cauchy_scale_test(x, scale0, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = scaleEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    }
  }
  return(sim_results)
}

sim_results_part_two <- future_map_dfr(scaleEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

sim_results <- bind_rows(sim_results_part_one, sim_results_part_two)

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 2

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  distinct(alt, test) %>%
  nrow() == 6

sim_results %>%
  filter(test == "cauchy_location_test") %>%
  distinct(effectSize) %>%
  nrow() == length(locationEffectSizes)

sim_results %>%
  filter(test == "cauchy_scale_test") %>%
  distinct(effectSize) %>%
  nrow() == length(scaleEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/cauchy_type_two.rds")

plan(sequential)
rm(list = ls())

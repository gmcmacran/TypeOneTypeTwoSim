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

location0 <- 0
scale0 <- 1
locationEffectSizes <- seq(2, 8, 2) %>%
  round(2) %>%
  setdiff(0)

run_sim <- function(locationEffectSizes) {
  sim_results <- tibble()
  for (locationEffectSize in locationEffectSizes) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "cauchy_location_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- c(rcauchy(n = N / 2, location = location0, scale = scale0), rcauchy(n = N / 2, location = location0 + locationEffectSize, scale = scale0))
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- cauchy_location_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, effectSize = locationEffectSize, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, test, x)
  }
  return(sim_results)
}

sim_results_part_one <- future_map_dfr(locationEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

location0 <- 0
scale0 <- 1
scaleEffectSizes <- seq(2, 6, 2)

run_sim <- function(scaleEffectSizes) {
  sim_results <- tibble()
  for (scaleEffectSize in scaleEffectSizes) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "cauchy_scale_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- c(rcauchy(n = N / 2, location = location0, scale = scale0), rcauchy(n = N / 2, location = location0, scale = scale0 + scaleEffectSize))
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- cauchy_scale_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, effectSize = scaleEffectSize, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, test, x)
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
  nrow() == 1

sim_results %>%
  distinct(alt, test) %>%
  nrow() == 2

sim_results %>%
  filter(test == "cauchy_location_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(locationEffectSizes)

sim_results %>%
  filter(test == "cauchy_scale_one_way") %>%
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
  saveRDS("results/cauchy_type_two_one_way.rds")

plan(sequential)
rm(list = ls())

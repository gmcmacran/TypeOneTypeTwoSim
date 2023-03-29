library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
N <- 500

################
# Type II
################

location0 <- 10
scale0 <- 10
locationEffectSizes <- seq(-4, 4, 1) %>%
  round(2) %>%
  setdiff(0)

sim_results <- tibble()
for (locationEffectSize in locationEffectSizes) {
  if (locationEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "cauchy_location_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rcauchy(n = N, location = location0 + locationEffectSize, scale = scale0)
        test <- cauchy_location_one_sample(x, location0, alt)
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
      testName <- "cauchy_location_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rcauchy(n = N, location = location0 + locationEffectSize, scale = scale0)
        test <- cauchy_location_one_sample(x, location0, alt)
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

rm(alt, locationEffectSize, x)

location0 <- 10
scale0 <- 10
scaleEffectSizes <- seq(-4, 4, 1) %>%
  setdiff(0)

for (scaleEffectSize in scaleEffectSizes) {
  if (scaleEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "cauchy_scale_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rcauchy(n = N, location = location0, scale = scale0 + scaleEffectSize)
        test <- cauchy_scale_one_sample(x, scale0, alt)
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
      testName <- "cauchy_scale_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rcauchy(n = N, location = location0, scale = scale0 + scaleEffectSize)
        test <- cauchy_scale_one_sample(x, scale0, alt)
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
  filter(test == "cauchy_location_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(locationEffectSizes)

sim_results %>%
  filter(test == "cauchy_scale_one_sample") %>%
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

rm(alt, scaleEffectSizes, x, test)

rm(list = ls())

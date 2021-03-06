library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
B <- 5000
N <- 200

################
# Type II
################

shape10 <- 5
shape20 <- 5
shape1EffectSizes <- seq(-2, 2, .25) %>%
  setdiff(0)

sim_results <- tibble()
for (shape1EffectSize in shape1EffectSizes) {
  if (shape1EffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape1_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape10 + shape1EffectSize, shape2 = shape20)
        test <- beta_shape1_lr_test(x, shape10, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = shape1EffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
  else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape1_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape10 + shape1EffectSize, shape2 = shape20)
        test <- beta_shape1_lr_test(x, shape10, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = shape1EffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

rm(alt, shape1EffectSize, x, test)

shape2EffectSizes <- seq(-2, 2, .25) %>%
  setdiff(0)

for (shape2EffectSize in shape2EffectSizes) {
  if (shape2EffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape2_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape10, shape2 = shape20 + shape2EffectSize)
        test <- beta_shape2_lr_test(x, shape20, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = shape2EffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
  else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape2_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape10, shape2 = shape20 + shape2EffectSize)
        test <- beta_shape2_lr_test(x, shape20, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = shape2EffectSize, stat = stats, pvalue = pvalues, alt = alts)
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
  filter(test == "beta_shape1_lr_test") %>%
  distinct(effectSize) %>%
  nrow() == length(shape1EffectSizes)

sim_results %>%
  filter(test == "beta_shape2_lr_test") %>%
  distinct(effectSize) %>%
  nrow() == length(shape2EffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/beta_type_two.rds")

rm(list = ls())

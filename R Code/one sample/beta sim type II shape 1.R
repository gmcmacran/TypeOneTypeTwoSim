library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 500

################
# Type II
################

shape10 <- 5
shape20 <- 5
shape1EffectSizes <- seq(-1.25, 1.25, .25) %>%
  setdiff(0)

sim_results <- tibble()
for (shape1EffectSize in shape1EffectSizes) {
  if (shape1EffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape1_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rbeta(N, shape1 = shape10 + shape1EffectSize, shape2 = shape20)
        test <- beta_shape1_one_sample(x, shape10, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = shape1EffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape1_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rbeta(N, shape1 = shape10 + shape1EffectSize, shape2 = shape20)
        test <- beta_shape1_one_sample(x, shape10, alt)
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
  distinct(effectSize) %>%
  nrow() == length(shape1EffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/beta_type_two_shape1.rds")

rm(list = ls())

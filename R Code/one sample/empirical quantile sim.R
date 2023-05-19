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
# Type I
################
Qs <- seq(.05, .95, .05)

sim_results <- tibble()
for (Q in Qs) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    CI_LBs <- vector(mode = "numeric", length = B)
    CI_UBs <- vector(mode = "numeric", length = B)
    testName <- "empirical_quantile_one_sample"
    for (i in 1:B) {
      set.seed(i)
      x <- rnorm(n = N, mean = 0, sd = 1)
      value <- qnorm(Q, mean = 0, sd = 1)
      test <- empirical_quantile_one_sample(x, Q, value, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
      CI_LBs[i] <- test$conf.int[1]
      CI_UBs[i] <- test$conf.int[2]
    }
    temp <- tibble(test = testName, Q = Q, value = value, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(Q) %>%
  nrow() == length(Qs)

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
  filter(
    alt == "two.sided",
    !is.na(CI_LB),
    !is.na(CI_UB)
  ) %>%
  summarise(CICheck = all(CI_LB < CI_UB))

# save
sim_results %>%
  saveRDS("results/empirical_quantile_type_one.rds")

rm(Q, Qs, sim_results, alt)

################
# Type II
################
value0 <- 3
variance0 <- 4
valueEffectSizes <- round(seq(-.45, .45, .10), 2)
valueEffectSizes <- valueEffectSizes[valueEffectSizes != 0]

sim_results <- tibble()
for (valueEffectSize in valueEffectSizes) {
  if (valueEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "empirical_quantile_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = value0 + valueEffectSize, sd = variance0^.5)
        test <- empirical_quantile_one_sample(x, .50, value0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = valueEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "empirical_quantile_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = value0 + valueEffectSize, sd = variance0^.5)
        test <- empirical_quantile_one_sample(x, .50, value0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = valueEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
rm(alt, valueEffectSize, x, test)

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
  nrow() == length(valueEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_quantile_type_two.rds")

rm(list = ls())

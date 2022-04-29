library(MLTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
B <- 5000
N <- 200

################
# Type I
################
rates <- 1:15

sim_results <- tibble()

for (rate in rates) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "exponentail_rate_lr_test"
    set.seed(1)
    for (i in 1:B) {
      x <- x <- rexp(n = N, rate = rate)
      test <- exponentail_rate_lr_test(x, rate, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, rate = rate, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i)
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/exponentail_type_one.rds")

rm(test, alt, rate, rates, x, sim_results)

################
# Type II
################
rate0 <- 5

rateEffectSizes <- seq(-1.2, 1.2, .10) %>%
  round(2) %>%
  setdiff(0)

sim_results <- tibble()
for (rateEffectSize in rateEffectSizes) {
  if (rateEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "exponentail_rate_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- x <- rexp(n = N, rate = rate0 + rateEffectSize)
        test <- exponentail_rate_lr_test(x, rate0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = rateEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
  else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "exponentail_rate_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- x <- rexp(n = N, rate = rate0 + rateEffectSize)
        test <- exponentail_rate_lr_test(x, rate0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = rateEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

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
  filter(test == "exponentail_rate_lr_test") %>%
  distinct(effectSize) %>%
  nrow() == length(rateEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  saveRDS("results/exponentail_type_two.rds")

rm(list = ls())

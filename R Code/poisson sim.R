library(LRTesteR)
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

lambdas <- 1:15

sim_results <- tibble()

for (lambda in lambdas) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "poisson_lambda_lr_test"
    set.seed(1)
    for (i in 1:B) {
      x <- rpois(n = N, lambda = lambda)
      test <- poisson_lambda_lr_test(x, lambda, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, lambda = lambda, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i)
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(lambda) %>%
  nrow() == length(lambdas)

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
  saveRDS("results/poisson_type_one.rds")

rm(test, alt, lambda, lambdas, x, sim_results)

################
# Type II
################
lambda0 <- 5

lambdaEffectSizes <- seq(-.60, .60, .10) %>%
  round(2) %>%
  setdiff(0)

sim_results <- tibble()
for (lambdaEffectSize in lambdaEffectSizes) {
  if (lambdaEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "poisson_lambda_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rpois(n = N, lambda = lambda0 + lambdaEffectSize)
        test <- poisson_lambda_lr_test(x, lambda0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = lambdaEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
  else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "poisson_lambda_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rpois(n = N, lambda = lambda0 + lambdaEffectSize)
        test <- poisson_lambda_lr_test(x, lambda0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = lambdaEffectSize, stat = stats, pvalue = pvalues, alt = alts)
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
  filter(test == "poisson_lambda_lr_test") %>%
  distinct(effectSize) %>%
  nrow() == length(lambdaEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  saveRDS("results/poisson_type_two.rds")

rm(list = ls())

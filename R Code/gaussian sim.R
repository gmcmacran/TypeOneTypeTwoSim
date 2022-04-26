library(MLTesteR)
library(tidyverse)
library(stringr)
library(EnvStats)

################
# Type I
################
B <- 5000
N <- 50

mus <- -3:3
variances <- 1:5

sim_results <- tibble()
for (mu in mus) {
  for (variance in variances) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_mean_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- gaussian_mu_lr_test(x, mu, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_variance_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- gaussian_variance_lr_test(x, variance, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
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
  distinct(mu) %>%
  nrow() == length(mus)

sim_results %>%
  distinct(variance) %>%
  nrow() == length(variances)

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
  saveRDS("results/gaussian_type_one.rds")

rm(sim_results, x, test, alt, mu, variance)

sim_results_02 <- tibble()
for (mu in mus) {
  for (variance in variances) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "t.test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- t.test(x = x, mu = mu, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "varTest"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- varTest(x = x, sigma.squared = variance, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

# Check structure
sim_results_02 %>%
  distinct(test) %>%
  nrow() == 2

sim_results_02 %>%
  distinct(mu) %>%
  nrow() == length(mus)

sim_results_02 %>%
  distinct(variance) %>%
  nrow() == length(variances)

sim_results_02 %>%
  distinct(alt) %>%
  nrow() == 3

sim_results_02 %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results_02 %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results_02 %>%
  saveRDS("results/gaussian_type_one_exact.rds")

rm(list = ls())

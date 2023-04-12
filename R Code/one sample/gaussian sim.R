library(LRTesteR)
library(tidyverse)
library(stringr)
library(EnvStats)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
N <- 2000

################
# Type I
################
mus <- seq(-4, 4, 2)
variances <- c(1, 3, 5)

sim_results <- tibble()
for (mu in mus) {
  for (variance in variances) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "gaussian_mu_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- gaussian_mu_one_sample(x, mu, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "gaussian_variance_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- gaussian_variance_one_sample(x, variance, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
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

all(sim_results$CI_LB < sim_results$CI_UB)

# save
sim_results %>%
  saveRDS("results/gaussian_type_one.rds")

rm(sim_results, x, test, alt, mu)

sim_results_02 <- tibble()
for (mu in mus) {
  for (variance in variances) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "t.test"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- t.test(x = x, mu = mu, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "varTest"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- varTest(x = x, sigma.squared = variance, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
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

all(sim_results_02$CI_LB < sim_results_02$CI_UB)

# save
sim_results_02 %>%
  saveRDS("results/gaussian_type_one_exact.rds")

rm(sim_results_02, x, test, alt, mu, mus, variance, variances)

################
# Type II
################

mu0 <- 0
variance0 <- 1
muEffectSizes <- seq(-.30, .30, .05) %>%
  round(2) %>%
  setdiff(0)

sim_results <- tibble()
for (muEffectSize in muEffectSizes) {
  if (muEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_mu_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0 + muEffectSize, sd = variance0^.5)
        test <- gaussian_mu_one_sample(x, mu0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = muEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_mu_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0 + muEffectSize, sd = variance0^.5)
        test <- gaussian_mu_one_sample(x, mu0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = muEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

rm(alt, muEffectSize, x)

mu0 <- 0
variance0 <- 15
varianceEffectSizes <- seq(-6, 6, 1) %>%
  setdiff(0)

for (varianceEffectSize in varianceEffectSizes) {
  if (varianceEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_variance_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0, sd = (variance0 + varianceEffectSize)^.5)
        test <- gaussian_variance_one_sample(x, variance0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = varianceEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_variance_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0, sd = (variance0 + varianceEffectSize)^.5)
        test <- gaussian_variance_one_sample(x, variance0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = varianceEffectSize, stat = stats, pvalue = pvalues, alt = alts)
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
  filter(test == "gaussian_mu_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(muEffectSizes)

sim_results %>%
  filter(test == "gaussian_variance_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(varianceEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/gaussian_type_two.rds")

rm(alt, varianceEffectSize, x, test)

mu0 <- 0
variance0 <- 1

sim_results_02 <- tibble()
for (muEffectSize in muEffectSizes) {
  if (muEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "t.test"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0 + muEffectSize, sd = variance0^.5)
        test <- t.test(x = x, mu = mu0, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = muEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "t.test"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0 + muEffectSize, sd = variance0^.5)
        test <- t.test(x = x, mu = mu0, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = muEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

rm(alt, muEffectSize, x)

mu0 <- 0
variance0 <- 15

for (varianceEffectSize in varianceEffectSizes) {
  if (varianceEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "varTest"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0, sd = (variance0 + varianceEffectSize)^.5)
        test <- varTest(x = x, sigma.squared = variance0, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = varianceEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "varTest"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0, sd = (variance0 + varianceEffectSize)^.5)
        test <- varTest(x = x, sigma.squared = variance0, alternative = alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = varianceEffectSize, stat = stats, pvalue = pvalues, alt = alts)
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
  distinct(alt) %>%
  nrow() == 3

sim_results_02 %>%
  distinct(alt, test) %>%
  nrow() == 6

sim_results_02 %>%
  filter(test == "t.test") %>%
  distinct(effectSize) %>%
  nrow() == length(muEffectSizes)

sim_results_02 %>%
  filter(test == "varTest") %>%
  distinct(effectSize) %>%
  nrow() == length(varianceEffectSizes)

sim_results_02 %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results_02 %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results_02 %>%
  saveRDS("results/gaussian_type_two_exact.rds")

rm(alt, varianceEffectSize, x, test)

rm(list = ls())

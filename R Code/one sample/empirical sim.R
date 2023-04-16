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
mus <- seq(-3, 3, .50)
variance <- 1

sim_results <- tibble()
for (mu in mus) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    CI_LBs <- vector(mode = "numeric", length = B)
    CI_UBs <- vector(mode = "numeric", length = B)
    testName <- "empirical_mu_one_sample"
    for (i in 1:B) {
      set.seed(i)
      x <- rnorm(n = N, mean = mu, sd = variance^.5)
      test <- empirical_mu_one_sample(x, mu, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
      CI_LBs[i] <- test$conf.int[1]
      CI_UBs[i] <- test$conf.int[2]
    }
    temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(mu) %>%
  nrow() == length(mus)

sim_results %>%
  distinct(variance) %>%
  nrow() == 1

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
  filter(alt == "two.sided") %>%
  summarise(CICheck = all(CI_LB < CI_UB))

# save
sim_results %>%
  saveRDS("results/empirical_type_one.rds")

rm(mu, mus, variance, sim_results, alt)

################
# Type II
################
mu0 <- 3
variance0 <- 4
muEffectSizes <- round(seq(-.70, .70, .10), 2)
muEffectSizes <- muEffectSizes[muEffectSizes != 0]

sim_results <- tibble()
for (muEffectSize in muEffectSizes) {
  if (muEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "empirical_mu_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0 + muEffectSize, sd = variance0^.5)
        test <- empirical_mu_one_sample(x, mu0, alt)
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
      testName <- "empirical_mu_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rnorm(n = N, mean = mu0 + muEffectSize, sd = variance0^.5)
        test <- empirical_mu_one_sample(x, mu0, alt)
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
rm(alt, muEffectSize, x, test)

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
  filter(test == "empirical_mu_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(muEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_type_two.rds")

rm(list = ls())

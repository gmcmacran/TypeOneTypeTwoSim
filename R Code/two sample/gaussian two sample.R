library(LRTesteR)
library(tidyverse)
library(lmtest)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
N <- 200

################
# Type I
################
mus <- seq(-4, 4, 2)
variances <- c(1, 3, 5)

sim_results <- tibble()
for (mu in mus) {
  for (variance in variances) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "gaussian_mu_one_way"
    set.seed(1)
    for (i in 1:B) {
      x <- rnorm(n = N, mean = mu, sd = variance^.5)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- gaussian_mu_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)

    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "gaussian_variance_one_way"
    set.seed(1)
    for (i in 1:B) {
      x <- rnorm(n = N, mean = mu, sd = variance^.5)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- gaussian_variance_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
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
  nrow() == 1

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/gaussian_type_one_one_way.rds")

rm(sim_results, mu, mus, variance, variances)

################
# Type II
################

mu <- 0
variance <- 1
muEffectSizes <- seq(1, 3, 1) %>%
  round(2)

sim_results <- tibble()
for (muEffectSize in muEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "gaussian_mu_one_way"
  set.seed(1)
  for (i in 1:B) {
    x <- c(rnorm(n = N / 2, mean = mu, sd = variance^.5), rnorm(n = N / 2, mean = mu + muEffectSize, sd = variance^.5))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- gaussian_mu_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = muEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, muEffectSize, test)
}

mu <- 0
variance <- 1
varianceEffectSizes <- seq(1, 5, 2)

for (varianceEffectSize in varianceEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "gaussian_variance_one_way"
  set.seed(1)
  for (i in 1:B) {
    x <- c(rnorm(n = N / 2, mean = mu, sd = variance^.5), rnorm(n = N / 2, mean = mu, sd = (variance + varianceEffectSize)^.5))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- gaussian_variance_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = varianceEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, varianceEffectSize, test)
}


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
  filter(test == "gaussian_mu_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(muEffectSizes)

sim_results %>%
  filter(test == "gaussian_variance_one_way") %>%
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
  saveRDS("results/gaussian_type_two_one_way.rds")

rm(list = ls())

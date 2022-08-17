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
    for (alt in c("two.sided", "less", "greater")) {
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
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
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

rm(sim_results, x, test, alt, mu)

sim_results_02 <- tibble()
for (mu in mus) {
  for (variance in variances) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "lmtest_gaussian_mu"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
        dat <- tibble(x = x, fctr = fctr)

        model <- lm(x ~ fctr)
        test <- lrtest(model)
        stats[i] <- test[["Chisq"]][2]
        pvalues[i] <- test[["Pr(>Chisq)"]][2]
        alts[i] <- "two.sided"
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, dat)
    }
  }
}

# Check structure
sim_results_02 %>%
  distinct(test) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(mu) %>%
  nrow() == length(mus)

sim_results_02 %>%
  distinct(variance) %>%
  nrow() == length(variances)

sim_results_02 %>%
  distinct(alt) %>%
  nrow() == 1

sim_results_02 %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results_02 %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results_02 %>%
  saveRDS("results/gaussian_type_one_one_way_exact.rds")

rm(sim_results_02, x, test, alt, mu, mus, variance, variances)

################
# Type II
################

mu <- 0
variance <- 1
muEffectSizes <- seq(.05, .30, .05) %>%
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
  rm(stats, pvalues, alts, testName, temp, i)
}


rm(muEffectSize, x)

mu <- 0
variance <- 15
varianceEffectSizes <- seq(1, 5, 1)

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
  rm(stats, pvalues, alts, testName, temp, i)
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

rm(varianceEffectSize, x, test)

mu <- 0
variance <- 1

sim_results_02 <- tibble()
for (muEffectSize in muEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "lmtest_gaussian_mu"
  set.seed(1)
  for (i in 1:B) {
    x <- c(rnorm(n = N / 2, mean = mu, sd = variance^.5), rnorm(n = N / 2, mean = mu + muEffectSize, sd = variance^.5))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    dat <- tibble(x = x, fctr = fctr)

    model <- lm(x ~ fctr)
    test <- lrtest(model)
    stats[i] <- test[["Chisq"]][2]
    pvalues[i] <- test[["Pr(>Chisq)"]][2]
    alts[i] <- "two.sided"
  }
  temp <- tibble(test = testName, effectSize = muEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results_02 <- sim_results_02 %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, dat)
}

rm(muEffectSize, x)

# Check structure
sim_results_02 %>%
  distinct(test) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(alt) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(alt, test) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(effectSize) %>%
  nrow() == length(muEffectSizes)

sim_results_02 %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results_02 %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results_02 %>%
  saveRDS("results/gaussian_type_two_one_way_exact.rds")

rm(list = ls())

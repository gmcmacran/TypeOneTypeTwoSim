library(LRTesteR)
library(tidyverse)
library(stringr)
library(statmod)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
N <- 200

stat.fun <- function(x) {
  out <- mean(x)
  return(out)
}

################
# Type I
################
mus <- seq(1, 9, 2)
shapes <- seq(1, 9, 2)

sim_results <- tibble()
for (mu in mus) {
  for (shape in shapes) {
    dispersion <- 1 / shape
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "bootstrap_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rinvgauss(n = N, mean = mu, shape = shape)
        test <- bootstrap_one_sample(x, mu, stat.fun, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, mu = mu, shape = shape, dispersion = dispersion, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
    }
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
  distinct(shape) %>%
  nrow() == length(shapes)

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
  saveRDS("results/bootstrap_type_one.rds")

rm(mu, mus, shape, shapes, dispersion, sim_results, alt)

################
# Type II
################
mu0 <- 10
shape0 <- 2
muEffectSizes <- seq(-2, 2, 1) %>%
  setdiff(0)

sim_results <- tibble()
for (muEffectSize in muEffectSizes) {
  if (muEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "bootstrap_one_sample"
      set.seed(1)
      for (i in 1:B) {
        x <- rinvgauss(n = N, mean = mu0 + muEffectSize, shape = shape0)
        test <- bootstrap_one_sample(x, mu0, stat.fun, alt)
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
      testName <- "bootstrap_one_sample"
      set.seed(1)
      for (i in 1:B) {
        x <- rinvgauss(n = N, mean = mu0 + muEffectSize, shape = shape0)
        test <- bootstrap_one_sample(x, mu0, stat.fun, alt)
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
  filter(test == "bootstrap_one_sample") %>%
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
  saveRDS("results/bootstrap_type_two.rds")

rm(list = ls())

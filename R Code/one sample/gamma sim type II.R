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
shape0 <- 5
rate0 <- 1
scale0 <- 1 / rate0

rateEffectSizes <- seq(-.45, .45, .05) %>%
  setdiff(0)

sim_results <- tibble()
for (rateEffectSize in rateEffectSizes) {
  if (rateEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_rate_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rgamma(N, shape = shape0, rate = rate0 + rateEffectSize)
        test <- gamma_rate_one_sample(x, rate0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = rateEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_rate_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rgamma(N, shape = shape0, rate = rate0 + rateEffectSize)
        test <- gamma_rate_one_sample(x, rate0, alt)
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
sim_results %>% saveRDS("results/gamma_type_two_rate.rds")
rm(sim_results)
rm(alt, rateEffectSize, x, test)

scaleEffectSizes <- seq(-.45, .45, .05) %>%
  setdiff(0)

sim_results <- tibble()
for (scaleEffectSize in scaleEffectSizes) {
  if (scaleEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_scale_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rgamma(N, shape = shape0, scale = scale0 + scaleEffectSize)
        test <- gamma_scale_one_sample(x, scale0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = scaleEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_scale_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rgamma(N, shape = shape0, scale = scale0 + scaleEffectSize)
        test <- gamma_scale_one_sample(x, scale0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = scaleEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("results/gamma_type_two_scale.rds")
rm(sim_results)
rm(alt, scaleEffectSize, x, test)

shapeEffectSizes <- seq(-2, 2, .5) %>%
  setdiff(0)

sim_results <- tibble()
for (shapeEffectSize in shapeEffectSizes) {
  if (shapeEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_shape_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rgamma(N, shape = shape0 + shapeEffectSize, rate = rate0)
        test <- gamma_shape_one_sample(x, shape0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = shapeEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  } else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_shape_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rgamma(N, shape = shape0 + shapeEffectSize, rate = rate0)
        test <- gamma_shape_one_sample(x, shape0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = shapeEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("results/gamma_type_two_shape.rds")
rm(sim_results)
rm(alt, shapeEffectSize, x, test)

sim_results <- bind_rows(
  readRDS(file = "results/gamma_type_two_rate.rds"),
  readRDS(file = "results/gamma_type_two_scale.rds"),
  readRDS(file = "results/gamma_type_two_shape.rds")
)

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 3

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  distinct(alt, test) %>%
  nrow() == 9

sim_results %>%
  filter(test == "gamma_rate_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(rateEffectSizes)

sim_results %>%
  filter(test == "gamma_scale_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(scaleEffectSizes)

sim_results %>%
  filter(test == "gamma_shape_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(shapeEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

rm(list = ls())

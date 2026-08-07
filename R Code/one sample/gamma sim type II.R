library(LRTesteR)
library(tidyverse)
library(stringr)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 5)
B <- 5000
N <- 500

################
# Type II
################
shape0 <- 5
rate0 <- 1
scale0 <- 1 / rate0

rateEffectSizes <- round(seq(-.30, .30, .05), 2) %>%
  setdiff(0)

run_sim <- function(rateEffectSizes) {
  sim_results <- tibble()
  for (rateEffectSize in rateEffectSizes) {
    if (rateEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "gamma_rate_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(N, shape = shape0, rate = rate0 + rateEffectSize)
          test <- gamma_rate_test(x, rate0, alt)
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
        testName <- "gamma_rate_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(N, shape = shape0, rate = rate0 + rateEffectSize)
          test <- gamma_rate_test(x, rate0, alt)
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
  return(sim_results)
}

sim_results <- future_map_dfr(rateEffectSizes, run_sim, .options = furrr_options(seed = TRUE))
sim_results %>% saveRDS("results/gamma_type_two_rate.rds")
rm(sim_results)

scaleEffectSizes <- round(seq(-.30, .30, .05), 2) %>%
  setdiff(0)

run_sim <- function(scaleEffectSizes) {
  sim_results <- tibble()
  for (scaleEffectSize in scaleEffectSizes) {
    if (scaleEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "gamma_scale_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(N, shape = shape0, scale = scale0 + scaleEffectSize)
          test <- gamma_scale_test(x, scale0, alt)
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
        testName <- "gamma_scale_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(N, shape = shape0, scale = scale0 + scaleEffectSize)
          test <- gamma_scale_test(x, scale0, alt)
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
  return(sim_results)
}
sim_results <- future_map_dfr(scaleEffectSizes, run_sim, .options = furrr_options(seed = TRUE))
sim_results %>% saveRDS("results/gamma_type_two_scale.rds")
rm(sim_results)

shapeEffectSizes <- seq(-1.5, 1.5, .25) %>%
  setdiff(0)

run_sim <- function(shapeEffectSizes) {
  sim_results <- tibble()
  for (shapeEffectSize in shapeEffectSizes) {
    if (shapeEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "gamma_shape_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(N, shape = shape0 + shapeEffectSize, rate = rate0)
          test <- gamma_shape_test(x, shape0, alt)
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
        testName <- "gamma_shape_test"
        for (i in 1:B) {
          set.seed(i)
          x <- rgamma(N, shape = shape0 + shapeEffectSize, rate = rate0)
          test <- gamma_shape_test(x, shape0, alt)
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
  return(sim_results)
}
sim_results <- future_map_dfr(shapeEffectSizes, run_sim, .options = furrr_options(seed = TRUE))
sim_results %>% saveRDS("results/gamma_type_two_shape.rds")
rm(sim_results)

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
  filter(test == "gamma_rate_test") %>%
  distinct(effectSize) %>%
  nrow() == length(rateEffectSizes)

sim_results %>%
  filter(test == "gamma_scale_test") %>%
  distinct(effectSize) %>%
  nrow() == length(scaleEffectSizes)

sim_results %>%
  filter(test == "gamma_shape_test") %>%
  distinct(effectSize) %>%
  nrow() == length(shapeEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

plan(sequential)
rm(list = ls())

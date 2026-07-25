library(LRTesteR)
library(tidyverse)
library(stringr)
library(statmod)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 5)
B <- 5000
N <- 500

################
# Type I
################
mus <- seq(1, 9, 2)
shapes <- seq(1, 9, 2)

run_sim <- function(mus) {
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
        testName <- "inverse_gaussian_mu_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(n = N, mean = mu, shape = shape)
          test <- inverse_gaussian_mu_one_sample(x, mu, alt)
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

  for (mu in mus) {
    for (shape in shapes) {
      dispersion <- 1 / shape
      for (alt in c("two.sided", "less", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        CI_LBs <- vector(mode = "numeric", length = B)
        CI_UBs <- vector(mode = "numeric", length = B)
        testName <- "inverse_gaussian_shape_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(n = N, mean = mu, shape = shape)
          test <- inverse_gaussian_shape_one_sample(x, shape, alt)
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

  for (mu in mus) {
    for (shape in shapes) {
      dispersion <- 1 / shape
      for (alt in c("two.sided", "less", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        CI_LBs <- vector(mode = "numeric", length = B)
        CI_UBs <- vector(mode = "numeric", length = B)
        testName <- "inverse_gaussian_dispersion_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(n = N, mean = mu, dispersion = dispersion)
          test <- inverse_gaussian_dispersion_one_sample(x, dispersion, alt)
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
  return(sim_results)
}

sim_results <- future_map_dfr(mus, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 3

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

all(sim_results$CI_LB < sim_results$CI_UB)

# save
sim_results %>%
  saveRDS("results/inverse_gaussian_type_one.rds")

rm(mus, shapes, sim_results, run_sim)

################
# Type II
################
mu0 <- 10
shape0 <- 2
muEffectSizes <- seq(-6, 6, 1) %>%
  setdiff(0)

run_sim <- function(muEffectSizes) {
  sim_results <- tibble()
  for (muEffectSize in muEffectSizes) {
    if (muEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "inverse_gaussian_mu_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(n = N, mean = mu0 + muEffectSize, shape = shape0)
          test <- inverse_gaussian_mu_one_sample(x, mu0, alt)
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
        testName <- "inverse_gaussian_mu_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(n = N, mean = mu0 + muEffectSize, shape = shape0)
          test <- inverse_gaussian_mu_one_sample(x, mu0, alt)
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
  return(sim_results)
}

sim_results_part_one <- future_map_dfr(muEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

mu0 <- 2
shape0 <- 10
shapeEffectSizes <- seq(-6, 6, 1) %>%
  setdiff(0)

run_sim <- function(shapeEffectSizes) {
  sim_results <- tibble()
  for (shapeEffectSize in shapeEffectSizes) {
    if (shapeEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "inverse_gaussian_shape_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(n = N, mean = mu0, shape = shape0 + shapeEffectSize)
          test <- inverse_gaussian_shape_one_sample(x, shape0, alt)
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
        testName <- "inverse_gaussian_shape_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(n = N, mean = mu0, shape = shape0 + shapeEffectSize)
          test <- inverse_gaussian_shape_one_sample(x, shape0, alt)
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
  rm(alt, shapeEffectSize, x, test)
  return(sim_results)
}

sim_results_part_two <- future_map_dfr(shapeEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

mu0 <- 2
dispersion0 <- 10
dispersionEffectSizes <- seq(-4, 4, 1) %>%
  setdiff(0)

run_sim <- function(dispersionEffectSizes) {
  sim_results <- tibble()
  for (dispersionEffectSize in dispersionEffectSizes) {
    if (dispersionEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "inverse_gaussian_dispersion_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(N, mean = mu0, dispersion = dispersion0 + dispersionEffectSize)
          test <- inverse_gaussian_dispersion_one_sample(x, dispersion0, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = dispersionEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    } else {
      for (alt in c("two.sided", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "inverse_gaussian_dispersion_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rinvgauss(N, mean = mu0, dispersion = dispersion0 + dispersionEffectSize)
          test <- inverse_gaussian_dispersion_one_sample(x, dispersion0, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = dispersionEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    }
  }
  rm(alt, dispersionEffectSize, x, test)
  return(sim_results)
}

sim_results_part_three <- future_map_dfr(dispersionEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

sim_results <- bind_rows(sim_results_part_one, sim_results_part_two, sim_results_part_three)

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
  filter(test == "inverse_gaussian_mu_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(muEffectSizes)

sim_results %>%
  filter(test == "inverse_gaussian_shape_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(shapeEffectSizes)

sim_results %>%
  filter(test == "inverse_gaussian_dispersion_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(dispersionEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/inverse_gaussian_type_two.rds")

plan(sequential)
rm(list = ls())

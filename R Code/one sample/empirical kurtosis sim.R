library(LRTesteR)
library(tidyverse)
library(stringr)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 15) # Only 15 values in kurtosis_es
B <- 5000
N <- 500

# Two-component normal scale mixture. With probability p, sd is s1.
# Otherwise sd is s2. Solves for s1 given a target excess kurtosis.
# Reachable ceiling is 3/p - 3. (57 for p = .05.)
calc_s1 <- function(exkurt, p = .05, s2 = 1) {
  q <- 1 - p
  k3 <- exkurt + 3
  a <- 3 * p - k3 * p^2
  b <- -2 * k3 * p * q * s2^2
  cc <- 3 * q * s2^4 - k3 * q^2 * s2^4
  disc <- pmax(b^2 - 4 * a * cc, 0) # exactly zero at exkurt = 0; guard rounding
  s1squared <- (-b + sqrt(disc)) / (2 * a)
  return(sqrt(s1squared))
}


# Symmetric beta. Excess kurtosis is -6 / (2a + 3). Covers (-2, 0).
calc_a <- function(exkurt) {
  a <- -3 / exkurt - 3 / 2
  return(a)
}

# Negative targets come from a symmetric beta. Positive (and zero)
# targets come from the normal scale mixture. Both are symmetric and
# have all moments finite, so any grid value is valid for the test.
rkurt <- function(n, exkurt, p = .05) {
  if (exkurt < 0) {
    x <- rbeta(n, calc_a(exkurt), calc_a(exkurt))
  } else {
    s1 <- calc_s1(exkurt, p)
    x <- rnorm(n, 0, ifelse(runif(n) < p, s1, 1))
  }
  return(x)
}

################
# Type I
################
kurtosis_es <- c(seq(-1.2, -.2, .2), seq(0, 1.2, .2))
stopifnot(all(kurtosis_es > -2), all(kurtosis_es < 3 / .05 - 3))

run_sim <- function(kurtosis_es) {
  sim_results <- tibble()
  for (kurtosis in kurtosis_es) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "empirical_kurtosis_one_sample"
      for (i in 1:B) {
        set.seed(i)
        x <- rkurt(n = N, exkurt = kurtosis)
        test <- empirical_kurtosis_one_sample(x, kurtosis, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, kurtosis = kurtosis, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i, test, x, CI_LBs, CI_UBs)
    }
  }
  return(sim_results)
}

sim_results <- future_map_dfr(kurtosis_es, run_sim, .options = furrr_options(seed = TRUE))

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(kurtosis) %>%
  nrow() == length(kurtosis_es)

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
  saveRDS("results/empirical_kurtosis_type_one.rds")

rm(kurtosis_es, sim_results, run_sim)

################
# Type II
################
kurtosisEffectSizes <- round(seq(-.30, .30, .10), 2)
kurtosisEffectSizes <- kurtosisEffectSizes[kurtosisEffectSizes != 0]

run_sim <- function(kurtosisEffectSizes) {
  sim_results <- tibble()
  for (kurtosisEffectSize in kurtosisEffectSizes) {
    if (kurtosisEffectSize < 0) {
      for (alt in c("two.sided", "less")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "empirical_kurtosis_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rkurt(n = N, exkurt = 1 + kurtosisEffectSize)
          test <- empirical_kurtosis_one_sample(x, 1, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = kurtosisEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    } else {
      for (alt in c("two.sided", "greater")) {
        stats <- vector(mode = "numeric", length = B)
        pvalues <- vector(mode = "numeric", length = B)
        alts <- vector(mode = "character", length = B)
        testName <- "empirical_kurtosis_one_sample"
        for (i in 1:B) {
          set.seed(i)
          x <- rkurt(n = N, exkurt = 1 + kurtosisEffectSize)
          test <- empirical_kurtosis_one_sample(x, 1, alt)
          stats[i] <- test$statistic
          pvalues[i] <- test$p.value
          alts[i] <- test$alternative
        }
        temp <- tibble(test = testName, effectSize = kurtosisEffectSize, stat = stats, pvalue = pvalues, alt = alts)
        sim_results <- sim_results %>% bind_rows(temp)
        rm(stats, pvalues, alts, testName, temp, i)
      }
    }
  }
  return(sim_results)
}

sim_results <- future_map_dfr(kurtosisEffectSizes, run_sim, .options = furrr_options(seed = TRUE))

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
  filter(test == "empirical_kurtosis_one_sample") %>%
  distinct(effectSize) %>%
  nrow() == length(kurtosisEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_kurtosis_type_two.rds")

rm(list = ls())

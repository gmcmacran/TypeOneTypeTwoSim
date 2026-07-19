library(LRTesteR)
library(tidyverse)
library(furrr)

################
# Simulation settings
################
plan(multisession, workers = 13) # Only 13 values of kurtosis are tested
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
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "empirical_kurtosis_one_way"
    for (i in 1:B) {
      set.seed(i)
      x <- rkurt(n = N, exkurt = kurtosis)
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- empirical_kurtosis_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, kurtosis = kurtosis, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
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
  nrow() == 1

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_kurtosis_type_one_one_way.rds")

rm(sim_results, run_sim, kurtosis_es)

################
# Type II
################
kurtosisEffectSizes <- c(round(seq(-1, -.2, .2), 2), round(seq(.2, 1, .2), 2))

run_sim <- function(kurtosisEffectSizes) {
  sim_results <- tibble()
  for (kurtosisEffectSize in kurtosisEffectSizes) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "empirical_kurtosis_one_way"
    for (i in 1:B) {
      set.seed(i)
      kurtosis <- sign(kurtosisEffectSize) * -.2
      x <- c(rkurt(n = N / 2, exkurt = kurtosis), rkurt(n = N / 2, exkurt = kurtosis + kurtosisEffectSize))
      fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
      test <- empirical_kurtosis_one_way(x, fctr)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, effectSize = kurtosisEffectSize, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i, fctr, x, kurtosisEffectSize, test)
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
  nrow() == 1

sim_results %>%
  distinct(alt, test) %>%
  nrow() == 1

sim_results %>%
  filter(test == "empirical_kurtosis_one_way") %>%
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
  saveRDS("results/empirical_kurtosis_type_two_one_way.rds")

rm(list = ls())

library(LRTesteR)
library(tidyverse)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 500

################
# Type I
################
Qs <- seq(.05, .95, .05)

sim_results <- tibble()
for (Q in Qs) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "empirical_quantile_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- rnorm(n = N, mean = 0, sd = 1)
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- empirical_quantile_one_way(x, Q, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, Q = Q, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(Q) %>%
  nrow() == length(Qs)

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
  saveRDS("results/empirical_quantile_type_one_one_way.rds")

rm(sim_results, Q, Qs)

################
# Type II
################

mu <- 0
variance <- 1
muEffectSizes <- seq(-.50, .50, .10) %>%
  round(2)
muEffectSizes <- muEffectSizes[muEffectSizes != 0]

sim_results <- tibble()
for (muEffectSize in muEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "empirical_quantile_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rnorm(n = N / 2, mean = mu, sd = variance^.5), rnorm(n = N / 2, mean = mu + muEffectSize, sd = variance^.5))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- empirical_quantile_one_way(x, .50, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = muEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, muEffectSize, test)
}

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
  saveRDS("results/empirical_quantile_type_two_one_way.rds")

rm(list = ls())

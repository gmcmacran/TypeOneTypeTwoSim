library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
N <- 200

################
# Type I
################
rates <- 1:15

sim_results <- tibble()

for (rate in rates) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "exponential_rate_one_way"
  set.seed(1)
  for (i in 1:B) {
    x <- rexp(n = N, rate = rate)
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- exponential_rate_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, rate = rate, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, test, x)
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

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
  saveRDS("results/exponential_type_one_one_way.rds")

rm(rate, rates, sim_results)

################
# Type II
################
rate0 <- 5

rateEffectSizes <- seq(-1.2, 1.2, .10) %>%
  round(2) %>%
  setdiff(0)

sim_results <- tibble()
for (rateEffectSize in rateEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "exponential_rate_one_way"
  set.seed(1)
  for (i in 1:B) {
    x <- c(rexp(n = N / 2, rate = rate0), rexp(n = N / 2, rate = rate0 + rateEffectSize))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- exponential_rate_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = rateEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, test, x)
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
  filter(test == "exponential_rate_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(rateEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  saveRDS("results/exponential_type_two_one_way.rds")

rm(list = ls())

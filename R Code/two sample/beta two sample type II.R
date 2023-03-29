library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
N <- 500

################
# Type II
################

shape10 <- 5
shape20 <- 5
shape1EffectSizes <- seq(-2, 2, .25) %>%
  setdiff(0)

sim_results <- tibble()
for (shape1EffectSize in shape1EffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "beta_shape1_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rbeta(N / 2, shape1 = shape10, shape2 = shape20), rbeta(N / 2, shape1 = shape10 + shape1EffectSize, shape2 = shape20))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- beta_shape1_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = shape1EffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr)
}

rm(shape1EffectSize, x, test)

shape2EffectSizes <- seq(-2, 2, .25) %>%
  setdiff(0)

for (shape2EffectSize in shape2EffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "beta_shape2_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rbeta(N / 2, shape1 = shape10, shape2 = shape20), rbeta(N / 2, shape1 = shape10, shape2 = shape20 + shape2EffectSize))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- beta_shape2_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = shape2EffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr)
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
  filter(test == "beta_shape1_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(shape1EffectSizes)

sim_results %>%
  filter(test == "beta_shape2_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(shape2EffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/beta_type_two_one_way.rds")

rm(list = ls())

library(LRTesteR)
library(tidyverse)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 500

calc_shape <- function(skew) {
  shape <- (2 / skew)^2
  return(shape)
}

################
# Type I
################
skews <- seq(1, 4, 1)

sim_results <- tibble()
for (skew in skews) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "empirical_skewness_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- rgamma(n = N, shape = calc_shape(skew))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- empirical_skewness_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, skew = skew, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, test)
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(skew) %>%
  nrow() == length(skews)

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
  saveRDS("results/empirical_skewness_type_one_one_way.rds")

rm(sim_results, skew, skews)

################
# Type II
################
skew <- 3
skewEffectSizes <- seq(.50, 2.0, .50) %>%
  round(2)

sim_results <- tibble()
for (skewEffectSize in skewEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "empirical_skewness_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rgamma(n = N / 2, shape = calc_shape(skew)), rgamma(n = N / 2, shape = calc_shape(skew + skewEffectSize)))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- empirical_skewness_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = skewEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, skewEffectSize, test)
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
  filter(test == "empirical_skewness_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(skewEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/empirical_skewness_type_two_one_way.rds")

rm(list = ls())

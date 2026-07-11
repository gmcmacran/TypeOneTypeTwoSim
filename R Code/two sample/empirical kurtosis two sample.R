library(LRTesteR)
library(tidyverse)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 500

calc_df <- function(kurtosis) {
  v <- 4 + (6 / kurtosis)
  return(v)
}

################
# Type I
################
kurtosis_es <- seq(.1, 1.4, .1)
all(calc_df(kurtosis_es) > 8) # Need all degrees of freedom above 8 so the 8th moment is finite.

sim_results <- tibble()
for (kurtosis in kurtosis_es) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "empirical_kurtosis_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- rt(n = N, df = calc_df(kurtosis))
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
  saveRDS("results/empirical_kurtosis,_type_one_one_way.rds")

rm(sim_results, kurtosis, kurtosis_es)

################
# Type II
################
kurtosis <- .1
kurtosisEffectSizes <- seq(.1, 1.3, .2) %>%
  round(2)

all(calc_df(kurtosis) > 8)
all(calc_df(kurtosis + kurtosisEffectSizes) > 8)

sim_results <- tibble()
for (kurtosisEffectSize in kurtosisEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "empirical_kurtosis_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rt(n = N / 2, df = calc_df(kurtosis)), rt(n = N / 2, df = calc_df(kurtosis + kurtosisEffectSizes)))
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

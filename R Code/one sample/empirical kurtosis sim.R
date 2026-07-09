library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 500

calc_df <- function(kurtosis) {
  excess_kurtosis <- kurtosis - 3
  v <- 4 + (6 / excess_kurtosis)
  return(v)
}

################
# Type I
################
kurtosis_es <- c(15, 9, 6, 5, 4)


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
      x <- rt(n = N, df = calc_df(kurtosis))
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

rm(kurtosis, kurtosis_es, sim_results, alt)

################
# Type II
################
kurtosisEffectSizes <- round(seq(-.30, .30, .10), 2)
kurtosisEffectSizes <- kurtosisEffectSizes[kurtosisEffectSizes != 0]

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
        x <- rt(n = N, df = calc_df(4 + kurtosisEffectSize))
        test <- empirical_kurtosis_one_sample(x, 4, alt)
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
        x <- rt(n = N, df = calc_df(4 + kurtosisEffectSize))
        test <- empirical_kurtosis_one_sample(x, 4, alt)
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
rm(alt, kurtosisEffectSize, x, test)

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

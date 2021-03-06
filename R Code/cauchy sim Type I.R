library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
B <- 5000
N <- 200

################
# Type I
################
locations <- seq(-4, 4, 2)
scales <- seq(1, 9, 2)

sim_results <- tibble()
for (location in locations) {
  for (scale in scales) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "cauchy_location_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rcauchy(N, location, scale)
        test <- cauchy_location_lr_test(x, location, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, location = location, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "cauchy_scale_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rcauchy(N, location, scale)
        test <- cauchy_scale_lr_test(x, scale, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, location = location, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 2

sim_results %>%
  distinct(location) %>%
  nrow() == length(locations)

sim_results %>%
  distinct(scale) %>%
  nrow() == length(scales)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/cauchy_type_one.rds")

rm(sim_results, x, test, alt, location, scale, locations, scales)

rm(list = ls())

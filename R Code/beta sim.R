library(MLTesteR)
library(tidyverse)
library(stringr)

################
# Type I
################
B <- 5000
N <- 200

shape1s <- seq(1, 9, 2)
shape2s <- seq(1, 9, 2)

sim_results <- tibble()
for (shape1 in shape1s) {
  for (shape2 in shape2s) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape1_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape1, shape2 = shape2)
        try(test <- beta_shape1_lr_test(x, shape1, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape1 = shape1, shape2 = shape2, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("results/beta_type_one_shape1.rds")
rm(sim_results)

sim_results <- tibble()
for (shape1 in shape1s) {
  for (shape2 in shape2s) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape2_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape1, shape2 = shape2)
        try(test <- beta_shape2_lr_test(x, shape2, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape1 = shape1, shape2 = shape2, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("results/beta_type_one_shape2.rds")
rm(sim_results)

sim_results <- bind_rows(
  readRDS(file = "results/beta_type_one_shape1.rds"),
  readRDS(file = "results/beta_type_one_shape2.rds")
)

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 2

sim_results %>%
  distinct(shape1) %>%
  nrow() == length(shape1s)

sim_results %>%
  distinct(shape2) %>%
  nrow() == length(shape2s)

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
  saveRDS("results/beta_type_one.rds")

rm(list = ls())

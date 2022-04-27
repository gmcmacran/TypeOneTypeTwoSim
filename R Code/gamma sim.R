library(MLTesteR)
library(tidyverse)
library(stringr)

################
# Type I
################
B <- 5000
N <- 200

shapes <- seq(1, 9, 2)
rates <- seq(1, 9, 2)

sim_results <- tibble()
for (shape in shapes) {
  for (rate in rates) {
    scale <- 1 / rate

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_rate_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, rate = rate)
        try(test <- gamma_rate_lr_test(x, rate, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("results/gamma_type_one_rate.rds")
rm(sim_results)

sim_results <- tibble()
for (shape in shapes) {
  for (rate in rates) {
    scale <- 1 / rate

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_scale_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, scale = scale)
        try(test <- gamma_scale_lr_test(x, scale, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("results/gamma_type_one_scale.rds")
rm(sim_results)

sim_results <- tibble()
for (shape in shapes) {
  for (rate in rates) {
    scale <- 1 / rate

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_shape_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, rate = rate)
        try(test <- gamma_shape_lr_test(x, shape, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("results/gamma_type_one_shape.rds")
rm(sim_results)

sim_results <- bind_rows(
  readRDS(file = "results/gamma_type_one_rate.rds"),
  readRDS(file = "results/gamma_type_one_scale.rds"),
  readRDS(file = "results/gamma_type_one_shape.rds")
)

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 3

sim_results %>%
  distinct(shape) %>%
  nrow() == length(shapes)

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(scale) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

rm(list = ls())

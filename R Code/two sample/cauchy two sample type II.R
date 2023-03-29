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

location0 <- 0
scale0 <- 1
locationEffectSizes <- seq(2, 8, 2) %>%
  round(2) %>%
  setdiff(0)

sim_results <- tibble()
for (locationEffectSize in locationEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "cauchy_location_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rcauchy(n = N / 2, location = location0, scale = scale0), rcauchy(n = N / 2, location = location0 + locationEffectSize, scale = scale0))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- cauchy_location_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = locationEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, test, x)
}

rm(locationEffectSize)

location0 <- 0
scale0 <- 1
scaleEffectSizes <- seq(2, 6, 2)

for (scaleEffectSize in scaleEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "cauchy_scale_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rcauchy(n = N / 2, location = location0, scale = scale0), rcauchy(n = N / 2, location = location0, scale = scale0 + scaleEffectSize))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- cauchy_scale_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = scaleEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, test, x)
}

rm(scaleEffectSize)

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
  filter(test == "cauchy_location_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(locationEffectSizes)

sim_results %>%
  filter(test == "cauchy_scale_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(scaleEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results %>%
  saveRDS("results/cauchy_type_two_one_way.rds")

rm(list = ls())

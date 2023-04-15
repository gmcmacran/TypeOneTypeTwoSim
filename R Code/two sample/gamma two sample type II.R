library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 5000
N <- 500

################
# Type II
################
shape0 <- 5
rate0 <- 1
scale0 <- 1 / rate0

rateEffectSizes <- seq(-.45, .45, .05) %>%
  setdiff(0)

sim_results <- tibble()
for (rateEffectSize in rateEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "gamma_rate_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rgamma(N / 2, shape = shape0, rate = rate0), rgamma(N / 2, shape = shape0, rate = rate0 + rateEffectSize))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- gamma_rate_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = rateEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr)
}

sim_results %>% saveRDS("results/gamma_type_two_rate_one_way.rds")
rm(sim_results)
rm(rateEffectSize, x, test)

scaleEffectSizes <- seq(-.45, .45, .05) %>%
  setdiff(0)

sim_results <- tibble()
for (scaleEffectSize in scaleEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "gamma_scale_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rgamma(N / 2, shape = shape0, scale = scale0), rgamma(N / 2, shape = shape0, scale = scale0 + scaleEffectSize))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- gamma_scale_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = scaleEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr)
}

sim_results %>% saveRDS("results/gamma_type_two_scale_one_way.rds")
rm(sim_results)
rm(scaleEffectSize, x, test)

shapeEffectSizes <- seq(-2, 2, .5) %>%
  setdiff(0)

sim_results <- tibble()
for (shapeEffectSize in shapeEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "gamma_shape_one_way"
  for (i in 1:B) {
    set.seed(i)
    x <- c(rgamma(N / 2, shape = shape0, rate = rate0), rgamma(N / 2, shape = shape0 + shapeEffectSize, rate = rate0))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- gamma_shape_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = shapeEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr)
}

sim_results %>% saveRDS("results/gamma_type_two_shape_one_way.rds")
rm(sim_results)
rm(shapeEffectSize, x, test)

sim_results <- bind_rows(
  readRDS(file = "results/gamma_type_two_rate_one_way.rds"),
  readRDS(file = "results/gamma_type_two_scale_one_way.rds"),
  readRDS(file = "results/gamma_type_two_shape_one_way.rds")
)

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 3

sim_results %>%
  distinct(alt) %>%
  nrow() == 1

sim_results %>%
  distinct(alt, test) %>%
  nrow() == 3

sim_results %>%
  filter(test == "gamma_rate_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(rateEffectSizes)

sim_results %>%
  filter(test == "gamma_scale_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(scaleEffectSizes)

sim_results %>%
  filter(test == "gamma_shape_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(shapeEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

rm(list = ls())

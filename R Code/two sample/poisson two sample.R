library(LRTesteR)
library(tidyverse)
library(stringr)
library(lmtest)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
N <- 200

################
# Type I
################

lambdas <- 1:15

sim_results <- tibble()

for (lambda in lambdas) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "poisson_lambda_one_way"
  set.seed(1)
  for (i in 1:B) {
    x <- rpois(n = N, lambda = lambda)
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- poisson_lambda_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, lambda = lambda, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, lambda, test)
}


# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(lambda) %>%
  nrow() == length(lambdas)

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
  saveRDS("results/poisson_type_one_one_way.rds")

rm(sim_results, lambdas)

################
# Type II
################
lambda0 <- 5

lambdaEffectSizes <- seq(-3, 3, 1) %>%
  round(2) %>%
  setdiff(0)

sim_results <- tibble()
for (lambdaEffectSize in lambdaEffectSizes) {
  stats <- vector(mode = "numeric", length = B)
  pvalues <- vector(mode = "numeric", length = B)
  alts <- vector(mode = "character", length = B)
  testName <- "poisson_lambda_one_way"
  set.seed(1)
  for (i in 1:B) {
    x <- c(rpois(n = N / 2, lambda = lambda0), rpois(n = N / 2, lambda = lambda0 + lambdaEffectSize))
    fctr <- factor(c(rep("1", N / 2), rep("2", N / 2)), levels = c("1", "2"))
    test <- poisson_lambda_one_way(x, fctr)
    stats[i] <- test$statistic
    pvalues[i] <- test$p.value
    alts[i] <- test$alternative
  }
  temp <- tibble(test = testName, effectSize = lambdaEffectSize, stat = stats, pvalue = pvalues, alt = alts)
  sim_results <- sim_results %>% bind_rows(temp)
  rm(stats, pvalues, alts, testName, temp, i, fctr, x, test, lambdaEffectSize)
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
  filter(test == "poisson_lambda_one_way") %>%
  distinct(effectSize) %>%
  nrow() == length(lambdaEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  saveRDS("results/poisson_type_two_one_way.rds")

rm(list = ls())

library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
compiler::enableJIT(3)
B <- 2000
calc_two_sided_p_value <- function(x, size, prob) {
  if (prob == 1) {
    (as.numeric(x == 0))
  } else {
    relErr <- 1 + 1e-07
    d <- dnbinom(x, size, prob)
    m <- size * (1 - prob) / prob
    if (x == m) {
      1
    } else if (x < m) {
      nearInf <- ceiling(m * 20)
      i <- seq.int(from = ceiling(m), to = nearInf)
      y <- sum(dnbinom(i, size, prob) <= d * relErr)
      pnbinom(x, size, prob) + pnbinom(pmax(nearInf - y, 0), size, prob, lower.tail = FALSE)
    } else {
      i <- seq.int(from = 0, to = floor(m))
      y <- sum(dnbinom(i, size, prob) <= d * relErr)
      pnbinom(y - 1, size, prob) + pnbinom(x - 1, size, prob, lower.tail = FALSE)
    }
  }
}

################
# Type I
################

ps <- seq(.05, .95, .10)
sizes <- seq(50, 150, 25)

all(ps < 1)
all(ps > 0)
all(sizes >= 50)

sim_results <- tibble()
for (p in ps) {
  for (size in sizes) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      CI_LBs <- vector(mode = "numeric", length = B)
      CI_UBs <- vector(mode = "numeric", length = B)
      testName <- "negative_binomial_p_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size, p)
        test <- negative_binomial_p_lr_test(x, size, p, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
        CI_LBs[i] <- test$conf.int[1]
        CI_UBs[i] <- test$conf.int[2]
      }
      temp <- tibble(test = testName, p = p, size = size, stat = stats, pvalue = pvalues, alt = alts, CI_LB = CI_LBs, CI_UB = CI_UBs)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(p) %>%
  nrow() == length(ps)

sim_results %>%
  distinct(size) %>%
  nrow() == length(sizes)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

all(sim_results$CI_LB < sim_results$CI_UB)

# save
sim_results %>%
  saveRDS("results/negative_binomial_type_one.rds")


# exact test
# Will not implemented CI logic for exact method.
# Using NAs
sim_results_02 <- tibble()
for (p in ps) {
  for (size in sizes) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "exact"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size, p)
        if (alt == "two.sided") {
          stats[i] <- x
          pvalues[i] <- calc_two_sided_p_value(x, size, p)
          alts[i] <- alt
        }
        if (alt == "less") {
          stats[i] <- x
          pvalues[i] <- pnbinom(q = x - 1, size = size, prob = p, lower.tail = FALSE)
          alts[i] <- alt
        }
        if (alt == "greater") {
          stats[i] <- x
          pvalues[i] <- pnbinom(q = x, size = size, prob = p, lower.tail = TRUE)
          alts[i] <- alt
        }
      }
      temp <- tibble(test = testName, p = p, size = size, stat = stats, pvalue = pvalues, alt = alts, CI_LB = -NA, CI_UB = NA)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

sim_results_02 %>%
  distinct(test) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(p) %>%
  nrow() == length(ps)

sim_results_02 %>%
  distinct(size) %>%
  nrow() == length(sizes)

sim_results_02 %>%
  distinct(alt) %>%
  nrow() == 3

sim_results_02 %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results_02 %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

# save
sim_results_02 %>%
  saveRDS("results/negative_binomial_type_one_exact.rds")

rm(sim_results, sim_results_02, test, alt, p, ps, size, sizes, x)

################
# Type II
################
p0 <- .50
size0 <- 50
pEffectSizes <- seq(-.30, .30, .02) %>%
  setdiff(0)

sim_results <- tibble()
for (pEffectSize in pEffectSizes) {
  if (pEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "negative_binomial_p_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size0, p0 + pEffectSize)
        test <- negative_binomial_p_lr_test(x, size0, p0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = pEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
  else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "negative_binomial_p_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size0, p0 + pEffectSize)
        test <- negative_binomial_p_lr_test(x, size0, p0, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, effectSize = pEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
rm(alt, pEffectSize, x, test)

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
  filter(test == "negative_binomial_p_lr_test") %>%
  distinct(effectSize) %>%
  nrow() == length(pEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  saveRDS("results/negative_binomial_type_two.rds")

rm(sim_results)

# exact test
sim_results_02 <- tibble()
for (pEffectSize in pEffectSizes) {
  if (pEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "exact"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size0, p0 + pEffectSize)
        if (alt == "two.sided") {
          stats[i] <- x
          pvalues[i] <- calc_two_sided_p_value(x, size0, p0)
          alts[i] <- alt
        }
        if (alt == "less") {
          stats[i] <- x
          pvalues[i] <- pnbinom(q = x - 1, size = size0, prob = p0, lower.tail = FALSE)
          alts[i] <- alt
        }
      }
      temp <- tibble(test = testName, effectSize = pEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
  else {
    for (alt in c("two.sided", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "exact"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size0, p0 + pEffectSize)
        if (alt == "two.sided") {
          stats[i] <- x
          pvalues[i] <- calc_two_sided_p_value(x, size0, p0)
          alts[i] <- alt
        }
        if (alt == "greater") {
          stats[i] <- x
          pvalues[i] <- pnbinom(q = x, size = size0, prob = p0, lower.tail = TRUE)
          alts[i] <- alt
        }
      }
      temp <- tibble(test = testName, effectSize = pEffectSize, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
rm(alt, pEffectSize, x)

# Check structure
sim_results_02 %>%
  distinct(test) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(alt) %>%
  nrow() == 3

sim_results_02 %>%
  distinct(alt, test) %>%
  nrow() == 3

sim_results_02 %>%
  filter(test == "exact") %>%
  distinct(effectSize) %>%
  nrow() == length(pEffectSizes)

sim_results_02 %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results_02 %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results_02 %>%
  saveRDS("results/negative_binomial_type_two_exact.rds")

rm(list = ls())

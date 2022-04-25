library(MLTesteR)
library(tidyverse)
library(stringr)

################
# Type I
################
B <- 5000

ps <- seq(.05, .95, .05)
sizes <- seq(05, 200, 10)

all(ps < 1)
all(ps > 0)
all(sizes > 0)

sim_results <- tibble()
for (p in ps) {
  for (size in sizes) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "negative_binomial_p_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size, p)
        test <- negative_binomial_p_lr_test(x, size, p, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, p = p, size = size, stat = stats, pvalue = pvalues, alt = alts)
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

# save
sim_results %>%
  saveRDS("results/negative_binomial_type_one.rds")


# exact test
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
          calc_two_sided_p_value <- function(x, size, prob) {
            if (prob == 0) {
              (x == 0)
            } else if (prob == 1) {
              (x == 0)
            } else {
              relErr <- 1 + 1e-07
              d <- dnbinom(x, size, prob)
              m <- size * (1 - prob) / prob
              if (x == m) {
                1
              } else if (x < m) {
                nearInf <- ceiling(m * 20)
                i <- seq.int(from = ceiling(m), to = nearInf)
                i <- setdiff(i, x)
                y <- sum(dnbinom(i, size, prob) < d * relErr)
                pnbinom(x, size, prob) + pnbinom(pmax(nearInf - y, 0), size, prob, lower.tail = FALSE)
              } else {
                i <- seq.int(from = 0, to = floor(m))
                i <- setdiff(i, x)
                y <- sum(dnbinom(i, size, prob) < d * relErr)
                pnbinom(y - 1, size, prob) + pnbinom(x - 1, size, prob, lower.tail = FALSE)
              }
            }
          }
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
      temp <- tibble(test = testName, p = p, size = size, stat = stats, pvalue = pvalues, alt = alts)
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

rm(list = ls())

library(MLTesteR)
library(tidyverse)
library(stringr)

################
# Type I
################
B <- 5000
N <- 50

# rates <- seq(.01, 1, .05)
rates <- 1:15

sim_results <- tibble()

for (rate in rates) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "exponentail_rate_lr_test"
    set.seed(1)
    for (i in 1:B) {
      x <- x <- rexp(n = N, rate = rate)
      test <- exponentail_rate_lr_test(x, rate, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, rate = rate, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i)
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(rate) %>%
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

# save
sim_results %>%
  saveRDS("results/exponentail_type_one.rds")

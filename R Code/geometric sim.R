library(LRTesteR)
library(tidyverse)
library(stringr)

################
# Simulation settings
################
B <- 5000
calc_two_sided_p_value <- function(x, prob) {
  if (prob == 0) {
    (as.numeric(x >= 0))
  } else if (prob == 1) {
    (as.numeric(x == 0))
  } else {
    relErr <- 1 + 1e-05
    d <- dgeom(x, prob)
    m <- (1 - prob) / prob
    if (x == m) {
      1
    } else if (x < m) {
      nearInf <- ceiling(m * 20)
      i <- seq.int(from = ceiling(m), to = nearInf)
      i <- setdiff(i, x)
      y <- sum(dgeom(i, prob) < d * relErr)
      pgeom(x, prob) + pgeom(pmax(nearInf - y, 0), prob, lower.tail = FALSE)
    } else {
      i <- seq.int(from = 0, to = floor(m))
      i <- setdiff(i, x)
      y <- sum(dgeom(i, prob) < d * relErr)
      pgeom(y - 1, prob) + pgeom(x - 1, prob, lower.tail = FALSE)
    }
  }
}

################
# Type I
################
ps <- seq(.05, .95, .10)

all(ps < 1)
all(ps > 0)

sim_results <- tibble()
for (p in ps) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "geometric_p_lr_test"
    set.seed(1)
    for (i in 1:B) {
      x <- rgeom(1, p)
      test <- geometric_p_lr_test(x, p, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, p = p, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i)
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
  saveRDS("results/geometric_type_one.rds")

# exact test
sim_results_02 <- tibble()
for (p in ps) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "exact"
    set.seed(1)
    for (i in 1:B) {
      x <- rgeom(1, p)
      if (alt == "two.sided") {
        stats[i] <- x
        pvalues[i] <- calc_two_sided_p_value(x, p)
        alts[i] <- alt
      }
      if (alt == "less") {
        stats[i] <- x
        pvalues[i] <- pgeom(q = x - 1, prob = p, lower.tail = FALSE)
        alts[i] <- alt
      }
      if (alt == "greater") {
        stats[i] <- x
        pvalues[i] <- pgeom(q = x, prob = p, lower.tail = TRUE)
        alts[i] <- alt
      }
    }
    temp <- tibble(test = testName, p = p, stat = stats, pvalue = pvalues, alt = alts)
    sim_results_02 <- sim_results_02 %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i)
  }
}

sim_results_02 %>%
  distinct(test) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(p) %>%
  nrow() == length(ps)

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
  saveRDS("results/geometric_type_one_exact.rds")

rm(test, alt, p, ps, x, sim_results, sim_results_02)

################
# Type II
################
p0 <- .50
pEffectSizes <- seq(-.45, .45, .05) %>%
  setdiff(0)

sim_results <- tibble()
for (pEffectSize in pEffectSizes) {
  if (pEffectSize < 0) {
    for (alt in c("two.sided", "less")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "geometric_p_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgeom(1, p0 + pEffectSize)
        test <- geometric_p_lr_test(x, p0, alt)
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
      testName <- "geometric_p_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgeom(1, p0 + pEffectSize)
        test <- geometric_p_lr_test(x, p0, alt)
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
  filter(test == "geometric_p_lr_test") %>%
  distinct(effectSize) %>%
  nrow() == length(pEffectSizes)

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results %>%
  saveRDS("results/geometric_type_two.rds")

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
        x <- rgeom(1, p0 + pEffectSize)
        if (alt == "two.sided") {
          stats[i] <- x
          pvalues[i] <- calc_two_sided_p_value(x, p0)
          alts[i] <- alt
        }
        if (alt == "less") {
          stats[i] <- x
          pvalues[i] <- pgeom(q = x - 1, prob = p0, lower.tail = FALSE)
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
        x <- rgeom(1, p0 + pEffectSize)
        if (alt == "two.sided") {
          stats[i] <- x
          pvalues[i] <- calc_two_sided_p_value(x, p0)
          alts[i] <- alt
        }
        if (alt == "greater") {
          stats[i] <- x
          pvalues[i] <- pgeom(q = x, prob = p0, lower.tail = TRUE)
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
  saveRDS("results/geometric_type_two_exact.rds")

rm(list = ls())

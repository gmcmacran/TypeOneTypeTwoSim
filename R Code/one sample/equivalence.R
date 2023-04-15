library(LRTesteR)
library(tidyverse)
library(scales)

################
# Simulation settings
################

compiler::enableJIT(3)
B <- 5000
Ns <- 10^(2:5)
confs <- c(.80, .90, .95)

compare_tests_normal <- function(test1, test2) {
  CI1 <- as.numeric(test1$conf.int)
  CI2 <- as.numeric(test2$conf.int)

  CI1 <- round(CI1, 1)
  CI2 <- round(CI2, 1)

  p1 <- test1$p.value
  p2 <- test2$p.value

  p1 <- round(p1, 2)
  p2 <- round(p2, 2)

  result <- all(CI1 == CI2) & p1 == p2
  return(result)
}

# change to number of decimals for match for p CI
compare_tests_binomial <- function(test1, test2) {
  CI1 <- as.numeric(test1$conf.int)
  CI2 <- as.numeric(test2$conf.int)

  CI1 <- round(CI1, 2)
  CI2 <- round(CI2, 2)

  p1 <- test1$p.value
  p2 <- test2$p.value

  p1 <- round(p1, 2)
  p2 <- round(p2, 2)

  result <- all(CI1 == CI2) & p1 == p2
  return(result)
}

################
# Run sim
################

sim_results_00 <- tibble()
for (N in Ns) {
  for (conf in confs) {
    for (i in 1:B) {
      set.seed(i)
      x <- rnorm(n = N, mean = 0, sd = 1)
      test1 <- stats::t.test(x = x, mu = 0, alternative = "two.sided", conf.level = conf)
      test2 <- gaussian_mu_one_sample(x = x, mu = 0, alternative = "two.sided", conf.level = conf)
      result <- compare_tests_normal(test1, test2)
      test <- "gaussian_mu_one_sample"
      temp <- tibble(test = test, N = N, conf = conf, b = i, match = result)
      sim_results_00 <- sim_results_00 %>% bind_rows(temp)
      rm(x, test1, test2, result, temp, test)
    }
  }
}


sim_results_01 <- tibble()
for (N in Ns) {
  for (conf in confs) {
    for (i in 1:B) {
      set.seed(i)
      x <- rnorm(n = N, mean = 0, sd = 1)
      test1 <- EnvStats::varTest(x = x, sigma.squared = 1, alternative = "two.sided", conf.level = conf)
      test2 <- gaussian_variance_one_sample(x = x, sigma.squared = 1, alternative = "two.sided", conf.level = conf)
      result <- compare_tests_normal(test1, test2)
      test <- "gaussian_variance_one_sample"
      temp <- tibble(test = test, N = N, conf = conf, b = i, match = result)
      sim_results_01 <- sim_results_01 %>% bind_rows(temp)
      rm(x, test1, test2, result, temp, test)
    }
  }
}

sim_results_02 <- tibble()
for (N in Ns) {
  for (conf in confs) {
    for (i in 1:B) {
      set.seed(i)
      x <- rbinom(n = 1, size = N, prob = .50)
      test1 <- stats::binom.test(x = x, n = N, p = .50, alternative = "two.sided", conf.level = conf)
      test2 <- binomial_p_one_sample(x = x, n = N, p = .50, alternative = "two.sided", conf.level = conf)
      result <- compare_tests_binomial(test1, test2)
      test <- "binomial_p_one_sample"
      temp <- tibble(test = test, N = N, conf = conf, b = i, match = result)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(x, test1, test2, result, temp, test)
    }
  }
}

################
# Analyze results
################

sim_results <- sim_results_00 %>%
  bind_rows(
    sim_results_01,
    sim_results_02
  )

temp <- sim_results %>%
  group_by(test, N, conf) %>%
  summarise(matchRate = mean(match)) %>%
  ungroup()

temp %>%
  ggplot(aes(x = N, y = matchRate)) +
  geom_point() +
  geom_line() +
  scale_x_log10() +
  scale_y_continuous(breaks = seq(0, 1, .1)) +
  labs(title = "Equality of CI and p value", x = "Sample Size (log10)", y = "Proportion of tests that matched") +
  facet_grid(cols = vars(test), rows = vars(conf))

ggsave(filename = "results/graphs/equality and N.png", width = 10, height = 10)

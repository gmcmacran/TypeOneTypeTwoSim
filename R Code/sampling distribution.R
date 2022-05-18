library(LRTesteR)
library(tidyverse)

###############
# Load data
###############
load_df <- function(fn) {
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat)
  return(DF)
}

fns <- c(
  "gaussian_type_one.rds",
  "gamma_type_one_rate.rds",
  "gamma_type_one_scale.rds",
  "gamma_type_one_shape.rds",
  "poisson_type_one.rds",
  "beta_type_one.rds",
  "negative_binomial_type_one.rds",
  "geometric_type_one.rds",
  "exponentail_type_one.rds",
  "binomail_type_one.rds"
)

typeI <- map_dfr(fns, load_df) %>%
  drop_na()

typeI %>%
  distinct(test)

typeI %>%
  distinct(alt)

typeI %>%
  filter(alt == "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  filter(alt != "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

###############
# Check sampling distribution
###############
param <- list(df = 1)
typeI %>%
  filter(alt == "two.sided") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) Two Sided Test", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_two_sided.png", width = 10, height = 10)

typeI %>%
  filter(alt == "less") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Normal) Less Test", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_less.png", width = 10, height = 10)

typeI %>%
  filter(alt == "greater") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Normal) Greater Test", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_greater.png", width = 10, height = 10)

rm(list = ls())

x <- typeI %>%
  filter(test == "gaussian_mean_lr_test", alt == "two.sided") %>%
  pull(stat)
car::qqPlot(x, dist="chisq", df=1, envelope = FALSE )

typeI %>%
  filter(alt == "two.sided", test == "gaussian_mean_lr_test") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  # scale_x_continuous(limits = c(0, 20)) +
  # scale_y_continuous(limits = c(0, 15)) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) Two Sided Test", x = "Theoretical", y = "Observed")

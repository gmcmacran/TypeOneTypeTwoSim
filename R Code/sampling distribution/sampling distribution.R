library(LRTesteR)
library(tidyverse)

###############
# Load data
###############
load_df <- function(fn) {
  if (str_detect(fn, "two")) {
    print(str_c("Possible Error. fn: ", fn))
  }
  fn <- str_c("results/", fn, collapse = "")
  DF <- readRDS(fn)
  DF <- DF %>%
    select(test, alt, stat, pvalue)
  return(DF)
}

fns <- c(
  "gaussian_type_one.rds",
  "gaussian_type_one_one_way.rds",
  "gamma_type_one_rate.rds",
  "gamma_type_one_rate_one_way.rds",
  "gamma_type_one_scale.rds",
  "gamma_type_one_scale_one_way.rds",
  "gamma_type_one_shape.rds",
  "gamma_type_one_shape_one_way.rds",
  "poisson_type_one.rds",
  "poisson_type_one_one_way.rds",
  "beta_type_one_shape1.rds",
  "beta_type_one_one_way_shape1.rds",
  "beta_type_one_shape2.rds",
  "beta_type_one_one_way_shape2.rds",
  "negative_binomial_type_one.rds",
  "negative_binomial_type_one_one_way.rds",
  "exponential_type_one.rds",
  "exponential_type_one_one_way.rds",
  "binomail_type_one.rds",
  "binomail_type_one_one_way.rds",
  "cauchy_type_one.rds",
  "cauchy_type_one_one_way.rds",
  "inverse_gaussian_type_one.rds",
  "inverse_gaussian_type_one_one_way.rds",
  "empirical_mu_type_one.rds",
  "empirical_mu_type_one_one_way.rds",
  "empirical_quantile_type_one.rds",
  "empirical_quantile_type_one_one_way.rds"
)

typeI <- map_dfr(fns, load_df)

typeI %>%
  drop_na() %>%
  nrow() == typeI %>%
  nrow()

typeI %>%
  distinct(test) %>%
  nrow() == 36

typeI %>%
  distinct(alt) %>%
  nrow() == 3

typeI %>%
  filter(alt == "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  filter(alt == "two.sided", stat < 0) %>%
  distinct(test)

typeI %>%
  filter(alt != "two.sided") %>%
  summarise(minStat = min(stat), maxStat = max(stat))

typeI %>%
  filter(alt != "two.sided") %>%
  group_by(test) %>%
  summarise(minStat = min(stat), maxStat = max(stat)) %>%
  arrange(test) %>%
  print(n = Inf)

###############
# Check sampling distribution
###############
typeI %>%
  filter(alt == "less") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Normal) Less Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_less.png", width = 10, height = 10)

typeI %>%
  filter(alt == "greater") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qnorm) +
  stat_qq_line(distribution = qnorm) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Normal) Greater Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_greater.png", width = 10, height = 10)

param <- list(df = 1)
typeI %>%
  filter(alt == "two.sided") %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) Two Sided Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_two_sided.png", width = 10, height = 10)

param <- list(df = 1)
typeI %>%
  filter(alt == "two.sided", !str_detect(test, "one_way")) %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) Two Sided Tests", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_two_sided_ref.png", width = 10, height = 10)

# The one sided QQ plots based on normal distribution look great.
# If X ~ N(), than X^2 ~ chi-square(df=1)
# So the QQ plot of squared stat should look just as great as well.
#
# Does the chi square QQ plot for one sided tests look about the same
# as the two sided's QQ plot?
# If yes, two sided test looks good.
# If no, there is something off with the two sided test.
typeI %>%
  filter(alt != "two.sided") %>%
  mutate(stat = stat^2) %>%
  ggplot(aes(sample = stat)) +
  stat_qq(distribution = qchisq, dparams = param["df"]) +
  stat_qq_line(distribution = qchisq, dparams = param["df"]) +
  facet_wrap(vars(test)) +
  labs(title = "QQ Plot (Chi Square) One Sided Tests Squared", x = "Theoretical", y = "Observed")

ggsave(filename = "results/graphs/sampling_distribution_one_sided_squared.png", width = 10, height = 10)

rm(list = ls())

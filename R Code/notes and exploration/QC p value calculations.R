###########################################
# Overview
#
# R does not seem to an exact test for p
# associated with negative binomial
# distribution. Making my own and
# testing it
###########################################
library(tidyverse)

####################
# Define
####################
exact_test <- function(num_failures, num_success, p, alternative) {
  # modification of binom.test's two sided logic
  calc_two_sided_p_value <- function(x, size, prob) {
    if (prob == 0) {
      (as.numeric(x >= 0))
    } else if (prob == 1) {
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

  calc_left_p_value <- function(x, size, prob) {
    pnbinom(q = x, size = size, prob = prob, lower.tail = TRUE)
  }

  calc_right_p_value <- function(x, size, prob) {
    pnbinom(q = x - 1, size = size, prob = prob, lower.tail = FALSE)
  }

  if (alternative == "two.sided") {
    p.value <- calc_two_sided_p_value(num_failures, num_success, p)
  }
  if (alternative == "greater") {
    p.value <- calc_left_p_value(num_failures, num_success, p)
  }
  if (alternative == "less") {
    p.value <- calc_right_p_value(num_failures, num_success, p)
  }
  return(list(p.value = p.value))
}


####################
# Check a few values 1 by 1
####################
exact_test(0, 1, 1.00, "two.sided")$p.value == 1
exact_test(1, 1, 1.00, "two.sided")$p.value == 0

exact_test(0, 1, 0, "two.sided")$p.value == 1
exact_test(1, 1, 0, "two.sided")$p.value == 1
exact_test(2, 1, 0, "two.sided")$p.value == 1

exact_test(0, 1, .50, "two.sided")$p.value == 1
exact_test(1, 1, .50, "two.sided")$p.value == 1

# Does p value increase as a function failure?
results <- c()
for (p in seq(.05, .95, .05))
  for (i in seq(2, 10, 1))
    results <- c(results, exact_test(i, 1, p, "greater")$p.value < exact_test(i+1, 1, p, "greater")$p.value)
all(results)

# Does p value decrease as a function failure?
results <- c()
for (p in seq(.05, .95, .05))
  for (i in seq(2, 10, 1))
    results <- c(results, exact_test(i, 1, p, "less")$p.value > exact_test(i+1, 1, p, "less")$p.value)
all(results)


exact_test(0, 1, 1.00, "two.sided")$p.value
exact_test(0, 1, 1.00, "greater")$p.value

exact_test(1, 1, 0, "two.sided")$p.value
exact_test(1, 1, 0, "less")$p.value

####################
# Symmetry tests
####################
# When the target number of successes large, the negative binomial is nearly
# symmetrical. When the distribution is symmetrical, the two sided p value
# should equal 2 * the min(one sided p value)


# If a distribution is symmetric, P(X = mean(x) - a) = P(X = mean(x) + a)
# This function finds the set of the parameters where
# mean(abs(P(X = mean(x) - a) - P(X = mean(x) + a))) is as small as possible
# If mean(abs(P(X = mean(x) - a) - P(X = mean(x) + a))) is 0, distribution
# is symmetrical.
#
# Rough notion of symmetry b/c negative binomial's tail goes on for ever.
search <- function(input) {
  prob <- input[1]
  size <- input[2]

  # clean up inputs
  prob <- pmax(pmin(prob, .99), .01)
  size <- round(pmax(1, size))

  x <- 1:1000
  ds <- dnbinom(x = x, size = size, prob = prob)
  m <- size * (1-prob) / prob

  left <- ds[1:sum(x <= floor(m))]
  right <- ds[ceiling(m):(ceiling(m) + length(left) - 1)]
  right <- rev(right)

  obj1 <- mean(abs(left - right))
  obj2 <- sum(ds[(ceiling(m) + length(left)):length(ds)])
  obj1 + obj2
}

gen_new_input <- function(input) {
  U <- runif(1)
  if (U <= .5) {
    input[1] <- input[1] + runif(1, -.01, .01)
    input[1] <- pmax(pmin(input[1], .99), .01)
  }
  else {
    input[2] <- input[2] + round(runif(1, -1, 1))
    input[2] <- round(pmax(1, input[2]))
  }
  input
}

search(c(.5, 10))
gen_new_input(c(.5, 10))

set.seed(1)
optim(c(.5, 10), search, gen_new_input, method = "SANN", control = list(maxit = 50000))

set.seed(2)
optim(c(.1, 10), search, gen_new_input, method = "SANN", control = list(maxit = 50000))

set.seed(3)
optim(c(.9, 10), search, gen_new_input, method = "SANN", control = list(maxit = 50000))

set.seed(4)
optim(c(0.1853969, 76), search, gen_new_input, method = "SANN", control = list(maxit = 50000))


# confirm symmetry
ps <- c(0.1671526, 0.1853969, 0.07481396)
sizes <- c(66, 76, 40)
ps <- c(0.1697183)
sizes <- c(102)
search <- tibble()
for (p in ps) {
  for (size in sizes) {
    temp <- tibble(x = 1:100)
    temp <- temp %>%
      mutate(size = size,
             p = p,
             prob = dnbinom(x = x, size = 10, prob = p))
    search <- search %>% bind_rows(temp)
  }
}

search <- search %>%
  filter(p == 0.1671526 & size == 66 | p == 0.1853969 & size == 76 | p == 0.07481396 & size == 40)

ggplot(search, aes(x = x, y = prob)) +
  geom_col() +
  labs(x = "x", y = "Probability") +
  facet_grid(vars(size), vars(p))




prob <- 0.1697183
size <- 102
size * (1-prob) / prob

exact_test(0, size, prob, "two.sided")$p.value
2*min(exact_test(0, size, prob, "greater")$p.value, exact_test(0, size, prob, "less")$p.value)

exact_test(1, size, prob, "two.sided")$p.value
2*min(exact_test(1, size, prob, "greater")$p.value, exact_test(1, size, prob, "less")$p.value)

exact_test(2, size, prob, "two.sided")$p.value
2*min(exact_test(2, size, prob, "greater")$p.value, exact_test(2, size, prob, "less")$p.value)

exact_test(333, size, prob, "two.sided")$p.value
2*min(exact_test(333, size, prob, "greater")$p.value, exact_test(333, size, prob, "less")$p.value)

exact_test(300, size, prob, "two.sided")$p.value
2*min(exact_test(300, size, prob, "greater")$p.value, exact_test(300, size, prob, "less")$p.value)

exact_test(400, size, prob, "two.sided")$p.value
2*min(exact_test(400, size, prob, "greater")$p.value, exact_test(400, size, prob, "less")$p.value)

exact_test(1, 1, 0, "two.sided")$p.value
exact_test(1, 1, 0, "less")$p.value




search %>%
  filter(p <= .35) %>%
  ggplot(aes(x = x, y = prob)) +
  geom_col() +
  labs(x = "x", y = "Probability") +
  facet_grid(vars(size), vars(p))

search %>%
  filter(p >= .25,
         p <= .55,
         size == 50) %>%
  ggplot(aes(x = x, y = prob)) +
  geom_col() +
  labs(x = "x", y = "Probability") +
  facet_grid(vars(size), vars(p))


for (failure in seq(0, 100, 1)) {
  ps <- seq(.05, .95, .05)
  temp <- temp %>%
    bind_rows(tibble(
      failure = failure,
      p = rep(ps, 3),
      alt = c(rep("two.sided", length(ps)), rep("greater", length(ps)), rep("less", length(ps)))
    ))
}
temp <- temp %>%
  rowwise() %>%
  mutate(p.value = exact_test(failure, p, alt)$p.value) %>%
  ungroup()
temp %>% pull(p.value) %>% min()
temp %>% pull(p.value) %>% max()

ggplot(temp, aes(x = failure, y = p.value)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  facet_grid(rows = vars(alt), cols = vars(p))

temp_02 <- temp %>%
  filter(alt == "two.sided") %>%
  select(p, failure, p.value) %>%
  mutate(type = "Two Sided") %>%
  bind_rows(
    temp %>%
      filter(alt %in% c("greater", "less")) %>%
      group_by(p, failure) %>%
      summarise(p.value = min(p.value)) %>%
      ungroup() %>%
      mutate(type = "One Sided")
  ) %>%
  arrange(p, failure, type)
temp_02

ggplot(temp_02, aes(x = failure, y = p.value)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  facet_grid(rows = vars(type), cols = vars(p))

temp_03 <- temp %>%
  filter(alt == "two.sided") %>%
  mutate(twoSidedP = p.value) %>%
  select(p, failure, twoSidedP) %>%
  inner_join(temp %>%
               filter(alt %in% c("greater", "less")) %>%
               group_by(p, failure) %>%
               summarise(oneSidedP = min(p.value)) %>%
               ungroup(),
             by = c("p", "failure")) %>%
  arrange(p, failure) %>%
  mutate(twoMoreOne = twoSidedP >= oneSidedP)

temp_03 %>%
  pull(twoMoreOne) %>%
  mean()

temp_03 %>%
  mutate(diff = twoSidedP - oneSidedP) %>%
  summarise(minDiff = min(diff),
            meanDiff = mean(diff),
            maxDiff = max(diff))





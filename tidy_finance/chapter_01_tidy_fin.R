# load libraries: ----------------------------------

library("tidyverse")
library("tidyquant")
library("scales")
library("data.table")
library("DBI")
library("RSQLite")
library("dplyr")
library("xts")
library("ggplot2")
library("frenchdata")
library("readxl")
library("googledrive")

# Set defaults: ----------------------------------

ggplot2::theme_set(theme_bw())

# chapter 1: -------------------------------------

prices <- tq_get(
  x = "AAPL",
  get = "stock.prices",
  from = "2000-01-01",
  to = "2022-12-31"
)

prices |>
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Apple stock prices between beginning of 2000 and end of 2022"
  )

returns <- prices |>
  arrange(date) |>
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  select(symbol, date, ret)

returns <- returns |>
  drop_na(ret)

quantile_05 <- quantile(returns$ret, probs = 0.5)

returns |>
  ggplot(aes(x = ret)) +
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept = quantile_05),
    linetype = "dashed"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Distribution of daily Apple stock returns"
  )

returns |>
  summarize(
    across(
      ret,
      list(
        daily_mean = mean,
        daily_sd = sd,
        daily_min = min,
        daily_max = max
      )
    )
  )

returns |>
  group_by(year = year(date)) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    ),
    .names = "{.fn}"
  )) |>
  print(n = Inf)

symbols <- tq_index("SP500") |>
  filter(company != "US DOLLAR")

symbols <- tq_index("DOW") |>
  filter(company != "US DOLLAR")

symbols

index_prices <- symbols |>
  tq_get(
    get = "stock.prices",
    from = "2000-01-01",
    to = "2022-12-31"
  )

index_prices |>
  ggplot(aes(
    x = date,
    y = adjusted,
    color = symbol
  )) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Stock prices of DOW index constituents"
  ) +
  theme(legend.position = "none")

all_returns <- index_prices |>
  group_by(symbol) |>
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  select(symbol, date, ret) |>
  drop_na(ret)

all_returns |>
  group_by(symbol) |>
  summarize(across(
    ret,
    list(
      daily_mean = mean,
      daily_sd = sd,
      daily_min = min,
      daily_max = max
    ),
    .names = "{.fn}"
  )) |>
  print(n = Inf)

trading_volume <- index_prices |>
  group_by(date) |>
  summarize(
    trading_volume = sum(volume * adjusted)
  )

trading_volume |>
  ggplot(
    aes(x = date, y = trading_volume)
  ) +
  geom_line() +
  labs(
    x = NULL, y = NULL,
    title = "Aggregate daily trading volume of DOW index constitutens"
  ) +
  scale_y_continuous(
    labels = scales::unit_format(unit = "B", scale = 1e-9)
  )

trading_volume |>
  ggplot(aes(x = lag(trading_volume), y = trading_volume)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1),
    linetype = "dashed"
  ) +
  labs(
    x = "Previous day aggregate trading volume",
    y = "Aggregate trading volume",
    title = "Persistence in daily trading volume of DOW index constituents"
  ) +
  scale_x_continuous(labels = unit_format(unit = "B", scale = 1e-9)) +
  scale_y_continuous(labels = unit_format(unit = "B", scale = 1e-9))

index_prices <- index_prices |>
  group_by(symbol) |>
  mutate(n = n()) |>
  ungroup() |>
  filter(n == max(n)) |>
  select(-n)

returns <- index_prices |>
  mutate(month = floor_date(date, "month")) |>
  group_by(symbol, month) |>
  summarize(price = last(adjusted), .groups = "drop_last") |>
  mutate(ret = price / lag(price) - 1) |>
  drop_na(ret) |>
  select(-price)

returns_matrix <- returns |>
  pivot_wider(
    names_from = symbol,
    values_from = ret
  ) |>
  select(-month)
sigma <- cov(returns_matrix)
mu <- colMeans(returns_matrix)

N <- ncol(returns_matrix)
iota <- rep(1, N)
sigma_inv <- solve(sigma)
mvp_weights <- sigma_inv %*% iota
mvp_weights <- mvp_weights / sum(mvp_weights)
tibble(
  average_ret = as.numeric(t(mvp_weights) %*% mu),
  volatility = as.numeric(sqrt(t(mvp_weights) %*% sigma %*% mvp_weights))
)

benchmark_multiple <- 3
mu_bar <- benchmark_multiple * t(mvp_weights) %*% mu
C <- as.numeric(t(iota) %*% sigma_inv %*% iota)
D <- as.numeric(t(iota) %*% sigma_inv %*% mu)
E <- as.numeric(t(mu) %*% sigma_inv %*% mu)
lambda_tilde <- as.numeric(2 * (mu_bar - D / C) / (E - D^2 / C))
efp_weights <- mvp_weights +
  lambda_tilde / 2 * (sigma_inv %*% mu - D * mvp_weights)

length_year <- 12
a <- seq(from = -0.4, to = 1.9, by = 0.01)
res <- tibble(
  a = a,
  mu = NA,
  sd = NA
)

for (i in seq_along(a)) {
  w <- (1 - a[i]) * mvp_weights + (a[i]) * efp_weights
  res$mu[i] <- length_year * t(w) %*% mu
  res$sd[i] <- sqrt(length_year) * sqrt(t(w) %*% sigma %*% w)
}

res |>
  ggplot(aes(x = sd, y = mu)) +
  geom_point() +
  geom_point(
    data = res |> filter(a %in% c(0, 1)),
    size = 4
  ) +
  geom_point(
    data = tibble(
      mu = length_year * mu,
      sd = sqrt(length_year) * sqrt(diag(sigma))
    ),
    aes(y = mu, x = sd), size = 1
  ) +
  labs(
    x = "Annualized standard deviation",
    y = "Annualized expected return",
    title = "Efficient frontier for DOW index constituents"
  ) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent)


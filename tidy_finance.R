#### load libraries
library("tidyquant")
library("tidyverse")
library("data.table")
library("xts")
library("ggplot2")

######## Set defaults

ggplot2::theme_set(theme_bw())

######## code

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

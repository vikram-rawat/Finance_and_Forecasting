# load library ------------------------------------------------------------

library(data.table)
library(timeSeries)
library(xts)
library(quantmod)
library(forecast)
library(ggplot2)
library(FinancialMath)
library(fpp)
library(fpp2)
library(fpp3)
library(tsbox)
library(mlr3verse)
library(alphavantager)

# set defaults ------------------------------------------------------------

config <- config::get(value = "alphavantage")

setDTthreads(0L)
theme_set(
  theme_bw()
)

options(digits = 10)


av_api_key(config$api_key)

Sys.setenv("_R_USE_PIPEBIND_" = "true")

# get data ----------------------------------------------------------------

eur_usd <- av_get(
  symbol = "EUR/USD",
  av_fun = "FX_DAILY",
  time_period = 40,
  outputsize = "full"
) |>
  as.data.table() |>
  as.xts(order.by = timestamp)

eur_usd_today <- av_get(
  symbol = "EUR/USD",
  av_fun = "FX_INTRADAY",
  interval = "5min",
  time_period = 40,
  outputsize = "full"
) |>
  as.data.table() |>
  as.xts(order.by = timestamp)

# monthly ---------------------------------------------------------------

eur_usd |>
  chartSeries()
addSMA(n = 20) 
addBBands(n = 5)
addRSI(n = 10, maType = "EMA")
addMACD(
  fast = 10,
  slow = 20,
  signal = 5,
  type = "EMA",
  histogram = TRUE
)

zoomChart(
  subset = "2021-05/",
  yrange = c(1.19, 1.23)
)

# daily -------------------------------------------------------------------

eur_usd_today |>
  chartSeries()
addSMA(n = 12) 
addBBands(n = 12)
addRSI(n = 14, maType = "EMA")
addMACD(
  fast = 24,
  slow = 52,
  signal = 18,
  type = "EMA",
  histogram = TRUE
)
zoomChart(
  subset = Sys.Date(),
  yrange = c(1.19, 1.23)
)

"Moving average convergence divergence"
"Relative strength Index"
"Bollinger Bands"
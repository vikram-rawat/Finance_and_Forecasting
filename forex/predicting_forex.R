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
library(alphavantager)
library(smartapi)
library(mlr3verse)
library(quantstrat)

# set defaults ------------------------------------------------------------

av_key <- config::get(value = "alphavantage")

# ab_key <- config::get(value = "angelbroking")
# 
# ab_obj <- create_connection_object(ab_key)
# 
# ab_data <- get_candle_data(
#   object = ab_obj,
#   exchange = "NSE",
#   symboltoken = 3045,
#   interval = "ONE_MINUTE",
#   fromdate = "2021-02-10 09:15",
#   todate = "2021-02-10 11:15"
# )

setDTthreads(0L)
theme_set(
  theme_bw()
)

options(digits = 10)

av_api_key(av_key$api_key)

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
addRSI(
  n = 10, 
  maType = "EMA"
)
addMACD(
  fast = 10,
  slow = 20,
  signal = 5,
  type = "EMA",
  histogram = TRUE
)

zoomChart(
  subset = "2021-05/",
  yrange = c(1.18, 1.22)
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
  subset = as.character(Sys.Date()),
  yrange = c(1.21, 1.22)
)

# create a strategy ---------------


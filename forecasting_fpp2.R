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
library(rrapply)
library(roll)
library(clock)
library(alphavantager)

# set defaults ------------------------------------------------------------

setDTthreads(0L)
theme_set(
  theme_bw()
)

options(digits = 10)

config <- config::get(value = "alphavantage")

av_api_key(config$api_key)

# get data ----------------------------------------------------------------

data("AirPassengers")
data("melsyd")
data("unemployment", package = "lmtest")

eur_usd <- av_get(
  symbol = "EUR/USD",
  av_fun = "FX_DAILY",
  time_period = 40,
  outputsize = "full"
) |> 
  as.data.table() |>
  as.xts(order.by = timestamp)

airp_xts <- as.xts(AirPassengers)
unemp_xts <- as.xts(unemployment)

# eur_usd <- getFX(
#   Currencies = "EUR/USD",
#   from = "2020-11-01",
#   auto.assign = FALSE
# )

# chapter 1 ---------------------------------------------------------------

y <- ts(data = c(123, 39, 78, 52, 110), start = 2012)

autoplot(y)
methods(generic.function = "autoplot")
autoplot.zoo(airp_xts)
autoplot.zoo(unemp_xts)

ts_xts(melsyd) |> 
  ts_ts() |> 
  autoplot() +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

autoplot(a10)
autoplot(airp_xts)
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

autoplot(eur_usd)

ggseasonplot(
  x = a10,
  year.labels = TRUE,
  year.labels.left = TRUE
) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

ggseasonplot(
  x = a10, 
  polar = TRUE
  ) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

autoplot(
  object = elecdemand[,c("Demand","Temperature")], 
  facets = TRUE
  ) +
  xlab("Year: 2014") + 
  ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

qplot(
  x = Temperature,
  y =  Demand, 
  data = as.data.frame(elecdemand)
  ) + 
  ylab("Demand (GW)") + 
  xlab("Temperature (Celsius)")

autoplot(
  visnights[,1:5], 
  facets = TRUE
  ) +
  ylab("Number of visitor nights each quarter (millions)")

as.data.frame(visnights[,1:5]) |>
  GGally::ggpairs()

ausbeer |> 
  plot()

ausbeer |>
  window(start = 1992) |>
  gglagplot()

ggAcf(visnights[,1])
plot(visnights[,1])

eur_usd |>
  chartSeries()

addSMA(n = 20) 
addBBands()
addRSI(n=14,maType="EMA")
addMACD(fast=12,slow=26,signal=9,type="EMA")
zoomChart(
  subset = "2021-05/",
  yrange = c(1.19,1.23)
)

"Moving average convergence divergence"
"Relative strength Index"
"Bollinger Bands"
# load libraries ----------------------------------------------------------
library("tsibble")
library("fable")
library("feasts")

# set defaults ------------------------------------------------------------

setDTthreads(0L)

theme_set(
  theme_bw()
)

options(digits = 10)

# code --------------------------------------------------------------------

tsibble_ap <- AirPassengers |> 
  as_tsibble() 

tsibble_ap |> 
  autoplot()

tsibble_ap |> 
  ACF() |> 
  autoplot()

tsibble_ap |> 
  PACF() |> 
  autoplot()

ap_lag <- tsibble_ap |> 
  mutate(lag1 = difference(value, lag = 1, order_by = index)) |> 
  select(index, lag1) |> 
  mutate( monthi = month(index))  

ap_lag <- ap_lag |> 
  as_tibble() |> 
  group_by(monthi) |> 
  summarise(mean_month = mean(lag1)) |> 
  inner_join(ap_lag,by = "monthi") |> 
  as_tsibble()

ap_lag <- ap_lag |> 
  mutate(
    lag1_season = lag1 - mean_month
  )

ap_lag |> 
  ACF() |> 
  autoplot()

ap_lag |> 
  autoplot()

ap_lag <- ap_lag |> 
  mutate(monthI = month(index)) 

ap_lag |> 
  group_by_key(monthI) |> 
  summarise(
    month_mean = mean(lag1)
  ) |> 
  autoplot()

ap_lag |> 
  PACF() |> 
  autoplot()

ap_lag |> 
  model(
    sarima = ARIMA(lag1 ~ 1 + pdq(2, 1, 0) + PDQ(0,1,1))
  ) |> 
  forecast(h = "1 year") |> 
  autoplot()

  
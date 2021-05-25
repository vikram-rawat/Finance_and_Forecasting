# load library ------------------------------------------------------------

library(magrittr)
library(data.table)
library(timeSeries)
library(xts)
library(quantmod)
library(forecast)
library(ggplot2)
library(ggthemes)
library(FinancialMath)

# set defaults ------------------------------------------------------------

setDTthreads(16)
theme_set(
  theme_solarized(
    light = FALSE,
    base_size = 14)
)

# get Data ----------------------------------------------------------------

data("AirPassengers")
data("unemployment", package = "lmtest")

airp_xts <- as.xts(AirPassengers)
unemp_xts <- as.xts(unemployment)

# chapter 1 ---------------------------------------------------------------

xts::first(
  x = airp_xts,
  n =  "1 years"
)

xts::last(
  x = airp_xts,
  n =  "1 years"
)

airp_xts %>% 
  convertIndex('POSIXct') %>% 
  tclass()

methods(class = "xts")
methods(generic.function = "plot")
start(airp_xts)
end(airp_xts)
head(airp_xts)
tail(airp_xts)

frequency(airp_xts)
summary(airp_xts)
plot(airp_xts,
     main = "Air Passenger")

plot(aggregate(AirPassengers))

period.sum(
  x = airp_xts,
  INDEX =  endpoints(
    x = airp_xts,
    on = "year"
  )
) %>% plot

airp_xts %>% 
  as.data.table %>% 
  ggplot(
    aes(index, V1)
    ) +
  geom_line(color = "white") +
  ylab("air")

chartSeries(airp_xts)
boxplot( 
  AirPassengers ~ cycle(AirPassengers))
boxplot( 
  coredata(airp_xts) ~ cycle(airp_xts))

frequency(airp_xts)

plot(decompose(AirPassengers))
plot(decompose(as.ts(airp_xts)))

class(unemployment)
plot(unemployment)
plot(unemp_xts)
aggregate(
  head(unemp_xts),
  by = as.yearqtr,
  FUN = sum) %>% 
  plot()
head(unemp_xts)

window(unemp_xts[,"UN"], freq = TRUE)
window(unemployment, freq = TRUE)

(unemp_xts[,"UN"] / lag(unemp_xts[,"UN"], 4)) %>% 
  plot()

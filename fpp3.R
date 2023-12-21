# load libraries ----------------------------------------------------------
library("dplyr")
library("ggplot2")
library("GGally")
library("data.table")
library("xts")
library("clock")
library("collapse")
library("broom")
library("fpp2")
library("fpp3")
library("tsibble")
library("fable")
library("fabletools")
library("feasts")
library("fable.prophet")
library("fasttime")
# set defaults ------------------------------------------------------------

data(a10)
setDTthreads(0L)
theme_set(
  theme_bw()
)

options(digits = 10)

# code --------------------------------------------------------------------

a10 |>
  as_tsibble() |>
  gg_season(value, labels = "both") +
  labs(
    y = "$ (millions)",
    title = "Seasonal plot: Antidiabetic drug sales"
  )

vic_elec |>
  gg_season(Demand, period = "day") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

vic_elec |>
  gg_season(Demand, period = "week")

vic_elec |>
  gg_season(Demand, period = "year")

a10 |>
  as_tsibble() |>
  gg_subseries(value)

holiday <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holiday |>
  autoplot(Trips)

holiday |>
  gg_season()

holiday |>
  gg_subseries()


vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

vic_elec_xts <- vic_elec |>
  as.xts()

elect_ts <- vic_elec_xts["2014"] |>
  as.data.table() |>
  as_tsibble(index = index)

all.equal(
  elect_ts,
  vic_elec |>
    filter(year(Time) == 2014)
)


vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(
    aes(
      x = Temperature,
      y = Demand,
      alpha = I(0.1),
      color = I("lightgreen")
    ),
  ) +
  geom_point() +
  labs(
    x = "Temprature (celcius)",
    y = "Electricity Demand"
  )

tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips)) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y")

tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips)) |>
  pivot_wider(values_from = Trips, names_from = State) |>
  ggpairs(columns = 2:9)

aus_production |>
  filter(year(Quarter) >= 2000) |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

aus_xts <- aus_production |>
  as.data.table()

aus_xts[
  ,
  Quarter := Quarter |>
    as.character() |>
    as.yearqtr(format = "%Y Q%q") |>
    as.Date()
]

aus_xts <- aus_xts |>
  as.xts()

aus_production |>
  filter(year(Quarter) >= 2000) |>
  ACF(Beer, lag_max = 9) |>
  autoplot() +
  labs(title = "Australian beer production")

a10 |>
  autoplot()

a10 |>
  as_tsibble() |>
  ACF(value) |>
  autoplot()

a10 |>
  as_tsibble() |>
  mutate(diff_value = difference(value, differences = 2, order_by = index))

a10 |>
  as_tsibble() |>
  mutate(diff_value = difference(value, differences = 1, order_by = index))

global_economy |>
  filter(Country == "Australia") |>
  autoplot()

global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP / Population)

print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))

aus_economy <- global_economy |>
  filter(Code == "AUS")

print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover),
    values_to = "Turnover"
  ) |>
  mutate(name = factor(name,
    levels = c("Turnover", "Adjusted_turnover")
  )) |>
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(
    title = "Turnover: Australian print media industry",
    y = "$AU"
  )

lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  _$lambda_guerrero

lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)
aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(
    y = "",
    title = paste0(
      "Transformed gas production with $\\lambda$ = ",
      round(lambda, 2)
    )
  )

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

us_retail_employment |>
  autoplot(Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))

dcmp |>
  components() |>
  autoplot()

global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

global_economy |>
  filter(Country == "Australia") |>
  mutate(
    ma5 = slider::slide_dbl(
      .x = Exports,
      .f = fmean,
      .before = 3,
      .after = 3,
      .complete = TRUE
    )
  ) |>
  autoplot(
    Exports
  ) +
  geom_line(
    aes(
      y = ma5
    ),
    color = "green"
  )

us_retail_employment |>
  mutate(
    ma12 = slider::slide_dbl(
      .x = Employed,
      .f = fmean,
      .before = 5,
      .after = 6,
      .complete = TRUE
    ),
    ma2x12 = slider::slide_dbl(
      .x = ma12,
      .f = fmean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    )
  ) |>
  autoplot(Employed, color = "grey") +
  geom_line(aes(y = ma2x12), colour = "royalblue") +
  geom_line(aes(y = ma12), colour = "green")


us_retail_employment |>
  model(
    classical_decomposition(Employed, type = "additive")
  ) |>
  components() |>
  autoplot()

us_retail_employment |>
  model(
    x11 = X_13ARIMA_SEATS(Employed ~ x11())
  ) |>
  components() |>
  autoplot()

us_retail_employment |>
  model(
    x11 = X_13ARIMA_SEATS(Employed ~ x11())
  ) |>
  components() |>
  gg_subseries(seasonal)

us_retail_employment |>
  model(
    seats = X_13ARIMA_SEATS(Employed ~ seats())
  ) |>
  components() |>
  autoplot()

us_retail_employment |>
  model(
    stl = STL(
      Employed ~ trend(window = 7) + season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot()



tourism |>
  features(
    .var = Trips,
    features = list(
      mean = fmean,
      sd = fsd
    )
  ) |>
  arrange(mean)

tourism |>
  filter(
    Region == "Kangaroo Island",
    Purpose == "Other"
  ) |>
  pull(Trips) |>
  fmean()

tourism |>
  features(
    .var = Trips,
    features = list(
      fquantile
    )
  )

tourism |>
  features(
    .var = Trips,
    features = feat_acf
  )

tourism |>
  features(
    .var = Trips,
    features = feat_stl
  ) |>
  ggplot(
    aes(
      x = trend_strength,
      y = seasonal_strength_year,
      col = Purpose
    )
  ) +
  geom_point() +
  facet_wrap(~State)

tourism |>
  features(
    .var = Trips,
    features = feat_stl
  ) |>
  left_join(
    tourism,
    by = c("State", "Region", "Purpose"),
    multiple = "all"
  ) |>
  filter(
    Purpose == "Holiday",
    State == "Western Australia"
  ) |>
  ggplot(
    aes(
      x = Quarter,
      y = Trips
    )
  ) +
  geom_line() +
  facet_wrap(~Region)

tourism_features <- tourism |>
  features(
    .var = Trips,
    features = feature_set(pkgs = "feasts")
  )

tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4 * (seasonal_peak_year == 0),
    seasonal_trough_year = seasonal_trough_year +
      4 * (seasonal_trough_year == 0)
  ) |>
  GGally::ggpairs(mapping = aes(color = Purpose, alpha = I(0.4)))

pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)

pcs |>
  ggplot(
    aes(x = .fittedPC1, y = .fittedPC2, col = Purpose),
    alpha = I(0.4)
  ) +
  geom_point() +
  facet_wrap(~Purpose)

outliers <- pcs |>
  filter(.fittedPC1 > 10) |>
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)

outliers |>
  left_join(
    tourism,
    by = c("State", "Region", "Purpose"),
    multiple = "all"
  ) |>
  mutate(
    Series = sprintf(
      fmt = "%s\n\n%s\n\n%s\n\n",
      State,
      Region,
      Purpose
    )
  ) |>
  ggplot(
    aes(x = Quarter, y = Trips)
  ) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free")

gdppc <- global_economy |>
  mutate(
    GDP_per_capita = GDP / Population
  )

gdppc |>
  filter(
    Country == "Sweden"
  ) |>
  autoplot(GDP)

fit <- gdppc |>
  model(
    trend_line = TSLM(GDP_per_capita ~ trend())
  )

fit |>
  forecast(h = "3 years") |>
  filter(Country == "Sweden") |>
  autoplot(gdppc)

bricks <- aus_production |>
  filter_index("1970 Q1" ~ "2004 Q4") |>
  select(Bricks)

bricks |>
  anyNA()

bricks |>
  model(
    mean = MEAN(Bricks)
  ) |>
  forecast(h = 4) |>
  autoplot(bricks)

bricks |>
  model(
    mean = MEAN(Bricks),
    snaive = SNAIVE(Bricks),
    naive = NAIVE(Bricks),
    drift = RW(Bricks ~ drift())
  ) |>
  forecast(h = 8) |>
  autoplot(bricks, level = NULL)

train <- aus_production |>
  filter_index("1992 Q1" ~ "2006 Q4")

beer_fit <- train |>
  model(
    mean = MEAN(Beer),
    naive = RW(Beer),
    snaive = SNAIVE(Beer)
  )

beer_fc <- beer_fit |>
  forecast(h = 14)

beer_fc |>
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    color = "orange"
  )

google_stock <- gafa_stock |>
  filter(Symbol == "GOOG") |>
  filter_index("2015" ~ .) |>
  mutate(
    day = row_number()
  ) |>
  update_tsibble(
    index = day,
    regular = TRUE
  )

google_2015 <- google_stock |>
  filter(year(Date) == 2015)

google_fit <- google_2015 |>
  model(
    mean = MEAN(Close),
    naive = NAIVE(Close),
    drift = NAIVE(Close ~ drift())
  )

google_jan_2016 <- google_stock |>
  filter(yearmonth(Date) == yearmonth("2016 Jan"))

google_fc <- google_fit |>
  forecast(new_data = google_jan_2016)

google_fc |>
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, color = "grey")

beer_fit |>
  augment()

google_2015 |>
  autoplot(Close)

aug <- google_2015 |>
  model(NAIVE(Close)) |>
  augment()

aug |>
  autoplot(.innov)

aug |>
  ggplot(aes(x = .innov)) +
  geom_histogram()

aug |>
  ACF(.innov) |>
  autoplot()

google_2015 |>
  model(
    NAIVE(Close)
  ) |>
  gg_tsresiduals()


aug |>
  features(
    .var = .innov,
    features = list(boxp = box_pierce, ljung = ljung_box),
    lag = 10
  )

fit <- google_2015 |>
  model(
    RW(Close ~ drift())
  )

tidy(fit)

fit |>
  augment() |>
  features(
    .var = .innov,
    features = list(ljung_box),
    lag = 10
  )

google_2015 |>
  model(
    NAIVE(Close)
  ) |>
  forecast(h = 10) |>
  hilo()

google_2015 |>
  model(
    NAIVE(Close)
  ) |>
  forecast(h = 10) |>
  autoplot(google_2015)

fit <- google_2015 |>
  model(
    NAIVE(Close)
  )

sim <- fit |>
  generate(h = 30, times = 10, bootstrap = TRUE)

google_2015 |>
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(data = sim, aes(y = .sim, color = as.factor(.rep))) +
  guides(color = "none")

fc <- fit |>
  forecast(h = 30, bootstrap = TRUE)

fc |>
  autoplot(google_2015)


google_2015 |>
  model(
    NAIVE(Close)
  ) |>
  forecast(
    h = 10,
    bootstrap = TRUE,
    times = 1000
  ) |>
  hilo()

prices |>
  filter(!is.na(eggs)) |>
  model(
    RW(log(eggs) ~ drift())
  ) |>
  forecast(h = 50) |>
  autoplot(
    prices |>
      filter(!is.na(eggs)),
    level = 80,
    point_forecast = list(mean = mean, median = median)
  )

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade")

dcmp <- us_retail_employment |>
  model(
    STL(Employed ~ trend(window = 7), robust = TRUE)
  ) |>
  components() |>
  select(-.model)

dcmp |>
  model(
    NAIVE(season_adjust)
  ) |>
  forecast() |>
  autoplot(dcmp)

fit_dcmp <- us_retail_employment |>
  model(
    stlf = decomposition_model(
      STL(Employed ~ trend(window = 7), robust = TRUE),
      NAIVE(season_adjust)
    )
  )

fit_dcmp |>
  forecast() |>
  autoplot(us_retail_employment)

fit_dcmp |>
  gg_tsresiduals()

recent_production <- aus_production |>
  filter_index("1992" ~ .)

beer_train <- recent_production |>
  filter_index(. ~ "2007")

beer_fit <- beer_train |>
  model(
    mean = MEAN(Beer),
    naive = NAIVE(Beer),
    snaive = SNAIVE(Beer),
    drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit |>
  forecast(h = 10)

beer_fc |>
  autoplot(
    recent_production,
    level = NULL
  )

beer_fc |>
  fabletools::accuracy(recent_production)

google_fit <- google_2015 |>
  model(
    mean = MEAN(Close),
    naive = NAIVE(Close),
    drift = RW(Close ~ drift())
  )

google_fc <- google_fit |>
  forecast(google_jan_2016)

google_fc |>
  autoplot(
    bind_rows(
      google_2015,
      google_jan_2016
    ),
    level = NULL
  )

google_fc |>
  accuracy(google_jan_2016)

google_fc |>
  filter(.model == "naive") |>
  autoplot(
    bind_rows(
      google_2015,
      google_jan_2016
    ),
    level = 80
  )

google_fc |>
  filter(
    .model == "naive",
    Date == "2016-01-04"
  ) |>
  accuracy(
    google_stock,
    list(
      qs = quantile_score,
      winkler = winkler_score
    ),
    level = 80,
    probs = 0.10
  )

google_fc |>
  accuracy(
    google_stock,
    list(
      crps = CRPS
    )
  )

google_fc |>
  accuracy(
    google_stock,
    list(
      skill_crps = skill_score(CRPS),
      skill_mse = skill_score(MSE)
    )
  )

google_2015_tr <- google_2015 |>
  stretch_tsibble(
    .init = 3,
    .step = 1
  ) |>
  relocate(
    Date,
    Symbol,
    .id
  )

google_2015_tr |>
  model(
    RW(Close ~ drift())
  ) |>
  forecast(h = 1) |>
  accuracy(
    google_2015
  )

google_2015 |>
  model(
    RW(Close ~ drift())
  ) |>
  accuracy()

fc <- google_2015_tr |>
  model(
    RW(Close ~ drift())
  ) |>
  forecast(h = 8) |>
  group_by(.id) |>
  mutate(h = row_number()) |>
  ungroup() |>
  as_fable(
    response = "Close",
    distribution = Close
  )

fc |>
  accuracy(
    google_2015,
    by = c("h", ".model")
  ) |>
  ggplot(
    aes(
      x = h,
      y = RMSE
    )
  ) +
  geom_point()

us_change |>
  pivot_longer(
    c(Consumption, Income),
    names_to = "Series"
  ) |>
  autoplot(value)

us_change |>
  model(TSLM(Consumption ~ Income)) |>
  report()

us_change |>
  model(TSLM(Consumption ~ Income))

us_change |>
  ggpairs(columns = 2:6)

fit_consMR <- us_change |>
  model(
    TSLM(
      Consumption ~ Income +
        Production +
        Savings
    )
  )

report(fit_consMR)

augment(fit_consMR) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit_consMR) |>
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  geom_smooth(method = "lm")

us_change |>
  model(
    TSLM(Consumption ~ Income)
  ) |>
  augment() |>
  forecast() |>
  autoplot(us_change)

fit_consMR |>
  gg_tsresiduals()

fit_consMR |>
  augment() |>
  features(
    .innov,
    ljung_box,
    lag = 10
  )

us_change |>
  left_join(residuals(fit_consMR), by = "Quarter") |>
  pivot_longer(
    Income:Unemployment,
    names_to = "regressor",
    values_to = "x"
  ) |>
  ggplot(aes(
    x = x,
    y = .resid
  )) +
  geom_point() +
  facet_wrap(
    . ~ regressor,
    scales = "free_x"
  )

fit_consMR |>
  augment() |>
  ggplot(
    aes(
      x = .fitted,
      y = .resid
    )
  ) +
  geom_point()

fit <- aus_airpassengers |>
  filter_index(. ~ "2011") |>
  left_join(guinea_rice, by = "Year") |>
  model(
    TSLM(
      Passengers ~ Production
    )
  )

fit |> report()

fit |>
  gg_tsresiduals()


recent_production <- aus_production |>
  filter_index("1992 Q1" ~ .)

recent_production |>
  autoplot(Beer)

fit_beer <- recent_production |>
  model(
    TSLM(
      Beer ~ trend() + season()
    )
  )

fit_beer |>
  report()

fit_beer |>
  augment() |>
  ggplot(aes(x = Quarter)) +
  geom_line(
    aes(y = Beer, color = I("orange")),
  ) +
  geom_line(
    aes(y = .fitted)
  )

fit_beer |>
  augment() |>
  ggplot(aes(
    x = Beer, y = .fitted,
    color = as.factor(quarter(Quarter))
  )) +
  geom_point() +
  geom_smooth(method = "loess")

fourier_beer <- recent_production |>
  model(
    TSLM(
      Beer ~ trend() + fourier(K = 2)
    )
  )

fourier_beer |> report()

fit_consMR |>
  glance() |>
  select(adj_r_squared, CV, AIC, AICc, BIC)

recent_production <- aus_production |>
  filter(year(Quarter) >= 1992)

fit_beer <- recent_production |>
  model(TSLM(Beer ~ trend() + season()))

fc_beer <- fit_beer |>
  forecast()

fc_beer |>
  autoplot(recent_production)

fit_consBest <- us_change |>
  model(
    TSLM(
      Consumption ~ Income + Savings + Unemployment
    )
  )

future_scenarios <- scenarios(
  Increase = new_data(
    us_change, 4
  ) |>
    mutate(
      Income = 1,
      Savings = 0.5,
      Unemployment = 0
    ),
  Decrease = new_data(
    us_change, 4
  ) |>
    mutate(
      Income = -1,
      Savings = -0.5,
      Unemployment = 0
    ),
  names_to = "scenario"
)

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change |>
  autoplot(Consumption) +
  autolayer(fc)

fit_cons <- us_change |>
  model(TSLM(Consumption ~ Income))

new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) |>
    mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) |>
    mutate(Income = 12),
  names_to = "Scenario"
)

fcast <- forecast(fit_cons, new_cons)

us_change |>
  autoplot(Consumption) +
  autolayer(fcast)

boston_men <- boston_marathon |>
  filter_index("1924" ~ .) |>
  filter(Event == "Men's open division") |>
  mutate(Minutes = as.numeric(Time) / 60)

boston_men |>
  autoplot(Minutes) +
  geom_smooth(method = "lm") +
  geom_smooth(method = "loess", color = "orange")

fit_trends <- boston_men |>
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )

fc_trends <- fit_trends |>
  forecast(h = 10)

boston_men |>
  autoplot(Minutes) +
  geom_line(
    data = fitted(fit_trends),
    aes(y = .fitted, colour = .model)
  ) +
  autolayer(
    fc_trends,
    alpha = 0.4,
    level = 80
  )

algeria_economy <- global_economy |>
  filter(Country == "Algeria")

algeria_economy |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")

fit <- algeria_economy |>
  model(
    ETS(
      Exports ~ error("A") +
        trend("N") +
        season("N")
    )
  )

fc <- fit |>
  forecast(h = 5)

fc |>
  autoplot(algeria_economy) +
  geom_line(
    aes(y = .fitted),
    col = "orange",
    data = augment(fit)
  )

fit |>
  gg_tsresiduals()

aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)

aus_economy |>
  autoplot(Pop)

fit <- aus_economy |>
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )

fit <- fit |>
  forecast(h = 10)

fit |>
  autoplot(aus_economy)

aus_economy |>
  model(
    holt_method = ETS(
      Pop ~ trend("A") + season("N") + error("A")
    ),
    damped_holt_method = ETS(
      Pop ~ trend("Ad", phi = 0.9) + season("N") + error("A")
    )
  ) |>
  forecast(h = 15) |>
  autoplot(aus_economy, level = NULL)

www_usage <- WWWusage |>
  as_tsibble()

www_usage |>
  autoplot()

www_usage |>
  stretch_tsibble(.init = 10) |>
  model(
    simple_exp_smoothing = ETS(
      value ~ error("A") + trend("N") + season("N")
    ),
    holt = ETS(
      value ~ error("A") + trend("A") + season("N")
    ),
    holt_damped = ETS(
      value ~ error("A") + trend("Ad") + season("N")
    )
  ) |>
  forecast(h = 5) |>
  accuracy(www_usage)


fit <- www_usage |>
  model(
    damped = ETS(value ~ error("A") + trend("Ad") + season("N"))
  )

tidy(fit)

fit |>
  forecast(h = 10) |>
  autoplot(www_usage)

aus_holidays <- tourism |>
  filter(
    Purpose == "Holiday"
  ) |>
  summarise(
    Trips = sum(Trips) / 1e3
  )

fit <- aus_holidays |>
  model(
    additive = ETS(
      Trips ~ error("A") + trend("A") + season("A")
    ),
    multiplicative = ETS(
      Trips ~ error("M") + trend("A") + season("M")
    )
  )

fc <- fit |>
  forecast(h = "3 years")

fc |>
  autoplot(aus_holidays, level = NULL)

sth_cross_ped <- pedestrian |>
  filter(
    Date >= "2016-07-01",
    Sensor == "Southern Cross Station"
  ) |>
  index_by(Date) |>
  summarise(
    Count = sum(Count) / 1e3
  )

sth_cross_ped |>
  filter(
    Date <= "2016-07-31"
  ) |>
  model(
    hw = ETS(
      Count ~ error("M") + trend("Ad") + season("M")
    )
  ) |>
  forecast(h = "2 weeks") |>
  autoplot(
    sth_cross_ped |>
      filter(
        Date <= "2016-08-14"
      )
  )

aus_holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(
    Trips = sum(Trips) / 1e3
  )

fit <- aus_holidays |>
  model(ETS(Trips))

fit |> report()

fit |>
  components() |>
  autoplot()

fit |>
  augment() |>
  autoplot()

fit |>
  forecast(h = 8) |>
  autoplot(aus_holidays)

google_2015 <- gafa_stock |>
  filter(
    Symbol == "GOOG",
    year(Date) == 2015
  )

google_2015 |>
  ACF(Close) |>
  autoplot()

google_2015 |>
  ACF(difference(Close)) |>
  autoplot()

google_2015 |>
  mutate(
    diff_close = difference(Close)
  ) |>
  features(
    diff_close,
    ljung_box,
    lag = 10
  )

PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6) |>
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
      difference(
        difference(log(Cost), 12), 1
      )
  ) |>
  as.data.table()

google_2015 |>
  mutate(
    diff_close = difference(Close)
  ) |>
  features(diff_close, unitroot_kpss)

google_2015 |>
  features(
    Close,
    unitroot_ndiffs
  )

aus_total_retail <- aus_retail |>
  summarise(Turnover = sum(Turnover))

aus_total_retail |>
  mutate(
    log_turnover = log(
      Turnover
    )
  ) |>
  features(
    log_turnover,
    unitroot_ndiffs
  )

global_economy |>
  filter(Code == "EGY") |>
  autoplot(Exports)

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(
    ARIMA(Exports)
  )

report(fit)

fit |>
  forecast(h = 10) |>
  autoplot(global_economy)

global_economy |>
  filter(
    Code == "EGY"
  ) |>
  ACF(Exports) |>
  autoplot()

global_economy |>
  filter(
    Code == "EGY"
  ) |>
  PACF(Exports) |>
  autoplot()

global_economy |>
  filter(
    Code == "EGY"
  ) |>
  model(
    ARIMA(Exports ~ pdq(4, 0, 0))
  ) |>
  report()

global_economy |>
  filter(
    Code == "EGY"
  ) |>
  model(
    ARIMA(Exports ~ pdq(1:4, 0:1, 0:2))
  ) |>
  report()

global_economy |>
  filter(
    Code == "CAF"
  ) |>
  autoplot(
    Exports
  )

global_economy |>
  filter(
    Code == "CAF"
  ) |>
  gg_tsdisplay(
    difference(Exports),
    plot_type = "partial"
  )

caf_fit <- global_economy |>
  filter(
    Code == "CAF"
  ) |>
  model(
    arima210 = ARIMA(Exports ~ pdq(2, 1, 0)),
    arima013 = ARIMA(Exports ~ pdq(0, 1, 3)),
    stepwise = ARIMA(Exports),
    search = ARIMA(Exports, stepwise = TRUE)
  )

caf_fit |>
  pivot_longer(!Country)

caf_fit |>
  glance() |>
  arrange(AICc) |>
  select(.model:BIC)

caf_fit |>
  select(search) |>
  gg_tsresiduals()


caf_fit |>
  augment() |>
  filter(.model == "search") |>
  features(
    .innov,
    ljung_box,
    lag = 10,
    dof = 3
  )

caf_fit |>
  forecast(h = 5) |>
  filter(.model == "search") |>
  autoplot(global_economy)

caf_fit |>
  select(Country, search) |>
  gg_arma()

#################

leisure <- us_employment |>
  filter(
    Title == "Leisure and Hospitality",
    year(Month) > 2000
  ) |>
  mutate(Employed = Employed / 1000) |>
  select(Month, Employed)

autoplot(leisure, Employed) +
  labs(
    title = "US employment: leisure and hospitality",
    y = "Number of people (millions)"
  )

leisure |>
  gg_tsdisplay(
    difference(Employed, 12),
    plot_type = "partial",
    lag = 36
  )

leisure |>
  gg_tsdisplay(
    Employed |>
      difference(12) |>
      difference(),
    plot_type = "partial",
    lag = 36
  ) +
  labs(
    title = "Double differenced",
    y = ""
  )

fit <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
    arima210011 = ARIMA(Employed ~ pdq(2, 1, 0) + PDQ(0, 1, 1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )

fit |>
  pivot_longer(
    everything(),
    names_to = "Model name",
    values_to = "Orders"
  )

glance(fit) |>
  arrange(AICc) |>
  select(.model:BIC)

fit |>
  select(auto) |>
  gg_tsresiduals(lag = 36)

augment(fit) |>
  filter(.model == "auto") |>
  features(
    .innov,
    ljung_box,
    lag = 24,
    dof = 4
  )

fit |>
  forecast(h = 36) |>
  filter(.model == "auto") |>
  autoplot(leisure)


h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

h02 |>
  mutate(log(Cost)) |>
  pivot_longer(-Month) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y = "", title = "Corticosteroid drug scripts (H02)")

h02 |>
  gg_tsdisplay(
    difference(log(Cost), 12),
    plot_type = "partial",
    lag_max = 24
  )

fit <- h02 |>
  model(
    ARIMA(log(Cost) ~ 0 + pdq(3, 0, 1) + PDQ(0, 1, 2))
  )

fit |>
  gg_tsresiduals(lag_max = 36)

fit |>
  augment() |>
  features(
    .innov,
    ljung_box,
    lag = 36,
    dof = 6
  )

h02 |>
  model(
    ARIMA(log(Cost) ~ pdq(3, 0, 1) + PDQ(0, 1, 2))
  ) |>
  forecast(h = 36) |>
  autoplot(h02)

##################

aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Population = Population / 1e6)

aus_economy |>
  slice(-n()) |>
  stretch_tsibble(.init = 10) |>
  model(
    ETS(Population),
    ARIMA(Population)
  ) |>
  forecast(h = 1) |>
  accuracy(aus_economy) |>
  select(.model, RMSE:MAPE)

aus_economy |>
  model(ETS(Population)) |>
  forecast(h = "5 years") |>
  autoplot(aus_economy |> filter(Year >= 2000)) +
  labs(
    title = "Australian population",
    y = "People (millions)"
  )

cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)

train <- cement |>
  filter_index(. ~ "2007 Q4")

fit_arima <- train |>
  model(ARIMA(Cement))

report(fit_arima)

fit_arima |>
  gg_tsresiduals(lag_max = 16)

augment(fit_arima) |>
  features(.innov, ljung_box, lag = 16, dof = 5)

fit_ets <- train |> model(ETS(Cement))
report(fit_ets)

fit_ets |>
  gg_tsresiduals(lag_max = 16)

augment(fit_ets) |>
  features(.innov, ljung_box, lag = 16)

bind_rows(
  fit_arima |> accuracy(),
  fit_ets |> accuracy(),
  fit_arima |> forecast(h = 10) |> accuracy(cement),
  fit_ets |> forecast(h = 10) |> accuracy(cement)
) |>
  select(-ME, -MPE, -ACF1)

cement |>
  model(ARIMA(Cement)) |>
  forecast(h = "3 years") |>
  autoplot(cement) +
  labs(
    title = "Cement production in Australia",
    y = "Tonnes ('000)"
  )

us_change |>
  pivot_longer(
    c(Consumption, Income),
    names_to = "var",
    values_to = "value"
  ) |>
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(
    title = "US consumption and personal income",
    y = "Quarterly % change"
  )

fit <- us_change |>
  model(
    ARIMA(
      Consumption ~ Income
    )
  )

fit |>
  report()

fit |>
  residuals()


bind_rows(
  `Regression residuals` =
    as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` =
    as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) |>
  mutate(
    type = factor(type, levels = c(
      "Regression residuals", "ARIMA residuals"
    ))
  ) |>
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

fit |>
  gg_tsresiduals()

fit |>
  augment() |>
  features(
    .innov,
    ljung_box,
    dof = 3,
    lag = 8
  )

us_change |> tail()
new_data(us_change, 8)

us_change_future <- new_data(us_change, 8) |>
  mutate(Income = mean(us_change$Income))

fit |>
  forecast(
    new_data = us_change_future
  ) |>
  autoplot(us_change) +
  labs(y = "Percentage change")


vic_elec_daily <- vic_elec |>
  filter(year(Time) == 2014) |>
  index_by(Date = date(Time)) |>
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))


vic_elec_daily |>
  ggplot(
    aes(
      x = Temperature,
      y = Demand,
      colour = Day_Type,
      size = I(3),
      alpha = I(0.3)
    )
  ) +
  geom_point() +
  labs(
    y = "Electricity demand (GW)",
    x = "Maximum daily temperature"
  )

vic_elec_daily |>
  pivot_longer(c(Demand, Temperature)) |>
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  ylab("")

fit <- vic_elec_daily |>
  model(
    ARIMA(
      Demand ~
        Temperature +
        I(Temperature^2) +
        (Day_Type == "Weekday")
    )
  )

fit |>
  gg_tsresiduals()

fit |>
  augment() |>
  features(
    .innov,
    ljung_box,
    dof = 6,
    lag = 14
  )

vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

fit |>
  forecast(vic_elec_future) |>
  autoplot(vic_elec_daily) +
  labs(
    title = "Daily electricity demand: Victoria",
    y = "GW"
  )

############

aus_airpassengers |>
  autoplot(Passengers) +
  labs(
    y = "Passengers (millions)",
    title = "Total annual air passengers"
  )

fit_deterministic <- aus_airpassengers |>
  model(
    deterministic = ARIMA(
      Passengers ~ 1 + trend() + pdq(d = 0)
    )
  )

report(fit_deterministic)

fit_stochastic <- aus_airpassengers |>
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))

report(fit_stochastic)

aus_airpassengers |>
  autoplot(Passengers) +
  autolayer(
    fit_stochastic |>
      forecast(h = 20),
    colour = "#0072B2",
    level = 95,
    alpha = 0.4
  ) +
  autolayer(
    fit_deterministic |>
      forecast(h = 20),
    colour = "#D55E00",
    alpha = 0.45,
    level = 95
  ) +
  labs(
    y = "Air passengers (millions)",
    title = "Forecasts from trend models"
  )


aus_cafe <- aus_retail |>
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) |>
  summarise(Turnover = sum(Turnover))

aus_cafe |>
  autoplot()

fit <- aus_cafe |>
  model(
    `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0, 0, 0)),
    `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0, 0, 0)),
    `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0, 0, 0)),
    `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0, 0, 0)),
    `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0, 0, 0)),
    `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0, 0, 0))
  )

fit |>
  forecast(h = "2 years") |>
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    aes(
      x = yearmonth("2007 Jan"), y = 4250,
      label = paste0("AICc = ", format(AICc))
    ),
    data = glance(fit)
  ) +
  labs(
    title = "Total monthly eating-out expenditure",
    y = "$ billions"
  )

insurance |>
  pivot_longer(Quotes:TVadverts) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")

fit <- insurance |>
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) |>
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) +
      TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) +
      TVadverts + lag(TVadverts) +
      lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) +
      TVadverts + lag(TVadverts) +
      lag(TVadverts, 2) + lag(TVadverts, 3))
  )

fit |>
  glance()

fit_best <- insurance |>
  model(
    ARIMA(
      Quotes ~ pdq(d = 0) +
        TVadverts + lag(TVadverts)
    )
  )

report(fit_best)

insurance_future <- new_data(insurance, 20) |>
  mutate(TVadverts = 8)

fit_best |>
  forecast(insurance_future) |>
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )

####################
####################
####################

tourism <- tsibble::tourism |>
  mutate(State = recode(State,
    `New South Wales` = "NSW",
    `Northern Territory` = "NT",
    `Queensland` = "QLD",
    `South Australia` = "SA",
    `Tasmania` = "TAS",
    `Victoria` = "VIC",
    `Western Australia` = "WA"
  ))

tourism_hts <- tourism |>
  aggregate_key(State / Region, Trips = sum(Trips))

tourism_hts |>
  filter(
    is_aggregated(Region)
  ) |>
  autoplot(Trips) +
  facet_wrap(vars(State))

tourism_hts |>
  filter(State == "NT" | State == "QLD" |
    State == "TAS" | State == "VIC", is_aggregated(Region)) |>
  select(-Region) |>
  mutate(State = factor(State, levels = c("QLD", "VIC", "NT", "TAS"))) |>
  gg_season(Trips) +
  facet_wrap(vars(State), nrow = 2, scales = "free_y") +
  labs(y = "Trips ('000)")

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(
    key = c(Gender, Legal, State, Indigenous),
    index = Quarter
  ) |>
  relocate(Quarter)

prison_gts <- prison |>
  aggregate_key(Gender * Legal * State, Count = sum(Count) / 1e3)

prison_gts |>
  filter(
    !is_aggregated(Gender), is_aggregated(Legal),
    is_aggregated(State)
  ) |>
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)")

prison_gts |>
  filter(
    !is_aggregated(Gender), is_aggregated(Legal),
    is_aggregated(State)
  ) |>
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)")

prison_gts |>
  filter(
    !is_aggregated(Gender), !is_aggregated(Legal),
    !is_aggregated(State)
  ) |>
  mutate(Gender = as.character(Gender)) |>
  ggplot(aes(
    x = Quarter, y = Count,
    group = Gender, colour = Gender
  )) +
  stat_summary(fun = sum, geom = "line") +
  labs(
    title = "Prison population by state and gender",
    y = "Number of prisoners ('000)"
  ) +
  facet_wrap(~ as.character(State),
    nrow = 1, scales = "free_y"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tourism_full <- tourism |>
  aggregate_key((State / Region) * Purpose, Trips = sum(Trips))

tourism_states |>
  filter(
    !is_aggregated(State)
  )

tourism_states <- tourism |>
  aggregate_key(State, Trips = sum(Trips))

fcasts_state <- tourism_states |>
  filter(!is_aggregated(State)) |>
  model(ets = ETS(Trips)) |>
  forecast()

# Sum bottom-level forecasts to get top-level forecasts
fcasts_national <- fcasts_state |>
  summarise(value = sum(Trips), .mean = mean(value))

tourism_states |>
  model(ets = ETS(Trips)) |>
  reconcile(bu = bottom_up(ets)) |>
  forecast() |>
  autoplot(tourism_states)

tourism_full <- tourism |>
  aggregate_key(
    .spec = (State / Region) * Purpose,
    Trips = sum(Trips)
  )

fit <- tourism_full |>
  filter(year(Quarter) <= 2015) |>
  model(
    base = ETS(Trips)
  ) |>
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink")
  )

fc <- fit |>
  forecast(h = "2 years")

fc |>
  filter(
    is_aggregated(Region),
    is_aggregated(Purpose)
  ) |>
  autoplot(
    tourism_full |>
      filter(
        year(Quarter) >= 2011
      ),
    level = NULL
  ) +
  facet_wrap(
    vars(State),
    scales = "free_y"
  )

fc |>
  filter(
    is_aggregated(State),
    !is_aggregated(Purpose)
  ) |>
  autoplot(
    tourism_full |>
      filter(
        year(Quarter) >= 2011
      ),
    level = NULL
  ) +
  facet_wrap(
    vars(Purpose),
    scales = "free_y"
  )

fc |>
  filter(
    is_aggregated(State),
    is_aggregated(Purpose)
  ) |>
  accuracy(
    data = tourism_full,
    measures = list(
      rmse = RMSE,
      mase = MASE
    )
  ) |>
  group_by(.model) |>
  summarise(
    rmse = mean(rmse),
    mase = mean(mase)
  )

##########

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(
    key = c(Gender, Legal, State, Indigenous),
    index = Quarter
  ) |>
  relocate(Quarter)

prison_gts <- prison |>
  aggregate_key(
    Gender * Legal * State,
    Count = sum(Count) / 1e3
  )

fit <- prison_gts |>
  filter(year(Quarter) <= 2014) |>
  model(
    base = ETS(Count)
  ) |>
  reconcile(
    bottom_up = bottom_up(base),
    MinT = min_trace(base, method = "mint_shrink")
  )

fc <- fit |>
  forecast(h = 8)

fc |>
  filter(
    is_aggregated(State),
    is_aggregated(Gender),
    is_aggregated(Legal)
  ) |>
  autoplot(
    prison_gts,
    alpha = 0.4,
    level = 90
  )


fc |>
  filter(
    .model %in% c("base", "MinT"),
    !is_aggregated(State), is_aggregated(Legal),
    is_aggregated(Gender)
  ) |>
  autoplot(
    prison_gts |> filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(
    title = "Prison population (by state)",
    y = "Number of prisoners ('000)"
  ) +
  facet_wrap(vars(State), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


fc |>
  filter(
    is_aggregated(State), is_aggregated(Gender),
    is_aggregated(Legal)
  ) |>
  accuracy(
    data = prison_gts,
    measures = list(
      mase = MASE,
      ss = skill_score(CRPS)
    )
  ) |>
  group_by(.model) |>
  summarise(mase = mean(mase), sspc = mean(ss) * 100)

###############

bank_calls |>
  fill_gaps() |>
  autoplot(Calls) +
  labs(
    y = "Calls",
    title = "Five-minute call volume to bank"
  )

calls <- bank_calls |>
  mutate(t = row_number()) |>
  update_tsibble(index = t, regular = TRUE)

calls |>
  model(
    STL(
      sqrt(Calls) ~ season(period = 169) +
        season(period = 5 * 169),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Observation")

# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(
    sqrt(Calls) ~ season(period = 169) +
      season(period = 5 * 169),
    robust = TRUE
  ),
  ETS(season_adjust ~ season("N"))
)

fc <- calls |>
  model(my_dcmp_spec) |>
  forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
  new_data(n = 7 * 24 * 60 / 5) |>
  mutate(time = format(DateTime, format = "%H:%M:%S")) |>
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) |>
  mutate(t = row_number() + max(calls$t)) |>
  left_join(fc, by = "t") |>
  as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times |>
  fill_gaps() |>
  autoplot(bank_calls |> tail(14 * 169) |> fill_gaps()) +
  labs(
    y = "Calls",
    title = "Five-minute call volume to bank"
  )


fit <- calls |>
  model(
    dhr = ARIMA(
      sqrt(Calls) ~
        PDQ(0, 0, 0) +
        pdq(d = 0) +
        fourier(period = 169, K = 10) +
        fourier(period = 5 * 169, K = 5)
    )
  )

fc <- fit |>
  forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
  new_data(n = 7 * 24 * 60 / 5) |>
  mutate(time = format(DateTime, format = "%H:%M:%S")) |>
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) |>
  mutate(t = row_number() + max(calls$t)) |>
  left_join(fc, by = "t") |>
  as_fable(response = "Calls", distribution = Calls)

fit <- calls |>
  model(
    dhr = ARIMA(sqrt(Calls) ~ PDQ(0, 0, 0) + pdq(d = 0) +
      fourier(period = 169, K = 10) +
      fourier(period = 5 * 169, K = 5))
  )

fc <- fit |>
  forecast(h = 5 * 169)

fc_with_times <- bank_calls |>
  new_data(n = 7 * 24 * 60 / 5) |>
  mutate(time = format(DateTime, format = "%H:%M:%S")) |>
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) |>
  mutate(t = row_number() + max(calls$t)) |>
  left_join(fc, by = "t") |>
  as_fable(response = "Calls", distribution = Calls)

fc_with_times |>
  fill_gaps() |>
  autoplot(
    bank_calls |>
      tail(14 * 169) |>
      fill_gaps()
  ) +
  labs(
    y = "Calls",
    title = "Five-minute call volume to bank"
  )


vic_elec |>
  pivot_longer(Demand:Temperature, names_to = "Series") |>
  ggplot(
    aes(
      x = Time,
      y = value,
      alpha = 0.2
    )
  ) +
  geom_line() +
  facet_grid(rows = vars(Series), scales = "free_y") +
  labs(y = "")


elec <- vic_elec |>
  mutate(
    DOW = wday(Date, label = TRUE),
    Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )

elec |>
  ggplot(aes(x = Temperature, y = Demand, col = Working_Day)) +
  geom_point(alpha = 0.1) +
  labs(x = "Temperature (degrees Celsius)", y = "Demand (MWh)")

fit <- elec |>
  model(
    ARIMA(Demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
      Temperature + Cooling + Working_Day +
      fourier(period = "day", K = 10) +
      fourier(period = "week", K = 5) +
      fourier(period = "year", K = 3))
  )

elec_newdata <- new_data(elec, 2 * 48) |>
  mutate(
    Temperature = tail(elec$Temperature, 2 * 48),
    Date = lubridate::as_date(Time),
    DOW = wday(Date, label = TRUE),
    Working_Day = (Date != "2015-01-01") &
      !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )

fc <- fit |>
  forecast(new_data = elec_newdata)

fc |>
  autoplot(elec |> tail(10 * 48)) +
  labs(
    title = "Half hourly electricity demand: Victoria",
    y = "Demand (MWh)", x = "Time [30m]"
  )

fit |>
  gg_tsresiduals()

####################

cement <- aus_production |>
  filter(year(Quarter) >= 1988)
train <- cement |>
  filter(year(Quarter) <= 2007)
fit <- train |>
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(Cement ~ season(
      period = 4, order = 2,
      type = "multiplicative"
    ))
  )

fc <- fit |> forecast(h = "2 years 6 months")
fc |> autoplot(cement, level = NULL)

fc |> accuracy(cement)

fit <- elec |>
  model(
    prophet(Demand ~ Temperature + Cooling + Working_Day +
      season(period = "day", order = 10) +
      season(period = "week", order = 5) +
      season(period = "year", order = 3))
  )

fit |>
  components() |>
  autoplot()

fit |>
  gg_tsresiduals()

fc <- fit |>
  forecast(new_data = elec_newdata)

fc |>
  autoplot(elec |> tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")

###############

fit <- us_change |>
  model(
    aicc = VAR(
      vars(
        Consumption,
        Income
      )
    ),
    bic = VAR(
      vars(
        Consumption,
        Income
      )
    )
  )

fit

glance(fit)

fit |>
  augment() |>
  ACF(.innov) |>
  autoplot()

fit |>
  select(aicc) |>
  forecast() |>
  autoplot(
    us_change |>
      filter(year(Quarter) > 2010)
  )

######

sunspots <- sunspot.year |> as_tsibble()

fit <- sunspots |>
  model(NNETAR(sqrt(value)))

plot <- fit |>
  forecast(h = 30) |>
  autoplot(sunspots) +
  labs(x = "Year", y = "Counts", title = "Yearly sunspots")

plot2 <- fit |>
  generate(times = 9, h = 30) |>
  autoplot(.sim) +
  autolayer(sunspots, value) +
  theme(legend.position = "none")

#####

cement <- aus_production |>
  filter(year(Quarter) >= 1988) |>
  select(Quarter, Cement)

cement_stl <- cement |>
  model(stl = STL(Cement))

cement_stl |>
  components() |>
  autoplot()


cement_stl |>
  generate(
    new_data = cement, times = 10,
    bootstrap_block_size = 8
  ) |>
  autoplot(.sim) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(
    title = "Cement production: Bootstrapped series",
    y = "Tonnes ('000)"
  )

sim <- cement_stl |>
  generate(
    new_data = cement, times = 100,
    bootstrap_block_size = 8
  ) |>
  select(-.model, -Cement)

ets_forecasts <- sim |>
  model(ets = ETS(.sim)) |>
  forecast(h = 12)

ets_forecasts |>
  update_tsibble(key = .rep) |>
  autoplot(.mean) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(
    title = "Cement production: bootstrapped forecasts",
    y = "Tonnes ('000)"
  )

bagged <- ets_forecasts |>
  summarise(
    bagged_mean = mean(.mean)
  )

cement |>
  model(ets = ETS(Cement)) |>
  forecast(h = 12) |>
  autoplot(cement) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(
    title = "Cement production in Australia",
    y = "Tonnes ('000)"
  )

######

attach(us_gasoline)
my_dcmp_spec <- decomposition_model(
  STL(Barrels),
  ETS(season_adjust ~ season("N"))
)
us_gasoline |>
  model(stl_ets = my_dcmp_spec) |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline) +
  labs(
    y = "Millions of barrels per day",
    title = "Weekly US gasoline production"
  )


gas_dhr <- us_gasoline |>
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))

gas_dhr |>
  forecast(h = "2 years") |>
  autoplot(us_gasoline) +
  labs(
    y = "Millions of barrels per day",
    title = "Weekly US gasoline production"
  )

j06 <- PBS |>
  filter(ATC2 == "J06") |>
  summarise(Scripts = sum(Scripts))

j06 |>
  autoplot(Scripts) +
  labs(
    y = "Number of scripts",
    title = "Sales for immune sera and immunoglobulins"
  )

j06 |>
  model(CROSTON(Scripts)) |>
  forecast(h = 6)

egg_prices <- prices |>
  filter(!is.na(eggs))

egg_prices |>
  model(ETS(log(eggs) ~ trend("A"))) |>
  forecast(h = 50) |>
  autoplot(egg_prices) +
  labs(
    title = "Annual egg prices",
    y = "$US (in cents adjusted for inflation) "
  )

scaled_logit <- function(x, lower = 0, upper = 1) {
  log((x - lower) / (upper - x))
}

inv_scaled_logit <- function(x, lower = 0, upper = 1) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}

my_scaled_logit <- new_transformation(
  scaled_logit, inv_scaled_logit
)
egg_prices |>
  model(
    ETS(
      my_scaled_logit(
        eggs,
        lower = 50,
        upper = 400
      ) ~ trend("A")
    )
  ) |>
  forecast(h = 50) |>
  autoplot(egg_prices) +
  labs(
    title = "Annual egg prices",
    y = "$US (in cents adjusted for inflation) "
  )


auscafe <- aus_retail |>
  filter(stringr::str_detect(Industry, "Takeaway")) |>
  summarise(Turnover = sum(Turnover))

train <- auscafe |>
  filter(year(Month) <= 2013)

STLF <- decomposition_model(
  STL(log(Turnover) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)

cafe_models <- train |>
  model(
    ets = ETS(Turnover),
    stlf = STLF,
    arima = ARIMA(log(Turnover))
  ) |>
  mutate(combination = (ets + stlf + arima) / 3)

cafe_fc <- cafe_models |>
  forecast(h = "5 years")

cafe_fc |>
  autoplot(
    auscafe |>
      filter(year(Month) > 2008),
    level = NULL
  )

cafe_fc |>
  accuracy(auscafe) |>
  arrange(RMSE)

cafe_fc |>
  filter(Month == min(Month))

cafe_futures <- cafe_models |>
  # Generate 1000 future sample paths
  generate(h = "5 years", times = 1000) |>
  # Compute forecast distributions from future sample paths
  as_tibble() |>
  group_by(Month, .model) |>
  summarise(
    dist = distributional::dist_sample(list(.sim))
  ) |>
  ungroup() |>
  # Create fable object
  as_fable(
    index = Month, key = .model,
    distribution = dist, response = "Turnover"
  )

cafe_futures |>
  filter(.model == "combination") |>
  autoplot(auscafe |> filter(year(Month) > 2008)) +
  labs(
    y = "$ billion",
    title = "Australian monthly expenditure on eating out"
  )

cafe_futures |>
  accuracy(auscafe,
    measures = interval_accuracy_measures,
    level = 95
  ) |>
  arrange(winkler)


fit <- auscafe |>
  # Fit a model to the data
  model(ETS(Turnover))

futures <- fit |>
  # Simulate 10000 future sample paths, each of length 12
  generate(times = 10000, h = 12) |>
  # Sum the results for each sample path
  as_tibble() |>
  group_by(.rep) |>
  summarise(.sim = sum(.sim)) |>
  # Store as a distribution
  summarise(total = distributional::dist_sample(list(.sim)))

futures |>
  mutate(
    mean = mean(total),
    pi80 = hilo(total, 80),
    pi95 = hilo(total, 95)
  )

forecast(fit, h = 12) |>
  as_tibble() |>
  summarise(total = sum(.mean))


backcasts <- auscafe |>
  mutate(reverse_time = rev(row_number())) |>
  update_tsibble(index = reverse_time) |>
  model(ets = ETS(Turnover ~ season(period = 12))) |>
  forecast(h = 15) |>
  mutate(
    Month = auscafe$Month[1] - (1:15)
  ) |>
  as_fable(
    index = Month,
    response = "Turnover",
    distribution = "Turnover"
  )

backcasts |>
  autoplot(
    auscafe |>
      filter(year(Month) < 1990)
  ) +
  labs(
    title = "Backcasts of Australian food expenditure",
    y = "$ (billions)"
  )


m3totsibble <- function(z) {
  bind_rows(
    as_tsibble(z$x) |> mutate(Type = "Training"),
    as_tsibble(z$xx) |> mutate(Type = "Test")
  ) |>
    mutate(
      st = z$st,
      type = z$type,
      period = z$period,
      description = z$description,
      sn = z$sn
    ) |>
    as_tibble()
}

short <- Mcomp::M3 |>
  subset("yearly") |>
  purrr::map_dfr(m3totsibble) |>
  group_by(sn) |>
  mutate(n = max(row_number())) |>
  filter(n <= 20) |>
  ungroup() |>
  as_tsibble(index = index, key = c(sn, period, st))


short_fit <- short |>
  model(arima = ARIMA(value))


training <- auscafe |> filter(year(Month) <= 2013)
test <- auscafe |> filter(year(Month) > 2013)
cafe_fit <- training |>
  model(ARIMA(log(Turnover)))
cafe_fit |>
  forecast(h = 60) |>
  autoplot(auscafe) +
  labs(
    title = "Australian food expenditure",
    y = "$ (billions)"
  )

fits12 <- fitted(cafe_fit, h = 12)
training |>
  autoplot(Turnover) +
  autolayer(fits12, .fitted, col = "#D55E00") +
  labs(
    title = "Australian food expenditure",
    y = "$ (billions)"
  )

cafe_fit |>
  refit(test) |>
  accuracy()
#############

tourism |>
  filter(
    Region == "Adelaide Hills", Purpose == "Visiting"
  ) |>
  autoplot(Trips) +
  labs(
    title = "Quarterly overnight trips to Adelaide Hills",
    y = "Number of trips"
  )


ah_decomp <- tourism |>
  filter(
    Region == "Adelaide Hills", Purpose == "Visiting"
  ) |>
  # Fit a non-seasonal STL decomposition
  model(
    stl = STL(Trips ~ season(period = 1), robust = TRUE)
  ) |>
  components()

ah_decomp |>
  autoplot()


outliers <- ah_decomp |>
  filter(
    remainder < quantile(remainder, 0.25) - 3 * IQR(remainder) |
      remainder > quantile(remainder, 0.75) + 3 * IQR(remainder)
  )

outliers


ah_miss <- tourism |>
  filter(
    Region == "Adelaide Hills",
    Purpose == "Visiting"
  ) |>
  # Remove outlying observations
  anti_join(outliers) |>
  # Replace with missing values
  fill_gaps()
ah_fill <- ah_miss |>
  # Fit ARIMA model to the data containing missing values
  model(ARIMA(Trips)) |>
  # Estimate Trips for all periods
  interpolate(ah_miss)
ah_fill |>
  # Only show outlying periods
  right_join(outliers |> select(-Trips))
#> # A tsibble: 1 x 9 [?]
#> # Key:       Region, State, Purpose [1]
#>   Region         State      Purpose Quarter Trips .model trend remai…¹ seaso…²
#>   <chr>          <chr>      <chr>     <qtr> <dbl> <chr>  <dbl>   <dbl>   <dbl>
#> 1 Adelaide Hills South Aus… Visiti… 2002 Q4  8.50 stl     11.1    70.0    81.1
#> # … with abbreviated variable names ¹​remainder, ²​season_adjust
ah_fill |>
  autoplot(Trips) +
  autolayer(ah_fill |> filter_index("2002 Q3" ~ "2003 Q1"),
    Trips,
    colour = "#D55E00"
  ) +
  labs(
    title = "Quarterly overnight trips to Adelaide Hills",
    y = "Number of trips"
  )

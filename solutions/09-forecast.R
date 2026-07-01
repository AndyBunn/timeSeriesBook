# Solutions for Chapter 6: Forecasting
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)
library(tsibble)
library(forecast)

# ---------------------------------------------------------------------------
# Exercise 1: Beat the climatology
# ---------------------------------------------------------------------------
# Build monthly mean temperature from the Bellingham record, withhold three
# years, race AR and Holt-Winters against a seasonal climatology baseline.

kbli <- read_csv("data/kbli.csv", show_col_types = FALSE) |>
  mutate(TAVG = (TMAX + TMIN) / 2) |>
  as_tsibble(index = DATE)

kbli_monthly <- kbli |>
  index_by(month = ~ yearmonth(.)) |>
  summarise(tavg = mean(TAVG, na.rm = TRUE))

temp <- ts(kbli_monthly$tavg, start = c(2000, 1), frequency = 12)

h <- 36 # withhold three years
temp_train <- head(temp, length(temp) - h)
temp_test  <- tail(temp, h)

# AR
temp_ar <- ar(temp_train)
ar_fc   <- predict(temp_ar, n.ahead = h)
ar_rmse <- sqrt(mean((ar_fc$pred - temp_test)^2))

# Holt-Winters
temp_hw <- HoltWinters(temp_train)
hw_fc   <- predict(temp_hw, n.ahead = h)
hw_rmse <- sqrt(mean((hw_fc - temp_test)^2))

# Climatology: each test month gets its training-period mean
monthly_mean <- tapply(as.numeric(temp_train), cycle(temp_train), mean)
clim_pred    <- monthly_mean[cycle(temp_test)]
clim_rmse    <- sqrt(mean((clim_pred - temp_test)^2))

c(ar = ar_rmse, hw = hw_rmse, clim = clim_rmse)
# Same story as the river: temperature is overwhelmingly seasonal, so the
# climatology baseline is hard to beat and neither model clears it by much.
# Most of the predictability in monthly temperature is just the annual cycle.

# ---------------------------------------------------------------------------
# Exercise 2: How far can you see?
# ---------------------------------------------------------------------------
# For each phi, find how many steps until the k-step forecast SD reaches 95%
# of the ceiling sigma / sqrt(1 - phi^2).

sigma <- 1
horizon_to_95 <- function(phi, kmax = 50) {
  k    <- 1:kmax
  sd_k <- sigma * sqrt(cumsum(phi^(2 * (0:(kmax - 1)))))
  ceil <- sigma / sqrt(1 - phi^2)
  list(ceiling = ceil,
       steps_to_95 = which(sd_k >= 0.95 * ceil)[1])
}

lapply(c(0.3, 0.7, 0.95), horizon_to_95)
# As phi -> 1 the ceiling rises (the series swings wider) AND the approach to
# it slows (more steps to reach 95%). A strongly autocorrelated series is
# harder to forecast far out, because the ceiling is high, but stays
# forecastable for longer, because the variance climbs to that ceiling slowly.
# This is the ACF decay read from the other end: slow ACF decay <-> slow
# variance saturation.

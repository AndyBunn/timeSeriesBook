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

kbliMonthly <- kbli |>
  index_by(month = ~ yearmonth(.)) |>
  summarise(tavg = mean(TAVG, na.rm = TRUE))

temp <- ts(kbliMonthly$tavg, start = c(2000, 1), frequency = 12)

h <- 36 # withhold three years
tempTrain <- head(temp, length(temp) - h)
tempTest  <- tail(temp, h)

# AR
tempAr <- ar(tempTrain)
arFc   <- predict(tempAr, n.ahead = h)
arRMSE <- sqrt(mean((arFc$pred - tempTest)^2))

# Holt-Winters
tempHw <- HoltWinters(tempTrain)
hwFc   <- predict(tempHw, n.ahead = h)
hwRMSE <- sqrt(mean((hwFc - tempTest)^2))

# Climatology: each test month gets its training-period mean
monthlyMean <- tapply(as.numeric(tempTrain), cycle(tempTrain), mean)
climPred    <- monthlyMean[cycle(tempTest)]
climRMSE    <- sqrt(mean((climPred - tempTest)^2))

c(ar = arRMSE, hw = hwRMSE, clim = climRMSE)
# Same story as the river: temperature is overwhelmingly seasonal, so the
# climatology baseline is hard to beat and neither model clears it by much.
# Most of the predictability in monthly temperature is just the annual cycle.

# ---------------------------------------------------------------------------
# Exercise 2: How far can you see?
# ---------------------------------------------------------------------------
# For each phi, find how many steps until the k-step forecast SD reaches 95%
# of the ceiling sigma / sqrt(1 - phi^2).

sigma <- 1
horizonTo95 <- function(phi, kmax = 50) {
  k    <- 1:kmax
  sdK <- sigma * sqrt(cumsum(phi^(2 * (0:(kmax - 1)))))
  ceil <- sigma / sqrt(1 - phi^2)
  list(ceiling = ceil,
    stepsTo95 = which(sdK >= 0.95 * ceil)[1])
}

lapply(c(0.3, 0.7, 0.95), horizonTo95)
# As phi -> 1 the ceiling rises (the series swings wider) AND the approach to
# it slows (more steps to reach 95%). A strongly autocorrelated series is
# harder to forecast far out, because the ceiling is high, but stays
# forecastable for longer, because the variance climbs to that ceiling slowly.
# This is the ACF decay read from the other end: slow ACF decay <-> slow
# variance saturation.

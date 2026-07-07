# Worked solutions for ch 02, Decomposition
# Instructor answer key. Not part of the rendered book.
# Run from the project root so the data path resolves.

library(tidyverse)
library(tsibble)

# ---------------------------------------------------------------------------
# Exercise 1: additive or multiplicative for Arizona?
# ---------------------------------------------------------------------------

# (1) Both decompositions, MSE in GWh. Multiplicative comes out ahead.
az <- read_csv("data/az_electricity.csv")
elecTs <- ts(az$SALES, start = c(2001, 1), frequency = 12)
ea <- decompose(elecTs, type = "additive")
em <- decompose(elecTs, type = "multiplicative")
c(additive       = mean((elecTs - (ea$trend + ea$seasonal))^2,  na.rm = TRUE),
  multiplicative = mean((elecTs - (em$trend * em$seasonal))^2, na.rm = TRUE))
# additive MSE is ~1.65x larger: a clear but not dramatic win for multiplicative.
plot(elecTs, ylab = "Residential sales (GWh)")
lines(em$trend * em$seasonal, col = "firebrick")   # tracks the growing summer peak
lines(ea$trend + ea$seasonal, col = "steelblue")   # fixed swing, lags the growth

# (2) The advantage scales with how much the level climbs. AZ grew ~1.5x over
# 2001-2025, so the multiplicative edge is modest (~1.65x). A series that
# tripled (think 1950s AirPassengers, ~3.8x) would show a much wider gap,
# because a fixed additive season falls badly behind a steeply rising level.
# A flat series would show almost no gap at all: with no growth, adding vs
# multiplying the same average season gives nearly identical fits, so MSE
# cannot tell them apart. MSE separates the models only when the level moves.


# ---------------------------------------------------------------------------
# Exercise 2: decompose the Bellingham weather (monthly)
# ---------------------------------------------------------------------------

kbli <- read_csv("data/kbli.csv") |>
  mutate(TEMP = (TMAX + TMIN) / 2)
kbliTs <- as_tsibble(kbli, index = DATE)

# Aggregate daily -> monthly: mean for temperature, sum for precipitation.
monthly <- kbliTs |>
  index_by(month = ~ yearmonth(.)) |>
  summarise(TEMP = mean(TEMP, na.rm = TRUE),
    PRCP = sum(PRCP, na.rm = TRUE))

# decompose() needs a ts with a seasonal frequency. Pull the start year/month
# from the first index value and set frequency = 12.
startYm <- min(monthly$month)
startYr <- as.integer(format(startYm, "%Y"))
startMo <- as.integer(format(startYm, "%m"))

tavgTs <- ts(monthly$TEMP, start = c(startYr, startMo), frequency = 12)
prcpTs <- ts(monthly$PRCP, start = c(startYr, startMo), frequency = 12)

tavgDecomp <- decompose(tavgTs)
prcpDecomp <- decompose(prcpTs)

plot(tavgDecomp)   # clean trend + a strong, regular seasonal cycle
plot(prcpDecomp)   # a weak season and a noisy residual

# Why precipitation is harder: temperature is a smooth state variable with a
# dominant annual cycle, so the moving average separates trend and season
# cleanly. Precipitation is spiky and event-driven, the wet-season timing
# shifts year to year, and the na.rm = TRUE sum treats gauge outages as dry
# days. So the residual panel carries much more of the variance.

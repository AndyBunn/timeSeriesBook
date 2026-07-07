# Solutions for Chapter 8: Regression
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)
library(nlme)
library(forecast)

# ---------------------------------------------------------------------------
# Exercise 1: Push the simulation
# ---------------------------------------------------------------------------
# Recover a known slope under autocorrelated errors, then change the predictor
# from white noise to an AR process and watch the OLS standard error start to
# lie.

simCompare <- function(n, phi, B1, arX = FALSE) {
  if (arX) {
    x <- as.numeric(arima.sim(list(ar = 0.8), n = n))
  } else {
    x <- rnorm(n)
  }
  eps <- as.numeric(arima.sim(list(ar = phi), n = n))
  eps <- eps - mean(eps)
  y <- B1 * x + eps
  ols <- lm(y ~ x)
  glsFit <- gls(y ~ x, correlation = corARMA(p = 1))
  tibble(
    olsSlope = coef(ols)[2],  olsSE = summary(ols)$coefficients[2, 2],
    glsSlope = coef(glsFit)[2], glsSE = summary(glsFit)$tTable[2, 2]
  )
}

# Smaller true slope relative to noise: the gap between OLS and GLS widens,
# because the autocorrelated errors dominate a weak signal.
set.seed(1)
map_dfr(c(0.1, 0.5, 1.0), ~ simCompare(n = 100, phi = 0.8, B1 = .x)) |>
  mutate(B1 = c(0.1, 0.5, 1.0), .before = 1)

# White-noise predictor: OLS SE is roughly right, coverage near 95%.
# Empirical spread of the OLS slope vs the mean reported OLS SE should match.
set.seed(2)
wn <- map_dfr(1:500, ~ simCompare(n = 100, phi = 0.8, B1 = 0.5, arX = FALSE))
c(trueSpread = sd(wn$olsSlope), meanReportedSE = mean(wn$olsSE))

# AR predictor: now the OLS SE understates the true spread. This is the
# standard-error lie, the same mechanism as the trend example in the chapter.
set.seed(3)
arx <- map_dfr(1:500, ~ simCompare(n = 100, phi = 0.8, B1 = 0.5, arX = TRUE))
c(trueSpread = sd(arx$olsSlope), meanReportedSE = mean(arx$olsSE))
# meanReportedSE comes out well below trueSpread: OLS thinks it is more
# certain than it is, exactly because the predictor now shares time structure
# with the errors.

# ---------------------------------------------------------------------------
# Exercise 2: Colorado River, with temperature
# ---------------------------------------------------------------------------
# Add March-to-July temperature to the flow model. Does temperature help, are
# the residuals still autocorrelated, and does GLS change the temperature story?

flow <- read_csv("data/woodhouse.csv", show_col_types = FALSE)

olsMulti <- lm(LeesWYflow ~ OctAprP + MarJulT, data = flow)
summary(olsMulti)
# Temperature is significant with a negative coefficient: warmer March-July
# means less flow for the same precipitation. R^2 climbs from ~0.66 (precip
# only) to ~0.74. This is the Woodhouse result: temperature amplifies deficits.

# Residuals are still autocorrelated, though less so than the precip-only model.
ggAcf(residuals(olsMulti)) + labs(title = "Residual ACF, flow ~ precip + temp")
round(acf(residuals(olsMulti), plot = FALSE)$acf[2], 2)   # lag-1 about 0.30

# Refit with AR(1) errors and compare the temperature coefficient and its SE.
glsMulti <- gls(LeesWYflow ~ OctAprP + MarJulT, data = flow,
  correlation = corAR1())
summary(glsMulti)

bind_rows(
  ols = broom::tidy(olsMulti) |> filter(term == "MarJulT") |>
    select(estimate, std.error),
  gls = tibble(estimate = coef(glsMulti)["MarJulT"],
    std.error = summary(glsMulti)$tTable["MarJulT", 2]),
  .id = "model"
)
# The temperature coefficient stays clearly negative and significant under GLS,
# so the Woodhouse temperature effect survives the corrected inference. The predictor
# (MarJulT) carries some autocorrelation of its own, so unlike the precip-only
# model this is closer to the standard-error-deflation regime; checking with GLS
# is the right move, and here the conclusion holds.

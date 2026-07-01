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

sim_compare <- function(n, phi, B1, ar_x = FALSE) {
  if (ar_x) {
    x <- as.numeric(arima.sim(list(ar = 0.8), n = n))
  } else {
    x <- rnorm(n)
  }
  eps <- as.numeric(arima.sim(list(ar = phi), n = n))
  eps <- eps - mean(eps)
  y <- B1 * x + eps
  ols <- lm(y ~ x)
  gls_fit <- gls(y ~ x, correlation = corARMA(p = 1))
  tibble(
    ols_slope = coef(ols)[2],  ols_se = summary(ols)$coefficients[2, 2],
    gls_slope = coef(gls_fit)[2], gls_se = summary(gls_fit)$tTable[2, 2]
  )
}

# Smaller true slope relative to noise: the gap between OLS and GLS widens,
# because the autocorrelated errors dominate a weak signal.
set.seed(1)
map_dfr(c(0.1, 0.5, 1.0), ~ sim_compare(n = 100, phi = 0.8, B1 = .x)) |>
  mutate(B1 = c(0.1, 0.5, 1.0), .before = 1)

# White-noise predictor: OLS SE is roughly right, coverage near 95%.
# Empirical spread of the OLS slope vs the mean reported OLS SE should match.
set.seed(2)
wn <- map_dfr(1:500, ~ sim_compare(n = 100, phi = 0.8, B1 = 0.5, ar_x = FALSE))
c(true_spread = sd(wn$ols_slope), mean_reported_se = mean(wn$ols_se))

# AR predictor: now the OLS SE understates the true spread. This is the
# standard-error lie, the same mechanism as the trend example in the chapter.
set.seed(3)
arx <- map_dfr(1:500, ~ sim_compare(n = 100, phi = 0.8, B1 = 0.5, ar_x = TRUE))
c(true_spread = sd(arx$ols_slope), mean_reported_se = mean(arx$ols_se))
# mean_reported_se comes out well below true_spread: OLS thinks it is more
# certain than it is, exactly because the predictor now shares time structure
# with the errors.

# ---------------------------------------------------------------------------
# Exercise 2: Colorado River, with temperature
# ---------------------------------------------------------------------------
# Add March-to-July temperature to the flow model. Does temperature help, are
# the residuals still autocorrelated, and does GLS change the temperature story?

flow <- read_csv("data/woodhouse.csv", show_col_types = FALSE)

ols_multi <- lm(LeesWYflow ~ OctAprP + MarJulT, data = flow)
summary(ols_multi)
# Temperature is significant with a negative coefficient: warmer March-July
# means less flow for the same precipitation. R^2 climbs from ~0.66 (precip
# only) to ~0.74. This is the Woodhouse result: temperature amplifies deficits.

# Residuals are still autocorrelated, though less so than the precip-only model.
ggAcf(residuals(ols_multi)) + labs(title = "Residual ACF, flow ~ precip + temp")
round(acf(residuals(ols_multi), plot = FALSE)$acf[2], 2)   # lag-1 about 0.30

# Refit with AR(1) errors and compare the temperature coefficient and its SE.
gls_multi <- gls(LeesWYflow ~ OctAprP + MarJulT, data = flow,
                 correlation = corAR1())
summary(gls_multi)

bind_rows(
  ols = broom::tidy(ols_multi) |> filter(term == "MarJulT") |>
    select(estimate, std.error),
  gls = tibble(estimate = coef(gls_multi)["MarJulT"],
               std.error = summary(gls_multi)$tTable["MarJulT", 2]),
  .id = "model"
)
# The temperature coefficient stays clearly negative and significant under GLS,
# so the Woodhouse temperature effect survives the corrected inference. The predictor
# (MarJulT) carries some autocorrelation of its own, so unlike the precip-only
# model this is closer to the standard-error-deflation regime; checking with GLS
# is the right move, and here the conclusion holds.

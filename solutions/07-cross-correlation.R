# Solutions for Chapter 7: Cross-Correlation
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)
library(forecast)

# ---------------------------------------------------------------------------
# Exercise 1: Forecast the grazers
# ---------------------------------------------------------------------------
# Split Lake Washington in two, fit grazers on one-month-lagged producers over
# the training years, and predict the held-out grazers.

lwa <- read_csv("data/lake_wa_plankton.csv", show_col_types = FALSE) |>
  mutate(prodLag = dplyr::lag(producers, 1))   # producers about a month ahead

train <- lwa |> filter(Year <= 1984) |> drop_na()
test  <- lwa |> filter(Year >= 1985) |> drop_na()

fit <- lm(grazers ~ prodLag, data = train)
summary(fit)

test$grazersHat <- predict(fit, newdata = test)
cor(test$grazers, test$grazersHat)             # skill on the held-out years

ggplot(test, aes(seq_along(grazers))) +
  geom_line(aes(y = grazers), color = "black") +
  geom_line(aes(y = grazersHat), color = "darkred") +
  labs(x = "Month of test period", y = "Grazer index",
    title = "Held-out grazers (black) and one-month-lag prediction (red)") +
  theme_minimal()

# How much can it explain? Not much. The lagged-producer R^2 is modest because
# the cross-correlation peak sits almost on top of lag zero: a one-month lag
# captures only the small asymmetric part of a relationship that is mostly
# contemporaneous and mostly seasonal. And the slope's standard error from this
# lm is optimistic, because both series share an annual cycle that leaves the
# residuals autocorrelated, the OLS-lies problem the regression chapter takes
# up. The honest claim from the chapter is the direction (algae lead grazers),
# not a trustworthy predictive slope.

# ---------------------------------------------------------------------------
# Exercise 2: Make your own spurious correlation
# ---------------------------------------------------------------------------
# Simulate independent random-walk pairs, measure the raw spurious peak, then
# prewhiten by differencing and confirm it collapses.

spuriousPeak <- function(seed) {
  set.seed(seed)
  a <- cumsum(rnorm(200))
  b <- cumsum(rnorm(200))
  raw <- max(abs(ccf(a, b, lag.max = 20, plot = FALSE)$acf))
  pw  <- max(abs(ccf(diff(a), diff(b), lag.max = 20, plot = FALSE)$acf))
  c(raw = raw, prewhitened = pw)
}

results <- map_dfr(1:12, spuriousPeak)
results$seed <- 1:12
results

# The raw peaks are enormous, routinely 0.4 to 0.9, every one of them a
# "relationship" between two series that share nothing. After differencing they
# collapse to noise size, a fifth of that or less. The lesson is the magnitude,
# not a strict threshold: the peak you would have reported shrinks to nothing.
mean(results$raw)          # typical raw spurious peak
mean(results$prewhitened)  # typical peak after prewhitening

# A note on bands: the dashed 2/sqrt(n) line is a per-lag threshold, so taking
# the single largest bar across 40-odd lags will graze it by chance even for
# pure noise. Do not score the differenced series by "did the max clear the
# band"; judge it by whether the towering structure is gone. It is.

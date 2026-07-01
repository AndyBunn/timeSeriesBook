# Solutions for The Frequency Domain chapter
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)

# ---------------------------------------------------------------------------
# Exercise 1: Three signals, three spectra
# ---------------------------------------------------------------------------
set.seed(11)
n <- 500
white <- rnorm(n)
ar1   <- as.numeric(arima.sim(model = list(ar = 0.6), n = n))
rw    <- cumsum(white)

spec_white <- spectrum(white, plot = FALSE)
spec_ar1   <- spectrum(ar1,   plot = FALSE)
spec_rw    <- spectrum(rw,    plot = FALSE)

bind_rows(
  tibble(series = "white noise", freq = spec_white$freq, spec = spec_white$spec),
  tibble(series = "AR(1), phi=0.6", freq = spec_ar1$freq, spec = spec_ar1$spec),
  tibble(series = "random walk", freq = spec_rw$freq, spec = spec_rw$spec)
) |>
  ggplot(aes(freq, spec)) +
  geom_line() +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  labs(x = "Frequency", y = "Spectral density")

cat("white noise low/high ratio:", spec_white$spec[1] / tail(spec_white$spec, 1), "\n")
cat("AR(1) low/high ratio:      ", spec_ar1$spec[1]   / tail(spec_ar1$spec, 1),   "\n")
cat("random walk low/high ratio:", spec_rw$spec[1]    / tail(spec_rw$spec, 1),    "\n")
# White noise is flat on average, but a raw, unsmoothed periodogram is a
# jagged, high-variance estimator even for pure noise: one bin can sit a
# thousand times taller than its neighbor for no reason at all. AR(1) with a
# positive phi gives classic red noise, power piled up at low frequencies and
# trailing off toward high ones (about a tenfold ratio here). The random walk
# takes that same shape and makes it extreme, thousands-fold, because a
# random walk isn't stationary: its variance grows with t (var = t * sigma^2,
# see the stationarity chapter), so nearly all its "power" collects at
# frequency zero. Running spectrum() on a random walk isn't wrong exactly,
# but it's answering a question the tool wasn't built for. A periodogram
# assumes the series is (weakly) stationary; feeding it a random walk is the
# frequency-domain version of fitting an ARMA model without checking whether
# a unit root is sitting there first.

# ---------------------------------------------------------------------------
# Exercise 2: Build your own cycles
# ---------------------------------------------------------------------------
set.seed(872)
n2  <- 1000
tm2 <- 1:n2
wav250 <- 0.3  * sin(2 * pi / 250 * tm2)
wav50  <- 0.75 * sin(2 * pi / 50  * tm2)
wav10  <- 1.00 * sin(2 * pi / 10  * tm2)
wav5   <- 0.50 * sin(2 * pi / 5   * tm2)
trendPart <- 0.01 * tm2
allWaveTrend <- wav250 + wav50 + wav10 + wav5 + trendPart + rnorm(n2)

spec_default   <- spectrum(allWaveTrend, plot = FALSE)                  # detrend = TRUE
spec_notrend   <- spectrum(allWaveTrend, detrend = FALSE, plot = FALSE)

cat("lowest-frequency bin, default (detrend = TRUE): ", spec_default$spec[1], "\n")
cat("lowest-frequency bin, detrend = FALSE:          ", spec_notrend$spec[1], "\n")
# spectrum()'s default is detrend = TRUE: it fits and removes a linear trend
# before running the Fourier transform, so a straightforward trend does NOT
# show up as a towering low-frequency spike the way you might expect (the
# four planted cycles still come out on top even with the trend added). Turn
# detrend off and the lowest-frequency bin jumps by roughly three orders of
# magnitude, because now the trend itself, the lowest-frequency "cycle"
# there is, dominates everything else in the plot. Neither setting is wrong;
# they answer different questions. detrend = TRUE asks "what cycles ride on
# top of any trend," which is usually what you want. detrend = FALSE shows
# you the trend's own footprint in the frequency domain, which is a good way
# to convince yourself that a trend really is just an extremely low
# frequency, not some separate kind of thing.

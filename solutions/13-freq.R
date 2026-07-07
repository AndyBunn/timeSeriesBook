# Solutions for the Spectral Analysis chapter
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

specWhite <- spectrum(white, plot = FALSE)
specAr1   <- spectrum(ar1,   plot = FALSE)
specRw    <- spectrum(rw,    plot = FALSE)

bind_rows(
  tibble(series = "white noise", freq = specWhite$freq, spec = specWhite$spec),
  tibble(series = "AR(1), phi=0.6", freq = specAr1$freq, spec = specAr1$spec),
  tibble(series = "random walk", freq = specRw$freq, spec = specRw$spec)
) |>
  ggplot(aes(freq, spec)) +
  geom_line() +
  facet_wrap(~series, ncol = 1, scales = "free_y") +
  labs(x = "Frequency", y = "Spectral density")

cat("white noise low/high ratio:", specWhite$spec[1] / tail(specWhite$spec, 1), "\n")
cat("AR(1) low/high ratio:      ", specAr1$spec[1]   / tail(specAr1$spec, 1),   "\n")
cat("random walk low/high ratio:", specRw$spec[1]    / tail(specRw$spec, 1),    "\n")
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

specDefault   <- spectrum(allWaveTrend, plot = FALSE)                  # detrend = TRUE
specNotrend   <- spectrum(allWaveTrend, detrend = FALSE, plot = FALSE)

cat("lowest-frequency bin, default (detrend = TRUE): ", specDefault$spec[1], "\n")
cat("lowest-frequency bin, detrend = FALSE:          ", specNotrend$spec[1], "\n")
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

# ---------------------------------------------------------------------------
# Exercise 3: Orbital insolation and the Milankovitch cycles
# ---------------------------------------------------------------------------
insolation <- read_csv("data/jul65N.csv", show_col_types = FALSE)

insolationSpec <- spectrum(insolation$W.per.m2, span = 5, plot = FALSE)
peakFreq   <- insolationSpec$freq[which.max(insolationSpec$spec)]
peakPeriod <- 1 / peakFreq
cat("tallest peak, period (kyr):", round(peakPeriod, 1), "\n")

milankovitchBands <- tibble(cycle = c("precession", "precession", "obliquity", "eccentricity"),
  period = c(23, 19, 41, 100)) |>
  mutate(freq = 1 / period)

insolationSpecDat <- tibble(freq = insolationSpec$freq, spec = insolationSpec$spec)
eccPct <- insolationSpecDat$spec[which.min(abs(insolationSpecDat$freq - 1 / 100))] /
  max(insolationSpecDat$spec) * 100
cat("eccentricity peak, % of tallest peak:", round(eccPct, 2), "\n")

ggplot(insolationSpecDat, aes(freq, spec)) +
  geom_line(color = "grey40") +
  geom_vline(data = milankovitchBands, aes(xintercept = freq, color = cycle),
    linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(limits = c(0, 0.06)) +
  labs(x = "Frequency (cycles / kyr)", y = "Spectral density", color = NULL,
    title = "The Milankovitch bands in July insolation at 65°N")
# All three bands show up. The tallest peak sits at about 23.8 kyr, squarely
# in the precession band (19-24 kyr, the wobble of Earth's axis interacting
# with its elliptical orbit); a smaller peak lands right at the 41 kyr
# obliquity period (about 22% the height of the tallest peak); eccentricity,
# at 100 kyr, is almost invisible, about 0.07% the height of the tallest
# peak on a linear scale. Every one of those numbers comes out of celestial
# mechanics, not from fitting anything to the data.
#
# The 100-kyr problem: eccentricity is the weakest of the three signals here,
# yet the last 800,000 years of ice-core and ocean-sediment records are
# dominated by a roughly 100,000-year glacial-interglacial cycle, riding on
# the weakest orbital band rather than the strongest. If ice volume responded
# to insolation the way a simple linear filter would, the biggest swings
# ought to track precession, not a peak too small to see on this plot.
# Untangling why the climate system amplifies the weak eccentricity signal so
# far out of proportion to its size is still an open problem in
# paleoclimatology; the periodogram hands you the forcing, clean and
# unambiguous, but what the planet does with that forcing is a separate
# question this exercise doesn't answer.

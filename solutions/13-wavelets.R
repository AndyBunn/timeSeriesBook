# Solutions for the Wavelets chapter
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)
library(dplR)
library(waveslim)

# ---------------------------------------------------------------------------
# Exercise 1: Move the switchpoint
# ---------------------------------------------------------------------------
set.seed(872)
n <- 400
tm <- 1:n
switchPoint <- 100  # instead of 200
sig <- ifelse(tm <= switchPoint, sin(2 * pi * tm / 10), sin(2 * pi * tm / 30)) + rnorm(n, sd = 0.3)

cwtOut <- morlet(y1 = sig, x1 = tm, p2 = 8, dj = 0.1, siglvl = 0.99)
wavelet.plot(cwtOut, useRaster = NA, reverse.y = TRUE, crn.lab = "Signal")
# The wavelet plot still localizes the switch cleanly wherever it happens.
# Moving it to 100 (a quarter of the way in) does one visible thing: the cone
# of influence eats into the period-10 band earlier on its left edge, because
# there's less record on that side to support a wide window. A switch placed
# very close to either end will start to look like the cone of influence has
# swallowed the shorter segment almost entirely, i.e. field data with a regime
# change near the start or end of the record is exactly where a wavelet
# transform is least trustworthy. That's the same "least confident where you
# need it most" lesson the filters chapter found with loess, now showing up
# as a design limit of the wavelet transform rather than an estimator quirk.

# A double switch (10 -> 30 -> 10) shows two clean handoffs, each with its own
# cone-of-influence bite, but otherwise nothing new: the wavelet transform
# doesn't care how many times the frequency changes, only how much record it
# has on each side of a given point.
switchA <- 130; switchB <- 270
sig2 <- rnorm(n, sd = 0.3) +
  case_when(tm <= switchA ~ sin(2 * pi * tm / 10),
            tm <= switchB ~ sin(2 * pi * tm / 30),
            TRUE ~ sin(2 * pi * tm / 10))
cwtOut2 <- morlet(y1 = sig2, x1 = tm, p2 = 8, dj = 0.1, siglvl = 0.99)
wavelet.plot(cwtOut2, useRaster = NA, reverse.y = TRUE, crn.lab = "Signal")

# ---------------------------------------------------------------------------
# Exercise 2: Confirm it with numbers (tree-ring chronology)
# ---------------------------------------------------------------------------
data(co021)
co021_rwi <- detrend(co021, method = "Spline")
co021_crn <- chron(co021_rwi)
chronVal <- co021_crn[, 1]
chronYrs <- as.numeric(time(co021_crn))

nScales <- trunc(log(length(chronYrs)) / log(2)) - 1
dwtOut <- mra(chronVal, wf = "la8", J = nScales, method = "modwt", boundary = "periodic")

mid <- length(chronYrs) %/% 2
tibble(level = names(dwtOut),
       early = map_dbl(dwtOut, ~ var(.x[1:mid])),
       late  = map_dbl(dwtOut, ~ var(.x[(mid + 1):length(chronYrs)]))) |>
  mutate(ratio = round(late / early, 2))
# Unlike the planted demo, no single level flips as dramatically here; measured
# data rarely hands you a clean answer. The biggest mover is D6 (roughly a
# 32-64 year timescale), whose variance climbs about 3.5-fold in the late
# half of the record. That's consistent with the continuous wavelet plot
# above it, which shows a red patch at similar periods concentrated later in
# the record rather than spread evenly across all 788 years. The moral: the
# discrete and continuous transforms are two views of the same underlying
# fact, and measured chronologies are messier than a two-cycle simulation, which
# is exactly why you plant a known signal first before trusting either tool
# on data where you don't already know the answer.

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
co021_rwi <- detrend(co021, method = "AgeDepSpline")  # matches the chapter's wave-chron chunk
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
# data rarely hands you a clean answer. The two biggest movers are D6 (roughly
# a 32-64 year timescale) and D8 (roughly 128-256 years), whose variance each
# climb close to 4-fold in the late half of the record. That's consistent with
# the continuous wavelet plot above it, which shows the strongest, longest-held
# color at multi-decadal-to-centennial periods concentrated later in the
# record rather than spread evenly across all 788 years. The moral: the
# discrete and continuous transforms are two views of the same underlying
# fact, and measured chronologies are messier than a two-cycle simulation, which
# is exactly why you plant a known signal first before trusting either tool
# on data where you don't already know the answer.

# ---------------------------------------------------------------------------
# Exercise 3: A new rhythm (blowflies)
# ---------------------------------------------------------------------------
blowfly <- read_csv("data/blowfly.csv", show_col_types = FALSE)

plot(blowfly$Day, blowfly$Count, type = "l", xlab = "Day", ylab = "Adult count",
     main = "Nicholson's blowfly population")
# The raw series alone shows more than one thing changing: the population
# swings get bigger in the back half of the record (mean count roughly
# doubles, and the max goes from under 9000 to nearly 14700), not just slower.
# Worth flagging before either transform, since it previews what the CWT's
# color will show: rising power, not just a longer period.

blowflyCwt <- morlet(y1 = blowfly$Count, x1 = blowfly$Day, p2 = 8, dj = 0.1, siglvl = 0.99)
wavelet.plot(blowflyCwt, useRaster = NA, reverse.y = TRUE, crn.lab = "Count")

# Same helper the chapter builds for the switching demo and the chronology,
# reproduced here since this script doesn't source the .qmd.
plot_mra <- function(dwtOut, x, xlab, unit, title) {
  nLevels <- length(dwtOut)
  dwtScaled <- scale(as.data.frame(dwtOut))
  levelLabels <- colnames(dwtScaled)
  scaleLabels <- c(paste0(2^(1:(nLevels - 1)), " ", unit),
                    paste0("> ", 2^(nLevels - 1), " ", unit))

  par(mar = c(3, 2, 2, 2), mgp = c(1.25, 0.25, 0), tcl = 0.5, xaxs = "i", yaxs = "i")
  plot(x, rep(1, length(x)), type = "n", axes = FALSE, ylab = "", xlab = "",
       ylim = c(-3, 5 * nLevels))
  title(main = title, line = 0.75)
  axis(side = 1)
  mtext(xlab, side = 1, line = 1.25)

  offset <- 0
  for (i in nLevels:1) {
    lines(x, dwtScaled[, i] + offset)
    abline(h = offset, lty = "dashed")
    mtext(levelLabels[i], side = 2, at = offset, line = 0)
    mtext(scaleLabels[i], side = 4, at = offset, line = 0)
    offset <- offset + 5
  }
  box()
}

blowflyScales <- trunc(log(length(blowfly$Day)) / log(2)) - 1
blowflyDwt <- mra(blowfly$Count, wf = "la8", J = blowflyScales, method = "modwt", boundary = "periodic")
plot_mra(blowflyDwt, blowfly$Day, "Day", "days", "Multiresolution decomposition of the blowfly population")

mid <- length(blowfly$Day) %/% 2
tibble(level = names(blowflyDwt),
       early = map_dbl(blowflyDwt, ~ var(.x[1:mid])),
       late  = map_dbl(blowflyDwt, ~ var(.x[(mid + 1):length(blowfly$Day)]))) |>
  mutate(ratio = round(late / early, 2))
# The period does drift. Taking the CWT's dominant period at each time step
# (whichever period has the most power that day) and comparing the two halves
# of the record: a period around 19 days in the first half, stretching to
# around 25 days in the second. The MRA backs this up: D5 (the ~32-day detail
# level, the closest rung on this dyadic ladder to that stretch) carries about
# 8 times more variance in the late half than the early half, the largest
# jump of any level, and lines up with where the CWT plot's color shifts to a
# longer period. Nicholson varied the adult food ration over the course of
# the experiment; a food-limited generation cycle has a built-in feedback
# delay, so changing the food changes the delay, which changes the period.
# That's a real, physical reason for a real series to do exactly what the
# planted switch demo did on purpose.
#
# The amplitude story shows up at the fast end of the decomposition too: D1,
# the finest ~2-day wiggle, carries more than twice the variance in the late
# half. That tracks the raw plot, once the population is running at roughly
# twice its earlier level, its fastest day-to-day swings scale up right along
# with it. That rise is local to the fast levels, though; D4 and D6 actually
# carry less variance late than early, so a level shift alone doesn't explain
# D5's 8-fold jump. D5 is doing something the level shift by itself can't,
# which is the tell that the period story and the amplitude story are two
# separate findings, not one.


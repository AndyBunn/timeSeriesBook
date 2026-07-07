# Solutions for the Filtering and Smoothing chapter
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)
library(zoo)

# ---------------------------------------------------------------------------
# Exercise 1: Tune the window yourself
# ---------------------------------------------------------------------------
set.seed(872)
n <- 500
x <- 1:n
noise <- arima.sim(model = list(order = c(2, 0, 1), ar = c(0.7, -0.2), ma = -0.4), n = n)
truth <- 0.3 * sin(2 * pi / 100 * x) + 0.5 * sin(2 * pi / 200 * x)
y <- as.numeric(noise) + truth
rmse <- function(a, b) sqrt(mean((a - b)^2, na.rm = TRUE))

maWidths <- seq(5, 150, by = 5)
maRMSE <- map_dbl(maWidths, ~ rmse(c(stats::filter(y, rep(1 / .x, .x), sides = 2)), truth))

ggplot(tibble(width = maWidths, rmse = maRMSE), aes(width, rmse)) +
  geom_line() + geom_point() +
  geom_vline(xintercept = 100, linetype = "dashed", color = "grey50") +
  labs(x = "Moving-average width", y = "RMSE to truth",
    title = "RMSE bottoms out near 50, then worsens, with a visible kink at 100")

cat("best MA width:", maWidths[which.min(maRMSE)], "\n")
# Best width lands around 50. RMSE climbs steadily past that, but there's a
# kink right at 100: a 100-point moving average is a box filter with a null
# at frequency 1/100, so it erases the planted 100-period cycle outright
# (see filt-null-demo in the chapter). That's not overfitting noise, it's the
# filter deleting signal because its width happens to match a period in the
# data. Any moving-average width that divides evenly into 100 or 200 pays
# some version of this penalty.

loSpans <- seq(0.02, 0.6, by = 0.02)
loRMSE <- map_dbl(loSpans, ~ rmse(loess(y ~ x, span = .x)$fitted, truth))

ggplot(tibble(span = loSpans, rmse = loRMSE), aes(span, rmse)) +
  geom_line() + geom_point() +
  labs(x = "Loess span", y = "RMSE to truth",
    title = "Loess RMSE is a smoother bowl, no cancellation kink")

cat("best loess span:", loSpans[which.min(loRMSE)],
  "->", round(loSpans[which.min(loRMSE)] * n), "points\n")
# Loess has no equivalent cliff: it's a local weighted regression, not a flat
# box average, so there's no single width where it goes exactly deaf to a
# cycle. The bowl is shallower and the minimum less dramatic than the moving
# average's, which is itself a small case for preferring loess when you don't
# know your target period exactly.

# ---------------------------------------------------------------------------
# Exercise 2: Revisit the case of the poorly-launched sensors
# ---------------------------------------------------------------------------
tmp <- read_csv("data/tmp.csv", show_col_types = FALSE) |> mutate(DateTime = DateTime - 180)
rad <- read_csv("data/rad.csv", show_col_types = FALSE)

# Method 1: dplyr join onto the hourly radiation timestamps, then interpolate
# the now-sparse (every-other-hour) temperature onto every hour.
oneHourTmp <- right_join(tmp, rad, by = "DateTime") |>
  select(-rad) |>
  mutate(tmpInterp = na.spline(tmp))
head(oneHourTmp)
stopifnot(nrow(oneHourTmp) == nrow(rad), sum(is.na(oneHourTmp$tmpInterp)) == 0)

# Method 2: the same idea with zoo, which does the time-alignment for you.
tmpZoo <- zoo(tmp$tmp, tmp$DateTime)
radZoo <- zoo(rad$rad, rad$DateTime)
oneHourTmp2 <- merge.zoo(radZoo, tmpZoo)
oneHourTmp2$radZoo <- NULL
oneHourTmp2$tmpInterp <- na.spline(oneHourTmp2$tmpZoo)
head(oneHourTmp2)

# Both land in the same place: use the complete hourly radiation index as the
# target grid, then interpolate temperature (only ever every other hour) onto
# it. The dplyr route makes the join explicit; the zoo route makes the time
# alignment implicit but is less code. Either is a defensible answer as long
# as the student can say which timestamps came from where and why the
# interpolation is safe here (temperature doesn't jump abruptly hour to hour).

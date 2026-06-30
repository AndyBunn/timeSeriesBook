# Solutions for the Trend Detection chapter
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)

# Hand-rolled tools from the chapter -----------------------------------------
mann_kendall <- function(x) {
  n <- length(x); S <- 0
  for (j in 2:n) for (i in 1:(j - 1)) S <- S + sign(x[j] - x[i])
  ties <- table(x)
  tie_term <- sum(ties * (ties - 1) * (2 * ties + 5))
  varS <- (n * (n - 1) * (2 * n + 5) - tie_term) / 18
  Z <- (S - sign(S)) / sqrt(varS)
  list(S = S, varS = varS, Z = Z, tau = S / (n * (n - 1) / 2),
       p = 2 * (1 - pnorm(abs(Z))))
}
theil_sen <- function(x, t = seq_along(x)) {
  n <- length(x); s <- c()
  for (j in 2:n) for (i in 1:(j - 1)) s <- c(s, (x[j] - x[i]) / (t[j] - t[i]))
  median(s)
}
mann_kendall_ess <- function(x) {
  base <- mann_kendall(x)
  r1 <- acf(x, plot = FALSE)$acf[2]
  infl <- if (r1 > 0) (1 + r1) / (1 - r1) else 1
  Z <- (base$S - sign(base$S)) / sqrt(base$varS * infl)
  list(r1 = r1, p = 2 * (1 - pnorm(abs(Z))))
}

# ---------------------------------------------------------------------------
# Exercise 1: Is winter warming faster than summer?
# ---------------------------------------------------------------------------
kbli <- read_csv("data/kbli.csv", show_col_types = FALSE) |>
  mutate(tavg = (TMAX + TMIN) / 2, year = year(DATE), month = month(DATE))

full_years <- kbli |> drop_na(tavg) |> count(year) |> filter(n >= 350) |> pull(year)

seasons <- kbli |>
  filter(year %in% full_years) |>
  group_by(year) |>
  summarise(winter = mean(tavg[month %in% c(12, 1, 2)], na.rm = TRUE),
            summer = mean(tavg[month %in% 6:8], na.rm = TRUE),
            .groups = "drop")

for (s in c("winter", "summer")) {
  x <- seasons[[s]]
  mk <- mann_kendall(x); ess <- mann_kendall_ess(x)
  cat(sprintf("%-7s  TheilSen=%.3f C/yr  MK p=%.3f  r1=%.2f  ESS-corrected p=%.3f\n",
              s, theil_sen(x, seasons$year), mk$p, ess$r1, ess$p))
}
# Discussion (results on the current kbli record, ~2000-present):
#   winter: Theil-Sen ~0.01 C/yr, MK p ~0.66, r1 ~ -0.13  -> no trend.
#   summer: Theil-Sen ~0.08 C/yr, MK p ~0.004 -> looks like clear warming,
#           BUT r1 ~0.52 is strong, and the ESS correction pushes p to ~0.11,
#           no longer significant.
# So the naive answer ("summer is warming fast") does not survive an accounting
# of the autocorrelation on this short record. That is the whole lesson of the
# chapter landing in an exercise: estimate the rate robustly, test it, then ask
# what the dependence does to the test before believing the p-value. Here it
# flips the verdict; on the winter Nooksack in the chapter body it did not. You
# only know which case you are in by checking.

# ---------------------------------------------------------------------------
# Exercise 2: A trend that isn't there + the sample-size sweep
# ---------------------------------------------------------------------------
noo <- read_csv("data/nooksack.csv", show_col_types = FALSE) |>
  mutate(year = year(DATE), month = month(DATE))
noo_full <- noo |> count(year) |> filter(n >= 350) |> pull(year)
summer <- noo |> filter(year %in% noo_full) |>
  group_by(year) |>
  summarise(summer = mean(FLOW[month %in% 7:9]), .groups = "drop") |>
  pull(summer)

cat("\nSummer Nooksack: naive MK p =", round(mann_kendall(summer)$p, 3),
    " ESS-corrected p =", round(mann_kendall_ess(summer)$p, 3), "\n")
# It was never significant, and the correction (which only ever raises the
# p-value) cannot make a non-trend significant. A correction removes false
# positives; it does not manufacture power.

# Sample-size sweep at fixed phi = 0.7: does more data help?
ar1_series <- function(phi, n) {
  if (phi == 0) rnorm(n) else as.numeric(arima.sim(list(ar = phi), n = n))
}
set.seed(11)
sweep <- map_dfr(c(30, 60, 100, 150, 200), function(n) {
  naive <- mean(replicate(400, mann_kendall(ar1_series(0.7, n))$p < 0.05))
  corrected <- mean(replicate(400, mann_kendall_ess(ar1_series(0.7, n))$p < 0.05))
  tibble(n = n, naive = naive, corrected = corrected)
})
print(sweep)
# Lesson: the naive false-positive rate does NOT shrink toward 5% as n grows;
# more autocorrelated data just makes Mann-Kendall more confident in the
# spurious trend. The effective-sample-size correction holds the rate near 5%
# at every n. Quantity of data does not fix a dependence problem; accounting
# for the dependence does.

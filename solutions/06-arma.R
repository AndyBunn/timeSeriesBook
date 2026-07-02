# Worked solutions for ch 04, ARMA(p,q)
# Instructor answer key. Not part of the rendered book.
# Run from the project root so the data path resolves.

library(forecast)
library(tseries)

LynxHare <- readRDS("data/LynxHare.rds")
lynx <- LynxHare[, "Lynx"]

# ---------------------------------------------------------------------------
# Exercise 1: bunnies and kitties
# ---------------------------------------------------------------------------

# The series is a strong, regular cycle of roughly 9-10 years, the classic
# predator-prey oscillation. A pure cycle in the data points to an AR(2)
# with complex roots (an AR(2) is the simplest model that can oscillate), so
# the prediction before fitting should be: at least an AR(2), maybe an MA term
# on top from the lag in how the lynx tracks the hare.

ggAcf(lynx)    # damped sinusoid: ACF that swings positive-negative-positive
ggPacf(lynx)   # strong bars at lags 1 and 2, the AR(2) signature

# Fit a grid. Push p a little higher than 2 to confirm we don't need it.
bics <- c()
for (p in 0:4) for (q in 0:2) {
  bics[paste0(p, q)] <- BIC(Arima(lynx, order = c(p, 0, q)))
}
sort(bics)[1:4]
# Winner is ARMA(2,1); the pure AR(2) and AR(3) are right behind it.

fit <- Arima(lynx, order = c(2, 0, 1))
fit
# ar1 ~ 1.49, ar2 ~ -0.85. The negative second coefficient with a positive
# first is what makes an AR(2) oscillate: the roots are complex, and their
# argument works out to a period of about 9-10 years, which matches the cycle
# you see in the raw series. auto.arima(lynx, ic = "bic") lands on the same
# ARMA(2,1).

# Residual check: clean residuals mean the AR(2,1) caught the cycle.
checkresiduals(fit)

# Speculation worth crediting: the AR(2) memory is the population dynamics
# themselves. Lynx numbers depend on this year and last year because the
# predator's growth lags the prey it eats. The small MA term can be read as
# the noisy, delayed way the lynx tracks the hare it is chasing.


# ---------------------------------------------------------------------------
# Exercise 2: test before you difference
# ---------------------------------------------------------------------------

adf.test(lynx)    # p ~ 0.01: rejects a unit root
kpss.test(lynx)   # p ~ 0.10: does not reject stationarity
# Both point to stationary, so do NOT difference. A cycling series is not the
# same as a trending one: it keeps returning to its mean rather than wandering
# off, so it has no unit root. Mechanistically a unit root is implausible
# anyway. A predator population is bounded below by zero and held in check
# above by its food supply, so it cannot accumulate shocks without limit the
# way a random walk does. Fit ARMA to the series as it stands (Exercise 1).

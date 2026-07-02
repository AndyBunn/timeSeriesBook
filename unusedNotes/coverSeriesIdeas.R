set.seed(872)  # same seed convention used elsewhere in the book

n_years <- 20
n_month <- n_years * 12   # 240 monthly obs, matches the 1980-2000 x-axis
n_burn  <- 500

# short-lag AR(2) behavior plus an annual (lag-12) seasonal AR term,
# written as one length-12 AR polynomial (nonzero only at lags 1, 2, 12)
ar_coefs     <- rep(0, 12)
ar_coefs[1]  <- 0.6
ar_coefs[2]  <- -0.3
ar_coefs[12] <- 0.5

ar_process <- arima.sim(model = list(ar = ar_coefs), n = n_month, n.start = n_burn)
# plot(ar_process)
# ggAcf(ar_process)
# ggPacf(ar_process)
ar_process <- as.numeric(ar_process)

# ACF/PACF computed on the stationary AR process itself, NOT a trending
# series -- running acf() through a trend is the exact mistake the
# stationarity chapter warns against.
cover_acf  <- acf(ar_process,  lag.max = 19, plot = FALSE)$acf   # lag 0..19
cover_pacf <- pacf(ar_process, lag.max = 19, plot = FALSE)$acf   # lag 1..19

# top panel shows "observed = trend + stationary component," so a small
# linear drift is added only for display, after ACF/PACF are computed
drift        <- seq(0, 0.6, length.out = n_month)
cover_series <- as.numeric(scale(ar_process)) + drift
trend_line   <- fitted(lm(cover_series ~ seq_along(cover_series)))
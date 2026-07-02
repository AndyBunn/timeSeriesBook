# Solutions for the Reconstruction chapter ("Earning Trust")
# Instructor answer key. Not part of the book build (.R, ignored by Quarto).

library(tidyverse)
library(slider)

flow <- read_csv("data/colorado_flow.csv", show_col_types = FALSE)
instrumental <- flow |> filter(!is.na(ObsMAF))

skill <- function(obs, pred, calib_mean) {
  sse <- sum((obs - pred)^2)
  c(RE = 1 - sse / sum((obs - calib_mean)^2),
    CE = 1 - sse / sum((obs - mean(obs))^2))
}
validate <- function(cal, ver) {
  m <- lm(ObsMAF ~ Proxy, data = cal)
  c(calib_R2 = summary(m)$r.squared,
    skill(ver$ObsMAF, predict(m, ver), mean(cal$ObsMAF)))
}

# ---------------------------------------------------------------------------
# Exercise 1: Move the wall
# ---------------------------------------------------------------------------
for (wall in c(1930, 1955, 1980)) {
  early <- filter(instrumental, Year <= wall)
  late  <- filter(instrumental, Year >  wall)
  cat(sprintf("wall %d | early->late: ", wall)); print(round(validate(early, late), 3))
  cat(sprintf("wall %d | late->early: ", wall)); print(round(validate(late, early), 3))
}
# RE and CE stay positive at every wall, in both directions: the proxy-flow
# relationship is stable across the instrumental record, so the reconstruction
# does not hinge on where the split falls. That stability is part of what earns
# trust in reaching back to 762; a reconstruction whose skill collapsed when you
# moved the wall would be telling you the relationship is not stationary even
# within the gauged era, let alone before it.

# Harder test: calibrate on the middle, verify on both ends at once.
mid  <- filter(instrumental, Year >= 1935 & Year <= 1975)
ends <- filter(instrumental, Year < 1935 | Year > 1975)
cat("\nmiddle -> both ends: "); print(round(validate(mid, ends), 3))

# ---------------------------------------------------------------------------
# Exercise 2: Find the pluvials
# ---------------------------------------------------------------------------
final_model <- lm(ObsMAF ~ Proxy, data = instrumental)
flow <- flow |>
  mutate(recon = predict(final_model, flow),
         recon25 = slide_dbl(recon, mean, .before = 12, .after = 12, .complete = TRUE))

inst_mean <- mean(instrumental$ObsMAF)
wettest <- flow |> filter(!is.na(recon25)) |> slice_max(recon25, n = 1)
cat(sprintf("\nWettest 25-yr window centered on %d: %.1f MAF (%d%% of the instrumental mean)\n",
            wettest$Year, wettest$recon25, round(100 * wettest$recon25 / inst_mean)))

compact <- mean(flow$recon[flow$Year %in% 1912:1921])
cat(sprintf("Compact decade 1912-1921: %.1f MAF (%d%% of instrumental mean)\n",
            compact, round(100 * compact / inst_mean)))
# Takeaway sentence for a water manager: the decade the Colorado was divided on
# (1912-1921, ~117% of the long-term mean) was among the wettest stretches in
# twelve centuries, so the compact's allocations were set against a flow the
# river has almost never sustained, and never for long.

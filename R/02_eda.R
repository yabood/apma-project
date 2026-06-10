#!/usr/bin/env Rscript
# Baseline EDA for Story A: what predicts a fatal crash?
# Confirm outcome variable, year coverage, and fatality rates by candidate predictors.

suppressPackageStartupMessages({
  library(data.table)
})

csv <- file.path("data", "raw", "crash_table.csv")
dt  <- fread(csv, showProgress = FALSE)

# ----- outcome -----
cat("== CrashType levels ==\n")
print(dt[, .N, by = CrashType][order(-N)])

cat("\n== Year coverage ==\n")
print(dt[, .N, by = Year][order(Year)])

cat("\n== Number_Of_Fatalities distribution ==\n")
print(dt[, .N, by = Number_Of_Fatalities][order(Number_Of_Fatalities)])

# Define binary outcome
dt[, fatal := as.integer(Number_Of_Fatalities > 0)]

cat(sprintf("\nOverall fatality rate: %.3f%% (%d / %d crashes had >= 1 fatality)\n",
            100 * mean(dt$fatal), sum(dt$fatal), nrow(dt)))

# ----- fatality rate by predictors -----
rate_by <- function(col) {
  out <- dt[, .(n = .N, fatal = sum(fatal),
                rate_pct = round(100 * mean(fatal), 3)),
            by = col][order(-rate_pct)]
  out
}

predictors <- c(
  "AlcoholInvolved", "SpeedRelated", "DistractedDriverInvolved",
  "DriverTextingInvolved", "DriverUsingCellPhone",
  "TeenDriverInvolved", "MatureDriverInvolved",
  "PedestrianInvolved", "BicycleInvolved", "MotorcycleInvolved",
  "WeatherCondition", "LightCondition_Category",
  "RoadwaySurfaceCondition", "RoadwayDescription", "RoadwayAlignment",
  "LocationTypeName", "Interstate_NonInterstate",
  "DaytimeNighttime_NHTSA", "Meteorological Season",
  "TypeOfCollision", "Single_Multi_Vehicle"
)

for (p in predictors) {
  cat(sprintf("\n== fatality rate by %s ==\n", p))
  print(rate_by(p), nrows = 30)
}

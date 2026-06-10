#!/usr/bin/env Rscript
# Tree models for comparison with the logistic regression.
#
# Two models:
#   1. CART (rpart) with a loss matrix to compensate for the 0.7% positive rate.
#      Predict-positive carries a large cost when wrong; this prevents the tree
#      from collapsing to "predict every crash as non-fatal".
#   2. Random forest on a balanced downsample for variable-importance ranking.
#
# Outputs:
#   data/derived/tree_importance.csv  - rpart variable importance
#   data/derived/tree_rules.txt       - readable rule listing
#   data/derived/rf_importance.csv    - RF variable importance (MeanDecreaseGini)

suppressPackageStartupMessages({
  library(data.table)
  library(rpart)
  library(randomForest)
})

set.seed(20260610)

csv <- file.path("data", "raw", "crash_table.csv")
dt  <- fread(csv, showProgress = FALSE)

derived_dir <- file.path("data", "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- prep (mirrors 04_logistic.R) ----------

dt[, fatal := factor(ifelse(Number_Of_Fatalities > 0, "Fatal", "NonFatal"),
                     levels = c("NonFatal", "Fatal"))]
drop_blanks <- function(x) ifelse(x == "" | x == "Not Provided" | x == "Unknown", NA, x)

bin_yes_no <- c("AlcoholInvolved", "SpeedRelated",
                "DistractedDriverInvolved",
                "TeenDriverInvolved", "MatureDriverInvolved",
                "PedestrianInvolved", "BicycleInvolved", "MotorcycleInvolved")
multi_lvl  <- c("Single_Multi_Vehicle", "DaytimeNighttime_NHTSA",
                "LocationTypeName", "Interstate_NonInterstate",
                "LightCondition_Category", "RoadwayAlignment",
                "WeatherCondition", "RoadwayDescription")

for (col in c(bin_yes_no, multi_lvl)) dt[, (col) := drop_blanks(get(col))]
m_dt <- dt[, c("fatal", bin_yes_no, multi_lvl), with = FALSE]
m_dt <- m_dt[complete.cases(m_dt)]
for (col in c(bin_yes_no, multi_lvl)) m_dt[, (col) := as.factor(get(col))]

cat(sprintf("Rows used: %d | fatal: %d (%.2f%%)\n",
            nrow(m_dt), sum(m_dt$fatal == "Fatal"),
            100 * mean(m_dt$fatal == "Fatal")))

# ---------- 1. CART with loss matrix ----------
# Penalize a missed fatal at ~ratio of class imbalance so tree splits at all.
loss_ratio <- floor(mean(m_dt$fatal == "NonFatal") / mean(m_dt$fatal == "Fatal"))
loss_mx <- matrix(c(0, 1, loss_ratio, 0), nrow = 2, byrow = TRUE,
                  dimnames = list(c("NonFatal", "Fatal"), c("NonFatal", "Fatal")))
cat(sprintf("\nUsing loss ratio: missing a fatal = %d * extra-flag\n", loss_ratio))

tree <- rpart(fatal ~ .,
              data    = m_dt,
              method  = "class",
              parms   = list(loss = loss_mx),
              control = rpart.control(cp = 0.001, maxdepth = 5, minbucket = 200))

cat("\n== rpart tree ==\n")
print(tree)

cat("\n== variable importance (rpart) ==\n")
imp_dt <- data.table(variable = names(tree$variable.importance),
                     importance = unname(tree$variable.importance))
setorder(imp_dt, -importance)
print(imp_dt)
fwrite(imp_dt, file.path(derived_dir, "tree_importance.csv"))

# Readable rule listing
sink(file.path(derived_dir, "tree_rules.txt"))
print(tree); cat("\n\n"); print(summary(tree))
sink()

saveRDS(tree, file.path(derived_dir, "tree_fit.rds"))

# ---------- 2. Random forest on balanced downsample ----------
# Take all fatals + same number of non-fatals -> balanced ~ 2 * n_fatal rows
n_fatal <- sum(m_dt$fatal == "Fatal")
rf_dt <- rbind(
  m_dt[fatal == "Fatal"],
  m_dt[fatal == "NonFatal"][sample(.N, n_fatal)]
)
cat(sprintf("\nRF training rows (balanced): %d\n", nrow(rf_dt)))

rf <- randomForest(fatal ~ ., data = rf_dt, ntree = 200, importance = TRUE)
cat("\n== RF confusion (OOB) ==\n"); print(rf$confusion)

imp_rf <- as.data.table(importance(rf), keep.rownames = "variable")
setorder(imp_rf, -MeanDecreaseGini)
cat("\n== RF importance ==\n"); print(imp_rf)
fwrite(imp_rf, file.path(derived_dir, "rf_importance.csv"))

saveRDS(rf, file.path(derived_dir, "rf_fit.rds"))
cat("\nDone.\n")

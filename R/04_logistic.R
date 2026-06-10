#!/usr/bin/env Rscript
# Logistic regression: P(fatal crash) ~ risk factors + context.
#
# Predictors are chosen from the significant ones in 03_hypothesis_tests.R.
# `TypeOfCollision` is intentionally excluded because its rare categories
# (Pedestrian, Bicyclist, Motorcyclist, Train) almost perfectly proxy the
# binary involvement flags and would cause separation / interpretation issues.
#
# Outputs:
#   data/derived/logistic_coefs.csv   - terms with OR + 95% CI + p
#   data/derived/logistic_metrics.csv - AUC, accuracy, calibration
#   data/derived/logistic_vif.csv     - multicollinearity diagnostics

suppressPackageStartupMessages({
  library(data.table)
  library(car)
  library(broom)
})

set.seed(20260610)

csv <- file.path("data", "raw", "crash_table.csv")
dt  <- fread(csv, showProgress = FALSE)

derived_dir <- file.path("data", "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- outcome + predictor prep ----------

dt[, fatal := as.integer(Number_Of_Fatalities > 0)]

# Drop very sparse / "Not Provided" rows on the predictors we'll use.
drop_blanks <- function(x) ifelse(x == "" | x == "Not Provided" | x == "Unknown", NA, x)

bin_yes_no <- c("AlcoholInvolved", "SpeedRelated",
                "DistractedDriverInvolved",
                "TeenDriverInvolved", "MatureDriverInvolved",
                "PedestrianInvolved", "BicycleInvolved", "MotorcycleInvolved")

multi_lvl <- c("Single_Multi_Vehicle", "DaytimeNighttime_NHTSA",
               "LocationTypeName", "Interstate_NonInterstate",
               "LightCondition_Category", "RoadwayAlignment",
               "WeatherCondition", "RoadwayDescription")

for (col in c(bin_yes_no, multi_lvl)) dt[, (col) := drop_blanks(get(col))]

model_cols <- c("fatal", bin_yes_no, multi_lvl)
m_dt <- dt[, ..model_cols]
m_dt <- m_dt[complete.cases(m_dt)]

# Set reference levels
m_dt[, (bin_yes_no) := lapply(.SD, function(x) factor(x, levels = c("No", "Yes"))),
     .SDcols = bin_yes_no]
m_dt[, Single_Multi_Vehicle      := factor(Single_Multi_Vehicle,
                                            levels = c("Multiple Vehicle Crashes",
                                                       "Single Vehicle Crashes"))]
m_dt[, DaytimeNighttime_NHTSA    := factor(DaytimeNighttime_NHTSA,
                                            levels = c("Daytime", "Nighttime"))]
m_dt[, LocationTypeName          := factor(LocationTypeName,
                                            levels = c("Urban", "Rural"))]
m_dt[, Interstate_NonInterstate  := factor(Interstate_NonInterstate,
                                            levels = c("Non-Interstate", "Interstate"))]
m_dt[, LightCondition_Category   := relevel(factor(LightCondition_Category), ref = "Daylight")]
m_dt[, RoadwayAlignment          := relevel(factor(RoadwayAlignment),       ref = "Straight - Level")]
m_dt[, WeatherCondition          := relevel(factor(WeatherCondition),       ref = "No Adverse Condition (Clear/Cloud)")]
m_dt[, RoadwayDescription        := relevel(factor(RoadwayDescription),     ref = "Two-Way, Not Divided")]

cat(sprintf("Rows used: %d (of %d) | fatal: %d (%.2f%%)\n",
            nrow(m_dt), nrow(dt), sum(m_dt$fatal), 100 * mean(m_dt$fatal)))

# ---------- train / test split ----------

n   <- nrow(m_dt)
idx <- sample.int(n, size = floor(0.8 * n))
train <- m_dt[idx]
test  <- m_dt[-idx]

# ---------- fit ----------

fit <- glm(fatal ~ ., data = train, family = binomial())

cat("\n== model summary ==\n")
print(summary(fit))

# ---------- VIF (GVIF for factors) ----------

vif_tab <- tryCatch(as.data.frame(car::vif(fit)), error = function(e) NULL)
if (!is.null(vif_tab)) {
  vif_tab$term <- rownames(vif_tab)
  cat("\n== VIF / GVIF ==\n"); print(vif_tab)
  fwrite(vif_tab, file.path(derived_dir, "logistic_vif.csv"))
}

# ---------- coefficient table with ORs ----------

tidy_fit <- broom::tidy(fit, conf.int = TRUE)
setDT(tidy_fit)
tidy_fit[, `:=`(
  or       = exp(estimate),
  or_lo    = exp(conf.low),
  or_hi    = exp(conf.high)
)]
setorder(tidy_fit, p.value)
print(tidy_fit[, .(term, or = round(or, 3), or_lo = round(or_lo, 3),
                   or_hi = round(or_hi, 3), p = signif(p.value, 3))], nrows = 100)
fwrite(tidy_fit, file.path(derived_dir, "logistic_coefs.csv"))

# ---------- evaluation ----------

# Simple trapezoidal AUC
auc <- function(scores, y) {
  ord <- order(scores, decreasing = TRUE)
  y <- y[ord]
  n_pos <- sum(y); n_neg <- sum(!y)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  cum_tpr <- cumsum(y)  / n_pos
  cum_fpr <- cumsum(!y) / n_neg
  tpr <- c(0, cum_tpr); fpr <- c(0, cum_fpr)
  sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
}

pred_train <- predict(fit, train, type = "response")
pred_test  <- predict(fit, test,  type = "response")

auc_train <- auc(pred_train, train$fatal)
auc_test  <- auc(pred_test,  test$fatal)

threshold <- 0.5
cm <- table(predicted = as.integer(pred_test > threshold), actual = test$fatal)
cat(sprintf("\nAUC train: %.4f | AUC test: %.4f\n", auc_train, auc_test))
cat("\nConfusion matrix @ 0.5 (rows=pred, cols=actual):\n"); print(cm)

# More informative threshold: classify positive if pred > prevalence
prev <- mean(train$fatal)
cm2 <- table(predicted = as.integer(pred_test > prev), actual = test$fatal)
cat(sprintf("\nConfusion matrix @ prevalence (%.4f):\n", prev)); print(cm2)

metrics <- data.table(
  metric = c("n_train", "n_test", "prevalence_train",
             "auc_train", "auc_test"),
  value  = c(nrow(train), nrow(test), round(prev, 5),
             round(auc_train, 4), round(auc_test, 4))
)
fwrite(metrics, file.path(derived_dir, "logistic_metrics.csv"))

# Save fitted model for reuse in the report
saveRDS(fit, file.path(derived_dir, "logistic_fit.rds"))
cat("\nWrote logistic_coefs.csv, logistic_metrics.csv, logistic_vif.csv, logistic_fit.rds\n")

#!/usr/bin/env Rscript
# Generate report figures from derived results.
# Reads:  data/derived/*.csv, data/derived/*.rds
# Writes: figures/*.png  (300 dpi, ~6x4 in)
#
# Figures:
#   fig01_fatality_rate_by_factor.png  unadjusted fatality rates for binary risk factors
#   fig02_forest_adjusted_ORs.png      adjusted logistic ORs with 95% CI
#   fig03_unadjusted_vs_adjusted.png   side-by-side OR comparison
#   fig04_cart_tree.png                rpart tree
#   fig05_rf_importance.png            random forest variable importance

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(rpart)
})

derived_dir <- file.path("data", "derived")
fig_dir     <- "figures"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

theme_set(theme_bw(base_size = 11))

save_png <- function(p, name, w = 6, h = 4) {
  ggsave(file.path(fig_dir, name), p, width = w, height = h, dpi = 300)
}

# ---------- Figure 1: unadjusted fatality rates ----------
ht <- fread(file.path(derived_dir, "hypothesis_tests.csv"))
binary_factors <- ht[!is.na(or) & predictor %in% c(
  "PedestrianInvolved", "MotorcycleInvolved", "AlcoholInvolved",
  "BicycleInvolved", "SpeedRelated", "Single_Multi_Vehicle",
  "DaytimeNighttime_NHTSA", "MatureDriverInvolved",
  "TeenDriverInvolved", "DistractedDriverInvolved",
  "Interstate_NonInterstate"
)]
binary_factors[, label := factor(predictor,
                                 levels = predictor[order(or)])]

p1 <- ggplot(binary_factors, aes(x = label, y = or)) +
  geom_col(fill = "#3b6db5") +
  geom_errorbar(aes(ymin = or_ci_lo, ymax = or_ci_hi), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_y_log10() +
  coord_flip() +
  labs(x = NULL,
       y = "Unadjusted odds ratio of fatal crash (log scale, ref = factor absent)",
       title = "Unadjusted fatality odds ratios by risk factor",
       subtitle = "Error bars: 95% CI; dashed line: OR = 1") +
  theme(plot.title.position = "plot")
save_png(p1, "fig01_fatality_rate_by_factor.png", w = 7, h = 4.5)

# ---------- Figure 2: adjusted ORs (logistic) ----------
co <- fread(file.path(derived_dir, "logistic_coefs.csv"))
co <- co[term != "(Intercept)"]
# Keep meaningful terms (drop tiny-cell weather categories with crazy CIs)
co_keep <- co[!grepl("Smoke/Dust|Blowing Sand", term)]
# Friendlier labels
clean_label <- function(x) {
  x <- sub("Yes$", "", x)
  x <- gsub("Involved", " involved", x)
  x <- gsub("_", " ", x)
  x <- gsub("NHTSA", "", x)
  x <- gsub("Single Vehicle Crashes", " (vs. multi-vehicle)", x, fixed = TRUE)
  x <- gsub("Single Multi Vehicle", "Single vehicle", x)
  x <- gsub("LocationTypeName", "", x)
  x <- gsub("Interstate Non Interstate", "", x)
  x <- gsub("Daytime ", "", x)
  x <- gsub("LightCondition Category", "Light: ", x)
  x <- gsub("RoadwayAlignment", "Alignment: ", x)
  x <- gsub("WeatherCondition", "Weather: ", x)
  x <- gsub("RoadwayDescription", "Roadway: ", x)
  trimws(x)
}
co_keep[, label := clean_label(term)]
co_keep[, sig := p.value < 0.05]
setorder(co_keep, or)
co_keep[, label := factor(label, levels = label)]

p2 <- ggplot(co_keep, aes(x = label, y = or, color = sig)) +
  geom_pointrange(aes(ymin = or_lo, ymax = or_hi)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_y_log10() +
  scale_color_manual(values = c(`TRUE` = "#1f4e79", `FALSE` = "grey60"),
                     labels = c("p >= 0.05", "p < 0.05")) +
  coord_flip() +
  labs(x = NULL,
       y = "Adjusted odds ratio of fatal crash (log scale)",
       color = NULL,
       title = "Adjusted odds ratios from multivariable logistic regression",
       subtitle = "Reference levels: 'No'/'Daytime'/'Urban'/'Non-Interstate'/'Daylight'/'Straight-Level'/'Clear'/'Two-Way Not Divided'") +
  theme(plot.title.position = "plot")
save_png(p2, "fig02_forest_adjusted_ORs.png", w = 8, h = 7)

# ---------- Figure 3: unadjusted vs adjusted ORs ----------
# Map each binary predictor's unadjusted OR vs. the corresponding adjusted OR.
# The adjusted term name is e.g. "AlcoholInvolvedYes".
adj <- co[, .(term, or, or_lo, or_hi)]
adj[, predictor := sub("Yes$", "", term)]
adj <- adj[predictor %in% binary_factors$predictor]
unadj <- binary_factors[, .(predictor, unadj_or = or, unadj_lo = or_ci_lo, unadj_hi = or_ci_hi)]
both  <- merge(adj, unadj, by = "predictor")
setnames(both, "or", "adj_or"); setnames(both, "or_lo", "adj_lo"); setnames(both, "or_hi", "adj_hi")

both_long <- rbind(
  both[, .(predictor, model = "Unadjusted", or = unadj_or, lo = unadj_lo, hi = unadj_hi)],
  both[, .(predictor, model = "Adjusted",   or = adj_or,   lo = adj_lo,   hi = adj_hi)]
)
both_long[, predictor := factor(predictor, levels = both[order(adj_or), predictor])]

p3 <- ggplot(both_long, aes(x = predictor, y = or, color = model)) +
  geom_pointrange(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_y_log10() +
  scale_color_manual(values = c(Unadjusted = "#b5b5b5", Adjusted = "#1f4e79")) +
  coord_flip() +
  labs(x = NULL, y = "Odds ratio of fatal crash (log scale)", color = NULL,
       title = "Adjustment changes the story",
       subtitle = "Some risk factors get stronger (Pedestrian, Bicycle, Mature driver) while others wash out (Nighttime, Interstate)") +
  theme(plot.title.position = "plot")
save_png(p3, "fig03_unadjusted_vs_adjusted.png", w = 8, h = 5)

# ---------- Figure 4: CART tree ----------
tree <- readRDS(file.path(derived_dir, "tree_fit.rds"))
png(file.path(fig_dir, "fig04_cart_tree.png"), width = 1800, height = 1200, res = 200)
par(mar = c(1, 1, 2, 1))
plot(tree, uniform = TRUE, branch = 0.4, margin = 0.1,
     main = "CART decision tree for fatal crash classification")
text(tree, use.n = TRUE, all = TRUE, cex = 0.6)
dev.off()

# ---------- Figure 5: RF importance ----------
rf_imp <- fread(file.path(derived_dir, "rf_importance.csv"))
setorder(rf_imp, MeanDecreaseGini)
rf_imp[, variable := factor(variable, levels = variable)]

p5 <- ggplot(rf_imp, aes(x = variable, y = MeanDecreaseGini)) +
  geom_col(fill = "#3b6db5") +
  coord_flip() +
  labs(x = NULL, y = "Mean decrease in Gini",
       title = "Random forest variable importance",
       subtitle = "Trained on a balanced sample (n = 5,228; 50/50 fatal/non-fatal)") +
  theme(plot.title.position = "plot")
save_png(p5, "fig05_rf_importance.png", w = 7, h = 5)

cat(sprintf("Wrote %d figures to %s/\n",
            length(list.files(fig_dir, pattern = "^fig.*png$")), fig_dir))

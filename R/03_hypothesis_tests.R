#!/usr/bin/env Rscript
# Hypothesis tests on key predictors of crash fatality.
#
# Approach:
#   - Outcome: binary `fatal` = Number_Of_Fatalities >= 1
#   - For 2-level predictors: chi-square test on 2x2 table + odds ratio with 95% CI
#   - For k-level predictors:  chi-square test of independence + Cramer's V
#   - Holm-Bonferroni adjustment for the family of tests
#
# Output: data/derived/hypothesis_tests.csv

suppressPackageStartupMessages({
  library(data.table)
})

csv <- file.path("data", "raw", "crash_table.csv")
dt  <- fread(csv, showProgress = FALSE)
dt[, fatal := as.integer(Number_Of_Fatalities > 0)]

derived_dir <- file.path("data", "derived")
dir.create(derived_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- helpers ----------

# Odds ratio for a 2x2 table where:
#   rows    = exposure (row 1 = exposed, row 2 = unexposed reference)
#   columns = outcome  (col "0" = non-fatal, col "1" = fatal)
# Haldane-Anscombe 0.5 correction for zero cells. 95% Wald CI on log scale.
odds_ratio <- function(tbl) {
  stopifnot(all(dim(tbl) == c(2, 2)))
  # exposed:    not_fatal = a, fatal = b
  # unexposed:  not_fatal = c, fatal = d
  a <- tbl[1, "0"]; b <- tbl[1, "1"]
  c <- tbl[2, "0"]; d <- tbl[2, "1"]
  if (any(c(a, b, c, d) == 0)) { a <- a + 0.5; b <- b + 0.5; c <- c + 0.5; d <- d + 0.5 }
  # OR = odds(fatal | exposed) / odds(fatal | unexposed) = (b/a) / (d/c)
  or  <- (b * c) / (a * d)
  se  <- sqrt(1/a + 1/b + 1/c + 1/d)
  ll  <- exp(log(or) - 1.96 * se)
  ul  <- exp(log(or) + 1.96 * se)
  list(or = or, ci_lo = ll, ci_hi = ul)
}

cramers_v <- function(chi2, n, tbl) {
  k <- min(nrow(tbl), ncol(tbl))
  sqrt(chi2 / (n * (k - 1)))
}

run_test <- function(predictor) {
  # Reference level for binary predictors: the "No" level if present, else first level.
  v <- dt[[predictor]]
  keep <- !is.na(v) & v != ""
  v <- v[keep]
  y <- dt$fatal[keep]
  if (uniqueN(v) < 2) return(NULL)

  tbl <- table(v, y)  # rows = predictor levels, cols = 0/1 fatal
  # ensure both fatality columns exist
  if (ncol(tbl) < 2) return(NULL)

  ct <- suppressWarnings(chisq.test(tbl))
  out <- data.table(
    predictor   = predictor,
    n_levels    = nrow(tbl),
    n_total     = sum(tbl),
    n_fatal     = sum(tbl[, "1"]),
    chi2        = unname(ct$statistic),
    df          = unname(ct$parameter),
    p_value     = ct$p.value,
    cramers_v   = cramers_v(ct$statistic, sum(tbl), tbl),
    or          = NA_real_,
    or_ci_lo    = NA_real_,
    or_ci_hi    = NA_real_,
    reference   = NA_character_,
    exposed     = NA_character_
  )

  # For binary Yes/No predictors, compute OR with No as reference.
  lvls <- rownames(tbl)
  if (nrow(tbl) == 2) {
    if (all(c("Yes", "No") %in% lvls)) {
      tbl2 <- tbl[c("Yes", "No"), , drop = FALSE]  # exposed first
      o <- odds_ratio(tbl2)
      out$or       <- o$or
      out$or_ci_lo <- o$ci_lo
      out$or_ci_hi <- o$ci_hi
      out$reference <- "No"
      out$exposed   <- "Yes"
    } else {
      # Pick larger level as reference
      ref <- lvls[which.max(rowSums(tbl))]
      exp <- setdiff(lvls, ref)
      tbl2 <- tbl[c(exp, ref), , drop = FALSE]
      o <- odds_ratio(tbl2)
      out$or       <- o$or
      out$or_ci_lo <- o$ci_lo
      out$or_ci_hi <- o$ci_hi
      out$reference <- ref
      out$exposed   <- exp
    }
  }

  out
}

predictors <- c(
  # primary risk factors
  "AlcoholInvolved", "SpeedRelated",
  "DistractedDriverInvolved", "DriverTextingInvolved", "DriverUsingCellPhone",
  "TeenDriverInvolved", "MatureDriverInvolved",
  # vulnerable users
  "PedestrianInvolved", "BicycleInvolved", "MotorcycleInvolved",
  # environment / context
  "WeatherCondition", "LightCondition_Category",
  "RoadwaySurfaceCondition", "RoadwayDescription", "RoadwayAlignment",
  "LocationTypeName", "Interstate_NonInterstate",
  "DaytimeNighttime_NHTSA", "Meteorological Season",
  # collision dynamics
  "TypeOfCollision", "Single_Multi_Vehicle"
)

results <- rbindlist(lapply(predictors, run_test), use.names = TRUE, fill = TRUE)
results[, p_adj_holm := p.adjust(p_value, method = "holm")]
results[, significant_005 := p_adj_holm < 0.05]

# Order by effect size — Cramer's V for multi-level, |log OR| for binary
results[, effect := ifelse(is.na(or), cramers_v, abs(log(or)))]
setorder(results, -effect)

# print clean version
print_cols <- c("predictor", "n_levels", "n_fatal", "chi2", "df",
                "p_value", "p_adj_holm", "cramers_v", "or", "or_ci_lo", "or_ci_hi",
                "exposed", "reference", "significant_005")

cat("== hypothesis tests (sorted by effect size) ==\n")
print(results[, ..print_cols], digits = 4)

out_path <- file.path(derived_dir, "hypothesis_tests.csv")
fwrite(results, out_path)
cat(sprintf("\nWrote %s\n", out_path))

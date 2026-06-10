#!/usr/bin/env Rscript
# Quick schema + basic-stats look at the crash dataset.
# Uses data.table for speed on the 235 MB CSV.

suppressPackageStartupMessages({
  library(data.table)
})

csv <- file.path("data", "raw", "crash_table.csv")
stopifnot(file.exists(csv))

dt <- fread(csv, showProgress = FALSE)

cat("rows:", nrow(dt), " cols:", ncol(dt), "\n\n")

cat("== columns ==\n")
col_info <- data.table(
  name    = names(dt),
  type    = sapply(dt, function(x) class(x)[1]),
  n_uniq  = sapply(dt, function(x) uniqueN(x)),
  n_na    = sapply(dt, function(x) sum(is.na(x))),
  example = sapply(dt, function(x) {
    v <- x[!is.na(x)][1]
    if (is.null(v) || length(v) == 0) "" else as.character(v)
  })
)
print(col_info, nrows = ncol(dt))

cat("\n== first 3 rows ==\n")
print(head(dt, 3))

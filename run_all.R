#!/usr/bin/env Rscript
# Reproduce the entire analysis end-to-end.
#
# Usage from the repo root:
#   Rscript run_all.R
#
# Steps:
#   1. install missing packages (asks before installing)
#   2. download the dataset                R/00_download.R
#   3. inspect schema (optional, fast)     R/01_inspect.R
#   4. EDA tables                          R/02_eda.R
#   5. hypothesis tests                    R/03_hypothesis_tests.R
#   6. logistic regression                 R/04_logistic.R
#   7. CART + random forest                R/05_tree.R
#   8. figures                             R/06_figures.R
#   9. (optional) render report            rmarkdown::render("report/report.Rmd")
#
# Step 9 needs a working LaTeX engine. The easiest way is:
#   install.packages("tinytex"); tinytex::install_tinytex()

required <- c("data.table", "ggplot2", "rpart", "randomForest",
              "car", "broom", "rmarkdown", "knitr")

missing <- setdiff(required, rownames(installed.packages()))
if (length(missing)) {
  message("Missing packages: ", paste(missing, collapse = ", "))
  ans <- readline("Install now from CRAN? [y/N]: ")
  if (tolower(trimws(ans)) %in% c("y", "yes")) {
    install.packages(missing, repos = "https://cloud.r-project.org")
  } else {
    stop("Aborting: please install the missing packages and rerun.")
  }
}

scripts <- c(
  "R/00_download.R",
  "R/02_eda.R",
  "R/03_hypothesis_tests.R",
  "R/04_logistic.R",
  "R/05_tree.R",
  "R/06_figures.R"
)

for (s in scripts) {
  message("\n========================================")
  message("== Running: ", s)
  message("========================================")
  t0 <- Sys.time()
  source(s, echo = FALSE)
  message(sprintf("== Done in %.1fs", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

# Optional render — only if a LaTeX engine is available.
rmd <- "report/report.Rmd"
if (file.exists(rmd) && requireNamespace("rmarkdown", quietly = TRUE)) {
  has_tex <- nchar(Sys.which("pdflatex")) > 0 ||
             (requireNamespace("tinytex", quietly = TRUE) && tinytex::is_tinytex())
  if (has_tex) {
    message("\n== Rendering report ==")
    rmarkdown::render(rmd, quiet = TRUE)
  } else {
    message("\nSkipping report render: no LaTeX engine found.")
    message("To enable: install.packages('tinytex'); tinytex::install_tinytex()")
  }
}

message("\nPipeline complete.")

#!/usr/bin/env Rscript
# Download the Virginia DMV crash dataset and accompanying data dictionary
# from the Virginia Open Data Portal. Idempotent: skips files already present.
#
# Source: https://data.virginia.gov/dataset/crash-data-virginia-department-of-motor-vehicles

raw_dir <- file.path("data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

resources <- list(
  list(
    name = "crash_table.csv",
    url  = "https://data.virginia.gov/dataset/228bccd9-5212-46fa-8d5e-d5840b80cc9d/resource/af75bb2f-bcc9-48d0-8b51-76af391a2980/download/1-crash_table-5.csv"
  ),
  list(
    name = "data_dictionary.docx",
    url  = "https://data.virginia.gov/dataset/228bccd9-5212-46fa-8d5e-d5840b80cc9d/resource/23e9cf4e-d0b3-489a-8422-aa19238314bb/download/6-va-highway-safety-2025-datathon-data-dictionary.docx"
  )
)

for (r in resources) {
  dest <- file.path(raw_dir, r$name)
  if (file.exists(dest)) {
    message(sprintf("[skip] %s already present", r$name))
    next
  }
  message(sprintf("[get]  %s", r$name))
  utils::download.file(r$url, dest, mode = "wb", quiet = FALSE)
}

message("Done.")

# Write the capture-history frame to derived_data/capture_history/ as CSV
# (inspection) and RDS (direct R/CMR-package use).
#
# Deliberately separate from export_with_metadata() -- that function's
# hardcoded OVERVIEW text ("Each row represents one survey night at one
# site") describes survey-level data (traps/rapid/observations) and would be
# wrong for this individual-capture-level table.
#
# Returns a character vector of the created file paths (for use with
# format = "file" in a tar_target()).
export_capture_history <- function(data, output_dir = "derived_data/capture_history") {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  csv_path <- file.path(output_dir, "capture_history.csv")
  rds_path <- file.path(output_dir, "capture_history.rds")

  write.csv(data, csv_path, row.names = FALSE)
  saveRDS(data, rds_path)

  message("Saved: ", csv_path)
  message("Saved: ", rds_path)

  c(csv_path, rds_path)
}

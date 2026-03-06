verify_and_render_rapid_assessment <- function(csv_path) {
  # Verify and render rapid assessment data quality report
  #
  # This function renders a Quarto document that validates rapid assessment data
  # and produces an HTML report. The report is only generated if all validation
  # checks pass. The output is saved in the same directory as the input CSV.
  #
  # @param csv_path Character; path to the CSV file containing rapid assessment data
  # @return Path to the rendered HTML file (invisibly), or stops with error if validation fails

  library(quarto)

  # Ensure CSV file exists
  if (!file.exists(csv_path)) {
    stop("CSV file not found: ", csv_path)
  }

  # Get absolute paths
  csv_path_abs <- normalizePath(csv_path, mustWork = TRUE)
  csv_dir_abs <- dirname(csv_path_abs)

  # Get directory and base name for output
  csv_basename <- tools::file_path_sans_ext(basename(csv_path_abs))
  output_file <- paste0(csv_basename, "_verification_report.html")
  output_path <- file.path(csv_dir_abs, output_file)

  # Path to the Quarto document
  qmd_path <- "quarto_reports/verify_new_rapid_assessment_data.qmd"

  if (!file.exists(qmd_path)) {
    stop("Quarto template not found: ", qmd_path)
  }

  # Create a copy of the .qmd in the target directory with the correct path
  target_qmd <- file.path(csv_dir_abs, ".temp_verification.qmd")
  qmd_content <- readLines(qmd_path)

  # Replace the path in the setup chunk with just the basename (since qmd will be in same dir)
  qmd_content <- gsub(
    'path <- "raw_data/survey_data/rapid_assessment_data_2026_onwards/adelaide_plains/19_01_2026.csv"',
    paste0('path <- "', basename(csv_path_abs), '"'),
    qmd_content,
    fixed = TRUE
  )

  writeLines(qmd_content, target_qmd)

  # Render the document
  cat("Rendering verification report for:", csv_path_abs, "\n")
  cat("Output will be saved to:", output_path, "\n\n")

  # Save current directory
  old_wd <- getwd()

  tryCatch({
    # Change to the CSV directory for rendering
    setwd(csv_dir_abs)

    # Render the document
    quarto::quarto_render(
      input = basename(target_qmd),
      output_file = output_file,
      quiet = FALSE
    )

    cat("\n✓ Verification report successfully generated!\n")
    cat("  Report location:", output_path, "\n")

    # Clean up temp qmd file
    unlink(target_qmd)

    # Restore working directory
    setwd(old_wd)

    invisible(output_path)

  }, error = function(e) {
    cat("\n❌ Rendering failed - validation errors detected\n")
    cat("Error message:", conditionMessage(e), "\n")

    # Clean up temp file
    if (file.exists(target_qmd)) unlink(target_qmd)

    # Restore working directory
    setwd(old_wd)

    stop("Verification failed. Please check the data for errors.", call. = FALSE)
  })
}


# Example usage:
# verify_and_render_rapid_assessment("raw_data/survey_data/rapid_assessment_data_2026_onwards/adelaide_plains/19_01_2026.csv")

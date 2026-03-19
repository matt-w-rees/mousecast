# Render a verification HTML report for a single rapid-assessment CSV file.
# Designed to be called inside a branched tar_target() that maps over CSV paths.
#
# @param csv_file         Character; path to the CSV file (one branch value from tar_files).
# @param site_information Data frame; site metadata returned by data_summarise_site_information(),
#                         serialised to a temp RDS so the QMD can read it without calling
#                         tar_read() mid-pipeline (avoids metadata race conditions).
# @return Character; absolute path to the rendered HTML file (tracked by targets via format = "file").
verify_and_render_rapid_assessment <- function(csv_file, site_information) {

  # resolve to absolute path so paths remain valid when working directory changes
  abs_csv  <- normalizePath(csv_file, mustWork = TRUE)
  out_file <- paste0(tools::file_path_sans_ext(basename(abs_csv)), ".html")
  dest     <- file.path(dirname(abs_csv), out_file)

  # write site_information to a temp RDS that the QMD reads via readRDS();
  # avoids calling tar_read() inside a rendered document mid-pipeline
  site_info_path <- tempfile(fileext = ".rds")
  saveRDS(site_information, site_info_path)

  # render from quarto_reports/ so relative resource paths inside the QMD resolve correctly;
  # move the output HTML to sit alongside its source CSV when done
  withr::with_dir("quarto_reports", {
    quarto::quarto_render(
      "verify_new_rapid_assessment_data.qmd",
      output_file    = out_file,
      execute_params = list(csv_path = abs_csv, site_info_path = site_info_path),
      quiet          = TRUE
    )
    file.rename(out_file, dest)
  })

  dest
}

# Render a seasonal summary HTML report for a single season.
# Designed to be called inside a branched tar_target() that maps over seasons and indices.
#
# Passing `data` and `aus_shp` as explicit arguments means targets tracks them as
# dependencies — the report will re-render whenever either object changes.
# Both are serialised to temp RDS files so the QMD reads them via readRDS() rather
# than calling tar_load() mid-pipeline (which can cause metadata race conditions).
#
# @param season       Character; single season label for this branch, e.g. "Autumn-2024".
# @param season_index Integer; position of this season in the full ordered sequence,
#                     used to build a zero-padded filename prefix for chronological sorting.
# @param data         Named list of survey data frames (traps, burrows, chewcards).
# @param aus_shp      sf object; Australian state boundaries used for mapping.
# @return Character; absolute path to the rendered HTML file (tracked via format = "file").
render_seasonal_summary <- function(season, season_index, data, aus_shp) {

  # zero-pad index so files sort chronologically in the filesystem
  out_file <- paste0(sprintf("%02d", season_index), "_", gsub("-", "_", season), ".html")
  dest_dir <- normalizePath("quarto_reports/seasonal_summaries", mustWork = FALSE)
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(dest_dir, out_file)

  # serialise data objects to temp RDS files so the QMD reads them without
  # calling tar_load() inside a render (avoids pipeline metadata race conditions)
  data_path    <- tempfile(fileext = ".rds")
  aus_shp_path <- tempfile(fileext = ".rds")
  saveRDS(data,    data_path)
  saveRDS(aus_shp, aus_shp_path)

  # render from quarto_reports/ so relative resource paths in the QMD resolve correctly
  withr::with_dir("quarto_reports", {
    quarto::quarto_render(
      "seasonal_summary.qmd",
      output_file    = out_file,
      execute_params = list(
        season       = season,
        data_path    = data_path,
        aus_shp_path = aus_shp_path
      ),
      quiet = TRUE
    )
    file.rename(out_file, file.path("seasonal_summaries", out_file))
  })

  dest
}

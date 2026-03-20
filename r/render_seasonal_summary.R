# Render a seasonal summary HTML report for a single season.
# Designed to be called inside a branched tar_target() that maps over seasons and indices.
#
# Accepts pre-filtered season data and pre-computed global stats rather than the
# full dataset so that targets only re-runs a branch when that season's data (or
# the global colour-scale bounds) actually changes.
#
# Both data objects are serialised to temp RDS files so the QMD reads them via
# readRDS() rather than calling tar_load() mid-pipeline (which can cause metadata
# race conditions).
#
# @param season        Character; single season label for this branch, e.g. "Autumn-2024".
# @param season_index  Integer; position of this season in the full ordered sequence,
#                      used to build a zero-padded filename prefix for chronological sorting.
# @param data_season   Named list of data frames already filtered to this season.
# @param global_stats  Named list of global colour-scale bounds (from get_seasonal_summary_global_stats).
# @param aus_shp       sf object; Australian state boundaries used for mapping.
# @return Character; absolute path to the rendered HTML file (tracked via format = "file").
render_seasonal_summary <- function(season, season_index, data_season, global_stats, aus_shp) {

  # zero-pad index so files sort chronologically in the filesystem
  out_file <- paste0(sprintf("%02d", season_index), "_", gsub("-", "_", season), ".html")
  dest_dir <- normalizePath("quarto_reports/seasonal_summaries", mustWork = FALSE)
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(dest_dir, out_file)

  # serialise data objects to temp RDS files so the QMD reads them without
  # calling tar_load() inside a render (avoids pipeline metadata race conditions)
  data_season_path   <- tempfile(fileext = ".rds")
  global_stats_path  <- tempfile(fileext = ".rds")
  aus_shp_path       <- tempfile(fileext = ".rds")
  saveRDS(data_season,  data_season_path)
  saveRDS(global_stats, global_stats_path)
  saveRDS(aus_shp,      aus_shp_path)

  # render from quarto_reports/ so relative resource paths in the QMD resolve correctly
  withr::with_dir("quarto_reports", {
    quarto::quarto_render(
      "seasonal_summary.qmd",
      output_file    = out_file,
      execute_params = list(
        season             = season,
        data_season_path   = data_season_path,
        global_stats_path  = global_stats_path,
        aus_shp_path       = aus_shp_path
      ),
      quiet = TRUE
    )
    file.rename(out_file, file.path("seasonal_summaries", out_file))
  })

  dest
}

# Superseded for rainfall by build_monthly_rainfall_raster()
# (r/b_build_monthly_rainfall_raster.R) -- SILO publishes pre-aggregated
# monthly totals directly for rainfall, so daily -> monthly aggregation isn't
# needed there. Still active for min_temp (this pipeline's temperature
# covariate), which has no monthly-pre-aggregated equivalent -- see
# r/b_download_silo_daily_data.R's header for the daily source this
# summarises.
#
# Summarise daily SILO NetCDF composites (r/b_download_silo_daily_data.R) to
# one raster layer per calendar month, for a single variable.
#
# Unlike GPP's MODIS/VIIRS composites (r/b_build_seasonal_gpp_raster.R), SILO
# is BOM's own interpolated national climate surface, not a remotely-sensed
# product with fill/sentinel codes or ocean noise to mask -- it's already
# land-only, so no equivalent to that function's valid_max clamp or aus_shp
# mask is needed here.
#
# A month is only summarised if it has at least `min_days_per_month` daily
# layers, so an in-progress month isn't reported on a fraction of the data
# (a full month has 28-31). Output layers are named "<month>_<year>" (e.g.
# "7_2026"), matching attach_time_variables()'s month_year format exactly
# (r/a_attach_time_variables.R) so attach_raster_covs() can match
# them straight against a data frame's own month_year column.
#
# Arguments:
#   files                file paths for one variable's annual NetCDFs (e.g.
#                        silo_daily_files[grepl("min_temp", silo_daily_files)])
#   summary_func         how daily values collapse to a month: "mean" (e.g.
#                        temperature) or "sum" (e.g. rainfall total) --
#                        also accepts "median"/"min"/"max"
#   min_days_per_month   minimum daily layers required to summarise a month
#                        (default 27; a full month has 28-31)
#   roi                  optional sf/SpatVector to crop to (+ margin_deg)
#                        before aggregating -- SILO's daily files are always
#                        national (no ROI-scoped download, unlike GPP).
#                        NULL (default) processes the full national extent.
#                        Confirmed live this session that this alone barely
#                        speeds anything up: each daily layer's HDF5 chunk
#                        (ncdump -h -s: min_temp:_ChunkSizes = 1, 681, 841)
#                        already covers the *entire country* in one chunk,
#                        so cropping after loading can't avoid decompressing
#                        it -- kept anyway since it shrinks the in-memory
#                        array tapp() then works with, but months (below) is
#                        the change that actually matters for speed.
#   margin_deg           degrees of margin added around roi's own extent
#                        before cropping (default 0.5); ignored if roi is NULL
#   months               optional integer vector (1-12) to restrict which
#                        calendar months are read and aggregated at all --
#                        e.g. c(6, 7, 8) for a winter-only covariate
#                        (build_min_temp_winter_raster(), r/b_build_min_temp_winter_raster.R).
#                        Applied before any per-cell work, so unwanted months'
#                        daily chunks are never decompressed in the first
#                        place -- confirmed this is what actually cuts cost,
#                        not roi above, since each day is its own full-country
#                        chunk regardless of spatial extent requested.
#                        NULL (default) keeps all 12 months.
#
# Returns a SpatRaster, one layer per month_year (for tar_terra_rast()).

summarise_silo_to_month <- function(files, summary_func = "mean", min_days_per_month = 27,
                                     roi = NULL, margin_deg = 0.5, months = NULL) {

  allowed_funcs <- c("mean", "sum", "median", "min", "max")
  if (!(summary_func %in% allowed_funcs)) {
    stop("summary_func must be one of: ", paste(allowed_funcs, collapse = ", "))
  }

  # ── 1. Load daily composites, one layer per day (metadata only -- terra is
  # lazy, actual decompression happens only when values are later read) ─────
  rasters <- terra::rast(files)

  if (is.null(terra::time(rasters))) {
    stop("Raster stack does not contain valid time metadata.")
  }

  all_months <- lubridate::month(terra::time(rasters))
  all_years  <- lubridate::year(terra::time(rasters))

  # Restrict to the requested calendar months BEFORE any further step --
  # this is what actually avoids decompressing unwanted days (see months'
  # own header note above), unlike cropping which happens too late to help.
  if (!is.null(months)) {
    keep_months <- all_months %in% months
    rasters    <- rasters[[keep_months]]
    all_months <- all_months[keep_months]
    all_years  <- all_years[keep_months]
  }

  if (!is.null(roi)) {
    bbox <- sf::st_bbox(roi)
    rasters <- terra::crop(rasters, terra::ext(
      bbox["xmin"] - margin_deg, bbox["xmax"] + margin_deg,
      bbox["ymin"] - margin_deg, bbox["ymax"] + margin_deg
    ))
  }

  # ── 2. Collapse days to month means/sums ──────────────────────────────────
  month_lbl <- paste0(all_months, "_", all_years)

  days_count      <- table(month_lbl)
  complete_months <- names(days_count[days_count >= min_days_per_month])
  keep      <- month_lbl %in% complete_months
  rasters   <- rasters[[keep]]
  month_lbl <- month_lbl[keep]

  summarised_raster <- terra::tapp(rasters, index = month_lbl, fun = match.fun(summary_func))
  # tapp() mangles index labels into syntactic names; restore the clean
  # "<month>_<year>" labels, in the same first-appearance order tapp() used
  # to build the layers (files are read in filename order, one per year, so
  # this is chronological already).
  names(summarised_raster) <- unique(month_lbl)

  summarised_raster
}

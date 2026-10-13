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
# Unlike GPP's MODIS/VIIRS composites (r/b_build_gpp_period_raster.R), SILO
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
#   margin_deg           degrees of margin added around roi's own extent
#                        before cropping (default 0.5); ignored if roi is NULL
#   months               optional integer vector (1-12) to restrict which
#                        calendar months are read and aggregated at all --
#                        e.g. c(6, 7, 8) for a winter-only covariate.
#                        NULL (default) keeps all 12 months.
#
# Each annual file is read by read_silo_daily() (r/b_read_silo_daily.R), which
# reads through GDAL's HDF5 driver -- about 1 s per year, against about 21 min
# per year through the netCDF driver this function used to read with (see that
# function's header). Files are summarised one year at a time, which gives the
# same months as summarising them all together, since SILO's files are
# calendar years and no month spans two files.
#
# Returns a SpatRaster, one layer per month_year (for tar_terra_rast()).

summarise_silo_to_month <- function(files, summary_func = "mean", min_days_per_month = 27,
                                     roi = NULL, margin_deg = 0.5, months = NULL) {

  allowed_funcs <- c("mean", "sum", "median", "min", "max")
  if (!(summary_func %in% allowed_funcs)) {
    stop("summary_func must be one of: ", paste(allowed_funcs, collapse = ", "))
  }

  # One year's file at a time, so only one year's values are held in memory.
  yearly_summaries <- lapply(files, function(file) {

    # The year's daily layers (requested months only, cropped, in memory) and their dates.
    silo <- read_silo_daily(file, roi = roi, margin_deg = margin_deg, months = months)

    # Each day's "<month>_<year>" label, in this pipeline's month_year convention.
    month_lbl <- paste0(lubridate::month(silo$dates), "_", lubridate::year(silo$dates))

    # Only months with enough days to trust (drops an in-progress month).
    days_count      <- table(month_lbl)
    complete_months <- names(days_count[days_count >= min_days_per_month])
    keep <- month_lbl %in% complete_months
    if (!any(keep)) {
      return(NULL)
    }
    days      <- silo$days[[keep]]
    month_lbl <- month_lbl[keep]

    # Collapse days to one layer per month with the named summary (terra's built-in version of it).
    summarised <- terra::tapp(days, index = month_lbl, fun = summary_func)

    # tapp() mangles index labels into syntactic names; restore the clean "<month>_<year>" labels,
    # in the same first-appearance (chronological) order tapp() used to build the layers.
    names(summarised) <- unique(month_lbl)
    summarised
  })

  # Stack the years in file (chronological) order.
  do.call(c, Filter(Negate(is.null), yearly_summaries))
}

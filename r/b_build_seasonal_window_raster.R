# Collapse a monthly covariate stack (e.g. the coarsened rain raster this
# pipeline builds -- see _targets.R's rainfall section) to one sum per year
# over a fixed calendar window -- a general version of the
# build_min_temp_winter_raster() (r/b_build_min_temp_winter_raster.R) pattern,
# parameterised on which months and which summary function, rather than
# min_temp's hard-coded winter mean.
#
# Used for rainfall (start_month = 4, end_month = 10, i.e. Apr-Oct) rather
# than a rolling window (compute_rolling_mean_raster(),
# r/b_compute_rolling_mean_raster.R) because rainfall isn't a farmer
# decision the way planted crop is -- there's no endogeneity problem that a
# fixed calendar window would create, so the simpler, more interpretable
# fixed window is used directly.
#
# start_month/end_month must both fall within the same calendar year (no
# Dec/Jan rollover support, unlike this pipeline's season_year_adj convention
# elsewhere, r/a_season_info.R) -- true for the Apr-Oct window this is
# currently used for.
#
# A year is only included if every month in the window is present, matching
# this pipeline's existing completeness convention elsewhere (e.g.
# build_min_temp_winter_raster()'s own 3-of-3 check) -- an in-progress or
# gappy window isn't reported on a partial summary.
#
# Arguments:
#   rast          monthly SpatRaster, one layer per month_year (e.g.
#                 attach_time_variables()'s "<month>_<year>" convention)
#   start_month   first calendar month of the window (inclusive, 1-12)
#   end_month     last calendar month of the window (inclusive, 1-12,
#                 must be >= start_month)
#   summary_func  how the window's months collapse to one value: "sum"
#                 (default, e.g. rainfall total) or "mean"
#
# Returns a SpatRaster, one layer per year (named "<year>"), the window summary.

build_seasonal_window_raster <- function(rast, start_month, end_month, summary_func = c("sum", "mean")) {

  summary_func <- match.arg(summary_func)
  stopifnot(start_month <= end_month)

  window_months <- start_month:end_month

  months <- as.integer(sub("_.*", "", names(rast)))
  years  <- as.integer(sub(".*_", "", names(rast)))

  window_idx   <- which(months %in% window_months)
  window_years <- years[window_idx]

  complete_years <- names(which(table(window_years) == length(window_months)))

  window_rast <- purrr::map(complete_years, function(yr) {
    idx <- window_idx[window_years == as.integer(yr)]
    terra::app(rast[[idx]], fun = summary_func)
  }) |>
    terra::rast()

  names(window_rast) <- complete_years
  window_rast
}

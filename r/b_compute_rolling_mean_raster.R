# Trailing rolling average, per pixel, across a monthly covariate stack's
# chronological sequence of layers (e.g. the coarsened GPP raster this
# pipeline builds -- see _targets.R's GPP section). Unlike a fixed calendar
# window (build_seasonal_window_raster(), r/b_build_seasonal_window_raster.R),
# a rolling window recomputes every month and so isn't tied to a particular
# start/end month -- used for GPP because GPP reflects whatever a farmer
# actually planted (or fallowed), which varies opportunistically in
# dual-cropping zones, rather than following one fixed cropping calendar.
#
# Layers are first sorted into chronological order (parsed from their
# "<month>_<year>" names, attach_time_variables()'s convention) since the
# rolling window is computed along that sequence, not the stack's original
# (arbitrary) layer order. A layer's rolling mean is only defined once
# `window` consecutive months are actually present immediately before it, so
# the first (window - 1) chronological layers are NA -- no partial-window
# averages.
#
# Arguments:
#   rast     SpatRaster with one layer per month_year; layer names must be
#            "<month>_<year>" (attach_time_variables()'s convention)
#   window   trailing window length in months (default 12)
#
# Returns a SpatRaster, one layer per month_year (chronologically ordered),
# of trailing rolling means.

compute_rolling_mean_raster <- function(rast, window = 12) {

  months <- as.integer(sub("_.*", "", names(rast)))
  years  <- as.integer(sub(".*_", "", names(rast)))

  chron_order <- order(years, months)
  rast <- rast[[chron_order]]

  rolling_mean <- function(x) {
    RcppRoll::roll_mean(x, n = window, align = "right", fill = NA_real_)
  }

  result <- terra::app(rast, fun = rolling_mean)
  names(result) <- names(rast)
  result
}

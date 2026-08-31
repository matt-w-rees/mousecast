# Leave-one-out (LOO) climatological z-score anomaly, per pixel per calendar
# month, across all years present in a monthly covariate stack (e.g. the
# coarsened rain/soil_moisture/GPP rasters this pipeline builds -- see
# _targets.R's coarse/anomaly section). For each pixel and calendar month,
# every year's value is compared against the mean and SD of *every other*
# year for that same month -- not the mean/SD including the year itself --
# so a single unusual year can't dampen its own anomaly by pulling the
# baseline toward it (a real risk with a short, ~40-year record where any
# one year is a non-trivial fraction of the baseline).
#
# Both the LOO mean and LOO SD are computed via the standard leave-one-out
# identities (not a per-year brute-force loop, which would be far slower
# across every pixel):
#   loo_mean_i = (sum(x) - x_i) / (n - 1)
#   loo_sd_i   = sqrt( (sum(x^2) - x_i^2 - (n-1) * loo_mean_i^2) / (n - 2) )
# (sample SD convention, matching R's own sd() -- n-2 degrees of freedom
# once one year is already held out). Confirmed directly against brute-force
# mean(x[-i])/sd(x[-i]) for a synthetic 5-year series -- exact match.
# z_i = (x_i - loo_mean_i) / loo_sd_i.
#
# A pixel/month needs at least 3 non-NA years for a defined LOO SD (n-2 >= 1
# once one year is excluded); fewer than that returns NA rather than
# dividing by zero.
#
# loo_var is also clamped to >= 0 before sqrt() in both branches below --
# traced live (2026-08, soil_moisture_anomaly_raster's own recurring "NaNs
# produced" warning) to a small number of pixels with genuinely ~constant
# values across every year (true variance ~0), where the sum-of-squares
# identity above lands on a tiny NEGATIVE float (~-1e-24, floating-point
# cancellation, not a real negative variance) instead of exactly 0. sqrt() of
# that emits NaN, which still ends up NA in the final anomaly raster either
# way -- the same outcome the code already intends for a genuine
# zero-variance cell -- so the clamp only silences the spurious warning, it
# doesn't change any result.
#
# smooth_window optionally blends each year's LOO mean/SD across neighbouring
# pixels (a 3x3 focal mean, say) before scoring the anomaly against them --
# used for GPP, where a single pixel's own history is noisier than its
# regional neighbourhood's, but left off (the default) for rain/soil_moisture,
# which come from already spatially-smoothed model surfaces (SILO, AWRA) and,
# for soil moisture especially, are meant to capture *this exact paddock's*
# state rather than a blended regional one. Smoothing needs neighbouring
# cells, which a per-cell terra::app() function can't see, so this switches
# to an explicit per-year loop (raster arithmetic for the LOO sum/sum_sq
# identity, then terra::focal() on the resulting mean/SD rasters, then the
# z-score) instead of app()'s vectorised-per-pixel path.
#
# Also handles a once-a-year stack (one layer per plain "<year>") and a
# once-per-season stack (one layer per "<Season>-<year_adj>", e.g.
# rain_seasonal_raster) the same way -- there's no single "calendar month"
# component to split by in either case; grouping is delegated to
# group_raster_layers() (r/b_group_raster_layers.R, see its header for the
# three naming shapes it detects), which returns calendar months, season
# names, or one shared group as appropriate.
#
# Arguments:
#   rast            SpatRaster with one layer per month_year ("<month>_<year>",
#                    e.g. a coarsened rain/soil_moisture/GPP stack), one
#                    layer per season_year_adj ("<Season>-<year_adj>", e.g.
#                    rain_seasonal_raster), or one layer per plain year (e.g.
#                    a fixed seasonal-window sum)
#   smooth_window    NULL (default) computes the LOO mean/SD purely per-pixel,
#                    unchanged from this function's original behaviour;
#                    supplying an odd window (in cells, e.g. 3) focal-smooths
#                    the LOO mean/SD across neighbouring pixels first
#
# Returns a SpatRaster, same shape/layer-names as rast, of z-score anomalies.

compute_loo_anomaly_raster <- function(rast, smooth_window = NULL) {

  # calendar month, season name, or one shared group -- see group_raster_layers()'s own header
  groups <- group_raster_layers(names(rast))

  # filled in below, one element per input layer, in rast's own original order
  anomaly_layers <- vector("list", terra::nlyr(rast))

  if (is.null(smooth_window)) {

    # Per-pixel LOO z-score for one calendar-month/season group's own years
    # (a vector x, one value per year) -- implements the header's LOO
    # identities directly.
    loo_z <- function(x) {
      n <- sum(!is.na(x))
      if (n < 3) return(rep(NA_real_, length(x))) # not enough years for a defined LOO SD (n-2 >= 1)

      total   <- sum(x, na.rm = TRUE)
      sum_sq  <- sum(x^2, na.rm = TRUE)

      loo_mean <- (total - x) / (n - 1)                                   # mean of every OTHER year
      loo_var  <- (sum_sq - x^2 - (n - 1) * loo_mean^2) / (n - 2)         # sample variance of every other year
      loo_var  <- pmax(loo_var, 0)                                        # guard against floating-point cancellation near zero (see header)
      loo_sd   <- sqrt(loo_var)

      (x - loo_mean) / loo_sd                                            # this year's z-score against that LOO baseline
    }

    for (g in sort(unique(groups))) {
      idx    <- which(groups == g)  # this group's own layer positions in rast
      subset <- rast[[idx]]         # just this group's own years

      # terra::app() applies loo_z() per pixel, across subset's own layer
      # (year) dimension -- vectorised, no explicit per-year loop needed here.
      z_subset <- terra::app(subset, fun = loo_z)
      names(z_subset) <- names(subset)

      # write each scored layer back into its own original position in anomaly_layers
      for (i in seq_along(idx)) anomaly_layers[[idx[i]]] <- z_subset[[i]]
    }

  } else {

    # Smoothing needs each year's own LOO mean/SD as an actual raster (so
    # terra::focal() can see neighbouring pixels), which app()'s per-pixel
    # vectorised path can't produce -- so this branch computes the LOO sum/
    # sum_sq identity via plain raster arithmetic instead, one year at a time.
    for (g in sort(unique(groups))) {
      idx    <- which(groups == g)  # this group's own layer positions in rast
      subset <- rast[[idx]]         # just this group's own years

      n_valid <- terra::app(subset, fun = function(x) sum(!is.na(x))) # per-pixel count of non-NA years
      total   <- terra::app(subset, fun = "sum", na.rm = TRUE)        # per-pixel sum across years
      sum_sq  <- terra::app(subset^2, fun = "sum", na.rm = TRUE)      # per-pixel sum of squares across years

      for (i in seq_along(idx)) {
        x_i <- subset[[i]]  # this one year's own raster

        loo_mean_i <- (total - x_i) / (n_valid - 1)                                 # mean of every OTHER year, per pixel
        loo_var_i  <- (sum_sq - x_i^2 - (n_valid - 1) * loo_mean_i^2) / (n_valid - 2) # sample variance of every other year
        loo_var_i  <- terra::ifel(loo_var_i < 0, 0, loo_var_i)                     # guard against floating-point cancellation near zero (see header)
        loo_sd_i   <- sqrt(loo_var_i)

        # not enough years for a defined LOO SD at this pixel -- NA rather than divide by zero/negative
        loo_mean_i <- terra::ifel(n_valid < 3, NA, loo_mean_i)
        loo_sd_i   <- terra::ifel(n_valid < 3, NA, loo_sd_i)

        # blend the LOO mean/SD across neighbouring pixels before scoring against them
        smoothed_mean_i <- terra::focal(loo_mean_i, w = smooth_window, fun = "mean", na.rm = TRUE)
        smoothed_sd_i   <- terra::focal(loo_sd_i,   w = smooth_window, fun = "mean", na.rm = TRUE)

        anomaly_layers[[idx[i]]] <- (x_i - smoothed_mean_i) / smoothed_sd_i # this year's z-score against the smoothed baseline
      }
    }
  }

  result <- terra::rast(anomaly_layers) # reassemble in original layer order (anomaly_layers was filled by position, not appended)
  names(result) <- names(rast)
  result
}

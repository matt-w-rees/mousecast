# Growing-season total to date: for each season-end month, a monthly covariate summed from the start of that
# year's winter-crop growing season (April by default) up to the season-end month, capped at the growing
# season's last month (October by default). Before the growing season starts (the Summer season-end month,
# February), it is the previous year's full growing-season total.
#
# Used for:
#   - rainfall (rain_growing_season_raster): in winter-only cropping regions, high April-October rainfall is
#     a key expected driver of mice building up;
#   - GPP (gpp_growing_season_raster): the season's crop growth, so its February value is last year's whole
#     crop -- a measure of crop size behind the harvest's spilled grain and stubble.
# Read at each season's end month like the other covariates:
#   Autumn (May)      April-May;
#   Winter (August)   April-August;
#   Spring (November) the full April-October total;
#   Summer (February) last year's full April-October total.
# Its anomaly (compute_loo_anomaly_raster(), grouped by calendar month) compares each value with the same
# window in other years, so an August value is only ever compared with other years' April-August totals.
#
# Arguments:
#   monthly_raster     monthly values, one layer per "<month>_<year>" (e.g. rain_raster_coarse, gpp_raster_coarse)
#   start_month        first month of the growing season (default 4, April)
#   end_month          last month of the growing season (default 10, October)
#   season_end_months  the 4 season-end calendar months (default 2, 5, 8, 11)
#
# Returns a SpatRaster with one layer per season-end month ("<month>_<year>"), holding the growing-season
# total to date (in the monthly raster's units). A layer whose window has any missing month is left out.

compute_growing_season_total_raster <- function(monthly_raster, start_month = 4, end_month = 10, season_end_months = c(2, 5, 8, 11)) {

  # Each layer's calendar month and year, from its "<month>_<year>" name.
  layer_month <- as.integer(sub("_.*", "", names(monthly_raster)))
  layer_year  <- as.integer(sub(".*_", "", names(monthly_raster)))

  # Every season-end month the raster covers.
  ends <- which(layer_month %in% season_end_months)

  layers <- lapply(ends, function(i) {
    m <- layer_month[i]
    y <- layer_year[i]
    # The growing season this season-end month reads: the current year's once it has started, otherwise
    # the previous year's (complete) one.
    gs_year <- if (m >= start_month) y else y - 1L
    # Months to sum: from the season's start up to this month, capped at the season's end.
    last_month <- if (m >= start_month) min(m, end_month) else end_month
    window <- paste0(start_month:last_month, "_", gs_year)
    # Skip a season-end month whose window isn't fully covered.
    if (!all(window %in% names(monthly_raster))) return(NULL)
    total <- sum(monthly_raster[[window]])
    names(total) <- names(monthly_raster)[i]
    total
  })

  terra::rast(Filter(Negate(is.null), layers))
}

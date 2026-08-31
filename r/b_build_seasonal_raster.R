# Collapse a monthly covariate stack (e.g. min_temp_raster,
# r/b_summarise_silo_to_month.R, or the coarsened rain_raster_coarse this
# pipeline builds -- see _targets.R's coarse/anomaly section) to one value
# per season (Summer/Autumn/Winter/Spring) per site-year, via summary_func --
# "mean" (e.g. min_temp's seasonal average) or "sum" (e.g. rainfall's
# seasonal total). General version of build_min_temp_seasonal_raster()
# (r_not_in_use/b_build_min_temp_seasonal_raster.R, superseded), which
# hard-coded "mean" for min_temp alone.
#
# Distinct from build_seasonal_window_raster() (r_not_in_use/b_build_seasonal_window_raster.R,
# no longer used in the pipeline), which collapses to one *fixed calendar window* (e.g. rainfall's
# former Apr-Oct growing-season total) per plain calendar year -- this instead produces a value for
# *every* one of the four seasons, keyed by season_year_adj, for covariates that need a real value
# at every seasonal time step rather than one fixed-window value carried forward across the year.
#
# Season/year_adj follow attach_time_variables()'s own convention exactly
# (r/a_attach_time_variables.R, r/a_season_info.R): Dec/Jan/Feb = Summer,
# Mar/Apr/May = Autumn, Jun/Jul/Aug = Winter, Sep/Oct/Nov = Spring; December
# belongs to the *following* year's Summer (year_adj), since Summer otherwise
# spans a calendar-year boundary. Layers are named "Season-year_adj" (e.g.
# "Winter-2026"), matching build_gpp_period_raster()'s own season-label
# convention (r/b_build_gpp_period_raster.R) so this can be attached the
# same way, via period_col = "season_year_adj".
#
# A season is only included if all 3 of its months are present, matching
# this pipeline's existing completeness convention elsewhere (e.g.
# summarise_silo_to_month()'s own min_days_per_month, build_gpp_period_raster()'s
# min_composites_per_period) -- an in-progress or gappy season isn't reported
# on a partial value.
#
# Arguments:
#   rast           monthly SpatRaster, one layer per month_year (e.g.
#                  attach_time_variables()'s "<month>_<year>" convention)
#   summary_func   "mean" or "sum" -- how a season's 3 months collapse to one value
#
# Returns a SpatRaster, one layer per season_year_adj (named "Season-year_adj"), the seasonal summary.

build_seasonal_raster <- function(rast, summary_func = c("mean", "sum")) {

  summary_func <- match.arg(summary_func)

  # parse each layer's own calendar month/year from its "<month>_<year>" name
  months <- as.integer(sub("_.*", "", names(rast)))
  years  <- as.integer(sub(".*_", "", names(rast)))

  # calendar month -> season name, matching attach_time_variables()'s own convention
  season_lookup <- c(
    `12` = "Summer", `1` = "Summer", `2` = "Summer",
    `3`  = "Autumn", `4` = "Autumn", `5` = "Autumn",
    `6`  = "Winter", `7` = "Winter", `8` = "Winter",
    `9`  = "Spring", `10` = "Spring", `11` = "Spring"
  )
  season   <- season_lookup[as.character(months)]
  year_adj <- ifelse(months == 12L, years + 1L, years) # December rolls into next year's Summer

  season_year_adj <- paste0(season, "-", year_adj)              # this layer's own season label, e.g. "Winter-2026"
  complete_labels <- names(which(table(season_year_adj) == 3))  # only seasons with all 3 of their months present

  # one output layer per complete season, collapsing its 3 monthly layers via summary_func
  seasonal_rast <- purrr::map(complete_labels, function(lbl) {
    idx <- which(season_year_adj == lbl) # this season's own 3 monthly layer positions
    terra::app(rast[[idx]], fun = summary_func)
  }) |>
    terra::rast()

  names(seasonal_rast) <- complete_labels
  seasonal_rast
}

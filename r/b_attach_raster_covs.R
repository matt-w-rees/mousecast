# Attach one covariate from a raster onto a paddock x period grid (e.g.
# paddock_season_grid, r/b_build_paddock_season_grid.R), by simple point
# extraction at each row's own longitude/latitude, matched to the raster's
# own layer for that row's period.
#
# period_col names whichever column in `data` identifies each row's period,
# matched as a string against names(raster) -- both must agree on the same
# convention for a given call. Four shapes are supported (see paddock_season_grid's
# own columns), though only the middle two are actually in use across this
# pipeline's covariates right now:
#   - "month_year" ("<month>_<year>", e.g. "7_2026") -- one raster layer per
#     calendar month, matched 1:1.
#   - "season_end_month_year" -- same "<month>_<year>" shape, but naming that
#     season's own last calendar month (build_paddock_season_grid()'s
#     lookup) -- used to read a monthly-grain raster (e.g. GPP's rolling
#     average, soil moisture) once per season rather than once per month.
#   - "season_year_adj" -- a raster that already has a real value per season
#     (one layer per "<Season>-<year_adj>", e.g. min_temp's seasonal mean,
#     rainfall's own per-season total), matched 1:1, no broadcasting needed.
#   - "year_adj" -- a once-a-year raster (one layer per plain "<year>"),
#     broadcast across every season of that year_adj cycle -- terra::extract()'s
#     layer = argument already supports many rows mapping to the same layer,
#     no special-casing needed. Not currently used by any covariate (rainfall's
#     former fixed-window total was the one user, since removed -- see
#     r_not_in_use/b_build_seasonal_window_raster.R), but kept as a supported
#     shape for any future once-a-year covariate.
#
# Point extraction only, no focal (moving-window) smoothing -- appropriate
# for coarse (SILO/AWRA/PML-grid, ~5-10 km) rasters where a single pixel
# already represents a broad area; a genuinely fine-resolution raster (e.g.
# native 500 m GPP) would need its own paddock-footprint-aware smoothing
# instead (see r_not_in_use/b_attach_seasonal_gpp.R for that pattern, retired
# once this pipeline stopped using native-resolution GPP as a covariate).
#
# Designed to be chained: attach_raster_covs() can be called repeatedly on
# the same data with a different raster/var_name (and period_col) each time,
# each call adding one new column.
#
# Arguments:
#   data        data frame with longitude, latitude, and period_col columns
#               (e.g. paddock_season_grid)
#   raster      SpatRaster with one layer per period_col value
#   var_name    name of the new column to add
#   period_col  column in data whose values are matched against names(raster)
#               (default "month_year")
#
# Returns data with one new numeric column (var_name), NA for any row whose
# period isn't present in raster (e.g. a month/year excluded upstream by a
# completeness check).

attach_raster_covs <- function(data, raster, var_name, period_col = "month_year") {

  stopifnot(all(c("longitude", "latitude", period_col) %in% names(data)))

  original_nrow     <- nrow(data)
  original_colnames <- colnames(data)

  # ── 1. Only rows whose period has a matching raster layer can be
  # extracted -- everything else stays NA (see header note) ────────────────
  available_layers <- names(raster)
  period_values <- as.character(data[[period_col]])
  extract_mask  <- period_values %in% available_layers
  data_subset   <- data[extract_mask, ]

  data_vect <- terra::vect(
    sf::st_as_sf(data_subset, coords = c("longitude", "latitude"), crs = 4326)
  )

  # layer = each row's own period value extracts each row from its own
  # matching layer in one call, rather than looping per period value --
  # multiple rows sharing the same period (e.g. every season in the same
  # year_adj cycle, for a once-a-year raster) all read from that one shared
  # layer fine.
  extracted <- terra::extract(raster, data_vect, layer = period_values[extract_mask])

  new_col <- var_name
  data[[new_col]] <- NA_real_
  data[[new_col]][extract_mask] <- extracted$value

  # ── 2. Integrity checks -- catch a silent row-count or column-shape
  # mismatch rather than let it propagate downstream unnoticed ─────────────
  if (nrow(data) != original_nrow) {
    warning("Row count changed during processing.")
  }

  new_cols_added <- setdiff(colnames(data), original_colnames)
  if (length(new_cols_added) != 1 || new_cols_added != new_col) {
    warning("Expected exactly one new column named '", new_col,
            "' but found: ", paste(new_cols_added, collapse = ", "))
  }

  data
}

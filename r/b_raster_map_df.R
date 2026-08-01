# Convert a SpatRaster's layers into a tidy long-format tibble (x, y, label,
# value) for a faceted spatial map -- raster_mean_series()'s (r/
# b_raster_mean_series.R) spatial counterpart: that function collapses a
# raster's spatial variation away to show its behaviour over time; this one
# keeps the spatial variation and instead facets over whichever layers the
# caller has already subsetted the raster to (e.g. rast[[tail(names(rast),
# 4)]] for the 4 most recent periods), used throughout covariate_summary.qmd
# wherever a time-series line chart alone would hide real regional
# differences (e.g. a whiplash swing concentrated in one part of the country
# averages out to something unremarkable in a national mean).
#
# Same two layer-naming conventions as raster_mean_series() (month_year,
# season_year_adj) are turned into human-readable facet labels; any other
# convention (e.g. build_seasonal_window_raster()'s plain year) is left as
# the raster's own layer name, already readable as-is.
#
# Arguments:
#   rast  SpatRaster, already subsetted to just the layers to map -- every
#         layer is included in the output, so subset before calling this,
#         not after
#
# Returns a tibble: x, y (cell centre coordinates), label (factor, in the
# same order as rast's own layers), value.

raster_map_df <- function(rast) {

  labels <- names(rast)
  is_month_year  <- grepl("^[0-9]+_[0-9]+$", labels)
  is_season_year <- grepl("^[A-Za-z]+-[0-9]+$", labels)

  if (all(is_month_year)) {
    month  <- as.integer(sub("_.*", "", labels))
    year   <- sub(".*_", "", labels)
    pretty <- paste(month.name[month], year)
  } else if (all(is_season_year)) {
    season   <- sub("-.*", "", labels)
    year_adj <- sub(".*-", "", labels)
    pretty   <- paste(season, year_adj)
  } else {
    pretty <- labels
  }

  names(rast) <- pretty

  as.data.frame(rast, xy = TRUE) |>
    tidyr::pivot_longer(-c(x, y), names_to = "label", values_to = "value") |>
    dplyr::mutate(label = factor(label, levels = pretty))
}

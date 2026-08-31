# Collapse per-date GPP composites (already masked and unit-converted) into
# period (season or month) means. Factored out of build_gpp_period_raster()
# (2026-08) as its own aggregation step.
#
# A period (season or month) is only summarised if it has at least
# min_composites_per_period composites, so an in-progress period isn't
# reported on a fraction of the data (a full season has ~11 8-day
# composites; a full month has ~3-4). Each output layer is named to match
# attach_time_variables()'s own convention (r/a_attach_time_variables.R)
# exactly -- "Season-Year" (e.g. "Winter-2026") for summarise_by = "season",
# "Month_Year" (e.g. "7_2026") for "month" -- so a monthly or seasonal GPP
# covariate can be left_join()'d/attach_raster_covs()'d straight onto survey
# or paddock data by that same column. Each layer carries its composite
# count as a "n_composites" layer metag (see terra::metags()), read back off
# by whichever consumer wants it.
#
# season_info() (r/a_season_info.R) gives the same Summer/Autumn/Winter/
# Spring, December-rolls-into-next-year convention used everywhere else in
# the pipeline. The month case needs no such year-boundary adjustment --
# plain calendar month/year -- and matches attach_time_variables()'s own
# month_year format ("<month>_<year>", e.g. "7_2026") exactly.
#
# Arguments:
#   rast                        SpatRaster, one layer per composite date,
#                               already masked and converted to gC/m^2/day
#   dates                       Date vector, same length/order as rast's own layers
#   summarise_by                "season" or "month" -- which period to collapse into
#   min_composites_per_period    minimum composites required to summarise a period
#
# Returns a SpatRaster, one layer per complete period, or NULL if no period
# has enough composites (e.g. VIIRS's gap-filled product can lag real-time by
# several months, so a block scoped to just the current calendar year can
# genuinely have zero complete months for most of the year -- terra's own
# `[[` errors ("[subset] no (valid) layer selected") rather than returning an
# empty raster, so this is checked explicitly instead of letting that
# happen). Callers combining several calls' outputs don't need to filter
# NULLs out first -- confirmed live that terra's c() silently skips NULL
# entries even via do.call(c, list(...)), so a NULL block just contributes
# zero layers.

aggregate_gpp_to_periods <- function(rast, dates, summarise_by, min_composites_per_period) {

  period_lbl <- vapply(dates, function(d) {
    if (summarise_by == "season") {
      s <- season_info(d)
      paste0(s$season, "-", s$year_adj)
    } else {
      paste0(lubridate::month(d), "_", lubridate::year(d))
    }
  }, character(1))

  composite_counts <- table(period_lbl)
  complete_periods <- names(which(composite_counts >= min_composites_per_period))
  keep       <- period_lbl %in% complete_periods
  if (!any(keep)) {
    message("No period in rast has >= ", min_composites_per_period,
            " composites (", length(dates), " composite(s) found, ",
            min(dates), " to ", max(dates), ") -- returning NULL.")
    return(NULL)
  }
  rast       <- rast[[keep]]
  period_lbl <- period_lbl[keep]

  period_rast <- terra::tapp(rast, index = period_lbl, fun = "mean", na.rm = TRUE)
  # tapp() mangles index labels into syntactic names (e.g. "Winter-2026" ->
  # "Winter.2026", or "7_2026" untouched but not guaranteed to stay so);
  # restore the clean labels, in the same first-appearance order tapp() used
  # to build the layers (dates was already sorted by the caller, so this is
  # chronological for both season and month).
  period_lvls <- unique(period_lbl)
  names(period_rast) <- period_lvls

  for (i in seq_along(period_lvls)) {
    terra::metags(period_rast, layer = i) <- c(
      n_composites = as.character(composite_counts[[period_lvls[i]]])
    )
  }

  period_rast
}

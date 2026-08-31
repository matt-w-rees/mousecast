# Finish a composite-grain GPP raster (already sentinel-clamped and daily-
# rate-converted -- load_and_clamp_gpp_composites()'s own output, or several
# such outputs already merged, e.g. combine_spatraster_list()) into period
# (season or month) means. The two operations deliberately deferred to here
# rather than done per-source, earlier: off-shore masking (mask_gpp_offshore(),
# safe to defer indefinitely -- a fixed geographic set of cells regardless
# of source/date/correction) and period aggregation (aggregate_gpp_to_periods(),
# which needs to run AFTER any bias-check/correction/splice work across
# NASA's own sources, not before -- a per-source monthly mean built too
# early would need rebuilding on every splice change, and can't be compared
# at the exact composite-date grain a correction ratio needs). See
# _targets.R's GPP section, "A) NASA", for where this actually gets called
# -- once, on the finished spliced record, not four times on raw sources.
#
# Arguments:
#   rast                        SpatRaster, one layer per composite date
#                               (as.character(date) names) -- already
#                               sentinel-clamped and daily-rate-converted
#   aus_shp                     sf polygon object of the Australian
#                               coastline, passed straight through to
#                               mask_gpp_offshore()
#   summarise_by                "season" (default) or "month" -- which
#                               period 8-day composites are collapsed into
#   min_composites_per_period    minimum 8-day composites required to
#                               summarise a period; defaults to 8 for
#                               summarise_by = "season" (a full season has
#                               ~11) or 3 for "month" (a full month has ~3-4)
#                               if left NULL
#
# Returns a SpatRaster, one layer per complete period -- see
# aggregate_gpp_to_periods()'s own header for the exact naming/NULL-handling.

finish_gpp_period_raster <- function(rast, aus_shp, summarise_by = c("season", "month"), min_composites_per_period = NULL) {

  summarise_by <- match.arg(summarise_by)
  if (is.null(min_composites_per_period)) {
    min_composites_per_period <- if (summarise_by == "season") 8 else 3
  }

  dates <- as.Date(names(rast)) # composite-grain layers are named as.character(date), load_and_clamp_gpp_composites()'s own convention
  rast  <- mask_gpp_offshore(rast, aus_shp)

  aggregate_gpp_to_periods(rast, dates, summarise_by, min_composites_per_period)
}

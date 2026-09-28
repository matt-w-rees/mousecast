# Stack per-date GPP composites into one SpatRaster, one layer per date --
# mosaicking same-date files together first where a date genuinely spans
# more than one file (e.g. two adjacent yearly VIIRS blocks both delivering
# the same boundary composite), extending each to the shared union extent
# (NA outside whatever region(s) actually delivered it) before stacking. A
# fast path below skips the general per-date loop entirely when every date
# maps to exactly one file (MODIS's own case) -- see
# load_and_clamp_gpp_composites()'s header for the performance investigation
# behind it. Uses Reduce(terra::merge, rasters), not do.call(terra::merge,
# rasters) -- do.call() with 2+ SpatRasters can silently mis-dispatch to
# base::merge.default() instead of terra's own S4 method when called from
# inside a nested closure, raising a spurious "argument 'x' is missing" error.
#
# Arguments:
#   gpp_tif                 primary source's own Gpp_500m composite file paths
#   gpp_tif_dates            Date vector, same length/order as gpp_tif -- each
#                            file's own composite date
#   gpp_dates                Date vector of every date to load (== sort(unique(gpp_tif_dates)))
#
# Returns a SpatRaster, one layer per gpp_dates entry, named by as.character(date).

load_gpp_composites_by_date <- function(gpp_tif, gpp_tif_dates, gpp_dates) {

  # Fast path: when every date maps to exactly one file, stack the file vector natively in one
  # terra::rast() call instead of looping per date (MODIS's own case). terra::rast() throws on a
  # mismatched-extent file vector rather than silently misaligning layers, so falling back to the
  # general path on ANY error here is safe.
  if (!anyDuplicated(gpp_tif_dates)) {
    fast_rast <- tryCatch({
      ordered <- gpp_tif[order(gpp_tif_dates)] # chronological file order
      r <- terra::rast(ordered)                # one layer per file, in that order
      names(r) <- as.character(sort(gpp_tif_dates))
      r
    }, error = function(e) NULL)
    if (!is.null(fast_rast)) return(fast_rast)
  }

  # General path -- a date spans more than one file (e.g. two adjacent
  # yearly VIIRS block folders both delivering the same boundary composite)
  # or per-date extents genuinely differ, so each date needs its own
  # merge/extend before the final stack.
  per_date_rast <- purrr::map(gpp_dates, function(d) {
    rasters <- lapply(gpp_tif[gpp_tif_dates == d], terra::rast)
    if (length(rasters) == 1) rasters[[1]] else Reduce(terra::merge, rasters)
  })

  full_ext <- Reduce(terra::union, lapply(per_date_rast, terra::ext)) # union extent across every date
  gpp_rast <- terra::rast(lapply(per_date_rast, terra::extend, y = full_ext)) # pad each date out to that shared extent, then stack
  names(gpp_rast) <- as.character(gpp_dates)
  gpp_rast
}

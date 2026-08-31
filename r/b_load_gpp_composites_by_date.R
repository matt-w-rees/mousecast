# Stack per-date GPP composites into one SpatRaster, one layer per date --
# mosaicking same-date files from different regions/sources together first
# where needed (a fast path below skips this entirely when it's not needed
# at all, e.g. MODIS's own single-file-per-date case -- see its own comment,
# and load_and_clamp_gpp_composites()'s header for the performance
# investigation behind it). Factored out of build_gpp_period_raster()
# (2026-08) as its own loading step, separate from masking/unit-conversion/
# aggregation. Used
# to also substitute in fill_gpp_fallback_dates()'s own output for any date
# the primary source didn't cover yet; that fallback mechanism (and its own
# fallback_rast_by_date argument here) was retired 2026-08 in favour of a
# monthly-grain merge instead -- see build_gpp_period_raster()'s own header.
#
# gpp_tif spans two products (MODIS and VIIRS, each downloaded as a single
# combined national AppEEARS task, see _targets.R) whose requested date
# ranges are non-overlapping by construction (MODIS's end_date sits one day
# before VIIRS's start_date), so for any given date there's only ever one
# file to use, not two competing satellites' independent estimates needing
# merged. The per-date merge/extend below is kept anyway as a defensive
# no-op for the single-file case (length(rasters) == 1), in case a date is
# ever split across more than one file again -- confirmed live this isn't
# just theoretical: a "correction years" file list (_targets.R) can
# genuinely hand two files for the same boundary date (one from each of two
# adjacent yearly VIIRS block folders, e.g. a Dec 31-anchored 8-day
# composite downloaded independently by both the just-frozen and the
# still-open block's own requests). Reduce(terra::merge, rasters), not
# do.call(terra::merge, rasters) -- confirmed live that do.call() with 2+
# SpatRasters can silently mis-dispatch to base::merge.default() instead of
# terra's own S4 method when called from inside a nested closure
# (purrr::map()'s own anonymous function here), raising "argument 'x' is
# missing" instead of merging; Reduce() calls terra::merge() pairwise with
# two explicit positional arguments each time, sidestepping that dispatch
# failure, and (unlike do.call()) is also correct for genuinely merging 3+
# same-date files, since terra::merge()'s own signature takes exactly two
# SpatRasters (x, y), not an arbitrary number.
#
# Dates covered by only one region are smaller than dates covered by both --
# every date is extended out to the shared union extent (NA outside whatever
# region(s) actually delivered it) so they can be stacked into one raster.
#
# Arguments:
#   gpp_tif                 primary source's own Gpp_500m composite file paths
#   gpp_tif_dates            Date vector, same length/order as gpp_tif -- each
#                            file's own composite date
#   gpp_dates                Date vector of every date to load (== sort(unique(gpp_tif_dates)))
#
# Returns a SpatRaster, one layer per gpp_dates entry, named by as.character(date).

load_gpp_composites_by_date <- function(gpp_tif, gpp_tif_dates, gpp_dates) {

  # Fast path: when every date maps to exactly one file, skip the general
  # per-date merge/extend loop below and let terra stack the file vector
  # natively in one call instead of ~1000s of separate R-level ones -- this
  # is MODIS's own case (a single combined national AppEEARS download, never
  # split across regions), confirmed live 2026-08 to be the dominant,
  # avoidable cost behind composite_gpp_rast_modis's ~2.5h runtime (see
  # load_and_clamp_gpp_composites()'s own header for the full investigation).
  # terra::rast() on a mismatched-extent file vector throws ("extents do not
  # match", confirmed live), never silently misaligns layers, so falling
  # back to the general path on ANY error here is safe, not just optimistic
  # -- covers a date genuinely split across files (this path is skipped
  # entirely then, via anyDuplicated()) as well as same-date-count-but-
  # different-extent files, however unlikely.
  if (!anyDuplicated(gpp_tif_dates)) {
    fast_rast <- tryCatch({
      ordered <- gpp_tif[order(gpp_tif_dates)]
      r <- terra::rast(ordered)
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

  full_ext <- Reduce(terra::union, lapply(per_date_rast, terra::ext))
  gpp_rast <- terra::rast(lapply(per_date_rast, terra::extend, y = full_ext))
  names(gpp_rast) <- as.character(gpp_dates)
  gpp_rast
}

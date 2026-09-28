# Load raw GPP composites from disk, clamp sentinel/fill codes and convert
# to a true daily rate -- everything that's safe and necessary to do per-
# composite-date BEFORE any bias-check/correction/splice needs a clean,
# comparable, correctly-scaled value to work with. Kept separate from
# build_gpp_period_raster()'s own final monthly stage -- see _targets.R's
# GPP section for where this is actually used: NASA's own bias-checking/
# correcting/splicing all happen on this function's own composite-grain
# output, not on a monthly one.
#
# Deliberately stops short of off-shore masking and period aggregation
# (finish_gpp_period_raster(), called separately, later, once) -- see that
# function's own header for why those two specifically are safe to defer.
# Daily-rate conversion is NOT deferred alongside them, even though it could
# be (the per-date scalar it applies is identical for every source on a
# shared composite date, so it cancels out of any ratio either way) --
# doing it here instead means every downstream bias-check/correction number
# is already in interpretable gC/m^2/day from the start, avoiding the exact
# unit-mismatch confusion (raw kg C/m^2/8-day vs. gC/m^2/day) that deferring
# conversion would otherwise risk in any downstream bias-check diagnostic.
#
# Performance: composite_gpp_rast_modis (_targets.R) clocks in at ~2.5h for
# MODIS's own ~1191-date archive if run single-threaded and non-branched
# (unlike VIIRS's equivalent, which branches per calendar-year block and so
# runs in ~1-2 minutes). A single file's own open+full-read cost is only
# ~0.3s (so raw I/O alone accounts for ~6 minutes of that, nowhere near
# 2.5h), and every MODIS composite's own extent is identical across every
# download batch -- so the real cost is terra's own threads defaulting to
# off (fixed here) plus load_gpp_composites_by_date()'s per-date R-level
# looping (map/merge/extend, one call per date) running even when nothing
# about a given date's file(s) actually needs it (fixed there, via a fast
# path -- see that function's own header). Both fixes are additive, not a
# rewrite of the underlying logic -- the general per-date path stays, still
# used whenever a date genuinely does span more than one file or a
# mismatched extent (VIIRS's own multi-region case).
#
# Arguments:
#   gpp_files   file paths under raw_data/predictor_variables/gpp (the whole
#               tracked folder(s) -- Gpp_500m_*.tif composites are picked out
#               by filename; Psn_QC_500m and supporting_files/ are ignored)
#   valid_max   sentinel/fill-code cutoff, kg C/m^2/8-day (default 3) -- see
#               clamp_gpp_sentinel_codes()
#
# Returns a SpatRaster, one layer per composite date (named
# as.character(date), sorted), values in gC/m^2/day, sentinel-clamped but
# NOT off-shore-masked -- or NULL if gpp_files has no Gpp_500m composites at all.

load_and_clamp_gpp_composites <- function(gpp_files, valid_max = 3) {

  # Let GDAL parallelise raster read/write across cores -- this terra
  # installation defaults to threads = 0 (off). Set here (not just once at
  # pipeline start-up) because this function is the one that actually runs
  # inside a crew worker process, and a worker's own terra package state
  # isn't inherited from the controller process that merely defines the
  # pipeline -- see this function's own header for the ~2.5h MODIS runtime
  # this (and load_gpp_composites_by_date()'s own fast path) was
  # investigated against.
  terra::terraOptions(threads = TRUE)

  # Restrict to the real Gpp_500m composite files -- gpp_files also carries
  # Psn_QC_500m and non-.tif support files (see this file's own header).
  gpp_tif       <- gpp_files[grepl("Gpp_500m.*\\.tif$", gpp_files)]
  # Each file's own composite date, parsed from its AppEEARS "doy<YYYY><DDD>" filename token.
  gpp_tif_dates <- as.Date(sub(".*doy([0-9]{4})([0-9]{3}).*", "\\1-\\2", gpp_tif), format = "%Y-%j")
  # One entry per distinct date, chronologically sorted -- a date can span more than one file (VIIRS's own multi-region case).
  gpp_dates     <- sort(unique(gpp_tif_dates))

  # No real composites to work with (e.g. an empty or non-GPP file list) -- NULL rather than erroring downstream on an empty raster.
  if (length(gpp_dates) == 0) {
    message("No Gpp_500m composites found in gpp_files -- returning NULL.")
    return(NULL)
  }

  # Merge each date's own file(s) into one multi-layer stack, one layer per composite date.
  gpp_rast <- load_gpp_composites_by_date(gpp_tif, gpp_tif_dates, gpp_dates)
  # Mask sensor sentinel/fill codes to NA (ocean not caught here -- see mask_gpp_offshore(), deferred to vi.).
  gpp_rast <- clamp_gpp_sentinel_codes(gpp_rast, valid_max)
  # Convert from raw kg C/m^2/8-day to a true mean daily rate (gC/m^2/day), returned as this function's own result.
  convert_gpp_to_daily_rate(gpp_rast, gpp_dates)
}

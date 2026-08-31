# Load raw GPP composites from disk, clamp sentinel/fill codes and convert
# to a true daily rate -- everything that's safe and necessary to do per-
# composite-date BEFORE any bias-check/correction/splice needs a clean,
# comparable, correctly-scaled value to work with (2026-08, factored out of
# build_gpp_period_raster() when the GPP pipeline needed a composite-grain
# stage distinct from its final monthly one -- see _targets.R's GPP section
# for where this is actually used: NASA's own bias-checking/correcting/
# splicing all happen on this function's own composite-grain output, not on
# a monthly one).
#
# Deliberately stops short of off-shore masking and period aggregation
# (finish_gpp_period_raster(), called separately, later, once) -- see that
# function's own header for why those two specifically are safe to defer.
# Daily-rate conversion is NOT deferred alongside them, even though it could
# be (the per-date scalar it applies is identical for every source on a
# shared composite date, so it cancels out of any ratio either way) --
# doing it here instead means every downstream bias-check/correction number
# is already in interpretable gC/m^2/day from the start, avoiding the exact
# unit-mismatch confusion (raw kg C/m^2/8-day vs. gC/m^2/day) an earlier
# version of this pipeline's own bias-check diagnostics ran into.
#
# Performance (2026-08): composite_gpp_rast_modis (_targets.R) was clocked at
# ~2.5h for MODIS's own ~1191-date archive, single-threaded and non-branched
# (unlike VIIRS's equivalent, which branches per calendar-year block and so
# runs in ~1-2 minutes). Diagnosed live, not guessed at: a single file's own
# open+full-read cost only ~0.3s (so raw I/O alone would be ~6 minutes for
# the whole archive, nowhere near 2.5h), and every MODIS composite's own
# extent was confirmed identical across every download batch fetched to
# date -- so the real cost was terra's own threads defaulting to off (fixed
# here) plus load_gpp_composites_by_date()'s per-date R-level looping
# (map/merge/extend, one call per date) running even when nothing about a
# given date's file(s) actually needed it (fixed there, via a fast path --
# see that function's own header). Both fixes are additive, not a rewrite of
# the underlying logic -- the general per-date path stays, still used
# whenever a date genuinely does span more than one file or a mismatched
# extent (VIIRS's own multi-region case).
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

  # Let GDAL parallelise raster read/write across cores -- confirmed live
  # 2026-08 this defaults to threads = 0 (off) in this terra installation.
  # Set here (not just once at pipeline start-up) because this function is
  # the one that actually runs inside a crew worker process, and a worker's
  # own terra package state isn't inherited from the controller process that
  # merely defines the pipeline -- see this function's own header for the
  # ~2.5h MODIS runtime this (and load_gpp_composites_by_date()'s own fast
  # path) was investigated against.
  terra::terraOptions(threads = TRUE)

  gpp_tif       <- gpp_files[grepl("Gpp_500m.*\\.tif$", gpp_files)]
  gpp_tif_dates <- as.Date(sub(".*doy([0-9]{4})([0-9]{3}).*", "\\1-\\2", gpp_tif), format = "%Y-%j")
  gpp_dates     <- sort(unique(gpp_tif_dates))

  if (length(gpp_dates) == 0) {
    message("No Gpp_500m composites found in gpp_files -- returning NULL.")
    return(NULL)
  }

  gpp_rast <- load_gpp_composites_by_date(gpp_tif, gpp_tif_dates, gpp_dates)
  gpp_rast <- clamp_gpp_sentinel_codes(gpp_rast, valid_max)
  convert_gpp_to_daily_rate(gpp_rast, gpp_dates)
}

# Build a seasonal or monthly mean GPP raster from 8-day Gpp_500m composites
# (MODIS and/or VIIRS, downloaded via r/b_download_gpp.R). A convenience
# wrapper around load_and_clamp_gpp_composites() (load, clamp, convert to
# daily rate) and finish_gpp_period_raster() (off-shore mask, aggregate to
# period means) -- see each function's own header for the split's rationale.
# The real NASA pipeline (_targets.R's "A) NASA") calls those two separately
# instead, with bias-checking/correcting/splicing in between; this one-shot
# wrapper is for callers that just want one raw source's own monthly raster
# directly (e.g. gpp_processing.qmd's report-only diagnostics).
#
# Arguments:
#   gpp_files                  file paths under raw_data/predictor_variables/gpp
#                               (the whole tracked folder(s), across both
#                               modis/ and viirs/ -- Gpp_500m_*.tif composites
#                               are picked out by filename; the Psn_QC_500m
#                               and supporting_files/ files are ignored)
#   aus_shp                    sf polygon object of the Australian coastline,
#                               passed straight through to mask_gpp_offshore()
#   summarise_by               "season" (default) or "month" -- which period
#                               8-day composites are collapsed into
#   min_composites_per_period  minimum 8-day composites required to summarise
#                               a period; defaults to 8 for summarise_by =
#                               "season" (a full season has ~11) or 3 for
#                               "month" (a full month has ~3-4) if left NULL
#   valid_max                  passed straight through to clamp_gpp_sentinel_codes()
#                               (default 3)
#
# Returns a SpatRaster, one layer per complete period (season_year_adj or
# month_year, see summarise_by), values in gC/m^2/day (matching PML-V2's own
# convention, build_pml_gpp_raster()) -- or NULL if gpp_files has no
# composites at all, or none of them add up to even one complete period (see
# aggregate_gpp_to_periods()).

build_gpp_period_raster <- function(gpp_files,
                                     aus_shp,
                                     summarise_by = c("season", "month"),
                                     min_composites_per_period = NULL,
                                     valid_max = 3) {

  summarise_by <- match.arg(summarise_by)

  # Composite grain: load, clamp sentinel/fill codes, convert to a daily rate.
  composite_rast <- load_and_clamp_gpp_composites(gpp_files, valid_max)
  if (is.null(composite_rast)) return(NULL) # no composites found

  # Off-shore mask, then aggregate composites into period (season/month) means.
  finish_gpp_period_raster(composite_rast, aus_shp, summarise_by, min_composites_per_period)
}

# Build a seasonal (Summer/Autumn/Winter/Spring) or monthly mean Gross
# Primary Productivity (GPP) raster from 8-day Gpp_500m composites, spanning
# two spliced satellite products (downloaded via r/b_download_gpp.R into
# raw_data/predictor_variables/gpp/{modis,viirs}/): MODIS (MOD17A2HGF.061,
# 2000-01-01+) and VIIRS (VNP17A2GF.002, 2012-01-01+) -- a well-established,
# NASA-endorsed continuous record (VIIRS's GPP algorithm is the designed
# operational continuation of MODIS's; confirmed live via
# appeears::rs_products()/rs_layers() that both products share identical
# layer names, fill-value codes, scale factor and 8-day cadence).
#
# A convenience wrapper (2026-08) around load_and_clamp_gpp_composites()
# (load, clamp sentinel/fill codes, convert to daily rate -- composite grain)
# and finish_gpp_period_raster() (off-shore mask, aggregate to period means)
# -- see each function's own header for the full rationale behind that
# split. The real NASA processing pipeline (_targets.R's GPP section, "A)
# NASA") calls those two functions separately, with bias-checking,
# correcting and splicing NASA's own sources happening in between, at
# composite grain -- this one-shot wrapper is for callers that just want a
# single raw-source's own monthly raster directly (e.g. the report-only
# national/paddock mean diagnostics, "x." in gpp_processing.qmd, which show
# each raw source on its own, not the corrected/spliced record).
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

  composite_rast <- load_and_clamp_gpp_composites(gpp_files, valid_max)
  if (is.null(composite_rast)) return(NULL)

  finish_gpp_period_raster(composite_rast, aus_shp, summarise_by, min_composites_per_period)
}

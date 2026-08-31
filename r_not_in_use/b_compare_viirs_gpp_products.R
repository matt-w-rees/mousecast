# SUPERSEDED (2026-08) by compare_gpp_rasters() (r/b_compare_gpp_rasters.R) --
# kept here for reference, no longer called from _targets.R. This function
# read raw composite files directly and compared at raw 8-day-composite
# grain; its own two callers (the GF/NGF and MODIS/VIIRS bias checks) now
# instead reuse the monthly rasters already built as their own targets
# (monthly_gpp_rast_gf_correction_years/monthly_gpp_rast_nongf_correction_years,
# monthly_gpp_rast_modis/monthly_gpp_rast_frozen) via the same generic
# compare_gpp_rasters() the PML check always used -- matching the temporal
# grain (monthly) and units (gC/m^2/day, via build_gpp_period_raster()'s own
# convert_gpp_to_daily_rate() step) that the actual pixel-scale corrections
# operate at, rather than a separate, finer-grained, differently-unit-scaled
# proxy for it. See r/b_compare_gpp_rasters.R's own header for the full
# investigation.
#
# General-purpose: compare two GPP composite sources at paddock locations, on
# the dates they both cover, to check whether it's safe to splice them
# together as one continuous covariate series -- built for (and named after)
# comparing VNP17A2GF.002 (gap-filled) against VNP17A2.002 (non-gap-filled,
# near-real-time), to check the VIIRS-internal gap-filled/near-real-time
# splice before deciding whether to use it for the most recent season a
# forecast needs (see the header note in r/b_download_gpp.R on why
# VNP17A2GF.002 alone lags real-time by up to a year). Kept generic enough
# to reuse for other two-source comparisons too -- e.g. spot-checking the
# MODIS (MOD17A2HGF.061) / VIIRS (VNP17A2GF.002) splice actually wired into
# the pipeline (see r/b_build_gpp_period_raster.R), by pointing gf_files/
# nongf_files at whichever two sources are being compared regardless of
# whether either is *actually* gap-filled. The risk being checked isn't
# whether splicing two sources is a reasonable strategy in general (it's
# standard practice for MODIS/VIIRS-family products) -- it's whether the two
# have a systematic bias relative to each other, which would show up as a
# discontinuity right at the splice point, exactly where a near-term
# forecast's covariate matters most.
#
# Deliberately compares at paddock centroids, not every raster pixel: an
# earlier whole-raster version of this (comparing all ~32M pixels across the
# full east extent) found a tiny median difference (~0.0002, negligible
# against typical GPP values ~0.01-0.05) but a surprisingly low correlation
# (~0.15) -- diluted by the vast non-agricultural majority of the raster
# (ocean, desert, urban), where both products report near-zero, mostly-noise
# GPP with no real signal to correlate. Paddock locations are both the
# actually-relevant comparison (this is what paddock_gpp_seasonal.R attaches
# to survey data) and far cheaper (point extraction vs. resampling
# full-extent rasters).
#
# Masking (mask_gpp_composites(), r/b_mask_gpp_composites.R -- ocean + valid_max
# sentinel-code masking together, the same shared step build_gpp_period_raster()
# uses) has no effect on this function's own output -- points are always
# paddock centroids, always on land, so an ocean mask never touches an
# extracted value -- but applying it anyway keeps this comparison honest about
# what counts as "valid" GPP, rather than silently drifting out of sync with
# the masking build_gpp_period_raster() actually applies to the record being
# checked (confirmed live 2026-08: this was a real, if currently harmless, gap
# before aus_shp was threaded through here).
#
# Arguments:
#   nongf_files   file paths for the first source's composites (originally
#                 VNP17A2.002; can be any GPP source) -- may include
#                 non-Gpp_500m files (e.g. Psn_QC_500m companions); these are
#                 filtered out internally, since a mixed list would otherwise
#                 let a wrong layer's file win an arbitrary [1]-index pick
#                 for a date covered by more than one file
#   gf_files      file paths for the second source's composites, covering the
#                 same area (originally VNP17A2GF.002; can be any GPP source)
#   points        sf point (or point-coordinate data.frame) object with
#                 longitude/latitude -- e.g. paddocks_sf's centroids
#   aus_shp       sf polygon object of the Australian coastline, passed
#                 straight through to mask_gpp_composites()
#   valid_max     sentinel/fill-code cutoff, kg C/m^2/8-day (default 3, same
#                 as build_gpp_period_raster()), passed straight through to
#                 mask_gpp_composites()
#
# Returns a tibble, one row per overlapping date: date, n_paddocks,
# bias_mean (gf - nongf, averaged over paired non-NA paddocks), bias_mad
# (median absolute bias, robust to outlier paddocks), correlation.

# Reads every file matching date `d`, merging when there's more than one --
# same handling as build_gpp_period_raster()'s own per_date_rast (see that
# function's header): a date can genuinely be covered by more than one file
# (e.g. two adjacent yearly block folders both requesting the same boundary
# date), so picking just the first would silently drop the other region's
# data instead of combining them.
.merge_same_date_files <- function(files, dates, d) {
  rasters <- lapply(files[dates == d], terra::rast)
  if (length(rasters) == 1) rasters[[1]] else Reduce(terra::merge, rasters)
}

compare_viirs_gpp_products <- function(nongf_files, gf_files, points, aus_shp, valid_max = 3) {

  # drop any non-Gpp_500m companion files (e.g. Psn_QC_500m) from each list
  nongf_files <- nongf_files[grepl("Gpp_500m.*\\.tif$", nongf_files)]
  gf_files    <- gf_files[grepl("Gpp_500m.*\\.tif$", gf_files)]

  # composite date, parsed from the AppEEARS "doy<YYYY><DDD>" filename token
  date_of <- function(files) as.Date(sub(".*doy([0-9]{4})([0-9]{3}).*", "\\1-\\2", files), format = "%Y-%j")

  nongf_dates <- date_of(nongf_files)
  gf_dates    <- date_of(gf_files)
  common      <- sort(intersect(nongf_dates, gf_dates)) # only dates both sources actually cover

  points_vect <- terra::vect(points, crs = "EPSG:4326")

  # one row per shared date -- extract both sources at the same points and compare
  purrr::map_dfr(common, function(d) {
    nongf <- .merge_same_date_files(nongf_files, nongf_dates, d) |> mask_gpp_composites(aus_shp, valid_max)
    gf    <- .merge_same_date_files(gf_files, gf_dates, d)       |> mask_gpp_composites(aus_shp, valid_max)

    nongf_vals <- terra::extract(nongf, points_vect, ID = FALSE)[[1]]
    gf_vals    <- terra::extract(gf, points_vect, ID = FALSE)[[1]]

    paired <- stats::na.omit(data.frame(nongf = nongf_vals, gf = gf_vals)) # keep only points with a value on both sides

    tibble::tibble(
      date        = d,
      n_paddocks  = nrow(paired),
      bias_mean   = mean(paired$gf - paired$nongf),               # signed average offset
      bias_mad    = stats::median(abs(paired$gf - paired$nongf)), # robust to outlier points
      correlation = stats::cor(paired$gf, paired$nongf)
    )
  })

}

# Apply _targets.R's gpp_pml_pixel_ratio_rast target's (build_gpp_pixel_ratio_raster(),
# r/b_build_gpp_pixel_ratio_raster.R) per-pixel, per-calendar-month ratio
# correction to PML-V2's raw GPP raster (build_pml_gpp_raster(), r/b_build_pml_gpp_raster.R),
# before splicing it onto this pipeline's own MOD17/VNP17 record for the
# pre-2000 period. Pixel-scale, not a single national factor -- see
# build_gpp_pixel_ratio_raster()'s own header for why (PML's raw value at
# survey paddocks differs hugely from its national mean, so a paddock-only
# correction applied everywhere doesn't generalise; this mirrors PML-V2's own
# published approach to its structurally identical AVHRR-to-MODIS/VIIRS
# splice, which is also pixel-scale, not a single/uniform factor).
#
# Each PML layer's own calendar month picks out the matching ratio LAYER
# (not a scalar) from ratio_correction_rast, then the two (same-extent,
# same-grid) rasters are multiplied elementwise -- a genuine per-pixel
# correction, not terra's usual length-nlyr() vector recycling. A ratio
# can't turn a non-negative PML value negative (unlike a subtraction).
#
# Arguments:
#   pml_gpp_rast            PML-V2's raw GPP SpatRaster, one layer per
#                           month_year (build_pml_gpp_raster()'s output)
#   ratio_correction_rast    SpatRaster, 12 layers named "1"-"12" (calendar
#                           month), one our/pml ratio per pixel per month
#                           (_targets.R's gpp_pml_pixel_ratio_rast target,
#                           build_gpp_pixel_ratio_raster()'s output) --
#                           same grid as pml_gpp_rast
#
# Returns a SpatRaster, same shape/layer-names/grid as pml_gpp_rast, ratio-corrected.

build_gpp_historic_corrected_raster <- function(pml_gpp_rast, ratio_correction_rast) {

  months <- as.integer(sub("_.*", "", names(pml_gpp_rast)))

  missing_months <- setdiff(months, as.integer(names(ratio_correction_rast)))
  if (length(missing_months) > 0) {
    stop("ratio_correction_rast has no entry for month(s): ", paste(missing_months, collapse = ", "))
  }

  ratio_matched <- ratio_correction_rast[[match(months, as.integer(names(ratio_correction_rast)))]]

  pml_gpp_rast * ratio_matched
}

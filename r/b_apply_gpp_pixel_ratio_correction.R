# Apply a per-pixel, per-calendar-month ratio correction
# (build_gpp_pixel_ratio_raster()'s output) to a raw GPP raster, elementwise.
# Pixel-scale, not a single national factor -- see
# build_gpp_pixel_ratio_raster()'s own header for why (a source's raw value
# at survey paddocks can differ hugely from its national mean, so a
# paddock-only correction applied everywhere doesn't generalise; this
# mirrors PML-V2's own published approach to its structurally identical
# AVHRR-to-MODIS/VIIRS splice, which is also pixel-scale, not a single/
# uniform factor).
#
# Each layer's own calendar month picks out the matching ratio LAYER (not a
# scalar) from ratio_correction_rast, then the two (same-extent, same-grid)
# rasters are multiplied elementwise -- a genuine per-pixel correction, not
# terra's usual length-nlyr() vector recycling. A ratio can't turn a
# non-negative GPP value negative (unlike a subtraction).
#
# Generic across every product this pipeline corrects this way (2026-08 --
# renamed from build_gpp_historic_corrected_raster() once it gained a second
# real caller, VIIRS non-gap-filled, alongside its original PML one; the
# function body itself never had any product-specific logic, only its old
# name did): PML-V2's raw GPP raster gets ratio-corrected before merging with
# this pipeline's own MOD17/VNP17 record (its pre-2000 portion is the only
# part that ever wins that merge); VIIRS non-gap-filled gets ratio-corrected
# before merging with gap-filled VIIRS the same way (its own months where
# gap-filled hasn't published yet are the only ones that ever win that
# merge) -- see _targets.R's "v. Compute pixel-scale corrections" and
# "vi. Splice into one continuous stack".
#
# Arguments:
#   raw_rast                 raw GPP SpatRaster to correct, one layer per
#                           period (see months_of) -- e.g.
#                           build_pml_gpp_raster()'s or
#                           build_gpp_period_raster()'s month_year-named
#                           output, or load_and_clamp_gpp_composites()'s own
#                           composite-grain (date-named) output
#   ratio_correction_rast    SpatRaster, 12 layers named "1"-"12" (calendar
#                           month), one ratio per pixel per month
#                           (build_gpp_pixel_ratio_raster()'s output) --
#                           same grid as raw_rast
#   months_of                function mapping raw_rast's own layer names to
#                           their calendar month (1-12) -- default parses
#                           the "<month>_<year>" convention; a composite-
#                           grain caller (date-named layers) passes
#                           something like function(x) lubridate::month(as.Date(x))
#                           instead -- 2026-08, same reason as
#                           build_gpp_pixel_ratio_raster()'s own months_of
#                           argument, see that function's header.
#
# Returns a SpatRaster, same shape/layer-names/grid as raw_rast, ratio-corrected.

apply_gpp_pixel_ratio_correction <- function(raw_rast, ratio_correction_rast,
                                              months_of = function(x) as.integer(sub("_.*", "", x))) {

  months <- months_of(names(raw_rast)) # each layer's own calendar month

  # ratio_correction_rast must have an entry for every month raw_rast actually needs
  missing_months <- setdiff(months, as.integer(names(ratio_correction_rast)))
  if (length(missing_months) > 0) {
    stop("ratio_correction_rast has no entry for month(s): ", paste(missing_months, collapse = ", "))
  }

  # build a same-length stack of ratio LAYERS, one per raw_rast layer, picking out
  # that layer's own calendar month's ratio (so e.g. every January layer gets
  # January's own ratio layer, in the same order as raw_rast)
  ratio_matched <- ratio_correction_rast[[match(months, as.integer(names(ratio_correction_rast)))]]

  raw_rast * ratio_matched # elementwise per-pixel correction, not a scalar multiply
}

# General-purpose: compute a per-pixel, per-calendar-month ratio between two
# same-grid GPP rasters over whatever month_year layers they share, instead
# of a single national/scalar factor calibrated at a handful of points (e.g.
# structured-survey paddocks) and then applied everywhere. Built for (and
# named after) this pipeline's two splice corrections that need it --
# gpp_pml_pixel_ratio_rast (PML-V2 vs MOD17/VNP17, pre-2000 splice) and
# gpp_gf_pixel_ratio_rast (VIIRS gap-filled vs non-gap-filled, near-real-time
# splice) -- but generic enough to reuse for any two-source GPP ratio.
#
# Why pixel-scale, not a single national factor: confirmed empirically this
# session that PML's own raw value at survey paddocks (productive
# grain-growing land) can differ hugely from its national mean (e.g. August:
# ~5.9 vs ~1.6 gC/m^2/day) -- a single paddock-calibrated correction, applied
# to the whole national raster, necessarily assumes that gap generalises
# everywhere, which it doesn't. gpp_rolling12_raster/gpp_anomaly_raster
# operate over the full raster (the anomaly baseline is a 3x3 focal smooth,
# so even a paddock's own value can be influenced by nearby non-paddock
# cells), so a correction that's only right at paddocks isn't necessarily
# right in their own neighbourhood either. This mirrors PML-V2's own
# published approach to its structurally identical AVHRR-to-MODIS/VIIRS
# splice (same dataset this pipeline uses, just one internal generation
# further back): Xu, Zhang et al. (essd-2026-94, PML-V2.2) explicitly
# rejected a single/unidirectional scaling correction there too
# ("unidirectional scaling might fail to perfectly preserve both identical
# means and consistent long-term trends") in favour of a per-pixel
# ("pixel-scale bidirectional consolidation") correction over a stable
# overlap window.
#
# max_ratio clamps both directions (1/max_ratio to max_ratio) -- guards
# against a blown-up ratio at low-signal pixels (e.g. near-desert cells
# where mean_denominator is close to zero) where a tiny denominator would
# otherwise produce an extreme correction; genuine 0/0 pixels (both sides
# validly zero, e.g. a pixel both sources agree has no productivity) are set
# to a neutral ratio of 1 (no correction) rather than left as NaN.
#
# A pixel with NO valid numerator data anywhere in the shared overlap gets NA
# here instead, deliberately NOT the same neutral-1 treatment -- confirmed
# live this session that PML-V2's own raw file has no masking applied at all
# (just a bounding-box crop), while this pipeline's own MOD17/VNP17 record is
# masked to the Australian coastline plus a fill/sentinel-code threshold
# (build_seasonal_gpp_raster()'s aus_shp/valid_max arguments) -- so PML
# retains valid values across tens of thousands of cells (~39,000 of ~55,000
# nationally, confirmed against the actual 1999/2000 splice) that MOD17/VNP17
# has zero data for at all. Defaulting those cells to ratio = 1 would leave
# PML's own raw, uncorrected value sitting there in the final corrected
# splice -- invisible in any paddock-level check (paddocks always have valid
# data on both sides), but it drags a national-mean view of the splice
# hugely out of line right at the 2000 boundary, since the two eras of the
# record end up averaged over two different geographic footprints. NA here
# instead means: this pixel gets excluded from the corrected output entirely
# (NA * anything = NA downstream in build_gpp_historic_corrected_raster()),
# so both eras of the splice end up sharing the same valid-pixel footprint.
#
# Arguments:
#   numerator_rast    the source whose typical level the correction should
#                     shift the denominator TOWARDS -- e.g. this pipeline's
#                     own MOD17/VNP17-based GPP (for the PML splice) or
#                     gap-filled VIIRS (for the near-real-time splice), one
#                     layer per month_year
#   denominator_rast  the source actually being corrected -- e.g. PML-V2 or
#                     non-gap-filled VIIRS, one layer per month_year, same
#                     grid as numerator_rast
#   max_ratio         both-direction clamp bound (default 10) -- see above
#
# Returns a SpatRaster, one layer per calendar month present in both inputs
# (named "1"-"12"), one numerator/denominator ratio value per pixel per
# month, averaged across every year (or, for a single-year comparison, just
# that year) both sides cover. NA wherever numerator_rast has no valid data
# at all for that pixel/month (see above) -- not necessarily the same
# footprint as denominator_rast's own valid cells.

build_gpp_pixel_ratio_raster <- function(numerator_rast, denominator_rast, max_ratio = 10) {

  common <- intersect(names(numerator_rast), names(denominator_rast))
  if (length(common) == 0) {
    stop("numerator_rast and denominator_rast share no month_year layers in common.")
  }
  months <- as.integer(sub("_.*", "", common))

  ratio_layers <- purrr::map(sort(unique(months)), function(m) {
    layer_names <- common[months == m]
    mean_numerator   <- terra::mean(numerator_rast[[layer_names]], na.rm = TRUE)
    mean_denominator <- terra::mean(denominator_rast[[layer_names]], na.rm = TRUE)

    # Captured from mean_numerator BEFORE any of the mutations below -- once a
    # value is written into a SpatRaster as NA, it's indistinguishable from a
    # "genuine" NaN produced by a later 0/0 division (confirmed live: is.nan()
    # can't tell them apart after the fact), so the "no numerator data at all"
    # condition has to be captured as its own boolean now, then applied last.
    no_numerator_data <- is.nan(mean_numerator)

    ratio <- mean_numerator / mean_denominator
    ratio <- terra::ifel(is.nan(ratio), 1, ratio) # genuine 0/0 (both sides validly zero) -> neutral, no correction
    ratio <- terra::ifel(no_numerator_data, NA, ratio) # no correction is meaningful with zero numerator data -- exclude, don't default to neutral
    terra::clamp(ratio, lower = 1 / max_ratio, upper = max_ratio, values = TRUE)
  })

  ratio_rast <- terra::rast(ratio_layers)
  names(ratio_rast) <- as.character(sort(unique(months)))
  ratio_rast
}

# Compare two GPP rasters -- already on the same grid, temporal grain and
# measurement units -- at paddock locations, on whichever periods (layer
# names, e.g. "month_year") both actually share. Getting the two inputs onto
# that common footing is each CALLER's own job first (resample_to_grid() for
# a grid mismatch, build_gpp_period_raster() for units/aggregation), not
# this function's -- it only ever does the comparison itself.
#
# Generalised (2026-08) from an earlier PML-specific version,
# check_gpp_pml_bias(), once a second, separate implementation
# (compare_viirs_gpp_products(), built for the VIIRS gap-filled/non-gap-filled
# splice) turned out to be solving the exact same problem a different way --
# reading raw composite files directly and comparing at raw 8-day-composite
# grain, rather than the monthly grain its own correction step
# (build_gpp_pixel_ratio_raster()) actually operates at. That meant the
# gap-filled/non-gap-filled diagnostic was silently re-reading and re-masking
# files from scratch at a finer grain than its own correction ever used,
# instead of reusing the monthly rasters (monthly_gpp_rast_gf_correction_years/
# monthly_gpp_rast_nongf_correction_years) already built as their own targets
# right next to it -- and was on a different unit convention entirely (raw
# kg C/m^2/8-day vs. check_gpp_pml_bias()'s gC/m^2/day), inviting exactly the
# kind of apples-to-oranges misreading that surfaced this whole investigation.
# compare_viirs_gpp_products() is retired to r_not_in_use/ -- see its own
# header there for the full detail.
#
# Also retired check_gpp_pml_bias()'s own inline crop-then-resample block --
# resample_to_grid() (r/b_resample_to_grid.R) already generalised that exact
# logic out for reuse elsewhere in this pipeline, but check_gpp_pml_bias()
# itself was never updated to actually call it. Every caller now resamples
# explicitly, in _targets.R, before this function ever runs.
#
# bias_mean/bias_mad are always "b minus a" -- e.g. rast_a = this pipeline's
# own MOD17/VNP17 record, rast_b = PML gives a positive bias_mean wherever
# PML reads higher. Each call site picks its own a/b order for whichever
# sign reads most naturally in context; see each target's own comment in
# _targets.R.
#
# Arguments:
#   rast_a, rast_b   SpatRaster, one layer per period (month_year, e.g.
#                    "7_2026", for a monthly-grain caller; a plain date, e.g.
#                    "2026-07-09", for a composite-grain one -- this
#                    function doesn't care which, it just matches by
#                    whatever name rast_a/rast_b's own layers share),
#                    already on the same grid and in the same units.
#   points           sf point (or point-coordinate data.frame) object with
#                    longitude/latitude columns -- e.g. structured_survey_points.
#
# Returns a tibble, one row per shared period: period (rast_a/rast_b's own
# shared layer name, whatever convention it happens to be -- grain-neutral,
# 2026-08, once composite-grain callers meant "month_year" could no longer
# be assumed), n_paddocks, bias_mean (mean(b - a) over paired non-NA
# paddocks), bias_mad (median absolute bias, robust to outlier paddocks),
# correlation, a_mean and b_mean (each side's own mean over the same paired
# paddocks).

compare_gpp_rasters <- function(rast_a, rast_b, points) {

  points_vect <- terra::vect(points, crs = "EPSG:4326")

  # only periods both rasters actually cover can be compared
  common <- intersect(names(rast_a), names(rast_b))
  if (length(common) == 0) {
    stop("rast_a and rast_b share no layers in common.")
  }

  # Extract every shared period in ONE terra::extract() call per side, not one call per period
  # (the original approach) -- each terra::extract() call carries real fixed overhead re-opening/
  # seeking the file-backed raster, so paying that overhead once per shared period rather than
  # once overall dominates the runtime for a comparison with many shared periods (confirmed live:
  # collapsed gpp_modis_bias_check's own 138-period comparison from ~1h16m to a couple of minutes,
  # identical numeric output). terra::extract() on a multi-layer SpatRaster already returns one
  # column per layer, named by layer, so a_vals[[p]]/b_vals[[p]] below picks out the same period's
  # column from each side's one-shot extraction.
  a_vals <- terra::extract(rast_a[[common]], points_vect, ID = FALSE)
  b_vals <- terra::extract(rast_b[[common]], points_vect, ID = FALSE)

  # one row per shared period -- pair up that period's column from each side and compare
  purrr::map_dfr(common, function(p) {
    paired <- stats::na.omit(data.frame(a = a_vals[[p]], b = b_vals[[p]])) # keep only points with a value on both sides

    tibble::tibble(
      period      = p,
      n_paddocks  = nrow(paired),
      bias_mean   = mean(paired$b - paired$a),                                        # signed average offset
      bias_mad    = stats::median(abs(paired$b - paired$a)),                           # robust to outlier paddocks
      correlation = if (nrow(paired) >= 2) stats::cor(paired$a, paired$b) else NA_real_,
      a_mean      = mean(paired$a),
      b_mean      = mean(paired$b)
    )
  })

}

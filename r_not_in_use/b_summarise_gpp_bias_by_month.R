# Aggregate a per-observation GPP bias comparison to a per-calendar-month
# correction table -- shared by _targets.R's gpp_pml_bias_correction (PML-V2
# vs MOD17/VNP17, from check_gpp_pml_bias()'s own gpp_pml_bias_check target)
# and gpp_gf_bias_correction (gap-filled vs non-gap-filled VIIRS, from
# compare_viirs_gpp_products()'s own gpp_gf_bias_check target): both need the
# exact same "average bias_mean by calendar month" step despite comparing
# different product pairs via different upstream logic (check_gpp_pml_bias()
# resamples onto PML's coarse grid before comparing at paddock points, since
# PML and MOD17/VNP17 are different resolutions; compare_viirs_gpp_products()
# compares two same-resolution VIIRS products directly, no resampling needed)
# -- only this shared aggregation step is factored out, not the comparisons
# themselves. (r_not_in_use/b_compute_gpp_pml_bias_correction.R and
# r_not_in_use/b_compute_gpp_gf_bias_correction.R are thin wrappers combining
# a comparison + this aggregation in one call; _targets.R now calls both
# steps directly instead, so the raw per-observation comparison stays
# available as its own target -- see those files' own headers.)
#
# A single flat correction would risk under/over-correcting whichever season
# the thing being corrected actually falls in -- confirmed seasonally
# structured for PML vs MOD17/VNP17 (~1.6-2 gC/m^2/day in autumn/winter up to
# ~5-7 in spring); not yet separately confirmed for gap-filled vs non-gap-
# filled VIIRS, but the same monthly granularity costs nothing extra here.
#
# Arguments:
#   per_obs  tibble with a bias_mean column, plus EITHER a month_year column
#            ("<month>_<year>", check_gpp_pml_bias()'s own convention) OR a
#            date column (compare_viirs_gpp_products()'s own convention) --
#            whichever is present is used to derive each row's calendar month
#
# Returns a tibble, one row per calendar month (1-12) with at least one
# observation: month, bias_correction (mean bias_mean across every
# observation for that month).

summarise_gpp_bias_by_month <- function(per_obs) {

  if ("month_year" %in% names(per_obs)) {
    per_obs <- dplyr::mutate(per_obs, month = as.integer(sub("_.*", "", month_year)))
  } else if ("date" %in% names(per_obs)) {
    per_obs <- dplyr::mutate(per_obs, month = lubridate::month(date))
  } else {
    stop("per_obs must have either a month_year or date column.")
  }

  per_obs |>
    dplyr::group_by(month) |>
    dplyr::summarise(bias_correction = mean(bias_mean), .groups = "drop") |>
    dplyr::arrange(month)
}

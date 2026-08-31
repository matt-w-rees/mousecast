# Aggregate check_gpp_pml_bias()'s per-month_year pml_mean/our_mean columns
# to a per-calendar-month ratio (our_mean / pml_mean) -- the actual
# correction build_gpp_historic_corrected_raster() applies to PML-V2's
# pre-2000 splice, replacing the flat additive correction
# (summarise_gpp_bias_by_month()'s bias_correction, still kept as its own
# target for the diagnostic figures in covariate_summary.qmd) that used to
# be applied there.
#
# Why a ratio, not an additive offset: confirmed empirically this session
# that PML's own seasonal cycle is much flatter than MOD17/VNP17's real one
# (PML's own monthly mean barely varies, ~1-2 gC/m^2/day year-round) while
# the calendar-month bias between them swings far more (~0.3 in autumn up to
# ~2.85 in August/September, i.e. bigger than PML's own typical value that
# month). Subtracting that swing from PML's comparatively flat signal
# doesn't recover MOD17/VNP17's shape -- it drives whichever months have the
# largest correction deeply negative (a real GPP can't be negative), because
# an additive correction implicitly assumes both sides share the same
# seasonal amplitude and differ only by a constant level shift, which isn't
# true here. A multiplicative correction rescales PML's own (compressed)
# seasonal signal proportionally instead: a near-zero PML month stays near
# zero rather than being pushed negative, and a month where PML
# under-represents the true growing-season peak gets scaled up towards it
# instead of receiving the same flat subtraction as every other month.
#
# Ratio of aggregated means (mean(our_mean) / mean(pml_mean), not a mean of
# each year's own our_mean/pml_mean ratio) -- more robust to any single
# low-PML year dominating the correction via a blown-up ratio, and
# consistent with how summarise_gpp_bias_by_month() aggregates bias_mean
# (average the components across years, not the derived per-year quantity).
#
# Arguments:
#   per_obs  tibble with pml_mean and our_mean columns, plus a month_year
#            column ("<month>_<year>") -- check_gpp_pml_bias()'s own output
#            (_targets.R's gpp_pml_bias_check target)
#
# Returns a tibble, one row per calendar month (1-12) with at least one
# observation: month, ratio_correction (mean(our_mean) / mean(pml_mean)
# across every observation for that month).

summarise_gpp_ratio_by_month <- function(per_obs) {

  per_obs |>
    dplyr::mutate(month = as.integer(sub("_.*", "", month_year))) |>
    dplyr::group_by(month) |>
    dplyr::summarise(ratio_correction = mean(our_mean) / mean(pml_mean), .groups = "drop") |>
    dplyr::arrange(month)
}

# Centring and scaling constants for a set of covariates: each one's mean and standard deviation in
# `data` (missing values ignored). build_hmm_data() computes these once from the model data and keeps
# them with it (attr(hmm_data, "covariate_scaling")); fit_plague_hmm() copies them onto the fitted
# model, so any later data (prediction grids, hindcasts, report curves) is scaled with exactly the
# constants the model was fitted with, however much new data has arrived since.
#
# Arguments:
#   data  a data frame holding every covariate in vars, on its original scale
#   vars  character vector of covariate names to scale
#
# Returns a tibble with one row per covariate: variable, centre (mean) and scale (standard deviation).
covariate_scaling <- function(data, vars) {
  tibble::tibble(
    variable = vars,
    centre   = vapply(vars, function(v) mean(data[[v]], na.rm = TRUE), numeric(1)),
    scale    = vapply(vars, function(v) stats::sd(data[[v]], na.rm = TRUE), numeric(1))
  )
}

# Adds a centred and scaled copy of each covariate in `scaling` (covariate_scaling()'s output) to
# `data`, named "<covariate>_scaled": (value - centre) / scale. The original column is kept, so plots
# and rasters stay in real units and only the model formulas use the "_scaled" names.
#
# Scaling stops a covariate's units setting the size of its coefficient (e.g. soil moisture's
# 0-0.8 range gave coefficients around -5), puts every level covariate on a comparable per-SD scale,
# and centres covariates so the intercepts describe an average season rather than a covariate value
# of zero. Centring matters most for a covariate with a zone-varying random slope (rainfall whiplash):
# uncentred, its slope deviations partly duplicate the zone intercept deviations.
#
# Arguments:
#   data     a data frame holding each scaled covariate's original column (a covariate missing from
#            data is skipped, so a prediction grid only needs the covariates its formulas use)
#   scaling  covariate_scaling()'s output -- the fitted model's own constants, never recomputed from
#            new data
#
# Returns data with the "_scaled" columns added (or overwritten).
scale_covariates <- function(data, scaling) {
  for (i in seq_len(nrow(scaling))) {
    v <- scaling$variable[i]
    if (v %in% names(data)) {
      data[[paste0(v, "_scaled")]] <- (data[[v]] - scaling$centre[i]) / scaling$scale[i]
    }
  }
  data
}

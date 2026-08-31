# Apply the fitted plague HMM's covariate-to-transition-probability
# relationship to the gridded covariate surface, producing a continuous
# "risk of entering plague next season" map across the zones the model was
# actually fit to -- not the whole national grid. CLAUDE.md's own Section D
# design describes generalising "to wherever the covariates exist", but that
# means un-monitored PADDOCKS within a modelled zone, not extrapolating
# beyond the zones used to fit the transition relationship at all (e.g. to
# Western Australia, whose climate/cropping calendar differs enough that
# CLAUDE.md itself calls out separately -- see the project overview). Masked
# to fit_zones (2026-08) after an initial unmasked version extrapolated into
# WA and produced values there with no calibration basis whatsoever.
#
# Uses hmmTMB's own HMM$predict(what = "tpm", newdata = ...) rather than
# manually re-deriving the linear predictor/inverse-link -- this is the
# package's purpose-built mechanism for evaluating a fitted model at new
# (out-of-sample) covariate values, confirmed via its own documentation.
#
# newdata needs a season column too (2026-08, once burrow_count's own
# detection formula went in, r/d_fit_plague_hmm.R) even though only the
# transition-side prediction ("tpm") is actually requested here -- confirmed
# live that $predict() validates every formula's covariates are present,
# observation formulas included, not just the ones the requested "what"
# actually uses. season is derived from period itself via season_info()
# (r/a_season_info.R, the same month->season convention used everywhere else
# in this pipeline), as a factor matching hmm_data's own levels exactly --
# mgcv-based prediction needs the same factor levels the model was fit with,
# not just the same values. (An era column was briefly required here too,
# 2026-08, while trap_count had its own ~era formula -- reverted the same
# day, see r/d_fit_plague_hmm.R's header, so no longer needed.)
#
# Every OTHER observation-formula covariate (2026-08, ADOPTED -- fixes a real, live bug found the
# same day) is now auto-filled by introspecting the fitted model itself, not hand-maintained here --
# the previous version hand-listed only log_burrow_metres_searched/log_trap_effort as hardcoded
# placeholder constants, and silently fell out of sync when burrow_count's own formula grew
# gpp_finescale/season:summer_zone_ne (r/d_fit_plague_hmm.R's "third pass" era, well before this
# fix) -- confirmed live this broke $predict() outright ("object 'gpp_finescale' not found").
# plague_hmm$obs()$formulas() returns the model's own real nested formula list (stream -> param ->
# state -> formula); plague_hmm$obs()$data() returns its own stored training data -- every
# covariate referenced there that isn't already one of the REAL per-cell columns above (season,
# ae_zone) gets its own training mean (numeric) or reference factor level (factor) instead. None of
# this affects the "tpm" quantity actually being predicted -- only transition-formula covariates do
# -- so any valid, in-range, non-NA value is correct, not just convenient; using the live training
# mean rather than a hardcoded literal also means this can never again go numerically stale the way
# the old log(349)/log(67) constants could have. This generalises automatically to any future
# observation-formula covariate too -- no more manual updates needed here when fit_plague_hmm()'s
# own formulas change, since there is no separate list left to fall out of sync.
#
# summer_zone_ne is the one exception handled explicitly rather than via the generic auto-fill --
# cheap to derive for real from the per-cell ae_zone already computed below (identical logic to
# r/d_build_hmm_data.R's own construction), so there's no reason to settle for a placeholder when
# the real value is already implied by data already in hand.
#
# A real (not placeholder) covariate genuinely missing after all this -- e.g. a transition-formula
# covariate whose raster argument was simply never supplied -- now fails loudly and specifically
# right here, rather than several layers down inside hmmTMB's own internal mgcv refit with an opaque
# "object not found" error nowhere near the actual cause.
#
# gpp_raster/soil_moisture_raster added 2026-08 alongside rain_whiplash_raster
# in the transition formula. Their own source rasters (gpp_rolling_raster_6,
# soil_moisture_raster_coarse -- _targets.R) are already trimmed to
# season-end months only (Feb/May/Aug/Nov, r/b_trim_to_season_end_months.R),
# unlike rain_whiplash_raster's own monthly grain -- so the "latest shared
# period" below has to be found across all three rasters' own layer name
# sets, not just assumed to be rain_whiplash's own latest month.
#
# min_temp_raster added 2026-08 (plague_hmm_fourth_pass, r/d_fit_plague_hmm.R) -- uses
# min_temp_raster_coarse (_targets.R), NOT min_temp_seasonal_raster, even though the latter is what
# the training covariate ("min_temp" column, r/d_build_hmm_data.R via paddock_season_covs) actually
# is (a genuine 3-month within-season mean, build_seasonal_raster()). min_temp_seasonal_raster's own
# layer names are season-grain ("Winter-2025", period_col = "season_year_adj" in _targets.R's own
# attach_raster_covs() call) -- incompatible with the "<month>_<year>" naming every other raster
# here uses, which the shared-period intersection below depends on. min_temp_raster_coarse instead
# gives the single raw month's own mean-minimum-temperature value at whichever month prediction
# lands on (matching rain_whiplash_raster's own monthly grain), a real but minor definitional
# mismatch from the true 3-month seasonal mean it's standing in for -- accepted rather than solved
# properly (e.g. building a genuine min_temp rolling-3-month-then-season-end-trimmed raster,
# mirroring gpp_rolling6's own construction) because minimum temperature changes smoothly across a
# season (unlike rainfall or GPP), so one month's own value is a reasonable proxy for that season's
# mean, not a materially different quantity.
#
# newdata also needs log_burrow_metres_searched and log_trap_effort (2026-08, once burrow_count's
# and trap_count's own search-effort offsets went in, r/d_fit_plague_hmm.R) for the same "every
# formula's covariates must be present" reason as season -- each fixed at its own average-effort
# scenario (mean burrow_metres_searched/trap_effort across hmm_data_first_pass, ~349m / ~67
# trap-nights), purely as a placeholder: both streams' mean formulas are entirely observation-side,
# so these values have zero effect on the transition-side ("tpm") prediction actually being
# computed here, the same way season's own placeholder value doesn't. (A genuine bug lived here
# briefly, 2026-08: paddock-seasons with burrow_transects_surveyed == 0 produce log(0) = -Inf, not
# NA -- is.na(-Inf) is FALSE, so it silently survived hmmTMB's own internal NA-fill and crashed
# mgcv::gam() specifically inside $predict(newdata = ...)'s internal dummy-response refit on the
# full training data, with an opaque "NA/NaN/Inf in foreign function call" error. r/d_build_hmm_data.R
# now NAs those rows out before taking log(), fixed at the source rather than patched here --
# trap_effort got the same guard pre-emptively even though number_functional_traps_session == 0
# never actually co-occurs with a non-NA trap_count in the current data.)
#
# newdata also needs a real (non-placeholder) ae_zone value per cell (2026-08, once the second-pass
# model's own s(ae_zone, bs = "re") transition term went in, r/d_fit_plague_hmm.R) -- unlike
# season/log_burrow_metres_searched/log_trap_effort above, this one genuinely affects the
# transition-side prediction (it's the whole point of the random effect), so it can't be a shared
# placeholder constant across every cell -- each cell needs its OWN zone. Built via
# terra::rasterize(fit_zones_sf, field = "ae_zone") onto the covariate grid as a template (same
# extent/resolution/crs, so its own as.data.frame(xy = TRUE, na.rm = FALSE) traverses cells in the
# identical row order cell_df already uses) rather than a per-cell sf::st_join -- avoids the
# extract()-style performance issue CLAUDE.md documents elsewhere for large per-polygon
# spatial joins, and this is the same "rasterize once, read off in stable row order" pattern this
# function already relies on for covariate_stack itself. Factor levels set explicitly from
# fit_zones_sf's own ae_zone column (exactly the zones the model was fit to, since fit_zones_sf is
# itself derived from hmm_data_first_pass -- _targets.R) rather than left to whatever happened to
# be sampled onto the raster, matching season's own explicit-levels treatment above for the same
# "mgcv needs the same factor levels as training" reason. This function stays usable for models
# without a zone term too (e.g. plague_hmm_first_pass) -- an unused newdata column is harmless,
# hmmTMB simply never reads it.
#
# The mapped quantity is specifically Pr(state 1 -> state 2), i.e. the
# probability a currently-not-plague paddock enters the plague state next
# season given this season's covariates -- the direct "early warning" signal
# CLAUDE.md's own stated purpose calls for, not e.g. the stationary
# distribution (a longer-run equilibrium quantity, a different question).
#
# Arguments:
#   plague_hmm            a fitted HMM object (r/d_fit_plague_hmm.R) whose
#                          transition formula uses exactly rain_whiplash_trough_to_peak
#                          (+ its own ae_zone interaction) + gpp_rolling6 +
#                          soil_moisture + min_temp -- rename the raster
#                          arguments below if a differently-specified model
#                          is ever passed in
#   rain_whiplash_raster   rain_whiplash_trough_to_peak (_targets.R) -- one
#                          layer per period, named "<month>_<year>"
#   gpp_raster             gpp_rolling_raster_6 (_targets.R) -- season-end-month
#                          grain only, same "<month>_<year>" layer naming
#   soil_moisture_raster   soil_moisture_raster_coarse (_targets.R) -- season-end-month
#                          grain only, same "<month>_<year>" layer naming
#   min_temp_raster        min_temp_raster_coarse (_targets.R) -- full monthly
#                          grain, same "<month>_<year>" layer naming; see
#                          header for why this (not min_temp_seasonal_raster)
#   fit_zones_sf           sf polygons of exactly the ae_zone(s) the model
#                          was fit to -- prediction is masked to this extent,
#                          nothing extrapolated beyond it
#   period                 which layer to predict for, e.g. "5_2026"; NULL
#                          (default) uses the chronologically latest period
#                          common to all four rasters, for a "current risk"
#                          snapshot
#
# Returns a single-layer SpatRaster of Pr(state 1 -> state 2), NA wherever
# any input covariate is NA or the cell falls outside fit_zones_sf.

predict_plague_risk <- function(plague_hmm, rain_whiplash_raster, gpp_raster, soil_moisture_raster,
                                 min_temp_raster, fit_zones_sf, period = NULL) {

  if (is.null(period)) {
    shared_periods <- Reduce(intersect, list(names(rain_whiplash_raster), names(gpp_raster), names(soil_moisture_raster), names(min_temp_raster)))
    period_month <- as.integer(sub("_.*", "", shared_periods))
    period_year  <- as.integer(sub(".*_", "", shared_periods))
    period <- shared_periods[order(period_year, period_month)][length(shared_periods)]
  }

  covariate_stack <- c(
    rain_whiplash_raster[[period]],
    gpp_raster[[period]],
    soil_moisture_raster[[period]],
    min_temp_raster[[period]]
  )
  names(covariate_stack) <- c("rain_whiplash_trough_to_peak", "gpp_rolling6", "soil_moisture", "min_temp")
  covariate_stack <- terra::mask(covariate_stack, terra::vect(fit_zones_sf))

  # one row per grid cell, in stable cell order, for rast(..., type = "xyz") below
  cell_df <- terra::as.data.frame(covariate_stack, xy = TRUE, na.rm = FALSE)

  # period is "<month>_<year>" (e.g. "5_2026") -- the 15th is an arbitrary but
  # safe within-month anchor, same convention raster_layer_dates() uses elsewhere
  period_month <- as.integer(sub("_.*", "", period))
  period_year  <- as.integer(sub(".*_", "", period))
  period_season <- season_info(as.Date(paste(period_year, period_month, "15", sep = "-")))$season
  cell_df$season <- factor(period_season, levels = c("Summer", "Autumn", "Winter", "Spring"), ordered = TRUE)

  # per-cell ae_zone, see header -- rasterized onto covariate_stack's own template so its
  # as.data.frame() row order matches cell_df's exactly, then set to the model's own training levels.
  zone_rast <- terra::rasterize(
    terra::vect(sf::st_transform(fit_zones_sf, terra::crs(covariate_stack))),
    covariate_stack[[1]],
    field = "ae_zone"
  )
  cell_df$ae_zone <- terra::as.data.frame(zone_rast, xy = TRUE, na.rm = FALSE)[[3]]
  cell_df$ae_zone <- factor(cell_df$ae_zone, levels = sort(unique(fit_zones_sf$ae_zone)))

  # summer_zone_ne: real, not a placeholder -- identical derivation to r/d_build_hmm_data.R's own,
  # cheap given ae_zone is already in hand (see header).
  training_data <- plague_hmm$obs()$data()
  cell_df$summer_zone_ne <- factor(
    ifelse(cell_df$ae_zone == "NSW NE Qld SE", "summer", "winter_only"),
    levels = levels(training_data$summer_zone_ne)
  )

  # Every remaining observation-formula covariate (see header -- introspected from the fitted model
  # itself, not hand-maintained): each one's own training mean (numeric) or reference factor level
  # (factor), for whatever the fitted model's own formulas reference that isn't already a real
  # per-cell column above.
  obs_vars <- unique(unlist(lapply(plague_hmm$obs()$formulas(), function(param_list) {
    unlist(lapply(param_list, function(state_list) unlist(lapply(state_list, all.vars))))
  })))
  for (v in setdiff(obs_vars, names(cell_df))) {
    cell_df[[v]] <- if (is.factor(training_data[[v]])) {
      factor(levels(training_data[[v]])[1], levels = levels(training_data[[v]]))
    } else {
      mean(training_data[[v]], na.rm = TRUE)
    }
  }

  # Fail loudly and specifically here if a TRANSITION covariate is genuinely missing (its own raster
  # argument never supplied) -- unlike the observation-side covariates above, these DO need real
  # per-cell values, so a placeholder can't safely paper over a missing one (see header).
  transition_terms <- unique(as.vector(plague_hmm$hid()$formula()))
  transition_formula <- stats::as.formula(transition_terms[transition_terms != "."][1])
  missing_transition_vars <- setdiff(all.vars(transition_formula), names(cell_df))
  if (length(missing_transition_vars) > 0) {
    stop("predict_plague_risk(): missing required transition covariate(s), no raster supplied: ",
         paste(missing_transition_vars, collapse = ", "))
  }

  # ae_zone included alongside the three real transition covariates -- a cell that fell inside
  # covariate_stack's mask but outside every zone polygon (a rare boundary/rounding case, mask and
  # rasterize aren't guaranteed pixel-identical) has no valid zone for the random effect either, so
  # it's excluded here the same way a missing rain_whiplash/gpp/soil_moisture value would be.
  covariate_cols <- c("rain_whiplash_trough_to_peak", "gpp_rolling6", "soil_moisture", "min_temp", "ae_zone")
  has_covariates <- stats::complete.cases(cell_df[covariate_cols])

  plague_risk <- rep(NA_real_, nrow(cell_df))
  tpm_pred <- plague_hmm$predict(what = "tpm", newdata = cell_df[has_covariates, , drop = FALSE])
  plague_risk[has_covariates] <- tpm_pred["state 1", "state 2", ]

  terra::rast(
    cbind(cell_df[c("x", "y")], plague_risk = plague_risk),
    type = "xyz",
    crs  = terra::crs(covariate_stack)
  )
}

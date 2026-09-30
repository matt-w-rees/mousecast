# Apply the fitted plague HMM's covariate-to-transition-probability
# relationship to the gridded covariate surface, producing a continuous
# "risk of entering plague next season" map across the zones the model was
# actually fit to -- not the whole national grid. CLAUDE.md's own Section D
# design describes generalising "to wherever the covariates exist", but that
# means un-monitored PADDOCKS within a modelled zone, not extrapolating
# beyond the zones used to fit the transition relationship at all. Masked to
# fit_zones (2026-08) after an initial unmasked version extrapolated into
# zones the model was never fit to (originally Western Australia specifically,
# back when hmm_states excluded it -- WA joined the fit itself 2026-09, see
# r/d_fit_plague_hmm.R's header, so this mask now simply reflects WA's own 4
# fitted zones like every other state's, no longer a blanket exclusion).
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
# mask_zone_extrapolation guards against a real, documented failure mode of the zone-varying design
# just above (2026-09, following a forecasting-literature review -- see quarto_reports/
# plague_model_summary.qmd's own "Known limitations": the far-west SA coast cluster's own extreme
# risk this season traces to rain_whiplash_trough_to_peak:ae_zoneSA Vic Mallee, by far the steepest
# zone slope in the model, estimated with ZERO real training data -- anchored or even raw
# observation -- at the covariate range (rain_whiplash_trough_to_peak > 2) actually driving those
# predictions there; the fitted slope is a linear extrapolation with nothing behind it at that
# range, not a genuinely well-supported high-risk read). Standard practice for a group-varying
# (mixed-model/species-distribution) prediction is to flag or mask a prediction once it extrapolates
# beyond the SPECIFIC group's own training covariate range, rather than trust a fitted slope's
# linear extension arbitrarily far past it -- this is that guard, generalised (not hardcoded to
# rain_whiplash_trough_to_peak) by introspecting the fitted model's own transition formula for
# whichever covariate(s) actually get a ":ae_zone" interaction term, so it keeps working if that
# changes. "That zone's own training range" means rows with real evidence specifically (a known
# anchor state, or at least one non-NA trap/burrow/chewcard observation) -- confirmed live this
# distinction is what actually catches the documented case: SA Vic Mallee's own covariate range
# across EVERY row (including pure covariate-driven placeholders with no anchor or observation
# behind them at all) already reaches past rain_whiplash_trough_to_peak > 2, so an unrestricted
# range check would silently let this exact cluster straight through unmasked. A pooled
# (non-zone-interacted) covariate isn't checked this way -- its one shared
# coefficient was already fit across the full national range, so there's no analogous
# single-zone-specific extrapolation risk to guard against, only the same generic "is this
# covariate NA at all" check has_covariates already does. Defaults to TRUE (masks by default) --
# the one place this changes existing behaviour, on purpose: this specific over-confidence finding
# is exactly the kind of thing that should be fixed pipeline-wide, not opt-in per caller. Set FALSE
# to recover the old, unguarded behaviour (e.g. to reproduce the specific cluster documented above
# for a side-by-side comparison).
#
# The mapped quantity is specifically Pr(state 1 -> state 2), i.e. the
# probability a currently-not-plague paddock enters the plague state next
# season given this season's covariates -- the direct "early warning" signal
# CLAUDE.md's own stated purpose calls for, not e.g. the stationary
# distribution (a longer-run equilibrium quantity, a different question).
#
# Arguments:
#   plague_hmm            a fitted HMM object (r/d_fit_plague_hmm.R), with any transition formula(s)
#   covariate_rasters      named list of SpatRasters, one per transition covariate, named by its
#                          hmm_data column (build_risk_grid_newdata(), r/d_build_risk_grid_newdata.R).
#                          Time-varying rasters have one layer per period, named "<month>_<year>";
#                          a single-layer raster is a static covariate. For the current model:
#                          rain_whiplash_trough_to_peak, gpp_rolling6 (gpp_rolling_raster_6,
#                          season-end months only), soil_moisture_anomaly (soil_moisture_anomaly_raster,
#                          each season-end month's z-score against the same month in other years)
#                          and min_temp (min_temp_raster_coarse, full monthly grain; see header for
#                          why this, not min_temp_seasonal_raster)
#   fit_zones_sf           sf polygons of exactly the ae_zone(s) the model
#                          was fit to -- prediction is masked to this extent,
#                          nothing extrapolated beyond it
#   period                 which layer to predict for, e.g. "5_2026"; NULL
#                          (default) uses the chronologically latest period
#                          common to the time-varying rasters, for a "current risk"
#                          snapshot -- see the "nowcast rasters" note
#                          just below for how this auto-detection is used to reach the true latest
#                          complete month, not just the latest season-end one
#   nowcast rasters        (2026-09) plague_risk_map_nowcast (_targets.R)
#                          passes gpp_rolling_raster_6_nowcast/
#                          soil_moisture_anomaly_raster_nowcast here instead of
#                          their season-end-trimmed namesakes -- same
#                          underlying computation, just without the trim to
#                          Feb/May/Aug/Nov, so the "latest shared period"
#                          logic above can land on whichever calendar month
#                          is actually most recently complete (e.g. July, in
#                          the middle of Spring) instead of being held back to
#                          the last season boundary. This is still real,
#                          complete-month data -- no partial-month averaging,
#                          no trend extrapolation -- just a relaxed alignment
#                          requirement, applied at the SAME point (this
#                          function's own period argument/auto-detection)
#                          regardless of which covariate needed it, rather
#                          than a GPP-specific patch: rain_whiplash_raster/
#                          min_temp_raster already never had a season-end
#                          trim to begin with (confirmed at their own
#                          construction, _targets.R), so they're passed
#                          unchanged either way. plague_risk_map keeps using
#                          the season-end-trimmed rasters unchanged -- this
#                          nowcast path is a separate, clearly-labelled target
#                          (plague_risk_map_nowcast), not a redefinition of
#                          the primary map
#   mask_zone_extrapolation TRUE (default) -- see header above -- masks a cell (NA, same treatment
#                          as a missing covariate) whenever a zone-interacted transition covariate's
#                          value there falls outside that specific ae_zone's own training range
#   n_post                 NULL (default) returns the single-layer MLE-only
#                          raster as before (the plague_risk_map target's own
#                          usage, unchanged). A number (e.g. 500) additionally
#                          draws that many posterior samples of the fitted
#                          model's fixed+random-effect coefficients (hmmTMB's
#                          own $predict(n_post = ...) mechanism, the same one
#                          quarto_reports/plague_model_summary.qmd's own
#                          covariate-effects-by-zone figures already use) and
#                          returns a 3-layer stack (mle/lcl/ucl) instead --
#                          confirmed live ~53s over the full ~8420-cell grid
#                          at n_post = 500, too slow to compute inside a
#                          report render, hence a separate cached target
#                          (plague_risk_map_uncertainty, _targets.R) rather
#                          than always computing it here.
#   level                  confidence level for lcl/ucl when n_post is set
#                          (ignored otherwise); default 0.95
#   seed                   RNG seed set immediately before posterior sampling
#                          when n_post is set (ignored otherwise) -- without
#                          this, re-running tar_make() would redraw different
#                          posterior samples each time and the target would
#                          never stabilise across pipeline re-runs.
#
# Returns a single-layer SpatRaster of Pr(state 1 -> state 2) when n_post is
# NULL, or a 3-layer (mle, lcl, ucl) SpatRaster when n_post is set. NA
# wherever any input covariate is NA, the cell falls outside fit_zones_sf, or
# (mask_zone_extrapolation, default TRUE) a zone-interacted covariate value
# there falls outside that zone's own training range.

predict_plague_risk <- function(plague_hmm, covariate_rasters, fit_zones_sf, period = NULL,
                                 mask_zone_extrapolation = TRUE, n_post = NULL, level = 0.95, seed = 20260807) {

  if (is.null(period)) {
    # Latest period shared by the model's time-varying covariate rasters (static single-layer ones
    # don't restrict it).
    used_rasters <- covariate_rasters[intersect(transition_covariates(plague_hmm), names(covariate_rasters))]
    time_varying <- used_rasters[vapply(used_rasters, terra::nlyr, numeric(1)) > 1]
    shared_periods <- Reduce(intersect, lapply(time_varying, names))
    period_month <- as.integer(sub("_.*", "", shared_periods))
    period_year  <- as.integer(sub(".*_", "", shared_periods))
    period <- shared_periods[order(period_year, period_month)][length(shared_periods)]
  }

  # Per-cell covariate table for this period (r/d_build_risk_grid_newdata.R) -- see header above for
  # what each step does and why.
  grid <- build_risk_grid_newdata(plague_hmm, covariate_rasters, fit_zones_sf, period, mask_zone_extrapolation)
  cell_df <- grid$cell_df
  has_covariates <- grid$has_covariates
  newdata <- grid$newdata

  if (is.null(n_post)) {
    plague_risk <- rep(NA_real_, nrow(cell_df))
    tpm_pred <- plague_hmm$predict(what = "tpm", newdata = newdata)
    plague_risk[has_covariates] <- tpm_pred["state 1", "state 2", ]

    return(terra::rast(
      cbind(cell_df[c("x", "y")], plague_risk = plague_risk),
      type = "xyz",
      crs  = grid$crs
    ))
  }

  # Uncertainty version -- see header. Same fixed seed every call so this target's own posterior
  # draws (and therefore lcl/ucl) don't silently drift on a re-run with no real covariate change.
  set.seed(seed)
  tpm_pred <- plague_hmm$predict(what = "tpm", newdata = newdata, n_post = n_post, level = level)

  mle <- rep(NA_real_, nrow(cell_df)); mle[has_covariates] <- tpm_pred$mle["state 1", "state 2", ]
  lcl <- rep(NA_real_, nrow(cell_df)); lcl[has_covariates] <- tpm_pred$lcl["state 1", "state 2", ]
  ucl <- rep(NA_real_, nrow(cell_df)); ucl[has_covariates] <- tpm_pred$ucl["state 1", "state 2", ]

  terra::rast(
    cbind(cell_df[c("x", "y")], mle = mle, lcl = lcl, ucl = ucl),
    type = "xyz",
    crs  = grid$crs
  )
}

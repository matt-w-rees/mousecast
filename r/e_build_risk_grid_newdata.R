# transition_covariates(): the covariates a fitted HMM's transition formulas need real per-cell
# values for -- every variable in either transition's formula (entering [1, 2], leaving [2, 1]),
# except the ones build_risk_grid_newdata() derives for each cell itself (ae_zone, season). These
# are the names covariate_rasters must supply.
#
# Arguments:
#   plague_hmm  a fitted HMM object (r/e_fit_plague_hmm.R)
#
# Returns a character vector of covariate names.
transition_covariates <- function(plague_hmm) {
  formulas <- unique(as.vector(plague_hmm$hid()$formula()))
  formula_vars <- unlist(lapply(formulas[formulas != "."], function(f) all.vars(stats::as.formula(f))))
  setdiff(unique(formula_vars), c("ae_zone", "season"))
}

# Builds the per-cell covariate table predict_plague_risk() and hindcast_plague_occurrence() feed to
# HMM$predict(what = "tpm", newdata = ...), for one "<month>_<year>" period: every transition
# covariate read off its raster (masked to fit_zones_sf), each cell's own ae_zone and season,
# placeholder values for every observation-formula covariate, and the zone-
# extrapolation guard. Shared by predict_plague_risk() and hindcast_plague_occurrence() so the
# published risk maps and the historic hindcast use exactly the same construction -- see
# r/e_predict_plague_risk.R's header for the rationale behind each step.
#
# The transition covariates are whatever the fitted formulas use (transition_covariates() above),
# so any formula can be mapped without changing this function: covariate_rasters supplies one
# raster per covariate, named by its hmm_data column. A multi-layer raster is read at period; a
# single-layer raster is a static covariate (the same value every period).
#
# Arguments:
#   plague_hmm               a fitted HMM object (r/e_fit_plague_hmm.R)
#   covariate_rasters        named list of SpatRasters, one per transition covariate (names = the
#                            hmm_data columns, e.g. list(rain_whiplash_trough_to_peak = ...,
#                            gpp_rolling6 = ...)); multi-layer rasters have "<month>_<year>" layer
#                            names. A factor covariate's raster holds its level labels or level
#                            positions. Extra entries are ignored.
#   fit_zones_sf             sf polygons of the ae_zone(s) the model was fit to
#   period                   which "<month>_<year>" layer to build, e.g. "5_2026"
#   mask_zone_extrapolation  see predict_plague_risk()
#
# Returns a list: cell_df (one row per grid cell, stable cell order, x/y plus every newdata column),
# has_covariates (logical, which rows are valid to predict), newdata (cell_df[has_covariates, ]) and
# crs (the covariate grid's CRS, for rebuilding a raster from cell_df's x/y).

build_risk_grid_newdata <- function(plague_hmm, covariate_rasters, fit_zones_sf, period, mask_zone_extrapolation = TRUE) {

  # Fail loudly if a transition covariate has no raster -- these need real per-cell values, so a
  # placeholder can't stand in for one (unlike the observation covariates below).
  needed_covariates <- transition_covariates(plague_hmm)
  missing_covariates <- setdiff(needed_covariates, names(covariate_rasters))
  if (length(missing_covariates) > 0) {
    stop("build_risk_grid_newdata(): no raster supplied for transition covariate(s): ",
         paste(missing_covariates, collapse = ", "))
  }

  # One layer per transition covariate: this period's layer, or the single layer of a static covariate.
  covariate_stack <- terra::rast(lapply(needed_covariates, function(cv) {
    r <- covariate_rasters[[cv]]
    if (terra::nlyr(r) == 1) r else r[[period]]
  }))
  names(covariate_stack) <- needed_covariates
  covariate_stack <- terra::mask(covariate_stack, terra::vect(fit_zones_sf))

  # one row per grid cell, in stable cell order, for rast(..., type = "xyz") below
  cell_df <- terra::as.data.frame(covariate_stack, xy = TRUE, na.rm = FALSE)

  # period is "<month>_<year>" (e.g. "5_2026") -- the 15th is an arbitrary but
  # safe within-month anchor, same convention raster_layer_dates() uses elsewhere
  period_month <- as.integer(sub("_.*", "", period))
  period_year  <- as.integer(sub(".*_", "", period))
  period_season <- season_info(as.Date(paste(period_year, period_month, "15", sep = "-")))$season
  cell_df$season <- factor(period_season, levels = c("Summer", "Autumn", "Winter", "Spring"))

  # per-cell ae_zone, see header -- rasterized onto covariate_stack's own template so its
  # as.data.frame() row order matches cell_df's exactly, then set to the model's own training levels.
  zone_rast <- terra::rasterize(
    terra::vect(sf::st_transform(fit_zones_sf, terra::crs(covariate_stack))),
    covariate_stack[[1]],
    field = "ae_zone"
  )
  cell_df$ae_zone <- terra::as.data.frame(zone_rast, xy = TRUE, na.rm = FALSE)[[3]]
  cell_df$ae_zone <- factor(cell_df$ae_zone, levels = sort(unique(fit_zones_sf$ae_zone)))

  # The fitted model's training data, for the observation-covariate placeholders below.
  training_data <- plague_hmm$obs()$data()

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

  # A transition covariate that is a factor in the training data (e.g. the static soil group) takes
  # the training levels. Its raster holds either the level labels (a categorical raster) or each
  # level's position in those levels (1, 2, ...) -- GeoTIFF storage can drop category labels.
  for (cv in needed_covariates) {
    if (is.factor(training_data[[cv]])) {
      values <- cell_df[[cv]]
      if (is.numeric(values)) values <- levels(training_data[[cv]])[values]
      cell_df[[cv]] <- factor(as.character(values), levels = levels(training_data[[cv]]))
    }
  }

  # Zone-extrapolation guard (see header's own "mask_zone_extrapolation" note for the full
  # rationale). A term is "zone-interacted" under either of the two forms this project's own
  # transition formula has used for a zone-varying covariate slope (terms()'s own term.labels, not a
  # hand-parsed formula string): a fixed "<covariate>:ae_zone" interaction (the earlier, since-
  # superseded "Model I" spec -- kept here so this still works if some future model variant reverts
  # to it) and the now-adopted hierarchical "s(ae_zone, by = <covariate>, bs = \"re\")" random-slope
  # term (2026-09, Pedersen et al. 2019 PeerJ 7:e6876's "Model GS" -- see r/e_fit_plague_hmm.R's
  # header for why partial pooling replaced the fixed interaction). Both forms are detected and
  # their own covariate name extracted, so this generalises to whichever covariate(s) the fitted
  # model's own transition formula actually gives a zone-varying slope, not just today's single case.
  # training_data (already built above, obs$data()) carries every transition covariate too, since
  # MarkovChain$new() and Observation$new() are both built from the SAME hmm_data in
  # fit_plague_hmm() -- confirmed live, no separate "transition training data" object exists to
  # pull this from instead.
  #
  # has_evidence restricts the per-zone range to rows that actually carry real evidence (a known
  # anchor state, or at least one non-NA observation stream) -- confirmed live this matters, not a
  # theoretical nicety: "SA Vic Mallee" alone has 2,850 rows total but only 782 with any real
  # evidence at all (every OTHER row is a pure covariate-driven placeholder, contributing nothing
  # to any coefficient's own estimation beyond the transition structure's own state-uncertainty
  # marginalisation) -- and its own rain_whiplash_trough_to_peak range across EVERY row (-0.87 to
  # 3.72) comfortably includes the > 2 values actually driving this documented over-confidence
  # cluster, while the evidence-only range (-0.63 to 1.80, zero evidence rows above 2) does not.
  # Using the unrestricted range here would silently let exactly this case through unmasked --
  # confirmed live before adopting the has_evidence restriction, not assumed.
  # Term labels from both transitions' formulas, so a zone-varying slope on either one is guarded.
  transition_terms <- unique(as.vector(plague_hmm$hid()$formula()))
  all_terms <- unique(unlist(lapply(transition_terms[transition_terms != "."],
                                    function(f) attr(stats::terms(stats::as.formula(f)), "term.labels"))))

  fixed_interaction_terms <- all_terms[grepl(":", all_terms) & grepl("ae_zone", all_terms) & !grepl("^s\\(", all_terms)]
  fixed_interaction_covariates <- sub(":ae_zone$|^ae_zone:", "", fixed_interaction_terms)

  hier_slope_terms <- all_terms[grepl('^s\\(ae_zone,\\s*by\\s*=', all_terms)]
  hier_slope_covariates <- sub('^s\\(ae_zone,\\s*by\\s*=\\s*([^,]+),.*\\)$', "\\1", hier_slope_terms)

  zone_interacted_covariates <- unique(c(fixed_interaction_covariates, hier_slope_covariates))

  in_zone_range <- rep(TRUE, nrow(cell_df))
  if (mask_zone_extrapolation && length(zone_interacted_covariates) > 0) {
    has_evidence <- !is.na(training_data$state) | !is.na(training_data$trap_count) |
      !is.na(training_data$burrow_count) | !is.na(training_data$chewcard_count) |
      !is.na(training_data$chewcard_block2) | !is.na(training_data$chewcard_block3) |
      !is.na(training_data$chewcard_block4)
    evidence_data <- training_data[has_evidence, , drop = FALSE]

    for (cv in zone_interacted_covariates) {
      # tapply() (base R, matching this function's own style -- no dplyr used elsewhere here) --
      # named vector keyed by ae_zone's own factor-level labels, so matching by name below is
      # correct regardless of factor level order.
      zone_lo <- tapply(evidence_data[[cv]], evidence_data$ae_zone, min, na.rm = TRUE)
      zone_hi <- tapply(evidence_data[[cv]], evidence_data$ae_zone, max, na.rm = TRUE)
      cell_lo <- zone_lo[as.character(cell_df$ae_zone)]
      cell_hi <- zone_hi[as.character(cell_df$ae_zone)]
      within_range <- !is.na(cell_lo) & !is.na(cell_df[[cv]]) & cell_df[[cv]] >= cell_lo & cell_df[[cv]] <= cell_hi
      in_zone_range <- in_zone_range & within_range
    }
  }

  # ae_zone included alongside the transition covariates -- a cell that fell inside
  # covariate_stack's mask but outside every zone polygon (a rare boundary/rounding case, mask and
  # rasterize aren't guaranteed pixel-identical) has no valid zone for the random effect either, so
  # it's excluded here the same way a missing covariate value would be.
  # in_zone_range is TRUE everywhere when mask_zone_extrapolation = FALSE or no covariate has a
  # zone interaction, so this line is a pure no-op extension in either of those cases.
  covariate_cols <- c(needed_covariates, "ae_zone")
  has_covariates <- stats::complete.cases(cell_df[covariate_cols]) & in_zone_range

  newdata <- cell_df[has_covariates, , drop = FALSE]

  list(cell_df = cell_df, has_covariates = has_covariates, newdata = newdata, crs = terra::crs(covariate_stack))
}

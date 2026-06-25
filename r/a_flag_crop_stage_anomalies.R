# Flag survey rows where the recorded crop_stage is agronomically implausible
# for that ae_zone's cropping system and the survey's calendar month.
#
# Two cropping systems matter in Australia's eastern/southern grain belt:
#   - single (winter-only): one winter crop per paddock per year (most of
#     southern Australia + all of WA).
#   - dual (summer + winter): paddocks north of ~Dubbo (northern NSW + Qld)
#     can carry either a winter or a summer crop, so plausibility depends on
#     which one a given row's crop_variety belongs to.
#
# The winter month-by-stage windows below come from this project's own pooled
# surveys_all data (a large, clean sample -- e.g. September flowering n=419,
# October ripening n=197, Dec-Mar stubble n=135-709). The summer windows come
# from general Australian agronomic knowledge instead, since the empirical
# summer-crop sample here is too sparse/noisy to derive windows from directly.
#
# This is a diagnostic-only function: it does not modify surveys_all, just
# returns the rows worth a second look, tiered by severity:
#   "implausible"  no precedent for this stage in this month in any
#                  applicable calendar
#   "unusual"      rare but real (the "possible" tier below)
#
# Excluded from checking: rows with no crop_stage to begin with (pasture/
# bare_soil/unknown-crop rows already have NA crop_stage by design), rows with
# no cropping_system (no ae_zone, or WA Ord -- an irrigated scheme with no
# fixed rain-fed calendar -- see attach_cropping_system()), lucerne
# (perennial, no annual stage cycle), and MouseAlert observations (crop_stage
# there is a different citizen-facing vocabulary -- "in crop"/"pre-sowing" --
# not the six ODK stages this function's calendars are built around).
#
# Requires data to already have a cropping_system column (single/dual/NA),
# attached at the paddock level by attach_cropping_system() -- see
# r/a_attach_cropping_system.R, which uses the same aez_zone_info()
# region_label grouping this function used to recompute internally.
flag_crop_stage_anomalies <- function(data) {

  odk_stages <- c("sown", "seedling", "vegetative", "flowering", "ripening", "stubble")

  # --- month -> {expected, possible} stage windows ---
  winter_calendar <- tibble::tribble(
    ~month, ~crop_stage,  ~plausibility,
    1,      "stubble",    "expected",
    2,      "stubble",    "expected",
    3,      "stubble",    "expected",
    4,      "stubble",    "expected",
    4,      "seedling",   "possible",
    5,      "sown",       "expected",
    5,      "seedling",   "expected",
    5,      "stubble",    "possible",
    6,      "sown",       "expected",
    6,      "seedling",   "expected",
    6,      "vegetative", "expected",
    7,      "vegetative", "expected",
    7,      "seedling",   "possible",
    8,      "vegetative", "expected",
    8,      "flowering",  "possible",
    9,      "vegetative", "expected",
    9,      "flowering",  "expected",
    9,      "ripening",   "expected",
    10,     "ripening",   "expected",
    10,     "flowering",  "possible",
    10,     "stubble",    "possible",
    11,     "ripening",   "expected",
    11,     "stubble",    "expected",
    12,     "stubble",    "expected",
    12,     "ripening",   "possible"
  )

  summer_calendar <- tibble::tribble(
    ~month, ~crop_stage,  ~plausibility,
    9,      "stubble",    "expected",
    9,      "sown",       "possible",
    10,     "stubble",    "expected",
    10,     "sown",       "possible",
    11,     "sown",       "expected",
    11,     "seedling",   "expected",
    11,     "stubble",    "expected",
    12,     "seedling",   "expected",
    12,     "vegetative", "expected",
    12,     "stubble",    "possible",
    1,      "vegetative", "expected",
    1,      "flowering",  "expected",
    2,      "vegetative", "expected",
    2,      "flowering",  "expected",
    3,      "flowering",  "expected",
    3,      "ripening",   "expected",
    4,      "ripening",   "expected",
    4,      "stubble",    "expected",
    5,      "stubble",    "expected",
    5,      "ripening",   "possible",
    6,      "stubble",    "expected",
    7,      "stubble",    "expected",
    8,      "stubble",    "expected"
  )

  # Pooled across both calendars, for dual-zone rows whose crop_variety
  # doesn't pin down a season (e.g. cereal_other, legume_other, unknown) --
  # "expected" if either calendar calls it expected, else "possible" if
  # either calls it possible.
  lenient_calendar <- dplyr::bind_rows(winter_calendar, summer_calendar) |>
    dplyr::group_by(month, crop_stage) |>
    dplyr::summarise(
      plausibility = if (any(plausibility == "expected")) "expected" else "possible",
      .groups = "drop"
    )

  calendar_by_season <- dplyr::bind_rows(
    dplyr::mutate(winter_calendar, season = "winter"),
    dplyr::mutate(summer_calendar, season = "summer")
  )

  # --- crop_variety -> season ---
  winter_varieties <- c(
    "wheat", "barley", "oats", "triticale", "chickpea", "faba_bean",
    "field_pea", "lentil", "lupin", "vetch", "canola", "safflower", "linseed"
  )
  summer_varieties <- c(
    "sorghum", "maize", "millet", "soybean", "mungbean", "sunflower",
    "cotton_other"
  )

  data <- data |>
    dplyr::filter(
      crop_stage %in% odk_stages,
      !is.na(cropping_system),
      is.na(crop_variety) | crop_variety != "lucerne"
    ) |>
    dplyr::mutate(
      survey_month = lubridate::month(survey_date),
      variety_season = dplyr::case_when(
        crop_variety %in% winter_varieties ~ "winter",
        crop_variety %in% summer_varieties ~ "summer",
        TRUE ~ "unknown"
      )
    )

  single_checked <- data |>
    dplyr::filter(cropping_system == "single") |>
    dplyr::left_join(winter_calendar, by = c("survey_month" = "month", "crop_stage"))

  dual_known <- data |>
    dplyr::filter(cropping_system == "dual", variety_season != "unknown") |>
    dplyr::left_join(calendar_by_season, by = c("survey_month" = "month", "crop_stage", "variety_season" = "season"))

  dual_unknown <- data |>
    dplyr::filter(cropping_system == "dual", variety_season == "unknown") |>
    dplyr::left_join(lenient_calendar, by = c("survey_month" = "month", "crop_stage"))

  dplyr::bind_rows(single_checked, dual_known, dual_unknown) |>
    dplyr::filter(is.na(plausibility) | plausibility == "possible") |>
    dplyr::mutate(
      severity = dplyr::if_else(is.na(plausibility), "implausible", "unusual"),
      reason = paste0(
        "crop_stage '", crop_stage, "' is ", severity, " for month ", survey_month,
        " in a ", cropping_system, "-cropping zone (", ae_zone, ")",
        dplyr::if_else(variety_season != "unknown", paste0(" [", variety_season, " crop: ", crop_variety, "]"), "")
      )
    ) |>
    dplyr::select(
      region_name, site_name, subsite_name, survey_date, ae_zone,
      crop_variety, crop_stage, cropping_system, severity, reason
    ) |>
    dplyr::arrange(severity, ae_zone, survey_date)
}

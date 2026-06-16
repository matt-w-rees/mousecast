# Survey Sites Map — Shiny app
#
# Data flow:
#   targets::tar_make() writes data_list_clean_paddocks and aez_adj as RDS
#   files to shiny/site_map/data/ via the site_map_app_data target.
#   This app reads those files at startup.  Re-run tar_make() whenever the
#   pipeline data changes, then restart or redeploy the app.
#
# Run locally from the project root:
#   shiny::runApp("shiny/site_map")
#
# Deploy to shinyapps.io / Posit Connect:
#   rsconnect::deployApp("shiny/site_map")
#   The data/ subdirectory is included automatically in the bundle.

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(dplyr)
  library(sf)
  library(leaflet)
  library(reactable)
  library(htmltools)
  library(ggplot2)
})

# ── Load pipeline outputs (once at startup, not inside reactive context) ─────
data_list_raw <- readRDS("data/data_list_clean_paddocks.rds")
aez_base      <- readRDS("data/aez_adj.rds")

# ── Download raw data feature (disabled) ────────────────────────────────────
# Combine all survey types into one flat frame, for the download buttons below.
# Drop month_year/season_year_adj before binding: attach_time_variables() builds
# these ordered factors with per-dataset level sets (observations spans a
# narrower year range than traps/rapid), so bind_rows() fails on the mismatched
# levels with "Can't combine ... <ordered>". Neither column is used in this app
# — month/season groupings are derived fresh below from survey_date.
# surveys_bound <- purrr::imap(data_list_raw, ~ dplyr::mutate(.x, data_type = .y) |>
#                                dplyr::select(-dplyr::any_of(c("month_year", "season_year_adj")))) |>
#   dplyr::bind_rows()
#
# surveys_raw <- surveys_bound |>
#   dplyr::mutate(survey_date = as.Date(survey_date))

# ── Pre-processed flat survey-level frame ────────────────────────────────────
# Built by combine_survey_data() in r/combine_survey_data.R, run once as the
# surveys_all target in _targets.R and exported here as data/surveys_all.rds —
# shared verbatim with quarto_reports/mouseforecast.com/raw_data_update.qmd so
# both consumers always see identical data.
surveys_all <- readRDS("data/surveys_all.rds")

# ── Paddock label lookup (paddock_id → site / subsite names) ─────────────────
# site_name / subsite_name exist in raw data but are not retained in surveys_all.
paddock_labels <- purrr::map(data_list_raw[c("traps", "rapid")], function(d) {
  dplyr::distinct(d, paddock_id, site_name, subsite_name)
}) |>
  dplyr::bind_rows() |>
  dplyr::distinct(paddock_id, site_name, subsite_name) |>
  dplyr::mutate(
    name_part = dplyr::case_when(
      !is.na(subsite_name) & subsite_name != site_name ~
        paste0(site_name, " / ", subsite_name),
      !is.na(site_name) ~ site_name,
      TRUE              ~ NA_character_
    )
  ) |>
  dplyr::group_by(paddock_id) |>
  dplyr::summarise(
    name_parts = paste(unique(na.omit(name_part)), collapse = ", "),
    .groups    = "drop"
  ) |>
  dplyr::mutate(
    pad_label = dplyr::if_else(
      name_parts != "",
      paste0(as.character(paddock_id), " — ", name_parts),
      as.character(paddock_id)
    )
  )

# ── Colour palettes ──────────────────────────────────────────────────────────
# Green -> yellow -> red diverging scale, matching
# quarto_reports/mouseforecast.com/raw_data_update.qmd's
# make_activity_gradient_fill() (same hex values as that qmd's base_cols
# Low/Moderate/High) — used for every "Continuous" colour-scale metric below
# so the map/table read the same way as the qmd's gradient report style.
activity_gradient_colours <- c("#91cf60", "#ffeda0", "#f03b20")

pal_aez_pct <- leaflet::colorNumeric(
  palette  = activity_gradient_colours,
  domain   = c(0, 100),
  na.color = "#BDBDBD"
)

marker_col <- "#555555"

# ── Per-AEZ detection stats ──────────────────────────────────────────────────
# Restricted to traps/rapid: these are the systematically-monitored paddock
# networks. MouseAlert observations are mostly one-off citizen reports at sites
# never otherwise surveyed (871 of 956 observation paddocks have no traps/rapid
# history) — pooling them in would massively inflate the denominator and distort
# the detection rate (e.g. SA Vic Bordertown-Wimmera goes from 93% on 15 sites
# to 55% on 62 sites). MouseAlert gets its own metric: compute_mouse_alert_by_aez().
# Uses the mice_detected column already computed on surveys_all (one definition,
# shared by the table, map, trend chart and pattern chart).
compute_mice_by_aez <- function(surveys) {
  surveys |>
    dplyr::filter(data_type %in% c("traps", "rapid")) |>
    dplyr::group_by(ae_zone, paddock_id) |>
    dplyr::summarise(ever_detected = any(mice_detected), .groups = "drop") |>
    dplyr::group_by(ae_zone) |>
    dplyr::summarise(
      n_sites      = dplyr::n(),
      n_detected   = sum(ever_detected),
      pct_detected = round(n_detected / n_sites * 100, 1),
      .groups      = "drop"
    )
}

# ── Per-AEZ MouseAlert stats ─────────────────────────────────────────────────
# Reported separately from compute_mice_by_aez() — see note above. Returns the
# average number of "high" abundance MouseAlert sightings per day within the
# supplied date window. Dividing by the window length makes the metric
# comparable across different filter periods (analogous to mice per trap night).
compute_mouse_alert_by_aez <- function(surveys, date_start, date_end) {
  # Guard against NULL/zero-length dates (can occur briefly during session init).
  n_days <- tryCatch(
    as.numeric(as.Date(date_end) - as.Date(date_start)) + 1,
    error = function(e) 1
  )
  if (length(n_days) == 0 || !is.finite(n_days) || n_days < 1) n_days <- 1
  surveys |>
    dplyr::filter(data_type == "observations", !is.na(ae_zone),
                  !is.na(mouse_abundance), mouse_abundance == "high") |>
    dplyr::group_by(ae_zone) |>
    dplyr::summarise(
      # raw count displayed in the table; avg_daily_high used for shading
      n_high_reports = dplyr::n(),
      avg_daily_high = round(dplyr::n() / n_days, 3),
      .groups = "drop"
    )
}

# ── Per-AEZ stats for a chosen map metric ────────────────────────────────────
# Returns ae_zone, n_sites, display_val, and (for pct_detected) n_detected /
# traffic_light so the polygon popup can show the right information.
compute_aez_display <- function(surveys, metric) {
  # isTRUE() handles zero-length or NULL metric without throwing "argument is of length zero"
  if (isTRUE(metric == "pct_detected")) {
    # Restricted to traps/rapid — see compute_mice_by_aez() for why MouseAlert
    # observations are excluded from this systematic-survey detection rate.
    surveys |>
      dplyr::filter(data_type %in% c("traps", "rapid")) |>
      dplyr::group_by(ae_zone, paddock_id) |>
      dplyr::summarise(detected = any(mice_detected), .groups = "drop") |>
      dplyr::group_by(ae_zone) |>
      dplyr::summarise(
        n_sites     = dplyr::n(),
        n_detected  = sum(detected),
        display_val = round(mean(detected) * 100, 1),
        .groups     = "drop"
      )
  } else {
    # Simple mean across all survey rows per AEZ — matches the table's aggregation
    # so map and table colours are directly comparable.
    surveys |>
      dplyr::group_by(ae_zone) |>
      dplyr::summarise(
        n_sites       = dplyr::n_distinct(paddock_id),
        n_detected    = NA_integer_,
        display_val   = round(mean(.data[[metric]], na.rm = TRUE), 3),
        traffic_light = NA_character_,
        .groups       = "drop"
      )
  }
}

# ── Continuous "activity gradient" colour scale ──────────────────────────────
# scales::colour_ramp() over activity_gradient_colours — the same Lab-space
# interpolation ggplot2's scale_fill_gradient2() uses for
# make_activity_gradient_fill() in raw_data_update.qmd, so a value of
# val / max_val maps to the same colour in both reports.
.activity_gradient_ramp <- scales::colour_ramp(activity_gradient_colours)
activity_gradient_color <- function(val, max_val) {
  # NA/missing → grey so empty cells are visually distinct from low values.
  result <- rep("#CCCCCC", length(val))
  valid  <- !is.na(val) & is.finite(val)
  if (any(valid) && max_val > 0) {
    t <- pmin(pmax(val[valid] / max_val, 0), 1)
    result[valid] <- .activity_gradient_ramp(t)
  }
  result
}

# ── ZNP50 permit — colours and classification ─────────────────────────────────
znp_colours <- c(Low = "#27AE60", Moderate = "#E67E22", High = "#C0392B")

pal_znp <- leaflet::colorFactor(
  palette  = unname(znp_colours),
  levels   = names(znp_colours),
  na.color = "#BDBDBD"
)

# Computes the ZNP50 classification per AEZ.
# Uses only rapid assessment surveys. For each AEZ, takes the paddock with the
# highest site-mean value for each metric (rather than averaging across sites),
# then applies thresholds to produce Low / Moderate / High.
compute_znp50_by_aez <- function(surveys) {
  rapid <- dplyr::filter(surveys, data_type == "rapid")

  if (nrow(rapid) == 0) {
    return(dplyr::tibble(
      ae_zone          = character(0), n_sites          = integer(0),
      max_chew_pct     = numeric(0),   max_burrows      = numeric(0),
      max_chew_per10   = numeric(0),   chew_class       = character(0),
      burrow_class     = character(0), chew_per10_class = character(0),
      overall_class    = character(0)
    ))
  }

  # Step 1: mean per site (paddock)
  site_stats <- rapid |>
    dplyr::group_by(ae_zone, paddock_id) |>
    dplyr::summarise(
      mean_chew      = mean(pct_chewcard, na.rm = TRUE),
      mean_burrows   = mean(result_burrow, na.rm = TRUE),
      mean_chew_per10 = mean(chew_per10,  na.rm = TRUE),
      .groups        = "drop"
    ) |>
    dplyr::mutate(
      mean_chew       = dplyr::if_else(is.nan(mean_chew),       NA_real_, mean_chew),
      mean_burrows    = dplyr::if_else(is.nan(mean_burrows),    NA_real_, mean_burrows),
      mean_chew_per10 = dplyr::if_else(is.nan(mean_chew_per10), NA_real_, mean_chew_per10)
    )

  # Step 2: max across sites within each AEZ, then classify
  site_stats |>
    dplyr::group_by(ae_zone) |>
    dplyr::summarise(
      n_sites          = dplyr::n(),
      max_chew_pct     = round(suppressWarnings(max(mean_chew,       na.rm = TRUE)), 1),
      max_burrows      = round(suppressWarnings(max(mean_burrows,    na.rm = TRUE)), 2),
      max_chew_per10   = round(suppressWarnings(max(mean_chew_per10, na.rm = TRUE)), 2),
      .groups          = "drop"
    ) |>
    dplyr::mutate(
      max_chew_pct   = dplyr::if_else(!is.finite(max_chew_pct),   NA_real_, max_chew_pct),
      max_burrows    = dplyr::if_else(!is.finite(max_burrows),    NA_real_, max_burrows),
      max_chew_per10 = dplyr::if_else(!is.finite(max_chew_per10), NA_real_, max_chew_per10),
      chew_class     = dplyr::case_when(
        is.na(max_chew_pct) ~ NA_character_,
        max_chew_pct >= 20  ~ "High",
        max_chew_pct >= 10  ~ "Moderate",
        TRUE                ~ "Low"
      ),
      burrow_class = dplyr::case_when(
        is.na(max_burrows) ~ NA_character_,
        max_burrows > 2    ~ "High",
        max_burrows > 1    ~ "Moderate",
        TRUE               ~ "Low"
      ),
      # Same thresholds as burrows but informational only — does not affect overall_class
      chew_per10_class = dplyr::case_when(
        is.na(max_chew_per10) ~ NA_character_,
        max_chew_per10 > 2    ~ "High",
        max_chew_per10 > 1    ~ "Moderate",
        TRUE                  ~ "Low"
      ),
      # Overall: worst of chew % and burrows only; chew_per10 is excluded
      overall_class = dplyr::case_when(
        (!is.na(chew_class)   & chew_class   == "High") |
        (!is.na(burrow_class) & burrow_class == "High")     ~ "High",
        (!is.na(chew_class)   & chew_class   == "Moderate") |
        (!is.na(burrow_class) & burrow_class == "Moderate") ~ "Moderate",
        (!is.na(chew_class)   & chew_class   == "Low") |
        (!is.na(burrow_class) & burrow_class == "Low")      ~ "Low",
        TRUE                                                 ~ NA_character_
      )
    )
}

# ── Dataset-wide reference levels ("ranges") ─────────────────────────────────
# Computed once by compute_metric_ranges() (r/compute_metric_ranges.R) from
# surveys_all and shared verbatim with
# quarto_reports/mouseforecast.com/raw_data_update.qmd via the metric_ranges
# target, exported here as data/metric_ranges.rds.
#   - max_result_traps / max_result_burrow / max_chew_per10 / max_avg_daily_high:
#     pooled-percentile references — the "1.0" anchor for
#     compute_activity_index().
#   - gradient_max_result_traps / gradient_max_result_burrow /
#     gradient_max_chew_per10 / gradient_max_avg_daily_high: per-AEZ-mean
#     colour-scale ceilings for the table/map gradient mode (activity_gradient_color()).
#   - trend_max_result_traps / trend_max_result_burrow / trend_max_chew_per10:
#     true row-level all-time maxima, used to normalise pad_trend_chart onto a
#     [0,1] axis.
metric_ranges <- readRDS("data/metric_ranges.rds")
max_result_traps             <- metric_ranges$max_result_traps
max_result_burrow             <- metric_ranges$max_result_burrow
max_chew_per10                <- metric_ranges$max_chew_per10
max_avg_daily_high            <- metric_ranges$max_avg_daily_high
gradient_max_result_traps     <- metric_ranges$gradient_max_result_traps
gradient_max_result_burrow     <- metric_ranges$gradient_max_result_burrow
gradient_max_chew_per10        <- metric_ranges$gradient_max_chew_per10
gradient_max_avg_daily_high    <- metric_ranges$gradient_max_avg_daily_high
trend_max_result_traps        <- metric_ranges$trend_max_result_traps
trend_max_result_burrow         <- metric_ranges$trend_max_result_burrow
trend_max_chew_per10           <- metric_ranges$trend_max_chew_per10

# ── Filter choices ────────────────────────────────────────────────────────────
year_choices    <- sort(unique(surveys_all$year_adj))
season_order    <- c("Summer", "Autumn", "Winter", "Spring")
season_choices  <- season_order[season_order %in% unique(surveys_all$season)]
type_choices    <- sort(unique(surveys_all$data_type))
project_choices <- sort(unique(surveys_all$project))
bait_choices    <- sort(unique(surveys_all$bait_history))
aez_choices        <- sort(unique(surveys_all$ae_zone))
aez_base_surveyed  <- dplyr::filter(aez_base, ae_zone %in% aez_choices)
date_min        <- min(surveys_all$survey_date)
date_max        <- max(surveys_all$survey_date)

# Approximate calendar length (days) of each season — used to convert a
# season's MouseAlert "high" report count into a daily rate (avg_daily_high).
# Close enough for a relative index; leap years are ignored.
season_n_days <- function(season) {
  dplyr::case_when(
    as.character(season) == "Summer" ~ 90,
    as.character(season) == "Autumn" ~ 92,
    as.character(season) == "Winter" ~ 92,
    as.character(season) == "Spring" ~ 91,
    TRUE                              ~ 91
  )
}

# ── Combined Mouse Activity Index ────────────────────────────────────────────
# Brings the five per-AEZ table metrics (paddocks detected, mice/trap night,
# burrows/transect, chew cards/10, MouseAlert avg. daily high reports) together
# into one headline read: a weighted mean of each metric scaled relative to a
# dataset-wide reference level (max_result_traps etc. — 1.0 = that reference
# level; a standout AEZ/season can exceed 1). compute_activity_index() expects
# the per-AEZ summary df built by the server's aez_summary_r() reactive (one
# row per AEZ, with those five columns already present). Weights default to
# equal (1 each) but are user-adjustable via sliders on Tab 1 — see
# activity_weights_r() in the server, which is passed into
# compute_activity_index() below.
activity_weights <- c(pct_detected = 1, result_traps = 1, result_burrow = 1,
                       chew_per10 = 1, avg_daily_high = 1)

compute_activity_index <- function(df, weights = activity_weights) {
  # Guard: matrix() warns "non-empty data for zero-extent matrix" when nrow == 0
  # but the weights vector is non-empty. Return early with the expected column.
  if (nrow(df) == 0) return(dplyr::mutate(df, activity_index = numeric(0)))

  # Reorder/name-match so the weight vector lines up with the norms columns
  # below regardless of how the caller constructed it.
  weights <- weights[c("pct_detected", "result_traps", "result_burrow", "chew_per10", "avg_daily_high")]

  norms <- cbind(
    df$pct_detected   / 100,
    df$result_traps   / max_result_traps,
    df$result_burrow  / max_result_burrow,
    df$chew_per10     / max_chew_per10,
    df$avg_daily_high / max_avg_daily_high
  )
  weight_mat <- matrix(weights, nrow = nrow(norms), ncol = 5, byrow = TRUE)
  weight_mat[is.na(norms)] <- NA_real_
  index <- rowSums(norms * weight_mat, na.rm = TRUE) / rowSums(weight_mat, na.rm = TRUE)

  df |>
    dplyr::mutate(
      activity_index = round(dplyr::if_else(is.nan(index), NA_real_, index), 3)
    )
}

# ── Per-season Mouse Activity Index (Tab 2: Detection Trends) ───────────────
# Generalises aez_summary_r()'s per-AEZ summary to a per-(year_adj, season,
# [ae_zone]) summary, so the combined index can be plotted as a trend
# over time. join_keys is c("year_adj","season") for the "Combined" trend view,
# or c("year_adj","season","ae_zone") for "Overlay"/"Facet by AEZ". d is
# pre-filtered survey data (e.g. trend_filtered()).
compute_trend_activity_index <- function(d, join_keys, weights) {
  # Paddocks surveyed (traps/rapid) and the share with a detection, per group —
  # mirrors compute_mice_by_aez()'s n_sites/pct_detected, but per season rather
  # than collapsed across the whole filter window.
  detect <- d |>
    dplyr::filter(data_type %in% c("traps", "rapid")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(join_keys, "paddock_id")))) |>
    dplyr::summarise(detected = any(mice_detected), .groups = "drop") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) |>
    dplyr::summarise(
      n_paddocks   = dplyr::n(),
      pct_detected = round(mean(detected) * 100, 1),
      .groups = "drop"
    )

  # Mean survey-level results per group, matching aez_summary_r()'s
  # mean(result_traps/result_burrow/chew_per10, na.rm = TRUE).
  means <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) |>
    dplyr::summarise(
      result_traps  = mean(result_traps, na.rm = TRUE),
      result_burrow = mean(result_burrow, na.rm = TRUE),
      chew_per10    = mean(chew_per10,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      result_traps  = round(dplyr::if_else(is.nan(result_traps), NA_real_, result_traps), 3),
      result_burrow = round(dplyr::if_else(is.nan(result_burrow), NA_real_, result_burrow), 2),
      chew_per10    = round(dplyr::if_else(is.nan(chew_per10),   NA_real_, chew_per10),   2)
    )

  # MouseAlert "high" reports per group, converted to a daily rate using the
  # season's approximate length (~90-92 days) — see season_n_days(). This is
  # the per-season analogue of compute_mouse_alert_by_aez()'s date-window rate.
  alerts <- d |>
    dplyr::filter(data_type == "observations", !is.na(mouse_abundance),
                  mouse_abundance == "high") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) |>
    dplyr::summarise(n_high_reports = dplyr::n(), .groups = "drop")

  d |>
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys))) |>
    dplyr::left_join(detect, by = join_keys) |>
    dplyr::left_join(means,  by = join_keys) |>
    dplyr::left_join(alerts, by = join_keys) |>
    dplyr::mutate(
      n_paddocks     = dplyr::if_else(is.na(n_paddocks), 0L, n_paddocks),
      n_high_reports = dplyr::if_else(is.na(n_high_reports), 0L, n_high_reports),
      avg_daily_high = round(n_high_reports / season_n_days(season), 3)
    ) |>
    compute_activity_index(weights = weights) |>
    dplyr::mutate(
      # Zero systematic survey paddocks -> "no data" overall, even if
      # MouseAlert reports exist — mirrors aez_summary_r()'s override.
      activity_index = replace(activity_index, n_paddocks == 0, NA_real_)
    )
}

# ── ZNP50 fortnightly schedule ───────────────────────────────────────────────
# Anchored on the first scheduled date; the 14-day cycle extends both forwards
# and backwards to cover the full history.
znp_anchor <- as.Date("2026-05-20")
znp_fortnight_ends <- sort(unique(c(
  seq(znp_anchor, by = "14 days",  length.out = 5),   # scheduled future dates
  seq(znp_anchor, by = "-14 days", length.out = 14)   # backwards ~6 months
)))

# Show only periods whose window has started and fall within the last 6 months,
# ordered most recent first.
znp_available_ends <- local({
  today <- Sys.Date()
  ends  <- znp_fortnight_ends[
    znp_fortnight_ends - 13 <= today &
    znp_fortnight_ends      >= today - 183
  ]
  rev(sort(ends))
})

znp_fortnight_choices <- setNames(
  as.character(znp_available_ends),
  paste("Fortnight ending", format(znp_available_ends, "%d %b %Y"))
)

znp_default_fortnight <- as.character(znp_available_ends[1])   # most recent

# ── ZNP50 history (pre-computed at startup) ───────────────────────────────────
# Uses only surveys_all and znp_available_ends — neither changes with user input
# — so computing this once at startup avoids re-running ~14 classify calls every
# time the ZNP50 tab is opened.
znp_history_wide <- local({
  period_dfs <- lapply(znp_available_ends, function(end_date) {
    start_date <- end_date - 13
    d      <- dplyr::filter(surveys_all, survey_date >= start_date, survey_date <= end_date)
    result <- dplyr::select(compute_znp50_by_aez(d), ae_zone, overall_class)
    names(result)[2] <- as.character(end_date)
    result
  })
  if (length(period_dfs) == 0) return(dplyr::tibble(ae_zone = character(0)))
  Reduce(function(a, b) dplyr::full_join(a, b, by = "ae_zone"), period_dfs) |>
    dplyr::arrange(ae_zone)
})

# ── Shared filter UI builder ─────────────────────────────────────────────────
# Two-step layout: (1) date window sets the dataset boundary, then
# (2) the optional selects narrow within that window.
make_filter_ui <- function(pfx, include_aez = FALSE, type_choices_param = type_choices) {
  list(
    # ── Step 1: date window ───────────────────────────────────────────────────
    tags$div(
      style = paste(
        "border-left: 3px solid #6c757d;",
        "background: #f8f9fa;",
        "border-radius: 0 4px 4px 0;",
        "padding: 10px 16px 6px 16px;",
        "margin-bottom: 0;"
      ),
      tags$p(
        tags$strong("1. Set date window"),
        tags$span(
          " — all data is limited to surveys within this range",
          style = "color: #888; font-size: 0.88em; font-weight: normal;"
        ),
        style = "margin: 0 0 8px 0; font-size: 0.95em;"
      ),
      fluidRow(
        column(3, dateInput(paste0(pfx, "date_start"), "From",
                            min = date_min, max = date_max, value = date_min)),
        column(3, dateInput(paste0(pfx, "date_end"),   "To",
                            min = date_min, max = date_max, value = date_max))
      )
    ),
    # ── Visual connector ─────────────────────────────────────────────────────
    tags$div(
      style = "border-left: 3px solid #dee2e6; margin-left: 0; padding-left: 16px; color: #aaa; line-height: 1.2; font-size: 1.1em;",
      HTML("&#8595;")
    ),
    # ── Step 2: optional filters within window ────────────────────────────────
    tags$div(
      style = paste(
        "border-left: 3px solid #dee2e6;",
        "background: #fdfdfd;",
        "border-radius: 0 4px 4px 0;",
        "padding: 10px 16px 6px 16px;",
        "margin-bottom: 8px;"
      ),
      tags$p(
        tags$strong("2. Narrow within window"),
        tags$span(
          " — optional; leave blank to include everything in the date window",
          style = "color: #888; font-size: 0.88em; font-weight: normal;"
        ),
        style = "margin: 0 0 8px 0; font-size: 0.95em;"
      ),
      fluidRow(
        column(3, selectInput(paste0(pfx, "year_filter"), "Year",
                              choices = year_choices, selected = NULL,
                              multiple = TRUE, selectize = TRUE)),
        column(3, selectInput(paste0(pfx, "season_filter"), "Season",
                              choices = season_choices, selected = NULL,
                              multiple = TRUE, selectize = TRUE)),
        column(3, selectInput(paste0(pfx, "type_filter"), "Data type",
                              choices = type_choices_param, selected = NULL,
                              multiple = TRUE, selectize = TRUE)),
        column(3, selectInput(paste0(pfx, "project_filter"), "Project",
                              choices = project_choices, selected = NULL,
                              multiple = TRUE, selectize = TRUE))
      ),
      fluidRow(
        column(3, selectInput(paste0(pfx, "bait_filter"), "Were mice baited in last 2 months?",
                              choices = bait_choices,
                              selected = c("no", "unsure"),
                              multiple = TRUE, selectize = TRUE))
      ),
      if (include_aez) tags$div(
        tags$p(
          tags$strong("Filter by agro-ecological zone"),
          tags$span(
            " — click zones to include; click again to deselect; nothing selected = all zones included",
            style = "color: #888; font-size: 0.88em; font-weight: normal;"
          ),
          style = "margin: 8px 0 4px 0; font-size: 0.95em;"
        ),
        leafletOutput(paste0(pfx, "aez_map"), height = "260px")
      )
    )
  )
}

# ── Date input guard ─────────────────────────────────────────────────────────
# dateInput() values can briefly become NULL/zero-length or NA while a user is
# editing the field (e.g. clearing it to type a new date), which would crash a
# dplyr::filter() comparison ("must be of size N or 1, not size 0"). Fall back
# to the supplied default (date_min/date_max) whenever that happens.
safe_date <- function(x, default) {
  if (length(x) == 0 || is.na(x)) default else x
}

# ── Mid-season date helper ───────────────────────────────────────────────────
# Season mid-months: Summer=Jan(1), Autumn=Apr(4), Winter=Jul(7), Spring=Oct(10).
# Guards the zero-row case: paste0() recycles zero-length inputs to "" rather
# than returning character(0), so as.Date(paste0(integer(0), "-", character(0),
# "-15")) becomes as.Date("--15") and errors with charToDate(). This happens
# whenever a trend_chart filter combination leaves no qualifying seasons.
season_mid_date <- function(year_adj, season) {
  if (length(year_adj) == 0) return(as.Date(character(0)))
  as.Date(paste0(
    year_adj, "-",
    dplyr::case_when(
      as.character(season) == "Summer" ~ "01",
      as.character(season) == "Autumn" ~ "04",
      as.character(season) == "Winter" ~ "07",
      TRUE                             ~ "10"
    ),
    "-15"
  ))
}

# ── Facet axis scale options ─────────────────────────────────────────────────
# Shared choices for the "Facet axis scales" radio buttons (Trend, Detection
# Patterns, Paddock Trends tabs) — values map directly onto facet_wrap()'s
# scales argument ("fixed" | "free_x" | "free_y" | "free").
facet_scales_choices <- c("Fixed" = "fixed", "Free x" = "free_x",
                           "Free y" = "free_y", "Free x & y" = "free")

# Validates a facet-scales input value, defaulting to "fixed" if NULL/invalid
# (e.g. before the UI has rendered).
facet_scales_value <- function(x) {
  if (isTRUE(x %in% facet_scales_choices)) x else "fixed"
}

# ── Shared filter reactive builder ───────────────────────────────────────────
# Returns a reactive that applies the namespaced filters to surveys_all.
make_filter_reactive <- function(input, pfx, aez_selected = NULL) {
  reactive({
    d <- surveys_all
    d <- dplyr::filter(d, survey_date >= safe_date(input[[paste0(pfx, "date_start")]], date_min),
                          survey_date <= safe_date(input[[paste0(pfx, "date_end")]], date_max))
    if (length(input[[paste0(pfx, "year_filter")]])    > 0) d <- dplyr::filter(d, year_adj     %in% input[[paste0(pfx, "year_filter")]])
    if (length(input[[paste0(pfx, "season_filter")]])  > 0) d <- dplyr::filter(d, season       %in% input[[paste0(pfx, "season_filter")]])
    if (length(input[[paste0(pfx, "type_filter")]])    > 0) d <- dplyr::filter(d, data_type    %in% input[[paste0(pfx, "type_filter")]])
    if (length(input[[paste0(pfx, "project_filter")]]) > 0) d <- dplyr::filter(d, project      %in% input[[paste0(pfx, "project_filter")]])
    if (length(input[[paste0(pfx, "bait_filter")]])    > 0) d <- dplyr::filter(d, bait_history %in% input[[paste0(pfx, "bait_filter")]])
    if (!is.null(aez_selected) && length(aez_selected()) > 0) d <- dplyr::filter(d, ae_zone %in% aez_selected())
    d
  })
}

# ── UI ───────────────────────────────────────────────────────────────────────
ui <- fluidPage(

  useShinyjs(),

  # Keepalive: ping the server every 55 s to prevent shinyapps.io from closing
  # idle sessions. The server-side observeEvent below receives the ping and does
  # nothing — enough to reset the idle timer without any visible effect.
  tags$script(HTML("
    setInterval(function() {
      Shiny.setInputValue('keepalive_ping', Date.now());
    }, 55000);
  ")),

  titlePanel("Mouse Survey Data Explorer"),

  tabsetPanel(

    # ── Tab 1: Map & Table ─────────────────────────────────────────────────
    tabPanel("Map & Table",

      tags$div(style = "margin-top: 15px;",

        make_filter_ui("map_"),

        uiOutput("no_data_msg"),
        tags$p(tags$strong("Survey effort in this date window"),
               style = "margin-bottom: 4px; font-size: 0.95em;"),
        tags$p(textOutput("summary_stats"),
               style = "color: #555; font-size: 0.9em; margin-bottom: 4px;"),

        # tags$p(tags$strong("Download raw datasets for this date window"),
        #        style = "margin-bottom: 4px; font-size: 0.95em;"),
        fluidRow(
          column(9,
            # downloadButton("download_traps", "Download traps (CSV)",
            #                class = "btn btn-outline-secondary btn-sm"),
            # tags$span(style = "display:inline-block; width:8px;"),
            # downloadButton("download_rapid", "Download rapid assessment (CSV)",
            #                class = "btn btn-outline-secondary btn-sm"),
            # tags$span(style = "display:inline-block; width:8px;"),
            # downloadButton("download_observations", "Download observations (CSV)",
            #                class = "btn btn-outline-secondary btn-sm")
            NULL
          ),
          column(3,
            actionButton("map_reset", "Reset filters",
                         class = "btn btn-outline-primary btn-sm",
                         style = "float:right;")
          ),
          style = "padding-bottom: 10px;"
        ),

        tags$hr(),
        tags$h4("Mouse Activity Index weights"),
        tags$p(style = "color: #555; font-size: 0.88em;",
               "Controls how much each metric contributes to the Mouse Activity Index column below. Equal weighting (1) by default — set a metric to 0 to exclude it entirely."),
        fluidRow(
          column(2, sliderInput("weight_pct_detected",   "% paddocks detected",      min = 0, max = 2, value = 1, step = 0.25)),
          column(2, sliderInput("weight_result_traps",   "Mice / trap night",         min = 0, max = 2, value = 1, step = 0.25)),
          column(2, sliderInput("weight_result_burrow",  "Burrows / transect",        min = 0, max = 2, value = 1, step = 0.25)),
          column(2, sliderInput("weight_chew_per10",     "Chew cards / 10",           min = 0, max = 2, value = 1, step = 0.25)),
          column(2, sliderInput("weight_avg_daily_high", "Avg. daily high reports",   min = 0, max = 2, value = 1, step = 0.25)),
          column(2, actionButton("weight_reset", "Reset to equal weights",
                                  class = "btn btn-outline-secondary btn-sm", style = "margin-top: 25px;"))
        ),

        # Colour-scale ceiling for the activity_index column/map: the value at
        # which the green-yellow-red gradient reaches full red, with yellow at
        # half this value (see activity_gradient_color()). activity_index isn't
        # capped at 1, so a standout AEZ/season can exceed this — lower it to
        # make red appear sooner. Default of 2 mirrors activity_index_gradient_max
        # in raw_data_update.qmd's execute_params.
        fluidRow(
          column(3, sliderInput("activity_index_gradient_max", "Activity index colour scale max",
                                 min = 0.5, max = 5, value = 2, step = 0.25))
        ),

        tags$hr(),
        tags$h4("Summary statistics within GRDC Agro-Ecological Zones"),
        tags$p("Result columns are specific to each data type: traps (mice per trap night) and
                rapid assessment (active burrows per transect; chew cards per 10 deployed)."),
        reactableOutput("table"),

        tags$p(
          style = "color: #555; font-size: 0.88em; margin-top: 10px;",
          "AEZs with zero traps/rapid paddocks surveyed in the selected date window have no data for any metric below — MouseAlert reports alone aren't enough to score an AEZ. Such AEZs are omitted from this table, and shown as \"No data\" (grey) on the map."
        ),
        tags$ul(
          style = "color: #555; font-size: 0.88em; margin-top: 4px; padding-left: 20px; line-height: 1.7;",
          tags$li(tags$b("Paddocks surveyed: "),
                  "Number of unique paddocks with at least one trap or rapid assessment survey in the selected date window. MouseAlert-only paddocks are excluded."),
          tags$li(tags$b("Paddocks detected (%): "),
                  "Proportion of surveyed paddocks where mice were detected in at least one visit (any trap catch, active burrow, or chewed card)."),
          tags$li(tags$b("Mice per trap night: "),
                  "Mean catch rate across all trap surveys in the AEZ (mice caught ÷ functional traps per survey night)."),
          tags$li(tags$b("Burrows per transect: "),
                  "Mean number of active burrows per transect across all rapid assessment surveys in the AEZ."),
          tags$li(tags$b("Chew cards per 10: "),
                  "Mean number of chew cards with ≥1% chew per 10 cards deployed, across all rapid assessment surveys in the AEZ."),
          tags$li(tags$b("No. high reports (MouseAlert): "),
                  "Count of citizen-science sightings rated 'high' abundance within the selected date window."),
          tags$li(tags$b("Mouse Activity Index: "),
                  "Combines all five metrics above into a single score: each metric is scaled relative to its dataset-wide reference level (1.0 = that reference level; standout AEZs can exceed 1), then averaged using the weights set above (equal by default).")
        ),

        tags$hr(),
        tags$h4("Map"),
        fluidRow(
          column(12,
            radioButtons("map_metric", "Colour AEZ zones by",
                         choices = c(
                           "% paddocks detected"        = "pct_detected",
                           "Mice / trap night"          = "result_traps",
                           "Burrows / transect"         = "result_burrow",
                           "Chew cards / 10"            = "chew_per10",
                           "Avg. daily high reports"    = "n_high_paddocks",
                           "Mouse Activity Index"       = "activity_index"
                         ),
                         selected = "pct_detected",
                         inline   = TRUE)
          )
        ),
        tags$div(
          style = "padding: 0 4%;",
          leafletOutput("map", width = "100%", height = "560px")
        )
      )
    ),

    # ── Tab 2: Detection Trends ─────────────────────────────────────────────
    tabPanel("Detection Trends",

      tags$div(style = "margin-top: 15px;",

        make_filter_ui("trend_", include_aez = TRUE),

        fluidRow(
          column(3,
            actionButton("trend_reset", "Reset filters",
                         class = "btn btn-outline-primary btn-sm")
          ),
          style = "padding-bottom: 10px;"
        ),

        tags$hr(),

        fluidRow(
          column(4,
            radioButtons("trend_metric", "Metric",
                         choices = c(
                           "Paddocks detected (%)"     = "pct_detected",
                           "Mice / trap night"          = "result_traps",
                           "Burrows / transect"         = "result_burrow",
                           "Chew cards / 10 cards"      = "chew_per10",
                           "High reports (MouseAlert)"  = "n_high_paddocks",
                           "Mouse Activity Index"       = "activity_index"
                         ),
                         selected = "pct_detected")
          ),
          column(4,
            radioButtons("trend_view", "AEZ breakdown",
                         choices = c(
                           "Combined"       = "combined",
                           "Overlay by AEZ" = "overlay",
                           "Facet by AEZ"   = "facet"
                         ),
                         selected = "combined"),
            conditionalPanel(
              condition = "input.trend_view == 'facet'",
              radioButtons("trend_facet_scales", "Facet axis scales",
                           choices = facet_scales_choices,
                           selected = "fixed", inline = TRUE)
            )
          ),
          column(4,
            numericInput("trend_min_paddocks",
                         "Min. paddocks per season",
                         value = 1, min = 1, step = 1, width = "100%"),
            uiOutput("trend_min_pads_note")
          )
        ),

        # Weight sliders only matter for the two combined metrics below —
        # independent of the Map & Table tab's weights (activity_weights_r()).
        conditionalPanel(
          condition = "input.trend_metric == 'activity_index'",
          tags$hr(),
          tags$h4("Mouse Activity Index weights"),
          tags$p(style = "color: #555; font-size: 0.88em;",
                 "Controls how much each metric contributes to the Mouse Activity Index above, computed per season. Equal weighting (1) by default — set a metric to 0 to exclude it entirely. Independent of the weights on the Map & Table tab."),
          fluidRow(
            column(2, sliderInput("trend_weight_pct_detected",   "% paddocks detected",      min = 0, max = 2, value = 1, step = 0.25)),
            column(2, sliderInput("trend_weight_result_traps",   "Mice / trap night",         min = 0, max = 2, value = 1, step = 0.25)),
            column(2, sliderInput("trend_weight_result_burrow",  "Burrows / transect",        min = 0, max = 2, value = 1, step = 0.25)),
            column(2, sliderInput("trend_weight_chew_per10",     "Chew cards / 10",           min = 0, max = 2, value = 1, step = 0.25)),
            column(2, sliderInput("trend_weight_avg_daily_high", "Avg. daily high reports",   min = 0, max = 2, value = 1, step = 0.25)),
            column(2, actionButton("trend_weight_reset", "Reset to equal weights",
                                    class = "btn btn-outline-secondary btn-sm", style = "margin-top: 25px;"))
          )
        ),

        uiOutput("trend_chart_ui"),

        tags$p(
          tags$b("Paddocks detected (%) and High reports (MouseAlert):"),
          " each point shows the seasonal summary across all paddocks meeting the minimum paddock threshold.",
          tags$br(),
          tags$b("Mice / trap night, Burrows / transect, Chew cards / 10:"),
          " semi-transparent points show every individual survey; the blue line shows the seasonal mean across paddocks.",
          tags$br(),
          tags$b("Mouse Activity Index:"),
          " each point shows the seasonal combined index using the weights set above (1 = the dataset-wide reference level for that mix of metrics; standout seasons can exceed 1), with the same \"no data\" rule as the Map & Table tab (an AEZ-season with zero surveyed paddocks is excluded, even if MouseAlert reports exist).",
          style = "color: #666; font-size: 0.85em; margin-top: 10px;"
        )
      )
    ),

    # ── Tab 3: Detection Patterns ──────────────────────────────────────────────
    tabPanel("Detection Patterns",

      tags$div(style = "margin-top: 15px;",

        make_filter_ui("pattern_", include_aez = TRUE),

        fluidRow(
          column(3,
            actionButton("pattern_reset", "Reset filters",
                         class = "btn btn-outline-primary btn-sm")
          ),
          style = "padding-bottom: 10px;"
        ),

        tags$hr(),

        fluidRow(
          column(3,
            radioButtons("pattern_xvar", "X variable",
                         choices = c(
                           "Season"       = "season",
                           "Month"        = "month",
                           "Ground cover" = "ground_cover",
                           "Bait history" = "bait_history"
                         ),
                         selected = "season")
          ),
          column(3,
            radioButtons("pattern_metric", "Metric",
                         choices = c(
                           "Paddocks detected (%)"     = "pct_detected",
                           "Mice / trap night"          = "result_traps",
                           "Burrows / transect"         = "result_burrow",
                           "Chew cards / 10 cards"      = "chew_per10",
                           "High reports (MouseAlert)"  = "n_high_paddocks"
                         ),
                         selected = "pct_detected")
          ),
          column(3,
            radioButtons("pattern_view", "AEZ breakdown",
                         choices = c(
                           "Combined"     = "combined",
                           "Facet by AEZ" = "facet"
                         ),
                         selected = "combined"),
            conditionalPanel(
              condition = "input.pattern_view == 'facet'",
              radioButtons("pattern_facet_scales", "Facet axis scales",
                           choices = facet_scales_choices,
                           selected = "fixed", inline = TRUE)
            )
          ),
          column(3,
            numericInput("pattern_min_paddocks",
                         "Min. paddocks per group",
                         value = 3, min = 1, step = 1, width = "100%"),
            tags$p("Groups with fewer paddocks are excluded.",
                   style = "color: #888; font-size: 0.85em; margin-top: 2px;")
          )
        ),

        uiOutput("pattern_note_ui"),
        uiOutput("pattern_chart_ui")
      )
    ),

    # ── Tab 4: Paddock Trends ──────────────────────────────────────────────────
    tabPanel("Paddock Trends",

      tags$div(style = "margin-top: 15px;",

        make_filter_ui("pad_trend_", include_aez = TRUE,
                       type_choices_param = c("traps", "rapid")),

        fluidRow(
          column(3,
            actionButton("pad_trend_reset", "Reset filters",
                         class = "btn btn-outline-primary btn-sm")
          ),
          style = "padding-bottom: 10px;"
        ),

        tags$hr(),

        fluidRow(
          column(4,
            numericInput("pad_trend_min_surveys",
                         "Min. surveys per paddock",
                         value = 10, min = 1, step = 1, width = "100%"),
            tags$p("Paddocks with fewer total surveys are excluded.",
                   style = "color: #888; font-size: 0.85em; margin-top: 2px;")
          ),
          column(4,
            radioButtons("pad_trend_facet_scales", "Facet axis scales",
                         choices = facet_scales_choices,
                         selected = "fixed", inline = TRUE)
          )
        ),

        uiOutput("pad_trend_chart_ui"),

        tags$p(
          "All three metrics are plotted on a common 0–1 scale by dividing each survey value by the",
          "maximum value observed across the entire dataset:",
          tags$b("mice / trap night"), " ÷ ", round(trend_max_result_traps, 3), ",",
          tags$b(" burrows / transect"), " ÷ ", round(trend_max_result_burrow, 2), ",",
          tags$b(" chew cards / 10"), " ÷ ", round(trend_max_chew_per10, 2), ".",
          style = "color: #666; font-size: 0.85em; margin-top: 10px;"
        )
      )
    ),

    # ── Tab 5: ZNP50 Permit ────────────────────────────────────────────────────
    tabPanel("ZNP50 Permit",

      tags$div(style = "margin-top: 15px;",

        fluidRow(
          column(4,
            selectInput("znp_fortnight", "Survey period",
                        choices  = znp_fortnight_choices,
                        selected = znp_default_fortnight)
          ),
          column(4,
            tags$div(style = "padding-top: 26px; color: #666; font-size: 0.9em;",
                     textOutput("znp_period_text"))
          ),
          style = "padding-bottom: 10px;"
        ),

        tags$hr(),
        tags$h4("ZNP50 Permit — Rapid Assessment Summary by AEZ"),
        tags$div(
          style = "color: #555; font-size: 0.9em; margin-bottom: 10px;",
          tags$p(style = "margin-bottom: 4px;",
            "Shows the highest site-level mean for each metric within each AEZ ",
            "(rapid assessment surveys only). Colour thresholds per column:"
          ),
          tags$ul(
            style = "margin: 0; padding-left: 20px;",
            tags$li(tags$b("Chew % per card — "),
                    "Low: <10%;  Moderate: ≥10%;  High: ≥20%"),
            tags$li(tags$b("Active burrows per transect — "),
                    "Low: ≤1;  Moderate: >1;  High: >2"),
            tags$li(tags$b("Chew cards per 10 — "),
                    "same thresholds as burrows (informational only)"),
            tags$li(tags$b("ZNP50 classification — "),
                    "takes the higher of chew % and burrows per transect; ",
                    "chew cards per 10 does not contribute")
          )
        ),
        reactableOutput("znp_table"),

        tags$hr(),
        tags$h4("ZNP50 Classification Map"),
        tags$div(
          style = "padding: 0 4%;",
          leafletOutput("znp_map", width = "100%", height = "560px")
        ),

        tags$hr(),
        tags$h4("ZNP50 Classification History"),
        tags$p("Overall classification per AEZ across all available fortnightly periods.",
               style = "color: #555; font-size: 0.9em;"),
        tags$div(
          style = "overflow-x: auto;",
          reactableOutput("znp_history_table")
        )
      )
    )
  )
)

# ── Server ───────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Receive keepalive pings from the UI — no-op, just resets the idle timer.
  observeEvent(input$keepalive_ping, {}, ignoreInit = TRUE)

  # ── AEZ selector map helper ────────────────────────────────────────────────
  # Renders a small clickable AEZ polygon map.
  # single = FALSE (default): click toggles zones in/out of a multi-selection.
  # single = TRUE: click always replaces the selection with the clicked zone;
  #   clicking the already-selected zone is a no-op (one zone always selected).
  # aez_rv: reactiveVal holding a character vector of selected ae_zone names.
  setup_aez_selector <- function(pfx, aez_rv, single = FALSE) {
    map_id <- paste0(pfx, "aez_map")

    output[[map_id]] <- renderLeaflet({
      # isolate() reads the initial selection without creating a reactive
      # dependency — so this block runs once and the proxy observer handles
      # all subsequent highlight updates without re-initialising the map.
      selected <- isolate(aez_rv())
      bb <- sf::st_bbox(aez_base_surveyed)
      leaflet(aez_base_surveyed, options = leafletOptions(zoomControl = FALSE)) |>
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
        addProviderTiles("CartoDB.Positron") |>
        addPolygons(
          layerId          = ~ae_zone,
          fillColor        = ifelse(aez_base_surveyed$ae_zone %in% selected, "#EC7014", "#AAAAAA"),
          fillOpacity      = ifelse(aez_base_surveyed$ae_zone %in% selected, 0.75, 0.45),
          color            = "white",
          weight           = 1,
          label            = ~ae_zone,
          highlightOptions = highlightOptions(weight = 2, fillOpacity = 0.65, bringToFront = TRUE)
        )
    })

    # Single-select: replace selection; multi-select: toggle in/out
    observeEvent(input[[paste0(map_id, "_shape_click")]], {
      click <- input[[paste0(map_id, "_shape_click")]]
      if (is.null(click$id)) return()
      if (single) {
        if (!identical(aez_rv(), click$id)) aez_rv(click$id)
      } else {
        current <- aez_rv()
        aez_rv(if (click$id %in% current) setdiff(current, click$id) else c(current, click$id))
      }
    })

    # Redraw polygons with selected zones highlighted in orange
    observe({
      selected <- aez_rv()
      leafletProxy(map_id) |>
        clearShapes() |>
        addPolygons(
          data             = aez_base_surveyed,
          layerId          = ~ae_zone,
          fillColor        = ifelse(aez_base_surveyed$ae_zone %in% selected, "#EC7014", "#AAAAAA"),
          fillOpacity      = ifelse(aez_base_surveyed$ae_zone %in% selected, 0.75, 0.45),
          color            = "white",
          weight           = 1,
          label            = ~ae_zone,
          highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.9, bringToFront = TRUE)
        )
    })
  }

  # ── Map tab filters ────────────────────────────────────────────────────────
  surveys_filtered <- make_filter_reactive(input, "map_")

  observeEvent(input$map_reset, {
    updateDateInput(session,   "map_date_start",    value    = date_min)
    updateDateInput(session,   "map_date_end",      value    = date_max)
    updateSelectInput(session, "map_year_filter",   selected = character(0))
    updateSelectInput(session, "map_season_filter", selected = character(0))
    updateSelectInput(session, "map_type_filter",   selected = character(0))
    updateSelectInput(session, "map_project_filter",selected = character(0))
    updateSelectInput(session, "map_bait_filter",   selected = c("no", "unsure"))
  })

  # Restore the Mouse Activity Index weight sliders to equal weighting.
  observeEvent(input$weight_reset, {
    updateSliderInput(session, "weight_pct_detected",   value = 1)
    updateSliderInput(session, "weight_result_traps",   value = 1)
    updateSliderInput(session, "weight_result_burrow",  value = 1)
    updateSliderInput(session, "weight_chew_per10",     value = 1)
    updateSliderInput(session, "weight_avg_daily_high", value = 1)
  })

  # Raw pipeline data filtered by map tab inputs — used for downloads.
  # surveys_raw_filtered <- reactive({
  #   d <- surveys_raw
  #   d <- dplyr::filter(d, survey_date >= safe_date(input$map_date_start, date_min),
  #                         survey_date <= safe_date(input$map_date_end, date_max))
  #   if (length(input$map_year_filter)    > 0) d <- dplyr::filter(d, year_adj     %in% input$map_year_filter)
  #   if (length(input$map_season_filter)  > 0) d <- dplyr::filter(d, season       %in% input$map_season_filter)
  #   if (length(input$map_type_filter)    > 0) d <- dplyr::filter(d, data_type    %in% input$map_type_filter)
  #   if (length(input$map_project_filter) > 0) d <- dplyr::filter(d, project      %in% input$map_project_filter)
  #   if (length(input$map_bait_filter)    > 0) d <- dplyr::filter(d, bait_history %in% input$map_bait_filter)
  #   d
  # })

  # ── Trend tab filters ──────────────────────────────────────────────────────
  trend_aez_selected <- reactiveVal(character(0))
  setup_aez_selector("trend_", trend_aez_selected)
  trend_filtered <- make_filter_reactive(input, "trend_", aez_selected = trend_aez_selected)

  observeEvent(input$trend_reset, {
    updateDateInput(session,   "trend_date_start",    value    = date_min)
    updateDateInput(session,   "trend_date_end",      value    = date_max)
    updateSelectInput(session, "trend_year_filter",   selected = character(0))
    updateSelectInput(session, "trend_season_filter", selected = character(0))
    updateSelectInput(session, "trend_type_filter",   selected = character(0))
    updateSelectInput(session, "trend_project_filter",selected = character(0))
    updateSelectInput(session, "trend_bait_filter",   selected = c("no", "unsure"))
    trend_aez_selected(character(0))
  })

  # User-adjustable weights for Tab 2's Mouse Activity Index / Activity
  # category trend metrics — independent of activity_weights_r() (Map & Table tab).
  trend_activity_weights_r <- reactive({
    req(input$trend_weight_pct_detected, input$trend_weight_result_traps, input$trend_weight_result_burrow,
        input$trend_weight_chew_per10, input$trend_weight_avg_daily_high)
    c(pct_detected   = input$trend_weight_pct_detected,
      result_traps   = input$trend_weight_result_traps,
      result_burrow  = input$trend_weight_result_burrow,
      chew_per10     = input$trend_weight_chew_per10,
      avg_daily_high = input$trend_weight_avg_daily_high)
  })

  observeEvent(input$trend_weight_reset, {
    updateSliderInput(session, "trend_weight_pct_detected",   value = 1)
    updateSliderInput(session, "trend_weight_result_traps",   value = 1)
    updateSliderInput(session, "trend_weight_result_burrow",  value = 1)
    updateSliderInput(session, "trend_weight_chew_per10",     value = 1)
    updateSliderInput(session, "trend_weight_avg_daily_high", value = 1)
  })

  # ── Map tab reactives ──────────────────────────────────────────────────────
  mice_by_aez_r        <- reactive({ compute_mice_by_aez(surveys_filtered()) })
  # n_days (the avg_daily_high denominator) is normally
  # map_date_end - map_date_start + 1 — the *selected* date window, regardless
  # of whether a survey happens to fall on its boundary dates — matching
  # compute_mouse_alert_by_aez() in raw_data_update.qmd. If a year/season
  # filter is also active, map_date_start/map_date_end default to the full
  # ~13.6-year history and don't reflect that narrowing, so n_days falls back
  # to the *actual* date range of the filtered data instead — otherwise a
  # season's worth of MouseAlert "high" reports would be diluted across ~4965
  # days instead of ~91, making a genuine spike (e.g. 20 reports in one
  # season) look negligible.
  mouse_alert_by_aez_r <- reactive({
    d <- surveys_filtered()
    date_range <- if (length(input$map_year_filter) > 0 || length(input$map_season_filter) > 0) {
      if (nrow(d) == 0) c(date_min, date_max) else range(d$survey_date)
    } else {
      c(safe_date(input$map_date_start, date_min), safe_date(input$map_date_end, date_max))
    }
    compute_mouse_alert_by_aez(d, date_range[1], date_range[2])
  })

  # User-adjustable weights for the Mouse Activity Index, from the sliders
  # under the summary table. req() guards the brief moment
  # before the sliders are initialised; isTruthy(0) is TRUE so a slider set
  # to 0 (exclude this metric) still passes.
  activity_weights_r <- reactive({
    req(input$weight_pct_detected, input$weight_result_traps, input$weight_result_burrow,
        input$weight_chew_per10, input$weight_avg_daily_high)
    c(pct_detected   = input$weight_pct_detected,
      result_traps   = input$weight_result_traps,
      result_burrow  = input$weight_result_burrow,
      chew_per10     = input$weight_chew_per10,
      avg_daily_high = input$weight_avg_daily_high)
  })

  aez_display_r <- reactive({
    req(input$map_metric)
    if (input$map_metric == "n_high_paddocks") {
      stats <- mouse_alert_by_aez_r() |>
        dplyr::transmute(
          ae_zone       = ae_zone,
          n_sites       = NA_integer_,
          n_detected    = NA_integer_,
          display_val   = as.numeric(avg_daily_high),
          traffic_light = NA_character_
        )
    } else if (input$map_metric == "activity_index") {
      # Continuous 0-1 combined index — see compute_activity_index().
      stats <- aez_summary_r() |>
        dplyr::transmute(
          ae_zone       = ae_zone,
          n_sites       = n_paddocks,
          n_detected    = NA_integer_,
          display_val   = as.numeric(activity_index),
          traffic_light = NA_character_
        )
    } else {
      stats <- compute_aez_display(surveys_filtered(), input$map_metric)
    }
    dplyr::left_join(aez_base, stats, by = "ae_zone")
  })

  surveys_for_table <- reactive({
    surveys_filtered() |>
      dplyr::left_join(
        dplyr::select(mice_by_aez_r(), ae_zone, pct_detected, n_sites, n_detected),
        by = "ae_zone"
      ) |>
      dplyr::left_join(
        dplyr::select(mouse_alert_by_aez_r(), ae_zone, n_high_reports, avg_daily_high),
        by = "ae_zone"
      )
  })

  # ── Per-AEZ summary (shared by the table and the combined-metric map) ──────
  # One row per AEZ with the five table metrics plus activity_index,
  # computed using the user's weight sliders.
  aez_summary_r <- reactive({
    surveys_for_table() |>
      dplyr::filter(!is.na(ae_zone)) |>
      dplyr::group_by(ae_zone) |>
      dplyr::summarise(
        # n_paddocks mirrors the denominator behind pct_detected (traps/rapid
        # only) — NA (-> 0 below) where the AEZ has no systematic survey
        # paddocks at all.
        n_paddocks     = dplyr::first(n_sites),
        pct_detected   = dplyr::first(pct_detected),
        result_traps   = mean(result_traps, na.rm = TRUE),
        result_burrow  = mean(result_burrow, na.rm = TRUE),
        chew_per10     = mean(chew_per10, na.rm = TRUE),
        n_high_reports = dplyr::first(n_high_reports),
        avg_daily_high = dplyr::first(avg_daily_high),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        # An AEZ with zero traps/rapid paddocks is a known "0", not missing —
        # show it as such (no grey shading) rather than NA.
        n_paddocks    = dplyr::if_else(is.na(n_paddocks), 0L, n_paddocks),
        result_traps  = round(dplyr::if_else(is.nan(result_traps), NA_real_, result_traps), 3),
        result_burrow = round(dplyr::if_else(is.nan(result_burrow), NA_real_, result_burrow), 2),
        chew_per10    = round(dplyr::if_else(is.nan(chew_per10), NA_real_, chew_per10), 2)
      ) |>
      compute_activity_index(weights = activity_weights_r()) |>
      dplyr::mutate(
        # Zero systematic survey paddocks -> the Mouse Activity Index is "no
        # data" overall, even if MouseAlert reports exist for the AEZ.
        # Citizen reports alone aren't a substitute for a survey baseline.
        activity_index = replace(activity_index, n_paddocks == 0, NA_real_)
      )
  })

  # ── No-data message ────────────────────────────────────────────────────────
  output$no_data_msg <- renderUI({
    if (nrow(surveys_filtered()) == 0)
      tags$div(class = "alert alert-warning", style = "margin-top:10px;",
               "No surveys match the current filters.")
  })

  # ── Summary stats ──────────────────────────────────────────────────────────
  output$summary_stats <- renderText({
    d <- surveys_filtered()
    if (nrow(d) == 0) return("")
    sprintf("%d paddocks · %d surveys · %d AEZs",
            dplyr::n_distinct(d$paddock_id), nrow(d), dplyr::n_distinct(d$ae_zone))
  })

  # ── Enable / disable download buttons ─────────────────────────────────────
  # observe({
  #   d <- surveys_raw_filtered()
  #   if (nrow(dplyr::filter(d, data_type == "traps")) > 0) shinyjs::enable("download_traps")
  #   else shinyjs::disable("download_traps")
  #   if (nrow(dplyr::filter(d, data_type == "rapid")) > 0) shinyjs::enable("download_rapid")
  #   else shinyjs::disable("download_rapid")
  #   if (nrow(dplyr::filter(d, data_type == "observations")) > 0) shinyjs::enable("download_observations")
  #   else shinyjs::disable("download_observations")
  # })

  # ── Initial map render ─────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    bb <- sf::st_bbox(aez_base)
    leaflet(width = "100%", height = "560px") |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
      addProviderTiles("CartoDB.Positron",     group = "Light (default)") |>
      addProviderTiles("Esri.WorldImagery",    group = "Satellite") |>
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") |>
      addLegend(position = "bottomleft",
                colors = c("#555555", "white"), labels = c("Detected", "Not detected"),
                title = "Survey sites", opacity = 0.85) |>
      addLegend(position = "bottomright", pal = pal_aez_pct, values = c(0, 100),
                title = "Mouse presence<br><small>% of paddocks with detection</small>",
                labFormat = labelFormat(suffix = "%"), opacity = 0.85,
                layerId = "aez_legend") |>
      addLayersControl(
        baseGroups    = c("Light (default)", "Satellite", "OpenStreetMap"),
        overlayGroups = c("AEZ zones", "Survey sites"),
        options       = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(position = "topright", options = scaleBarOptions(imperial = FALSE))
  })

  # ── Reactive map updates ───────────────────────────────────────────────────
  observe({
    req(input$map_metric, input$map_date_start, input$map_date_end)
    aez    <- aez_display_r()
    surv   <- surveys_filtered()
    metric <- input$map_metric

    # Build a palette and legend config for the selected metric
    if (isTRUE(metric == "pct_detected")) {
      pal_fill   <- pal_aez_pct
      pal_legend <- pal_aez_pct
      leg_vals   <- c(0, 100)
      leg_sfx    <- "%"
      leg_title  <- "Mouse presence<br><small>% of paddocks with detection</small>"
    } else {
      # Use the same max as the table colour scale for each metric so the two
      # representations are directly comparable:
      #   - Systematic metrics: fixed global per-AEZ mean max (effort-standardised)
      #   - n_high_paddocks: full-dataset rate max (avg_daily_high anchored to full dataset)
      #   - activity_index: NOT capped at 1 by compute_activity_index() — a
      #     standout AEZ/season can exceed its reference level. Anchor to the
      #     activity_index_gradient_max slider ceiling rather than the current
      #     view's own max, so a quiet period reads as green/yellow; the table
      #     uses the same input for the same reason.
      # switch() default of 1 prevents NULL result if metric is unrecognised,
      # which would make is.finite(NULL) return logical(0) and crash the if().
      dm <- switch(metric,
        result_traps    = gradient_max_result_traps,
        result_burrow   = gradient_max_result_burrow,
        chew_per10      = gradient_max_chew_per10,
        n_high_paddocks = gradient_max_avg_daily_high,
        activity_index  = input$activity_index_gradient_max,
        1
      )
      if (!is.finite(dm) || dm == 0) dm <- 1
      pal_fill_base <- leaflet::colorNumeric(
        palette  = activity_gradient_colours,
        domain   = c(0, dm),
        na.color = "#BDBDBD"
      )
      # Wrap palette to clamp values to the domain ceiling before colour lookup.
      # Prevents out-of-range warnings when a filtered window produces a higher
      # rate than the full-dataset reference max (e.g. a narrow high-activity
      # period). NA values pass through unchanged; popup text uses the raw value.
      # pal_fill clamps values to the domain ceiling before colour lookup to
      # prevent out-of-range warnings; pal_legend must be the raw colorNumeric
      # so addLegend can read its colorType attribute internally.
      pal_fill   <- function(x) pal_fill_base(pmin(x, dm))
      pal_legend <- pal_fill_base
      leg_vals   <- c(0, dm)
      leg_sfx   <- ""
      leg_title <- switch(metric,
        result_traps    = "Mice / trap night<br><small>mean per AEZ</small>",
        result_burrow   = "Burrows / transect<br><small>mean per AEZ</small>",
        chew_per10      = "Chew cards / 10<br><small>mean per AEZ</small>",
        n_high_paddocks = "MouseAlert<br><small>Avg. daily high reports</small>",
        activity_index  = "Mouse Activity Index<br><small>all metrics combined, relative to typical AEZ levels</small>"
      )
    }

    metric_label <- switch(metric,
      pct_detected    = "Paddocks detected",
      result_traps    = "Mice / trap night",
      result_burrow   = "Burrows / transect",
      chew_per10      = "Chew cards / 10",
      n_high_paddocks = "Avg. daily high reports (MouseAlert)",
      activity_index  = "Mouse Activity Index"
    )

    # Build per-AEZ popup text — branch outside mutate to avoid scalar-condition issues.
    # isTRUE() guards against zero-length metric which would crash if().
    if (isTRUE(metric == "pct_detected")) {
      aez <- dplyr::mutate(aez,
        popup_text = paste0(
          "<b>AEZ:</b> ", ae_zone, "<br>",
          "<b>Paddocks detected:</b> ",
          ifelse(is.na(display_val), "—",
                 paste0(n_detected, " / ", n_sites, " (", display_val, "%)"))
        )
      )
    } else {
      aez <- dplyr::mutate(aez,
        popup_text = paste0(
          "<b>AEZ:</b> ", ae_zone, "<br>",
          "<b>", metric_label, ":</b> ",
          ifelse(is.na(display_val) | !is.finite(display_val), "—",
                 as.character(round(display_val, 2))), "<br>",
          "<b>Paddocks surveyed:</b> ", ifelse(is.na(n_sites), "—", as.character(n_sites))
        )
      )
    }

    proxy <- leafletProxy("map") |>
      clearGroup("AEZ zones") |>
      addPolygons(
        data        = aez,
        group       = "AEZ zones",
        layerId     = ~ae_zone,
        fillColor   = ~pal_fill(display_val),
        fillOpacity = 0.8,
        color       = "white",
        weight      = 1.2,
        label       = ~ae_zone,
        popup       = ~popup_text,
        highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 1.0, bringToFront = FALSE)
      ) |>
      clearGroup("Survey sites") |>
      removeControl("aez_legend") |>
      addLegend(
        position  = "bottomright",
        pal       = pal_legend,
        values    = leg_vals,
        title     = leg_title,
        labFormat = labelFormat(suffix = leg_sfx),
        opacity   = 0.85,
        layerId   = "aez_legend"
      )

    if (isTRUE(metric == "n_high_paddocks")) {
      # When the MouseAlert metric is selected, plot one circle per paddock that
      # has any MouseAlert observation in the filtered window. Fill opacity mirrors
      # the systematic-survey convention: filled = detected (medium/high abundance).
      ma_surv <- dplyr::filter(surv, data_type == "observations",
                               !is.na(longitude_paddock), !is.na(latitude_paddock))
      if (nrow(ma_surv) > 0) {
        ma_sites <- ma_surv |>
          dplyr::group_by(paddock_id, ae_zone, state) |>
          dplyr::summarise(
            longitude     = dplyr::first(longitude_paddock),
            latitude      = dplyr::first(latitude_paddock),
            mice_detected = any(mice_detected, na.rm = TRUE),
            n_reports     = dplyr::n(),
            n_high        = sum(as.character(mouse_abundance) == "high", na.rm = TRUE),
            .groups       = "drop"
          ) |>
          dplyr::mutate(label_popup = paste0(
            "<b>Paddock ID:</b> ", paddock_id, "<br>",
            "<b>State:</b> ",      state,      "<br>",
            "<b>AEZ:</b> ",        ae_zone,    "<br><br>",
            "<b>MouseAlert reports:</b> ", n_reports,
            " total (", n_high, " high)"
          ))
        proxy |>
          addCircleMarkers(
            data        = ma_sites,
            lng         = ~longitude, lat = ~latitude,
            group       = "Survey sites",
            radius      = 4,
            color       = marker_col, fillColor = marker_col,
            fillOpacity = ~ifelse(mice_detected, 0.85, 0),
            stroke      = TRUE, weight = 1.5, opacity = 1,
            popup       = ~label_popup, label = ~paddock_id
          )
      }
    } else {
      # Restrict site markers to the data type(s) that back the selected metric so
      # the circles on the map match what is being summarised in the AEZ polygons.
      surv_types <- switch(metric,
        result_traps  = "traps",
        result_burrow = "rapid",
        chew_per10    = "rapid",
        c("traps", "rapid")   # pct_detected: both contribute
      )
      # For single-method metrics, also drop rows where that method's result is
      # NA (e.g. a rapid survey that deployed burrow transects but no chew cards
      # should not appear when the chew_per10 metric is selected, and vice versa).
      metric_col <- switch(metric,
        result_traps  = "result_traps",
        result_burrow = "result_burrow",
        chew_per10    = "chew_per10",
        NULL  # pct_detected uses both types, no extra column filter needed
      )
      sys_surv <- dplyr::filter(surv, data_type %in% surv_types,
                                !is.na(longitude_paddock), !is.na(latitude_paddock))
      if (!is.null(metric_col)) {
        sys_surv <- dplyr::filter(sys_surv, !is.na(.data[[metric_col]]))
      }
      if (nrow(sys_surv) > 0) {
        # Metric-specific labels and value extractor used in the popup.
        metric_col_label <- switch(metric,
          result_traps  = "Mice / trap night",
          result_burrow = "Burrows / transect",
          chew_per10    = "Chew cards / 10",
          "Detected (%)"   # pct_detected
        )
        metric_val_fn <- switch(metric,
          result_traps  = function(d) mean(d$result_traps, na.rm = TRUE),
          result_burrow = function(d) mean(d$result_burrow, na.rm = TRUE),
          chew_per10    = function(d) mean(d$chew_per10,   na.rm = TRUE),
          function(d)    mean(d$mice_detected,             na.rm = TRUE) * 100
        )
        metric_fmt_fn <- switch(metric,
          result_traps  = function(v) paste0(round(v, 3)),
          result_burrow = function(v) paste0(round(v, 2)),
          chew_per10    = function(v) paste0(round(v, 2)),
          function(v)    paste0(round(v, 1), "%")
        )

        surv_sites <- sys_surv |>
          dplyr::group_by(paddock_id, ae_zone, state) |>
          dplyr::summarise(
            longitude     = dplyr::first(longitude_paddock),
            latitude      = dplyr::first(latitude_paddock),
            mice_detected = any(mice_detected, na.rm = TRUE),
            n_surveys     = dplyr::n(),
            metric_val    = metric_val_fn(dplyr::pick(dplyr::everything())),
            .groups       = "drop"
          ) |>
          dplyr::mutate(
            label_popup = paste0(
              "<b>Paddock ID:</b> ", paddock_id, "<br>",
              "<b>State:</b> ",      state,      "<br>",
              "<b>AEZ:</b> ",        ae_zone,    "<br><br>",
              "<b>", metric_col_label, " (", n_surveys, " surveys):</b> ",
              ifelse(is.finite(metric_val), metric_fmt_fn(metric_val), "—"), "<br>",
              "<b>Detected:</b> ", ifelse(mice_detected, "Yes", "No")
            )
          )

        proxy |>
          addCircleMarkers(
            data        = surv_sites,
            lng         = ~longitude, lat = ~latitude,
            group       = "Survey sites",
            radius      = 4,
            color       = marker_col, fillColor = marker_col,
            fillOpacity = ~ifelse(mice_detected, 0.85, 0),
            stroke      = TRUE, weight = 1.5, opacity = 1,
            popup       = ~label_popup, label = ~paddock_id
          )
      }
    }
  })

  # ── Summary table ──────────────────────────────────────────────────────────
  output$table <- renderReactable({
    df <- aez_summary_r() |>
      # AEZs with zero systematic survey paddocks have no data for this table
      # (MouseAlert reports alone aren't shown here) — drop the row entirely
      # rather than showing a mostly-empty/grey row. They still appear on the
      # map, shaded as "No data".
      dplyr::filter(n_paddocks > 0) |>
      dplyr::mutate(
        # Each systematic metric is greyed independently via
        # activity_gradient_color()'s own NA handling — i.e. only when 0
        # paddocks contributed a value to that specific metric. Paddocks
        # surveyed (n_paddocks) is a known count (possibly 0) and is never
        # shaded.
        bg_pct_detected    = activity_gradient_color(pct_detected, 100),
        bg_result_traps    = activity_gradient_color(result_traps, gradient_max_result_traps),
        bg_result_burrow   = activity_gradient_color(result_burrow, gradient_max_result_burrow),
        bg_chew_per10      = activity_gradient_color(chew_per10, gradient_max_chew_per10),
        # MouseAlert: anchor to the full-dataset rate max so colour scale is consistent across filters.
        bg_avg_daily_high  = activity_gradient_color(avg_daily_high, gradient_max_avg_daily_high),
        bg_activity_index  = activity_gradient_color(activity_index, input$activity_index_gradient_max)
      ) |>
      dplyr::arrange(ae_zone)

    bg_style <- function(col) {
      # Converts hex to rgba(r,g,b,0.8) so cell background matches map opacity.
      # Luminance is computed from the raw RGB values (not alpha-adjusted) so
      # the white-text threshold is unaffected by the transparency.
      JS(sprintf(
        "function(rowInfo) {
          var bg = rowInfo.row['%s'];
          if (!bg) return {};
          var r = parseInt(bg.slice(1,3),16),
              g = parseInt(bg.slice(3,5),16),
              b = parseInt(bg.slice(5,7),16),
              lum = 0.299*(r/255) + 0.587*(g/255) + 0.114*(b/255);
          return {
            background: 'rgba(' + r + ',' + g + ',' + b + ',0.8)',
            color: lum < 0.45 ? 'white' : 'inherit'
          };
        }",
        col
      ))
    }

    reactable::reactable(
      df,
      columns = list(
        ae_zone         = reactable::colDef(name = "AEZ", minWidth = 160),
        n_paddocks      = reactable::colDef(name = "Paddocks surveyed", minWidth = 130),
        pct_detected    = reactable::colDef(name = "Paddocks detected (%)", minWidth = 130,
                            format = reactable::colFormat(suffix = "%", digits = 1),
                            style  = bg_style("bg_pct_detected")),
        result_traps    = reactable::colDef(name = "Mice per trap (average)", minWidth = 130,
                            format = reactable::colFormat(digits = 3),
                            style  = bg_style("bg_result_traps")),
        result_burrow   = reactable::colDef(name = "Burrows per transect (average)", minWidth = 130,
                            format = reactable::colFormat(digits = 2),
                            style  = bg_style("bg_result_burrow")),
        chew_per10      = reactable::colDef(name = "Chew cards per 10 (average)", minWidth = 130,
                            format = reactable::colFormat(digits = 2),
                            style  = bg_style("bg_chew_per10")),
        n_high_reports  = reactable::colDef(name = "No. high reports", minWidth = 130,
                            style = bg_style("bg_avg_daily_high")),
        activity_index    = reactable::colDef(name = "Mouse Activity Index", minWidth = 130,
                            format = reactable::colFormat(digits = 2),
                            style  = bg_style("bg_activity_index")),
        avg_daily_high     = reactable::colDef(show = FALSE),
        bg_pct_detected    = reactable::colDef(show = FALSE),
        bg_result_traps    = reactable::colDef(show = FALSE),
        bg_result_burrow   = reactable::colDef(show = FALSE),
        bg_chew_per10      = reactable::colDef(show = FALSE),
        bg_avg_daily_high  = reactable::colDef(show = FALSE),
        bg_activity_index    = reactable::colDef(show = FALSE)
      ),
      columnGroups = list(
        reactable::colGroup(name = "Systematic surveys (traps / rapid assessment)",
                            columns = c("n_paddocks", "pct_detected", "result_traps", "result_burrow", "chew_per10")),
        reactable::colGroup(name = "MouseAlert",
                            columns = "n_high_reports"),
        reactable::colGroup(name = "Combined (all metrics)",
                            columns = "activity_index")
      ),
      defaultSorted = list(ae_zone = "asc"),
      pagination = FALSE, bordered = TRUE, striped = FALSE, highlight = TRUE
    )
  })

  # ── Download handlers ──────────────────────────────────────────────────────
  # output$download_traps <- downloadHandler(
  #   filename = function() paste0("survey_traps_", Sys.Date(), ".csv"),
  #   content  = function(file)
  #     write.csv(dplyr::filter(surveys_raw_filtered(), data_type == "traps"), file, row.names = FALSE)
  # )
  # output$download_rapid <- downloadHandler(
  #   filename = function() paste0("survey_rapid_", Sys.Date(), ".csv"),
  #   content  = function(file)
  #     write.csv(dplyr::filter(surveys_raw_filtered(), data_type == "rapid"), file, row.names = FALSE)
  # )
  # output$download_observations <- downloadHandler(
  #   filename = function() paste0("survey_observations_", Sys.Date(), ".csv"),
  #   content  = function(file)
  #     write.csv(dplyr::filter(surveys_raw_filtered(), data_type == "observations"), file, row.names = FALSE)
  # )

  # Contextual note below the min-paddocks input explaining what is filtered
  output$trend_min_pads_note <- renderUI({
    note <- if (isTRUE(input$trend_metric == "n_high_paddocks"))
      "Seasons with fewer distinct MouseAlert reporting paddocks are excluded."
    else
      "Seasons with fewer systematically surveyed paddocks are excluded."
    tags$p(note, style = "color: #888; font-size: 0.85em; margin-top: 2px;")
  })

  # ── Detection trend chart ──────────────────────────────────────────────────
  # Plot height varies: facet needs more room than combined/overlay.
  output$trend_chart_ui <- renderUI({
    h <- if (isTRUE(input$trend_view == "facet")) "700px" else "380px"
    plotOutput("trend_chart", height = h)
  })

  output$trend_chart <- renderPlot({
    suppressWarnings({
    d        <- trend_filtered()
    view     <- input$trend_view
    metric   <- input$trend_metric
    # is.na(NULL) returns logical(0) which crashes if(); guard with is.null() first (short-circuit ||)
    min_pads <- { v <- input$trend_min_paddocks; if (is.null(v) || is.na(v)) 1L else max(1L, as.integer(v)) }
    # Drop rows with missing year_adj/season: these feed season_date = as.Date(...)
    # below and an NA there ("NA-01-15") makes as.Date() error out the whole plot.
    d <- dplyr::filter(d, !is.na(year_adj), !is.na(season))
    if (nrow(d) == 0) return(NULL)

    # Raw per-survey metrics plot individual observations with a seasonal-mean
    # line overlaid; aggregated metrics summarise to seasonal means only.
    is_raw        <- metric %in% c("result_traps", "result_burrow", "chew_per10")
    trend_summary <- NULL  # only populated for raw metrics

    y_label <- switch(metric,
      pct_detected    = "% paddocks with detection",
      result_traps    = "Mice / trap night",
      result_burrow   = "Burrows / transect",
      chew_per10      = "Chew cards per 10",
      n_high_paddocks = "High MouseAlert reports",
      activity_index  = "Mouse Activity Index"
    )

    # Group variables: add ae_zone for overlay/facet views
    grp <- if (view == "combined") c("year_adj", "season", "paddock_id")
           else                    c("year_adj", "season", "ae_zone", "paddock_id")

    if (metric == "pct_detected") {
      # Restricted to traps/rapid — see compute_mice_by_aez() for why MouseAlert
      # observations are excluded from this systematic-survey detection rate.
      trend <- d |>
        dplyr::filter(data_type %in% c("traps", "rapid")) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
        dplyr::summarise(val = any(mice_detected), .groups = "drop") |>
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(grp, "paddock_id")))) |>
        dplyr::summarise(n_paddocks = dplyr::n(), y = round(mean(val) * 100, 1), .groups = "drop")
    } else if (metric == "n_high_paddocks") {
      # Each observation row is one citizen-science report; count high reports per
      # season. n_paddocks tracks unique reporting paddocks for the min-filter.
      trend <- d |>
        dplyr::filter(data_type == "observations", !is.na(mouse_abundance),
                      mouse_abundance == "high") |>
        dplyr::group_by(dplyr::across(dplyr::all_of(setdiff(grp, "paddock_id")))) |>
        dplyr::summarise(
          n_paddocks = dplyr::n_distinct(paddock_id),
          y          = dplyr::n(),
          .groups    = "drop"
        )
    } else if (metric == "activity_index") {
      # Combined per-season Mouse Activity Index — see
      # compute_trend_activity_index() (a per-season generalisation of
      # aez_summary_r()), using the weights set in the conditional panel above.
      join_keys <- setdiff(grp, "paddock_id")
      trend <- compute_trend_activity_index(d, join_keys, trend_activity_weights_r())
      trend$y <- trend[[metric]]
    } else {
      join_keys <- setdiff(grp, "paddock_id")
      # Compute seasonal mean-of-means for the trend line, with mid-season dates
      # so it shares the same date x-axis as the raw points.
      trend_summary <- d |>
        dplyr::filter(!is.na(.data[[metric]])) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
        dplyr::summarise(val = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") |>
        dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) |>
        dplyr::summarise(
          n_paddocks = sum(!is.na(val) & is.finite(val)),
          y          = mean(val, na.rm = TRUE),
          .groups    = "drop"
        ) |>
        dplyr::filter(n_paddocks >= min_pads) |>
        dplyr::mutate(season_date = season_mid_date(year_adj, season))
      # Raw per-survey points restricted to qualifying seasons only
      trend <- d |>
        dplyr::filter(!is.na(.data[[metric]])) |>
        dplyr::semi_join(dplyr::select(trend_summary, dplyr::all_of(join_keys)),
                         by = join_keys) |>
        dplyr::rename(y = dplyr::all_of(metric))
    }

    # Aggregated metrics: apply min-paddock filter and derive a mid-season date
    # so the x-axis is proportional in time and gaps appear as real gaps.
    if (!is_raw) {
      trend <- trend |>
        dplyr::filter(n_paddocks >= min_pads) |>
        dplyr::mutate(season_date = season_mid_date(year_adj, season))
    }

    if (nrow(trend) == 0) return(NULL)

    base_theme <- ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
                     panel.grid.major.x = ggplot2::element_blank())

    x_scale <- ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y")

    # Raw metrics use actual survey_date; aggregated use derived mid-season date
    base_aes <- if (is_raw) ggplot2::aes(x = survey_date, y = y)
                else        ggplot2::aes(x = season_date,  y = y, group = 1)

    p <- ggplot2::ggplot(trend, base_aes) +
      ggplot2::scale_y_continuous(
        # activity_index is normalised against a dataset-wide AEZ-mean
        # reference (see compute_activity_index()); a single standout season
        # can exceed that reference (index > 1), so don't cap the axis at 1 —
        # a fixed c(0,1) limit would silently drop those points and break
        # geom_line() into fragments (same issue trend_max_* avoids elsewhere).
        limits = c(0, if (metric == "pct_detected") 100 else NA),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      x_scale +
      ggplot2::labs(x = NULL, y = y_label) +
      base_theme

    if (view == "combined") {
      if (metric == "n_high_paddocks") {
        # Discrete seasonal counts are better shown as bars than connected points
        p <- p + ggplot2::geom_col(fill = "grey50", width = 60, na.rm = TRUE)
      } else if (is_raw) {
        p <- p +
          ggplot2::geom_point(colour = "black", size = 1.5, alpha = 0.4, na.rm = TRUE) +
          ggplot2::geom_line(data = trend_summary,
                             ggplot2::aes(x = season_date, y = y, group = 1),
                             colour = "steelblue", linewidth = 1.1,
                             inherit.aes = FALSE, na.rm = TRUE)
      } else {
        p <- p +
          ggplot2::geom_line(colour = "grey70", linewidth = 0.9, na.rm = TRUE) +
          ggplot2::geom_point(colour = "black", size = 2.5, na.rm = TRUE)
      }
    } else if (view == "overlay") {
      if (is_raw) {
        p <- p +
          ggplot2::aes(colour = ae_zone) +
          ggplot2::geom_point(size = 1.5, alpha = 0.4, na.rm = TRUE) +
          ggplot2::geom_line(data = trend_summary,
                             ggplot2::aes(x = season_date, y = y,
                                          group = ae_zone, colour = ae_zone),
                             linewidth = 1.1, inherit.aes = FALSE, na.rm = TRUE)
      } else {
        p <- p +
          ggplot2::aes(group = ae_zone, colour = ae_zone) +
          ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
          ggplot2::geom_point(size = 2.5, na.rm = TRUE)
      }
      p <- p +
        ggplot2::scale_colour_brewer(palette = "Paired") +
        ggplot2::labs(colour = "AEZ") +
        ggplot2::theme(legend.position = "right")
    } else {
      if (metric == "n_high_paddocks") {
        p <- p + ggplot2::geom_col(fill = "grey50", width = 60, na.rm = TRUE)
      } else if (is_raw) {
        p <- p +
          ggplot2::geom_point(colour = "black", size = 1.5, alpha = 0.4, na.rm = TRUE) +
          ggplot2::geom_line(data = trend_summary,
                             ggplot2::aes(x = season_date, y = y, group = 1),
                             colour = "steelblue", linewidth = 1.1,
                             inherit.aes = FALSE, na.rm = TRUE)
      } else {
        p <- p +
          ggplot2::geom_line(colour = "grey70", linewidth = 0.9, na.rm = TRUE) +
          ggplot2::geom_point(colour = "black", size = 2.5, na.rm = TRUE)
      }
      facet_scales <- facet_scales_value(input$trend_facet_scales)
      p <- p +
        ggplot2::facet_wrap(~ ae_zone, ncol = 2, axes = "all", scales = facet_scales) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
    }

    print(p)
  }) # end suppressWarnings
  })

  # ── Detection Patterns tab ────────────────────────────────────────────────
  pattern_aez_selected <- reactiveVal(character(0))
  setup_aez_selector("pattern_", pattern_aez_selected)
  pattern_filtered <- make_filter_reactive(input, "pattern_", aez_selected = pattern_aez_selected)

  observeEvent(input$pattern_reset, {
    updateDateInput(session,   "pattern_date_start",    value    = date_min)
    updateDateInput(session,   "pattern_date_end",      value    = date_max)
    updateSelectInput(session, "pattern_year_filter",   selected = character(0))
    updateSelectInput(session, "pattern_season_filter", selected = character(0))
    updateSelectInput(session, "pattern_type_filter",   selected = character(0))
    updateSelectInput(session, "pattern_project_filter",selected = character(0))
    updateSelectInput(session, "pattern_bait_filter",   selected = c("no", "unsure"))
    pattern_aez_selected(character(0))
  })

  # Contextual note when x-variable or metric restricts to a single data type
  output$pattern_note_ui <- renderUI({
    notes <- character(0)
    if (isTRUE(input$pattern_xvar   == "ground_cover")) notes <- c(notes, "Ground cover is recorded for rapid assessment surveys only — trap surveys excluded.")
    if (isTRUE(input$pattern_metric == "result_traps")) notes <- c(notes, "Mice per trap night applies to trapping surveys only.")
    if (isTRUE(input$pattern_metric %in% c("result_burrow", "chew_per10"))) notes <- c(notes, "This metric applies to rapid assessment surveys only.")
    if (isTRUE(input$pattern_metric == "n_high_paddocks") &&
        isTRUE(input$pattern_xvar %in% c("ground_cover", "bait_history")))
      notes <- c(notes, "MouseAlert reports do not include ground cover or bait history — select Season or Month as the x-variable.")
    if (length(notes) > 0)
      tags$p(paste(notes, collapse = " "), style = "color: #888; font-size: 0.85em; margin-bottom: 8px;")
  })

  # Adaptive plot height: taller for facet view
  output$pattern_chart_ui <- renderUI({
    h <- if (isTRUE(input$pattern_view == "facet")) "700px" else "380px"
    plotOutput("pattern_chart", height = h)
  })

  output$pattern_chart <- renderPlot({
    suppressWarnings({
    d        <- pattern_filtered()
    xvar     <- input$pattern_xvar
    metric   <- input$pattern_metric
    view     <- input$pattern_view
    # is.na(NULL) returns logical(0) which crashes if(); guard with is.null() first (short-circuit ||)
    min_pads <- { v <- input$pattern_min_paddocks; if (is.null(v) || is.na(v)) 1L else max(1L, as.integer(v)) }

    if (nrow(d) == 0) return(NULL)

    # Annotate x-value column and apply any implicit data-type restriction
    if (xvar == "ground_cover") {
      d <- dplyr::filter(d, data_type == "rapid", !is.na(ground_cover_percent))
      d <- dplyr::mutate(d,
        xval = cut(ground_cover_percent,
                   breaks = c(-Inf, 25, 50, 75, Inf),
                   labels = c("<25%", "25–50%", "50–75%", "≥75%"),
                   right  = FALSE)
      )
      x_label <- "Ground cover (%)"
    } else if (xvar == "season") {
      d       <- dplyr::mutate(d, xval = factor(season, levels = c("Summer","Autumn","Winter","Spring")))
      x_label <- "Season"
    } else if (xvar == "month") {
      d       <- dplyr::mutate(d, xval = month)
      x_label <- "Month"
    } else {
      d       <- dplyr::mutate(d, xval = factor(bait_history))
      x_label <- "Were mice baited in last 2 months?"
    }

    # Restrict to data type relevant to the chosen metric
    if (metric == "result_traps")                    d <- dplyr::filter(d, data_type == "traps")
    if (metric %in% c("result_burrow", "chew_per10")) d <- dplyr::filter(d, data_type == "rapid")
    if (metric == "n_high_paddocks")                 d <- dplyr::filter(d, data_type == "observations",
                                                                        !is.na(mouse_abundance), mouse_abundance == "high")

    if (nrow(d) == 0) return(NULL)

    by_aez  <- view == "facet"
    grp_pad <- c("xval", "paddock_id", if (by_aez) "ae_zone" else NULL)
    grp_sum <- c("xval",               if (by_aez) "ae_zone" else NULL)

    # Aggregate to one value per paddock × x-category (avoid pseudoreplication)
    if (metric == "pct_detected") {
      # Restricted to traps/rapid — see compute_mice_by_aez() for why MouseAlert
      # observations are excluded from this systematic-survey detection rate.
      per_pad <- d |>
        dplyr::filter(data_type %in% c("traps", "rapid")) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp_pad))) |>
        dplyr::summarise(val = as.numeric(any(mice_detected)), .groups = "drop")
    } else if (metric == "n_high_paddocks") {
      # val = total high reports from this paddock in this x-category.
      # d is already restricted to high-abundance observations above.
      per_pad <- d |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp_pad))) |>
        dplyr::summarise(val = dplyr::n(), .groups = "drop")
    } else {
      per_pad <- d |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp_pad))) |>
        dplyr::summarise(val = mean(.data[[metric]], na.rm = TRUE), .groups = "drop") |>
        dplyr::filter(!is.na(val), is.finite(val))
    }

    # Summary (mean ± SE) across paddocks per group
    trend <- per_pad |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_sum))) |>
      dplyr::summarise(
        n_paddocks = dplyr::n(),
        y  = if (metric == "pct_detected") round(mean(val, na.rm = TRUE) * 100, 1)
             else                          round(mean(val, na.rm = TRUE), 3),
        se = if (metric == "pct_detected")
               sqrt(mean(val, na.rm = TRUE) * (1 - mean(val, na.rm = TRUE)) / dplyr::n()) * 100
             else
               sd(val, na.rm = TRUE) / sqrt(sum(!is.na(val))),
        .groups = "drop"
      ) |>
      dplyr::filter(n_paddocks >= min_pads) |>
      dplyr::mutate(ymin = pmax(0, y - se), ymax = y + se)

    if (nrow(trend) == 0) return(NULL)

    y_label <- switch(metric,
      pct_detected    = "% paddocks with detection",
      result_traps    = "Mean mice / trap night",
      result_burrow   = "Mean burrows / transect",
      chew_per10      = "Mean chew cards per 10",
      n_high_paddocks = "Mean high reports per paddock"
    )

    p <- ggplot2::ggplot(trend, ggplot2::aes(x = xval, y = y)) +
      ggplot2::geom_col(fill = "grey50", width = 0.6, na.rm = TRUE) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = ymin, ymax = ymax),
                             width = 0.2, colour = "#555555", linewidth = 0.7,
                             na.rm = TRUE) +
      ggplot2::scale_y_continuous(
        limits = c(0, if (metric == "pct_detected") 105 else NA),
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::labs(x = x_label, y = y_label) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        axis.text.x        = ggplot2::element_text(angle = if (xvar == "month") 45 else 0, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank()
      )

    if (by_aez) {
      facet_scales <- facet_scales_value(input$pattern_facet_scales)
      p <- p +
        ggplot2::facet_wrap(~ ae_zone, ncol = 2, axes = "all", scales = facet_scales) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
    }

    print(p)
  }) # end suppressWarnings
  })

  # ── Paddock Trends tab ────────────────────────────────────────────────────
  # Single-select map: one AEZ is always highlighted; clicking replaces it.
  # Initialised to the first AEZ so the plot is never empty on load.
  pad_trend_aez_selected <- reactiveVal(aez_choices[1])
  setup_aez_selector("pad_trend_", pad_trend_aez_selected, single = TRUE)
  pad_trend_filtered <- make_filter_reactive(input, "pad_trend_", aez_selected = pad_trend_aez_selected)

  observeEvent(input$pad_trend_reset, {
    updateDateInput(session,   "pad_trend_date_start",    value    = date_min)
    updateDateInput(session,   "pad_trend_date_end",      value    = date_max)
    updateSelectInput(session, "pad_trend_year_filter",   selected = character(0))
    updateSelectInput(session, "pad_trend_season_filter", selected = character(0))
    updateSelectInput(session, "pad_trend_type_filter",   selected = character(0))
    updateSelectInput(session, "pad_trend_project_filter",selected = character(0))
    updateSelectInput(session, "pad_trend_bait_filter",   selected = c("no", "unsure"))
    pad_trend_aez_selected(aez_choices[1])
  })

  # renderUI just provides the scrollable viewport; height is controlled
  # authoritatively by the height function in renderPlot below.
  output$pad_trend_chart_ui <- renderUI({
    tags$div(
      style = "max-height: 800px; overflow-y: auto; border: 1px solid #e0e0e0; border-radius: 4px;",
      plotOutput("pad_trend_chart")
    )
  })

  output$pad_trend_chart <- renderPlot({
    suppressWarnings({
    d        <- pad_trend_filtered()
    min_surv <- { v <- input$pad_trend_min_surveys; if (is.null(v) || is.na(v)) 1L else max(1L, as.integer(v)) }
    if (nrow(d) == 0) return(NULL)

    # Build long-format data: one row per survey × metric, values normalised to
    # 0-1 by dividing each metric by its full-dataset max so all three are
    # directly comparable on the same y-axis.
    trap_rows <- dplyr::filter(d, data_type == "traps", !is.na(result_traps)) |>
      dplyr::transmute(paddock_id, ae_zone, survey_date,
                       metric = "Mice / trap night",
                       value  = result_traps / max(trend_max_result_traps, 1e-9))
    burrow_rows <- dplyr::filter(d, data_type == "rapid", !is.na(result_burrow)) |>
      dplyr::transmute(paddock_id, ae_zone, survey_date,
                       metric = "Burrows / transect",
                       value  = result_burrow / max(trend_max_result_burrow, 1e-9))
    chew_rows <- dplyr::filter(d, data_type == "rapid", !is.na(chew_per10)) |>
      dplyr::transmute(paddock_id, ae_zone, survey_date,
                       metric = "Chew cards / 10",
                       value  = chew_per10 / max(trend_max_chew_per10, 1e-9))
    plot_d <- dplyr::bind_rows(trap_rows, burrow_rows, chew_rows)
    if (nrow(plot_d) == 0) return(NULL)

    # Filter to paddocks with enough total surveys, ordered within AEZ by count
    pad_order <- plot_d |>
      dplyr::count(ae_zone, paddock_id, name = "n_surveys") |>
      dplyr::filter(n_surveys >= min_surv) |>
      dplyr::arrange(ae_zone, dplyr::desc(n_surveys))
    if (nrow(pad_order) == 0) return(NULL)

    # Attach site/subsite label and set ordered factors for nested facets
    pad_order <- dplyr::left_join(
      pad_order, paddock_labels[, c("paddock_id", "pad_label")], by = "paddock_id"
    ) |> dplyr::mutate(pad_label = dplyr::coalesce(pad_label, as.character(paddock_id)))

    plot_d <- dplyr::filter(plot_d, paddock_id %in% pad_order$paddock_id) |>
      dplyr::left_join(paddock_labels[, c("paddock_id", "pad_label")], by = "paddock_id") |>
      dplyr::mutate(
        pad_label = dplyr::coalesce(pad_label, as.character(paddock_id)),
        pad_label = factor(pad_label, levels = unique(pad_order$pad_label)),
        metric    = factor(metric, levels = c("Mice / trap night",
                                              "Burrows / transect",
                                              "Chew cards / 10"))
      )

    # "Free x/y" axis scales let each paddock's panel zoom to its own data
    # range — useful since paddocks differ widely in survey history length and
    # in how close their values get to the dataset-wide max. The y-axis upper
    # limit is only fixed at 1.05 when y isn't free; with "Free y"/"Free x & y"
    # it's left to each panel's own data (still floored at 0).
    facet_scales <- facet_scales_value(input$pad_trend_facet_scales)
    y_limits     <- if (facet_scales %in% c("free_y", "free")) c(0, NA) else c(0, 1.05)

    p <- ggplot2::ggplot(plot_d, ggplot2::aes(x = survey_date, y = value, colour = metric)) +
      ggplot2::geom_point(size = 1.5, na.rm = TRUE) +
      ggplot2::geom_line(linewidth = 0.5, na.rm = TRUE) +
      ggplot2::facet_wrap(vars(pad_label), ncol = 2, axes = "all", scales = facet_scales) +
      ggplot2::scale_colour_brewer(palette = "Dark2", name = NULL) +
      ggplot2::scale_y_continuous(
        limits = y_limits,
        expand = ggplot2::expansion(mult = c(0, 0.05))
      ) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(x = "", y = "Survey index (0–1, scaled to dataset max)") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.position    = "top",
        strip.background   = ggplot2::element_blank(),
        strip.text         = ggplot2::element_text(size = 9),
        axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.major.x = ggplot2::element_blank()
      )
    print(p)
  }) # end suppressWarnings
  }, height = function() {
    d        <- pad_trend_filtered()
    min_surv <- { v <- input$pad_trend_min_surveys; if (is.null(v) || is.na(v)) 1L else max(1L, as.integer(v)) }
    n_pads   <- tryCatch({
      # Mirror the plot's long-format construction so the n_surveys threshold is
      # applied identically and the height matches what actually renders.
      trap_rows   <- dplyr::filter(d, data_type == "traps", !is.na(result_traps)) |>
        dplyr::transmute(paddock_id, ae_zone, survey_date, .m = "t")
      burrow_rows <- dplyr::filter(d, data_type == "rapid", !is.na(result_burrow)) |>
        dplyr::transmute(paddock_id, ae_zone, survey_date, .m = "b")
      chew_rows   <- dplyr::filter(d, data_type == "rapid", !is.na(chew_per10)) |>
        dplyr::transmute(paddock_id, ae_zone, survey_date, .m = "c")
      dplyr::bind_rows(trap_rows, burrow_rows, chew_rows) |>
        dplyr::count(ae_zone, paddock_id) |>
        dplyr::filter(n >= min_surv) |>
        nrow()
    }, error = function(e) 1L)
    # Shiny multiplies this height by the client's device pixel ratio (2 on
    # Retina/HiDPI displays) before handing it to the cairo PNG device, which
    # errors ("too big") above 32767px per side. Cap so height * pixelratio
    # always stays under that limit, whatever the client's pixelratio is.
    pixelratio <- session$clientData$pixelratio
    if (is.null(pixelratio) || is.na(pixelratio) || pixelratio < 1) pixelratio <- 1
    max_height <- floor(32767 / pixelratio)
    min(max_height, max(400L, ceiling(n_pads / 2L) * 220L))
  })

  # ── ZNP50 tab ──────────────────────────────────────────────────────────────
  znp_filtered <- reactive({
    end_date   <- as.Date(input$znp_fortnight)
    start_date <- end_date - 13
    dplyr::filter(surveys_all, survey_date >= start_date, survey_date <= end_date)
  })

  output$znp_period_text <- renderText({
    end_date   <- as.Date(input$znp_fortnight)
    start_date <- end_date - 13
    paste0(format(start_date, "%d %b %Y"), " – ", format(end_date, "%d %b %Y"))
  })

  znp_by_aez_r <- reactive({ compute_znp50_by_aez(znp_filtered()) })

  znp_aez_sf_r <- reactive({
    dplyr::left_join(aez_base, znp_by_aez_r(), by = "ae_zone")
  })

  # ── ZNP50 table ────────────────────────────────────────────────────────────
  output$znp_table <- renderReactable({
    df <- znp_by_aez_r() |> dplyr::arrange(ae_zone)

    class_bg <- function(val) {
      dplyr::case_when(
        val == "High"     ~ znp_colours[["High"]],
        val == "Moderate" ~ znp_colours[["Moderate"]],
        val == "Low"      ~ znp_colours[["Low"]],
        TRUE              ~ NA_character_
      )
    }

    df <- dplyr::mutate(df,
      bg_chew      = class_bg(chew_class),
      bg_burrow    = class_bg(burrow_class),
      bg_chew10    = class_bg(chew_per10_class),
      bg_overall   = class_bg(overall_class)
    ) |>
    dplyr::relocate(max_chew_per10, bg_chew10, .after = n_sites)

    cell_style <- function(bg_col) {
      JS(sprintf(
        "function(rowInfo) { var bg = rowInfo.row['%s']; return bg ? { background: bg, color: '#fff', fontWeight: 'bold' } : {}; }",
        bg_col
      ))
    }

    reactable::reactable(
      df,
      columns = list(
        ae_zone       = reactable::colDef(name = "AEZ", minWidth = 200),
        n_sites       = reactable::colDef(name = "Paddocks surveyed", minWidth = 130),
        max_chew_pct  = reactable::colDef(name = "Max chew % (site mean)",
                          format = reactable::colFormat(suffix = "%", digits = 1),
                          style  = cell_style("bg_chew")),
        chew_class    = reactable::colDef(show = FALSE),
        max_burrows      = reactable::colDef(name = "Max burrows / transect (site mean)",
                             format = reactable::colFormat(digits = 2),
                             style  = cell_style("bg_burrow")),
        burrow_class     = reactable::colDef(show = FALSE),
        max_chew_per10   = reactable::colDef(name = "Max chew cards / 10 (site mean)",
                             format = reactable::colFormat(digits = 2),
                             style  = cell_style("bg_chew10")),
        chew_per10_class = reactable::colDef(show = FALSE),
        overall_class    = reactable::colDef(name = "ZNP50 classification", minWidth = 150,
                             style  = cell_style("bg_overall")),
        bg_chew          = reactable::colDef(show = FALSE),
        bg_burrow        = reactable::colDef(show = FALSE),
        bg_chew10        = reactable::colDef(show = FALSE),
        bg_overall       = reactable::colDef(show = FALSE)
      ),
      defaultSorted = list(ae_zone = "asc"),
      pagination = FALSE, bordered = TRUE, striped = FALSE, highlight = TRUE
    )
  })

  # ── ZNP50 history table ────────────────────────────────────────────────────
  output$znp_history_table <- renderReactable({
    df          <- znp_history_wide
    period_cols <- setdiff(names(df), "ae_zone")

    # JS style function per column (ISO date used as JS object key)
    make_style <- function(col_id) {
      JS(sprintf(
        "function(rowInfo) {
          var v = rowInfo.row['%s'];
          if (v === 'High')     return { background: '#C0392B', color: '#fff', fontWeight: 'bold' };
          if (v === 'Moderate') return { background: '#E67E22', color: '#fff', fontWeight: 'bold' };
          if (v === 'Low')      return { background: '#27AE60', color: '#fff', fontWeight: 'bold' };
          return {};
        }",
        col_id
      ))
    }

    period_coldefs <- setNames(
      lapply(period_cols, function(col_id) {
        reactable::colDef(
          name     = format(as.Date(col_id), "%d %b %Y"),
          minWidth = 110,
          align    = "center",
          style    = make_style(col_id)
        )
      }),
      period_cols
    )

    all_coldefs <- c(
      list(ae_zone = reactable::colDef(name = "AEZ", minWidth = 200, sticky = "left")),
      period_coldefs
    )

    reactable::reactable(
      df,
      columns    = all_coldefs,
      defaultSorted = list(ae_zone = "asc"),
      pagination = FALSE,
      bordered   = TRUE,
      highlight  = TRUE,
      wrap       = FALSE
    )
  })

  # ── ZNP50 map ─────────────────────────────────────────────────────────────
  # Polygons and paddock markers are built inside renderLeaflet (not a proxy
  # observer) so the map renders complete when the tab is first opened,
  # regardless of when the session started. The map resets to the default
  # Australia-wide zoom on each fortnight change, which is acceptable here.
  output$znp_map <- renderLeaflet({
    aez_sf <- znp_aez_sf_r()
    surv   <- dplyr::filter(znp_filtered(), data_type == "rapid",
                             !is.na(longitude_paddock), !is.na(latitude_paddock))

    bb <- sf::st_bbox(aez_base)
    m <- leaflet(width = "100%", height = "560px") |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
      addProviderTiles("CartoDB.Positron",     group = "Light (default)") |>
      addProviderTiles("Esri.WorldImagery",    group = "Satellite") |>
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetMap") |>
      addLegend(position = "bottomleft",
                colors = c("#555555", "white"), labels = c("Detected", "Not detected"),
                title = "Survey paddocks", opacity = 0.85) |>
      addLegend(
        position = "bottomright",
        colors   = c(znp_colours[["Low"]], znp_colours[["Moderate"]], znp_colours[["High"]], "#BDBDBD"),
        labels   = c("Low", "Moderate", "High", "No data"),
        title    = "ZNP50 classification",
        opacity  = 0.85
      ) |>
      addLayersControl(
        baseGroups    = c("Light (default)", "Satellite", "OpenStreetMap"),
        overlayGroups = c("AEZ zones", "Survey paddocks"),
        options       = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(position = "topright", options = scaleBarOptions(imperial = FALSE)) |>
      addPolygons(
        data        = aez_sf,
        group       = "AEZ zones",
        layerId     = ~ae_zone,
        fillColor   = ~pal_znp(overall_class),
        fillOpacity = 0.75,
        color       = "white",
        weight      = 1.2,
        label       = ~ae_zone,
        popup       = ~paste0(
          "<b>AEZ:</b> ", ae_zone, "<br>",
          "<b>ZNP50 classification:</b> ",
          ifelse(is.na(overall_class), "No data", overall_class), "<br>",
          "<b>Sites surveyed:</b> ", ifelse(is.na(n_sites), "—", as.character(n_sites)), "<br>",
          "<b>Max chew %:</b> ",
          ifelse(is.na(max_chew_pct), "—",
                 paste0(max_chew_pct, "% (", ifelse(is.na(chew_class), "—", chew_class), ")")), "<br>",
          "<b>Max burrows / transect:</b> ",
          ifelse(is.na(max_burrows), "—",
                 paste0(max_burrows, " (", ifelse(is.na(burrow_class), "—", burrow_class), ")"))
        ),
        highlightOptions = highlightOptions(weight = 2.5, fillOpacity = 0.6, bringToFront = FALSE)
      )

    if (nrow(surv) > 0) {
      sites <- surv |>
        dplyr::group_by(paddock_id, ae_zone, state) |>
        dplyr::summarise(
          longitude       = dplyr::first(longitude_paddock),
          latitude        = dplyr::first(latitude_paddock),
          mice_detected   = any(mice_detected),
          n_surveys       = dplyr::n(),
          mean_chew_pct   = round(mean(pct_chewcard, na.rm = TRUE), 1),
          mean_burrows    = round(mean(result_burrow, na.rm = TRUE), 2),
          mean_chew_per10 = round(mean(chew_per10,  na.rm = TRUE), 2),
          .groups         = "drop"
        ) |>
        dplyr::mutate(
          mean_chew_pct   = dplyr::if_else(is.nan(mean_chew_pct),   NA_real_, mean_chew_pct),
          mean_burrows    = dplyr::if_else(is.nan(mean_burrows),    NA_real_, mean_burrows),
          mean_chew_per10 = dplyr::if_else(is.nan(mean_chew_per10), NA_real_, mean_chew_per10),
          popup_text = paste0(
            "<b>Paddock:</b> ", paddock_id, "<br>",
            "<b>State:</b> ", state, "  <b>AEZ:</b> ", ae_zone, "<br>",
            "<b>Surveys this period:</b> ", n_surveys, "<br>",
            "<b>Mean chew % per card:</b> ",
            ifelse(is.na(mean_chew_pct), "—", paste0(mean_chew_pct, "%")), "<br>",
            "<b>Mean burrows / transect:</b> ",
            ifelse(is.na(mean_burrows), "—", mean_burrows), "<br>",
            "<b>Mean chew cards / 10:</b> ",
            ifelse(is.na(mean_chew_per10), "—", mean_chew_per10)
          )
        )

      m <- m |>
        addCircleMarkers(
          data        = sites,
          lng         = ~longitude, lat = ~latitude,
          group       = "Survey paddocks",
          radius      = 4,
          color       = marker_col, fillColor = marker_col,
          fillOpacity = ~ifelse(mice_detected, 0.85, 0),
          stroke      = TRUE, weight = 1.5, opacity = 1,
          popup       = ~popup_text, label = ~paddock_id
        )
    }

    m
  })
}

shinyApp(ui, server)

# Each AE zone's plague history by season, and from it a "post-plague" indicator for the medium -> plague
# transition: a zone that has just come out of a plague is unlikely to erupt again straight away, even when
# conditions are good (densities crash too low to respond). Across 1983-2025 in the SA and Victorian
# records, every year of high rainfall whiplash in the Mallee that didn't bring a plague (1999, 2012, 2022,
# 2023) came within two years of a plague; setting whiplash to zero in those years raised its AUC for
# plague onsets from 0.88 to 0.96 (2026-10-12). Without such a term, those years flatten the whiplash slope.
#
# A zone is in plague in a season when either:
#   - its Australian state(s) recorded a plague that year (yearly_plague_occurrence, a state-year record
#     covering 1900 onwards; a zone spanning two states counts a plague in either), or
#   - at least `min_mapped_share` of its paddocks with a map anchor that season were mapped as plague
#     (plague_state_anchors' plague_state = 2; the maps start in 2013).
# Seasons with neither a record nor a map anchor carry the zone's last known status forward, so an
# unmapped season (e.g. every summer, whose maps are dropped) doesn't break a plague into two.
#
# post_plague is 1 when the zone's plague ended recently: it was in plague at some point in the
# `seasons_back` seasons before last (default 4, a year), but not last season. A year rather than two: the
# yearly records mark a whole calendar year as plague, so in the Mallee the 2018 record runs to Spring 2018
# and the 2020-21 plague (mapped from Winter 2020) began only five seasons later -- a longer window held it
# back. A year still covers the high-whiplash years without a plague that followed a plague within about a
# year (1999, 2012, 2022 and early 2023 in the Mallee).
#
# Last season, not this one, so the value is known when a forecast is made (this season's maps come
# later). Keeping the previous season out also leaves the term off while a plague is still running, so it
# doesn't hold back a plague spreading to more paddocks, or a plague that eases but is still recorded
# continuing into a second year. A plague that eases for a full season and then erupts again is held back
# but not ruled out: the coefficient is estimated.
#
# WA has no state-year records, so before the maps its zones never count as in plague.
#
# Arguments:
#   plague_state_anchors      the paddock x season panel with map anchors (r/c_anchor_plague_state.R):
#                             ae_zone, state (Australian state), year_adj, season, plague_state
#   yearly_plague_occurrence  state-year records: a Year column and one column per state name (1 = plague)
#   seasons_back              how many seasons a finished plague counts as recent (default 4)
#   min_mapped_share          share of a zone's map-anchored paddocks mapped as plague for the zone to count
#                             as in plague that season (default 0.1)
#
# Returns one row per zone x season: ae_zone, year_adj, season, period ("<season-end month>_<year>",
# the raster layer naming used across the pipeline), zone_plague (TRUE/FALSE) and post_plague (0/1).

build_zone_plague_history <- function(plague_state_anchors, yearly_plague_occurrence, seasons_back = 4, min_mapped_share = 0.1) {

  # Season-end month of each season (Summer ends in February of its year_adj).
  season_end_month <- c(Summer = 2L, Autumn = 5L, Winter = 8L, Spring = 11L)
  # Full state names used by the yearly records, from the anchors' abbreviations.
  state_names <- c(NSW = "New South Wales", QLD = "Queensland", SA = "South Australia", VIC = "Victoria", WA = "Western Australia")

  anchors <- dplyr::filter(plague_state_anchors, !is.na(ae_zone))

  # Every zone x season from the first record-covered year to the panel's last season.
  seasons <- tidyr::expand_grid(year_adj = seq(min(anchors$year_adj), max(anchors$year_adj)),
                                season = c("Summer", "Autumn", "Winter", "Spring"))
  zones <- sort(unique(as.character(anchors$ae_zone)))
  grid <- tidyr::expand_grid(ae_zone = zones, seasons)

  # Which Australian states each zone's paddocks fall in.
  zone_states <- anchors |>
    dplyr::distinct(ae_zone = as.character(ae_zone), state) |>
    dplyr::mutate(state_name = state_names[state])

  # State-year records as one row per state x plague year.
  records <- yearly_plague_occurrence |>
    tidyr::pivot_longer(-Year, names_to = "state_name", values_to = "plague") |>
    dplyr::filter(plague %in% 1) |>
    dplyr::transmute(state_name, year_adj = Year, recorded = TRUE)
  # A zone-year is recorded as plague if any of the zone's states recorded one.
  zone_recorded <- zone_states |>
    dplyr::inner_join(records, by = "state_name", relationship = "many-to-many") |>
    dplyr::distinct(ae_zone, year_adj, recorded)

  # Map anchors: share of each zone-season's map-anchored paddocks mapped as plague.
  zone_mapped <- anchors |>
    dplyr::filter(!is.na(plague_state)) |>
    dplyr::group_by(ae_zone = as.character(ae_zone), year_adj, season = as.character(season)) |>
    dplyr::summarise(mapped_share = mean(plague_state == 2), .groups = "drop")

  grid |>
    dplyr::left_join(zone_recorded, by = c("ae_zone", "year_adj")) |>
    dplyr::left_join(zone_mapped, by = c("ae_zone", "year_adj", "season")) |>
    dplyr::mutate(
      season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring")),
      # Known status: plague if recorded or mapped; not plague if mapped below the share (and not recorded),
      # or if the zone's states have records but none that year; unknown otherwise.
      has_records = ae_zone %in% zone_states$ae_zone[!is.na(zone_states$state_name) & zone_states$state_name %in% names(yearly_plague_occurrence)],
      known = dplyr::case_when(recorded %in% TRUE ~ TRUE,
                               mapped_share >= min_mapped_share ~ TRUE,
                               !is.na(mapped_share) ~ FALSE,
                               has_records ~ FALSE,
                               .default = NA)
    ) |>
    dplyr::arrange(ae_zone, year_adj, season) |>
    dplyr::group_by(ae_zone) |>
    dplyr::mutate(
      # Unknown seasons carry the last known status forward (none before the first known season).
      zone_plague = dplyr::coalesce(zoo::na.locf(known, na.rm = FALSE), FALSE),
      # In plague last season, and at any point in the seasons_back seasons before that.
      last_season = dplyr::lag(zone_plague, 1, default = FALSE),
      recent = vapply(seq_along(zone_plague), function(i) {
        # The seasons_back seasons before last season.
        if (i <= 2) FALSE else any(zone_plague[seq(max(1, i - seasons_back - 1), i - 2)])
      }, logical(1)),
      post_plague = as.numeric(recent & !last_season)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(period = paste0(season_end_month[as.character(season)], "_", year_adj)) |>
    dplyr::select(ae_zone, year_adj, season, period, zone_plague, post_plague)
}

# Restrict MouseAlert observations to AE zones that also have structured
# (traps/rapid) survey data. MouseAlert reports from a zone with no
# structured monitoring at all aren't usable alongside this project's
# structured-survey-based modelling, and (confirmed directly) are the
# dominant source of load_paddocks()'s candidate-paddock bloat: of 2,927
# distinct MouseAlert coordinates, 1,113 fall outside every AE zone entirely
# (non-agricultural land) and a further 1,165 are in an AE zone with no
# structured survey coverage -- only 1,762 are in one of the 12 AE zones
# that actually have structured data.
#
# ae_zone is determined via a direct, cheap point-in-polygon join against
# aez_adj (37 features) -- not via paddocks_sf/load_paddocks()'s own
# (expensive) epaddock-polygon matching -- so this filter can run before
# load_paddocks() ever sees MouseAlert's coordinates, and before paddocks_sf
# exists at all.
#
# Arguments:
#   data_mouse_alert   cleaned MouseAlert data frame (clean_data_mouse_alert(),
#                      r/a_clean_data_mouse_alert.R) with longitude/latitude
#   data_traps         raw trap data frame with longitude/latitude
#   data_rapid         raw rapid-assessment data frame with longitude/latitude
#   aez_adj            sf polygon object with an ae_zone column
#
# Returns data_mouse_alert filtered to rows whose ae_zone has structured
# survey data (same columns as data_mouse_alert, no ae_zone column added).

filter_mouse_alert_to_structured_zones <- function(data_mouse_alert, data_traps, data_rapid, aez_adj) {

  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  # AE zones with at least one structured (traps/rapid) survey coordinate.
  # Select longitude/latitude before binding: attach_time_variables() builds
  # month_year/season_year_adj factor levels from each dataset's own date
  # range, so binding full data frames fails once datasets span different
  # ranges (same fix match_surveys_to_paddocks() already uses,
  # r/a_match_surveys_to_paddocks.R) -- moot here since only coordinates are
  # used, but kept consistent with that established pattern.
  structured_coords <- dplyr::bind_rows(
    dplyr::select(data_traps, longitude, latitude),
    dplyr::select(data_rapid, longitude, latitude)
  ) |>
    dplyr::filter(!is.na(longitude), !is.na(latitude)) |>
    dplyr::distinct()

  structured_pts <- sf::st_as_sf(structured_coords, coords = c("longitude", "latitude"), crs = 4326)

  structured_ae_zones <- suppressMessages(suppressWarnings(
    sf::st_join(structured_pts, dplyr::select(aez_adj, ae_zone), join = sf::st_intersects, largest = TRUE)
  ))$ae_zone |> unique() |> stats::na.omit()

  # Each MouseAlert row's own ae_zone, via the same direct join.
  mouse_alert_sf <- sf::st_as_sf(data_mouse_alert, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  mouse_alert_joined <- suppressMessages(suppressWarnings(
    sf::st_join(mouse_alert_sf, dplyr::select(aez_adj, ae_zone), join = sf::st_intersects, largest = TRUE)
  ))

  n_before <- nrow(data_mouse_alert)
  kept <- mouse_alert_joined |>
    sf::st_drop_geometry() |>
    dplyr::filter(ae_zone %in% structured_ae_zones) |>
    dplyr::select(-ae_zone)

  message(n_before - nrow(kept), " of ", n_before, " MouseAlert row(s) dropped -- ",
          "outside every AE zone that has structured (traps/rapid) survey data.")

  kept
}

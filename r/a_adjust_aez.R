# Adjusted agro-ecological zones: GRDC AE zones with few surveys merged, for the plague HMM's burrow and
# chew-card zone effects (r/e_fit_plague_hmm.R), where several zone x state effects had too little data
# with the original 11 zones.
#
#   NSW NE Qld SE + NSW NW Qld SW -> merged, then split at the NSW/Qld border: "NSW N" and "QLD S"
#   NSW Central + NSW Vic Slopes  -> "NSW Central-Slopes"
#   WA Central/Eastern/Northern/Sandplain -> "WA"
#   SA Midnorth-Lower Yorke Eyre, SA Vic Bordertown-Wimmera, SA Vic Mallee, Qld Central -> unchanged
#
# ae_zone_adj_of() is the rule itself, applied both to the polygons here (adjust_aez()) and to each
# paddock in the model data (build_hmm_data(), r/e_build_hmm_data.R), so the two always agree. The
# north/south split follows the earlier version of this function (kept in r_not_in_use/a_adjust_aez.R).
#
# Arguments:
#   ae_zone  original GRDC AE zone names (as in the aez target, "/" replaced by " ")
#   state    Australian state abbreviation (e.g. "NSW", "QLD") of the same paddock or polygon piece
#
# Returns the adjusted zone name for each element.
ae_zone_adj_of <- function(ae_zone, state) {
  ae_zone <- as.character(ae_zone)
  dplyr::case_when(
    ae_zone %in% c("NSW NE Qld SE", "NSW NW Qld SW") & state == "QLD" ~ "QLD S",
    ae_zone %in% c("NSW NE Qld SE", "NSW NW Qld SW")                  ~ "NSW N",
    ae_zone %in% c("NSW Central", "NSW Vic Slopes")                   ~ "NSW Central-Slopes",
    grepl("^WA", ae_zone)                                             ~ "WA",
    TRUE                                                              ~ ae_zone
  )
}

# Builds the adjusted zone polygons: every AE zone is split at state borders, each piece takes its
# adjusted zone name from ae_zone_adj_of(), and the pieces are dissolved by that name.
#
# Arguments:
#   aez      original AE zone polygons, with an ae_zone column (the aez target)
#   aus_shp  state polygons, with an ST_NAME column (the aus_shp target)
#
# Returns an sf of adjusted zone polygons with an ae_zone_adj column.
adjust_aez <- function(aez, aus_shp) {
  # Planar geometry for the intersection and union (s2 rejects some of these zone boundaries).
  s2_was_on <- sf::sf_use_s2()
  suppressMessages(sf::sf_use_s2(FALSE))
  on.exit(suppressMessages(sf::sf_use_s2(s2_was_on)), add = TRUE)

  states <- sf::st_transform(dplyr::select(aus_shp, state = ST_NAME), sf::st_crs(aez))

  # One piece per zone x state overlap, named by the adjustment rule.
  pieces <- suppressMessages(suppressWarnings(
    sf::st_intersection(sf::st_make_valid(dplyr::select(aez, ae_zone)), sf::st_make_valid(states))
  ))
  pieces$ae_zone_adj <- ae_zone_adj_of(pieces$ae_zone, pieces$state)

  # Dissolve the pieces into one polygon per adjusted zone.
  pieces |>
    dplyr::group_by(ae_zone_adj) |>
    dplyr::summarise(.groups = "drop") |>
    sf::st_make_valid()
}

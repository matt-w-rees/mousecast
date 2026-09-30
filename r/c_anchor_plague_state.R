# Add a semi-supervised HMM state anchor to the paddock x season panel
# (r/c_build_survey_state_wide.R), from recent_plague_maps (r/c_read_recent_plague_maps.R's
# hand-digitised, paddock-scale plague-extent polygons) -- REPLACES (2026-09) the previous
# state x year / state x season (a)+(b) rule, which is kept, not deleted, in
# r_not_in_use/c_anchor_plague_state.R (see that file's own header for the full history of what
# was tried before this and why).
#
# Arguments:
#   survey_state_wide  r/c_build_survey_state_wide.R's output -- one row per paddock x
#                       season_year_adj, with its own longitude/latitude/season/year_adj columns
#   recent_plague_maps r/c_read_recent_plague_maps.R's output -- one polygon feature per reported
#                       area of mouse activity, keyed by season/year
#   census_start       "<Season> <YYYY>" (year_adj convention) -- first season the maps are read
#                       as a complete census of plague activity, see decision (3) below
#   lookback_seasons   seasons before each plague anchor that are un-anchored, see "BACKWARD
#                       un-anchoring" below
#   map_season_absence TRUE (default): not-plague anchors only in seasons with no plague mapped
#                       anywhere, see decision (4) below
#   map_season_scope   "national" (default): a plague mapped anywhere releases every paddock's
#                       not-plague anchor that season; "region": only paddocks in the same region
#                       as the mapped plague -- WA or the eastern states (west/east of 129 E)
#
# Rule, decided directly with the project owner (2026-09) rather than re-derived from first
# principles (unlike the (a)+(b) rule's own iterative history) -- see chat for the 3 explicit calls
# made:
#   1) FULL REPLACEMENT, not layered alongside the old (a)+(b) rule -- plague_state = 2 anchors
#      come only from recent_plague_maps' own polygons, and every season before census_start is
#      completely unanchored (plague_state = NA; see decision (3) for the window itself). Not yet
#      re-validated the way every step of the old rule's own history was (convergence, coefficient
#      plausibility, pseudo-residuals, per CLAUDE.md) -- treat plague_hmm's next live re-fit under
#      this rule as the first real check, not this function's own correctness.
#   2) ALL THREE severities (minor/moderate/severe) count as plague evidence for a paddock inside a
#      mapped polygon -- not just moderate+severe or severe-only.
#   3) CENSUS WINDOW (revised 2026-09 with the project owner): every paddock-season from
#      census_start (default Summer 2013) up to the latest season recent_plague_maps covers anchors
#      to plague_state = 1 (non-plague) unless it falls inside a polygon mapped for that exact
#      season -- WHETHER OR NOT that season has a map/folder at all. The maps are read as a
#      complete record of every plague from census_start onward: a season with no map means no
#      plague was reported anywhere, not that nobody looked. (Originally, 2026-09, only seasons WITH
#      a map got plague_state = 1 and unmapped seasons stayed NA -- that left e.g. 2019 and
#      2022-2025 fully unanchored.) Before census_start everything stays NA: the maps don't reach
#      back that far, and yearly_plague_occurrence.csv flags real plagues there (e.g. 2010/2011 in
#      every eastern state) that a blanket plague_state = 1 would directly contradict. After the
#      latest mapped season everything also stays NA, since a season not yet mapped (or not yet
#      happened -- the panel runs past the current date) isn't evidence of no plague.
#   4) MAP-SEASON ABSENCE (2026-09-30, decided with the project owner): in a season with a plague
#      mapped ANYWHERE in Australia, only the plague_state = 2 anchors inside the mapped extents are
#      kept; every other paddock that season is left unanchored (NA), for the model to estimate,
#      since hand-drawn extents are least reliable about absence while a plague is under way
#      (edges, unmapped smaller outbreaks). Seasons with no plague mapped anywhere keep their
#      plague_state = 1 anchors. Tested against full absence anchoring on the adopted transition
#      formula: the plague state grew only slightly (6.1% to 7.5% of paddock-seasons, Viterbi) and
#      the pre-2013 hindcast was similar (same-season onset AUC 0.69 vs 0.63, one-season-ahead
#      0.72 vs 0.76, occupancy 0.68 vs 0.67). Dropping absence anchors altogether let the plague
#      state take over (88%), and keeping them only where surveyed wasn't enough either (47%) --
#      absence anchors on unsurveyed paddocks are what hold "plague" to its mapped meaning.
#      map_season_absence = FALSE restores decision (3) alone.
#   5) REGIONAL SCOPE (2026-09-30, decided with the project owner): with map_season_scope = "region"
#      (as _targets.R uses), decision (4) applies within each region separately -- WA or the eastern
#      states, split at 129 E -- so a plague mapped only in WA keeps the eastern states' not-plague
#      anchors that season, and vice versa, since the two cropping systems are separate. Against the
#      national scope: pre-2013 hindcast one-season-ahead onset 0.73 vs 0.72, same-season onset 0.61
#      vs 0.69, occupancy 0.67 vs 0.68 (all within the year-resampled intervals); chosen on principle.
#
# No multi-season FORWARD persistence/propagation (unlike the old rule's propagate_seasons): the
# old rule's propagation was anchored to a real, documented signal (yearly_plague_occurrence.csv's
# own consecutive-year runs); recent_plague_maps has no equivalent multi-season structure to
# propagate from -- 9 disconnected mapped seasons, not a continuous rated history -- so inventing a
# forward persistence rule here would be untested by the same standard as the coverage tradeoff in
# (1) above. Revisit only with real evidence for how long one of these mapped extents should be
# assumed to persist.
#
# BACKWARD un-anchoring (added 2026-09, decided directly with the project owner): a paddock is
# rarely confirmed in plague the very first season an outbreak actually begins -- detection/mapping
# lags real onset. Forcing plague_state = 1 (decision (3) above) onto the lookback_seasons seasons
# immediately before that SAME paddock's own plague_state = 2 anchor would therefore overstate how
# abruptly the transition happens: those lead-up seasons have no confirmed sign in the map, but
# "not confirmed" isn't the same evidence as condition (3)'s own "confirmed absent" reading of a
# season inside the census window. So a forced plague_state = 1 there is un-anchored back to NA
# (unknown) instead -- not flipped to plague_state = 2, since there's equally no direct evidence
# those seasons WERE in plague, only that ruling them out is overconfident. This only ever loosens
# an existing plague_state = 1 into NA; it never touches an existing plague_state = 2 (even where
# two plague anchors are themselves close enough for their own lookback windows to overlap) and
# never invents a new anchor for a season outside the census window (already NA).
# lookback_seasons defaults to 2, matching the old rule's own propagate_seasons default -- not
# independently re-derived, since there's no equivalent real per-outbreak duration signal here to
# calibrate it against; revisit if a real onset-lag estimate becomes available.
#
# Matching grain: paddock-level point-in-polygon (survey_state_wide's own longitude/latitude against
# recent_plague_maps' polygons for the exact same season + year_adj), not state or subregion --
# the whole point of switching to this data source is the finer spatial grain a hand-digitised map
# offers over a qualitative per-region rating.
#
# Returns survey_state_wide with plague_state added (2L = plague, 1L = not-plague, NA = unanchored)
# -- same coding as the superseded rule, so nothing downstream (r/e_build_hmm_data.R onward) needs
# to change.

anchor_plague_state <- function(survey_state_wide, recent_plague_maps, census_start = "Summer 2013",
                                lookback_seasons = 2, map_season_absence = TRUE,
                                map_season_scope = c("national", "region")) {

  map_season_scope <- match.arg(map_season_scope)

  # Summer/Autumn/Winter/Spring as one steadily-increasing integer ("quarter"), so "N seasons
  # before" is just subtraction -- Summer's own rank (0) means a Summer 2018 quarter number is
  # exactly 1 more than Spring 2017's, matching r/a_season_info.R's own year_adj convention (a
  # calendar-year-boundary-spanning Summer belongs to the FOLLOWING year_adj) without needing that
  # boundary handled as a special case here.
  season_rank <- c(Summer = 0L, Autumn = 1L, Winter = 2L, Spring = 3L)

  # census_start's own quarter number, parsed from its "<Season> <YYYY>" label.
  census_start_quarter <- as.integer(stringr::str_extract(census_start, "[0-9]{4}")) * 4L +
    season_rank[[stringr::str_extract(census_start, "^[A-Za-z]+")]]

  # The census window ends at the latest season recent_plague_maps covers -- anything after that
  # hasn't been mapped yet, so stays NA (decision (3) above).
  census_end_quarter <- max(recent_plague_maps$year * 4L + season_rank[recent_plague_maps$season])

  result <- survey_state_wide |>
    dplyr::mutate(.row_id = dplyr::row_number(), quarter = year_adj * 4L + season_rank[season])

  # Every paddock-season inside the census window with coordinates -- the only rows that can be
  # anchored at all; everything else stays NA.
  candidate_points <- result |>
    dplyr::filter(quarter >= census_start_quarter, quarter <= census_end_quarter,
                  !is.na(longitude), !is.na(latitude)) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = sf::st_crs(recent_plague_maps), remove = FALSE)

  # Which polygons (of any season) each point falls inside -- one spatial pass over every row,
  # rather than one per season, since a paddock's location is fixed across seasons.
  intersecting_polys <- sf::st_intersects(candidate_points, recent_plague_maps)

  # plague_state = 2 if any of those polygons was mapped for the point's own season + year_adj (any
  # severity, per decision (2) above), else plague_state = 1 (per decision (3) above) -- including
  # every season with no map at all.
  poly_season <- recent_plague_maps$season
  poly_year   <- recent_plague_maps$year
  is_inside <- purrr::pmap_lgl(
    list(intersecting_polys, candidate_points$season, candidate_points$year_adj),
    function(poly_idx, season, year_adj) any(poly_season[poly_idx] == season & poly_year[poly_idx] == year_adj)
  )

  anchored <- tibble::tibble(.row_id = candidate_points$.row_id, plague_state = ifelse(is_inside, 2L, 1L))

  # Decision (4): in a season with a plague mapped anywhere, not-plague anchors are left unanchored.
  # With map_season_scope = "region", a season is "mapped" only within the region (WA or the
  # eastern states, split at 129 E) its plague polygons are in.
  if (map_season_absence) {
    poly_region  <- if (map_season_scope == "region") ifelse(sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(recent_plague_maps))))[, 1] < 129, "west", "east") else "all"
    point_region <- if (map_season_scope == "region") ifelse(candidate_points$longitude < 129, "west", "east") else "all"
    mapped_seasons <- unique(paste(poly_season, poly_year, poly_region))
    in_mapped_season <- paste(candidate_points$season, candidate_points$year_adj, point_region) %in% mapped_seasons
    anchored$plague_state[in_mapped_season & anchored$plague_state == 1L] <- NA_integer_
  }

  result <- dplyr::left_join(result, anchored, by = ".row_id")

  # Un-anchor a forced plague_state = 1 for the SAME paddock in the lookback_seasons immediately
  # before one of that paddock's own plague_state = 2 anchors -- see "BACKWARD un-anchoring" above.
  # Built from the plague_state = 2 anchors as originally computed (before this step runs), so
  # overlapping lookback windows from two nearby plague anchors can never chain into un-anchoring a
  # THIRD paddock's or season's row this rule wasn't actually meant to reach.
  plague_anchor_quarters <- result |>
    dplyr::filter(plague_state == 2L) |>
    dplyr::distinct(paddock_id, quarter)

  lookback_targets <- purrr::map_dfr(seq_len(lookback_seasons), function(k) {
    dplyr::mutate(plague_anchor_quarters, quarter = quarter - k)
  }) |>
    dplyr::distinct(paddock_id, quarter) |>
    dplyr::mutate(.in_lookback = TRUE)

  result |>
    dplyr::left_join(lookback_targets, by = c("paddock_id", "quarter")) |>
    dplyr::mutate(plague_state = dplyr::if_else(
      !is.na(.in_lookback) & plague_state == 1L, NA_integer_, plague_state
    )) |>
    dplyr::select(-.row_id, -quarter, -.in_lookback)
}

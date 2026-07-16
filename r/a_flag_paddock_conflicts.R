# Flag two classes of within-paddock inconsistency in the cleaned survey data.
# Both checks operate on traps/rapid surveys only (structured-survey data with
# GPS-matched paddock_ids); MouseAlert observations are excluded.
#
# 1. NAME CONFLICTS (pre-2026 data only)
#    Before 2026, site and subsite names were a controlled vocabulary with
#    exact, consistent spelling.  Multiple distinct site_names mapping to the
#    same paddock_id in that era strongly suggests a GPS coordinate error
#    placing surveys from different farms into the same paddock polygon, or an
#    ePaddock boundary spanning two distinct survey sites.  From 2026 onward
#    site/subsite became free text so spelling variation is expected and is not
#    flagged.  Subsite-name divergence within a paddock is reported as a count
#    for context (many paddocks have multiple legitimate sub-plots), but only
#    site_name divergence is used as the filter criterion.
#
# 2. CROP / STAGE CONFLICTS (all years)
#    If the same paddock is recorded with >1 distinct crop_variety OR >1
#    distinct crop_stage within the same (year_adj, season), one or more
#    entries are likely in error, or the ePaddock polygon spans multiple real
#    paddocks carrying different crops.  Rows without any crop information
#    (pasture, bare_soil, unknown crop) are excluded from this check.
#
# Both outputs include coord_same (TRUE when all surveys share identical
# coordinates) and max_dist_m (maximum pairwise distance in metres between
# the distinct survey coordinates in that group), computed by max_coord_dist_m()
# below.  Identical coordinates suggest copy-pasted GPS entries; large
# separations suggest genuinely different locations within (or accidentally
# assigned to) the same ePaddock polygon.
#
# Returns a named list with two data frames:
#   $name_conflicts  one row per paddock with a site_name conflict (pre-2026)
#   $crop_conflicts  one row per (paddock, year_adj, season) with a
#                    crop_variety or crop_stage conflict (all years)

# Helper: maximum pairwise great-circle distance (metres) between a set of
# (longitude, latitude) points.  Deduplicates coordinates first so the cost
# scales with unique locations rather than survey count.  Returns 0 when all
# points are identical.
max_coord_dist_m <- function(longitude, latitude) {
  coords <- unique(data.frame(longitude = longitude, latitude = latitude))
  coords <- coords[!is.na(coords$longitude) & !is.na(coords$latitude), ]
  if (nrow(coords) <= 1) return(0)
  pts   <- sf::st_as_sf(coords, coords = c("longitude", "latitude"), crs = 4326)
  dists <- sf::st_distance(pts)
  # as.numeric() strips the units class so the return type is always double.
  round(as.numeric(max(dists, na.rm = TRUE)))
}

flag_paddock_conflicts <- function(data) {

  # Restrict to structured surveys with a valid paddock assignment.
  surveys <- data |>
    dplyr::filter(
      data_type %in% c("traps", "rapid"),
      !is.na(paddock_id)
    )

  # ── 1. Name conflicts (pre-2026 only) ──────────────────────────────────────
  # Use calendar year of survey_date (not year_adj) so the 2025/2026 cutoff
  # aligns with the switch from controlled vocabulary to free text.
  name_conflicts <- surveys |>
    dplyr::filter(lubridate::year(survey_date) <= 2025) |>
    dplyr::group_by(paddock_id) |>
    dplyr::summarise(
      # Pipe-separated list of every distinct site_name observed at this paddock.
      site_names      = paste(sort(unique(site_name[!is.na(site_name)])),        collapse = " | "),
      n_site_names    = dplyr::n_distinct(site_name,    na.rm = TRUE),
      # Subsite counts included for context only -- not used as filter criterion.
      subsite_names   = paste(sort(unique(subsite_name[!is.na(subsite_name)])),  collapse = " | "),
      n_subsite_names = dplyr::n_distinct(subsite_name, na.rm = TRUE),
      ae_zone         = paste(sort(unique(ae_zone[!is.na(ae_zone)])),            collapse = " | "),
      n_surveys       = dplyr::n(),
      date_min        = min(survey_date, na.rm = TRUE),
      date_max        = max(survey_date, na.rm = TRUE),
      # TRUE = all surveys share the exact same GPS point (copy-pasted coordinates).
      coord_same      = dplyr::n_distinct(paste(longitude, latitude)) == 1,
      # Maximum separation between any two survey points in this paddock (metres).
      max_dist_m      = max_coord_dist_m(longitude, latitude),
      .groups = "drop"
    ) |>
    # Only retain paddocks where the site_name genuinely differs across surveys.
    dplyr::filter(n_site_names > 1) |>
    dplyr::arrange(dplyr::desc(n_site_names), paddock_id)

  # ── 2. Crop / stage conflicts (all years) ──────────────────────────────────
  # Rows without any crop information (pasture, bare_soil, or unknown-crop rows
  # carry NA crop_variety and NA crop_stage by design) are excluded.
  crop_conflicts <- surveys |>
    dplyr::filter(!is.na(crop_variety) | !is.na(crop_stage)) |>
    dplyr::group_by(paddock_id, year_adj, season) |>
    dplyr::summarise(
      site_names       = paste(sort(unique(site_name[!is.na(site_name)])),       collapse = " | "),
      subsite_names    = paste(sort(unique(subsite_name[!is.na(subsite_name)])), collapse = " | "),
      crop_varieties   = paste(sort(unique(crop_variety[!is.na(crop_variety)])), collapse = " | "),
      n_crop_varieties = dplyr::n_distinct(crop_variety, na.rm = TRUE),
      crop_stages      = paste(sort(unique(crop_stage[!is.na(crop_stage)])),     collapse = " | "),
      n_crop_stages    = dplyr::n_distinct(crop_stage,   na.rm = TRUE),
      n_surveys        = dplyr::n(),
      coord_same       = dplyr::n_distinct(paste(longitude, latitude)) == 1,
      max_dist_m       = max_coord_dist_m(longitude, latitude),
      .groups = "drop"
    ) |>
    # Flag any season-paddock where variety OR stage is inconsistent.
    dplyr::filter(n_crop_varieties > 1 | n_crop_stages > 1) |>
    dplyr::arrange(paddock_id, year_adj, season)

  list(
    name_conflicts = name_conflicts,
    crop_conflicts = crop_conflicts
  )
}

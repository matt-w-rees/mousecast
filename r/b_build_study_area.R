# Build the single "study area" polygon shared by every temporally-varying
# covariate download in section B (GPP for now; other covariates -- climate,
# soil, etc. -- as they're added). One canonical region of interest instead
# of each covariate function building its own, as download_gpp() used to via
# the now-retired gpp_roi() (superseded -- see r_not_in_use/b_gpp_roi.R).
#
# Existing purpose: run GPP (or any future covariate) processing on a small
# test area first, or grow it gradually, rather than always committing to
# the full national request. download_gpp() checks a saved copy of the ROI
# actually used for its last download (see its header) and, if this target's
# selection has grown since then, requests only the newly-added area instead
# of re-downloading everything -- so shrinking back down to a small test area
# and growing again later doesn't waste AppEEARS quota/time.
#
# ae_zones scopes the selection to specific agroecological zones; NULL
# (default) selects every AEZ holding at least one structured (traps/rapid)
# survey -- MouseAlert-only zones are excluded, the same structured-vs-
# citizen-observation distinction CLAUDE.md draws elsewhere (this mirrors
# the reasoning _targets.R previously used for its retired
# structured_survey_subregions variable, generalised from GRDC subregions to
# AEZs since AEZ is this target's default granularity).
#
# Buffered (not just dissolved) so covariate extraction near a boundary
# paddock isn't reading clipped/no-data edge pixels -- same rationale the
# old gpp_roi() used.
#
# Arguments:
#   aez_adj                    sf polygon object with an ae_zone column
#   ae_zones                   character vector of aez_adj$ae_zone values to
#                               include; NULL (default) selects every AEZ
#                               with at least one structured survey, in which
#                               case paddocks_sf and structured_paddock_ids
#                               must be supplied
#   paddocks_sf                sf polygon object with paddock_id and ae_zone
#                               columns -- only needed for the default
#                               selection (NULL if ae_zones is supplied
#                               directly, as the pipeline now always does)
#   structured_paddock_ids     character/numeric vector of paddock_ids with
#                               structured (traps/rapid) survey data -- the
#                               pipeline's shared structured_paddock_ids
#                               target (_targets.R); only needed for the
#                               default selection, same as paddocks_sf above
#   buffer_km                  buffer (km) added around the dissolved
#                               selection (default 20)
#
# Returns a one-row sf polygon (the buffered, dissolved study area).

build_study_area <- function(aez_adj,
                              ae_zones = NULL,
                              paddocks_sf = NULL,
                              structured_paddock_ids = NULL,
                              buffer_km = 20) {

  # ── 1. Default selection: every AEZ with at least one structured survey ──
  if (is.null(ae_zones)) {
    if (is.null(paddocks_sf) || is.null(structured_paddock_ids)) {
      stop("build_study_area(): supply either ae_zones, or both paddocks_sf and structured_paddock_ids to compute the default selection.")
    }
    ae_zones <- paddocks_sf$ae_zone[paddocks_sf$paddock_id %in% structured_paddock_ids] |>
      unique() |>
      stats::na.omit() # a paddock outside every AEZ polygon would otherwise contribute a stray NA "zone"
  }

  selected <- dplyr::filter(aez_adj, ae_zone %in% ae_zones) # just the AEZ polygons actually selected

  if (nrow(selected) == 0) {
    stop("No ae_zones matched aez_adj (or none were supplied and no AEZ holds structured survey data).")
  }

  # ── 2. Buffer, dissolve to one polygon ────────────────────────────────────
  # sf buffers geographic (lon/lat) coordinates correctly, in metres, via s2
  # -- no need to reproject to a projected CRS first (s2 must stay on here;
  # turning it off, as attach_grdc_subregion() does for its own intersect/
  # nearest-feature joins, switches st_buffer()'s dist argument from metres
  # to decimal degrees instead). st_make_valid() either side of the union
  # guards against topology defects (duplicate vertices) in the raw
  # reference layer that s2 buffering surfaces as invalid geometry, which
  # would otherwise break rs_build_task()'s internal GeoJSON conversion
  # downstream in download_gpp().
  selected |>
    sf::st_buffer(dist = buffer_km * 1000) |>
    sf::st_make_valid() |>
    sf::st_union() |>
    sf::st_make_valid() |>
    sf::st_sf(geometry = _)
}

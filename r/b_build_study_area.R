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
# Selection is OR'd across whichever of states/grdc_subregions/ae_zones is
# supplied: each named filter pulls the matching polygons from its own
# reference layer, and all supplied filters' results are unioned together
# (so e.g. passing both states and ae_zones adds both, rather than
# intersecting them). If none of the three are supplied, the default is
# every agroecological zone holding at least one structured (traps/rapid)
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
#   paddocks_sf                sf polygon object with paddock_id and ae_zone
#                               columns -- the default selection reads each
#                               structured paddock's ae_zone straight off
#                               this, rather than off data_list_clean_paddocks
#   structured_paddock_ids     character/numeric vector of paddock_ids with
#                               structured (traps/rapid) survey data -- the
#                               pipeline's shared structured_paddock_ids
#                               target (_targets.R), also used by
#                               paddock_month_grid and the gpp_pml_bias_check/
#                               gpp_gf_bias_check targets so all three agree
#                               on the same definition
#   aus_shp                    sf polygon object with an ST_NAME state column
#   grdc_subregion_adj         sf polygon object with a grdc_subregion column
#   aez_adj                    sf polygon object with an ae_zone column
#   states                     character vector of aus_shp$ST_NAME values to
#                               include; NULL (default) skips this filter
#   grdc_subregions             character vector of
#                               grdc_subregion_adj$grdc_subregion values to
#                               include; NULL (default) skips this filter
#   ae_zones                   character vector of aez_adj$ae_zone values to
#                               include; NULL (default) triggers the
#                               structured-survey default described above
#   buffer_km                  buffer (km) added around the dissolved
#                               selection (default 20)
#
# Returns a one-row sf polygon (the buffered, dissolved study area).

build_study_area <- function(paddocks_sf,
                              structured_paddock_ids,
                              aus_shp,
                              grdc_subregion_adj,
                              aez_adj,
                              states = NULL,
                              grdc_subregions = NULL,
                              ae_zones = NULL,
                              buffer_km = 20) {

  # ── 1. Default selection: every AEZ with at least one structured survey ──
  if (is.null(states) && is.null(grdc_subregions) && is.null(ae_zones)) {
    ae_zones <- paddocks_sf$ae_zone[paddocks_sf$paddock_id %in% structured_paddock_ids] |>
      unique() |>
      stats::na.omit()
  }

  # ── 2. Pull in whichever reference layers were filtered on ───────────────
  # Only geometry is kept from each layer, via sf::st_geometry() rather than
  # a hardcoded column name -- aez_adj's geometry column is "geom", not
  # "geometry" like the other two layers -- so bind_rows() below doesn't need
  # matching attribute schemas (or even matching geometry column names)
  # across three different source layers.
  selected <- list()

  if (!is.null(states)) {
    selected$states <- sf::st_sf(geometry = sf::st_geometry(dplyr::filter(aus_shp, ST_NAME %in% states)))
  }
  if (!is.null(grdc_subregions)) {
    selected$grdc <- sf::st_sf(geometry = sf::st_geometry(dplyr::filter(grdc_subregion_adj, grdc_subregion %in% grdc_subregions)))
  }
  if (!is.null(ae_zones)) {
    selected$aez <- sf::st_sf(geometry = sf::st_geometry(dplyr::filter(aez_adj, ae_zone %in% ae_zones)))
  }

  combined <- dplyr::bind_rows(selected)

  if (nrow(combined) == 0) {
    stop("No states, grdc_subregions or ae_zones matched their reference layer ",
         "(or none were supplied and no AEZ holds structured survey data).")
  }

  # ── 3. Buffer, dissolve to one polygon ────────────────────────────────────
  # sf buffers geographic (lon/lat) coordinates correctly, in metres, via s2
  # -- no need to reproject to a projected CRS first (s2 must stay on here;
  # turning it off, as attach_grdc_subregion() does for its own intersect/
  # nearest-feature joins, switches st_buffer()'s dist argument from metres
  # to decimal degrees instead). st_make_valid() either side of the union
  # guards against topology defects (duplicate vertices) in the raw
  # reference layers that s2 buffering surfaces as invalid geometry, which
  # would otherwise break rs_build_task()'s internal GeoJSON conversion
  # downstream in download_gpp().
  combined |>
    sf::st_buffer(dist = buffer_km * 1000) |>
    sf::st_make_valid() |>
    sf::st_union() |>
    sf::st_make_valid() |>
    sf::st_sf(geometry = _)
}

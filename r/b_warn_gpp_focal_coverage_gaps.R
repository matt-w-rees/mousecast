# Flag any paddock whose own gpp_finescale value (build_gpp_focal_raster(),
# r/b_build_gpp_focal_raster.R) is NA in EVERY season-end month -- i.e. even
# its whole focal window, not just its own centre pixel, has no valid land
# data to average from in any month, so compute_focal_raster()'s own
# na.policy = "all" has nothing left to recover. A paddock NA in only SOME
# months (e.g. one month's own cloud contamination) is not flagged here --
# that's ordinary, expected missingness, not this structural coastal-masking
# gap.
#
# Kept as its own pipeline step (2026-08), separate from build_gpp_focal_raster()
# itself, so the raster build stays a pure build with no validation/warning
# side effect bundled in (CLAUDE.md's function-per-task convention) -- this
# pipeline needs to keep catching this automatically as new paddocks are
# added over time, not just for the ones already found.
#
# terra::extract(focal_rast, paddocks_in_scope, ID = TRUE) -- ID = TRUE,
# not FALSE. Fixed a real bug (2026-08): paddocks_in_scope is POLYGON
# geometry, and terra::extract() on polygons returns one row PER INTERSECTED
# RASTER CELL, not one row per polygon -- confirmed live, 2,689 paddocks at
# this raster's native ~463m resolution produced 12,338 extracted rows, ~4.6
# cells/paddock on average. The previous version called extract(..., ID =
# FALSE) and fed the result straight into rowSums(...) == 0, then indexed
# paddocks_in_scope$paddock_id by ROW POSITION -- silently comparing
# "extracted-row 81" against "paddock 81" as if they were the same thing,
# which only holds if every paddock contributes exactly one row. Re-run
# properly (grouping cells back to their own real polygon ID, only flagging
# a paddock if ALL of its own cells are NA in ALL months) found ZERO
# genuinely always-NA paddocks -- the three paddock_id values the old
# version had been reporting (2213622, 4028663, 4028664) were an artifact of
# that misalignment, not real coastal-masking gaps; one of them
# (paddock_id 4028664) already has a fully-populated gpp_finescale column in
# hmm_data, which shouldn't be possible if it were genuinely always-NA,
# and is what surfaced the bug in the first place.
#
# message(), not warning() (2026-08) -- this is a console-only note on the
# outcome for any affected paddock, not a pipeline-level data-quality
# warning that needs surfacing via tar_meta()'s own warnings column (unlike
# e.g. gpp_gf_bias_check); a message() doesn't appear there. Prints nothing
# at all when, as currently, no paddock is actually affected.
#
# Arguments:
#   focal_rast    SpatRaster, one layer per season-end month (build_gpp_focal_raster()'s output)
#   paddocks_sf    sf paddock table with a "state" column (paddocks_sf_with_soil_type)
#   states         Australian state abbreviations to check (e.g. eastern_states)
#
# Returns (invisibly) a character vector of flagged paddock_id values (empty
# if none) -- also prints a message() naming them and describing the
# consequence, when any are found.

warn_gpp_focal_coverage_gaps <- function(focal_rast, paddocks_sf, states) {

  paddocks_in_scope <- dplyr::filter(paddocks_sf, state %in% states)

  # ID = TRUE keeps each extracted raster cell tagged with its own
  # originating polygon -- see header for why this matters.
  extracted  <- terra::extract(focal_rast, paddocks_in_scope, ID = TRUE)
  value_cols <- setdiff(names(extracted), "ID")

  # a paddock is flagged only if EVERY one of its own cells is NA in EVERY
  # season-end month -- grouped by the real polygon ID, not row position.
  per_paddock_always_na <- extracted |>
    dplyr::group_by(ID) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols), ~ all(is.na(.x))), .groups = "drop")
  flagged_ids <- per_paddock_always_na$ID[apply(per_paddock_always_na[value_cols], 1, all)]
  flagged     <- paddocks_in_scope$paddock_id[flagged_ids]

  if (length(flagged) > 0) {
    message(
      "warn_gpp_focal_coverage_gaps(): gpp_finescale is NA in every season-end month for ", length(flagged),
      " paddock(s) -- no valid land pixel anywhere in their own focal window, so burrow_count observations ",
      "from these paddocks can't be evaluated by the detection formula's gpp_finescale term (paddock_id: ",
      paste(flagged, collapse = ", "), ")"
    )
  }

  invisible(flagged)
}

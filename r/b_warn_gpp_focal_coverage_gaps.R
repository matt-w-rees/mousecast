# Flag any paddock whose own gpp_finescale value (build_gpp_focal_raster())
# is NA in EVERY season-end month -- i.e. its whole focal window, not just
# its centre pixel, has no valid land data in any month, so
# compute_focal_raster()'s own na.policy = "all" has nothing left to
# recover. A paddock NA in only SOME months is not flagged -- that's
# ordinary missingness, not this structural coastal-masking gap. Kept as
# its own pipeline step, separate from the raster build itself (CLAUDE.md's
# function-per-task convention), so this keeps getting checked as new
# paddocks are added over time.
#
# terra::extract(..., ID = TRUE) is required, not ID = FALSE -- extract()
# on POLYGON geometry returns one row PER INTERSECTED RASTER CELL, not one
# row per polygon, so results must be grouped back to their own real
# polygon ID before checking "all cells NA", not indexed by row position
# (fixed a real bug here that produced false positives). message(), not
# warning() -- a console-only note, not a pipeline-level data-quality
# warning surfaced via tar_meta().
#
# Arguments:
#   focal_rast    SpatRaster, one layer per season-end month (build_gpp_focal_raster()'s output)
#   paddocks_sf    sf paddock table (paddocks_sf_with_soil_type) -- every paddock is checked, matching
#                  build_gpp_focal_raster()'s own all-paddocks extent
#
# Returns (invisibly) a character vector of flagged paddock_id values (empty
# if none) -- also prints a message() naming them and describing the
# consequence, when any are found.

warn_gpp_focal_coverage_gaps <- function(focal_rast, paddocks_sf) {

  # ID = TRUE keeps each extracted raster cell tagged with its own
  # originating polygon -- see header for why this matters.
  extracted  <- terra::extract(focal_rast, paddocks_sf, ID = TRUE)
  value_cols <- setdiff(names(extracted), "ID")

  # a paddock is flagged only if EVERY one of its own cells is NA in EVERY
  # season-end month -- grouped by the real polygon ID, not row position.
  per_paddock_always_na <- extracted |>
    dplyr::group_by(ID) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(value_cols), ~ all(is.na(.x))), .groups = "drop")
  flagged_ids <- per_paddock_always_na$ID[apply(per_paddock_always_na[value_cols], 1, all)]
  flagged     <- paddocks_sf$paddock_id[flagged_ids]

  if (length(flagged) > 0) {
    message(
      "warn_gpp_focal_coverage_gaps(): gpp_finescale is NA in every season-end month for ", length(flagged),
      " paddock(s) -- no valid land pixel anywhere in their own focal window, so gpp_finescale can't be ",
      "used for any observation from these paddocks, in either of its own consumers (Section E's ",
      "burrow_count detection formula, Section D's chewcard_count_exploration.qmd) (paddock_id: ",
      paste(flagged, collapse = ", "), ")"
    )
  }

  invisible(flagged)
}

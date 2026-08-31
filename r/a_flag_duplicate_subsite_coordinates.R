# Shared by clean_data_access_monitoring_rapid() and
# clean_data_access_monitoring_traps() -- previously duplicated (byte-for-byte
# apart from the printed caller name) in each; moved here as the single copy
# both source via tar_source("r/").
#
# Flags distinct subsites sharing identical coordinates -- likely a
# data-entry error in the Access database's site reference table (e.g.
# multiple transects/trap lines/grids at one site recorded with a single
# site-level coordinate instead of per-subsite coordinates), which causes
# downstream joins on (longitude, latitude) to merge those subsites into one
# paddock. Printed (not warned/errored) since this is surfaced for manual
# investigation, not something the pipeline can resolve itself.
#
# Arguments:
#   cleaned      data frame with longitude, latitude, site_name, subsite_name
#   caller_name  calling function's own name (e.g. "clean_data_access_monitoring_rapid"),
#                used only in the printed message so it's clear which source the flagged rows came from
#
# Returns cleaned's own dup_coords subset, invisibly (callers currently only use this for its message() side effect).
.flag_duplicate_subsite_coordinates <- function(cleaned, caller_name) {

  dup_coords <- cleaned |>
    dplyr::distinct(longitude, latitude, site_name, subsite_name) |>
    dplyr::group_by(longitude, latitude) |>
    dplyr::filter(dplyr::n_distinct(subsite_name) > 1) |>
    dplyr::ungroup() |>
    dplyr::arrange(longitude, latitude, subsite_name)

  if (nrow(dup_coords) > 0) {
    message(caller_name, "(): subsites sharing identical coordinates:")
    print(dup_coords, n = Inf)
  }

  invisible(dup_coords)
}

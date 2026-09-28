# Flag GPP (MODIS or VIIRS) composite files that look corrupt or incomplete.
# Two signals: "readable" (the authoritative one -- actually opens and reads
# the file's data, via gpp_file_readable()) and "undersized" (a secondary,
# informational heuristic -- suspiciously small relative to its own (region,
# layer) group's median, using a robust MAD rather than a fixed byte
# threshold, since genuine composites still vary a little in size). File
# size varies systematically by layer (Gpp_500m vs. the smaller Psn_QC_500m)
# and by download batch (each AppEEARS job lands in its own dated
# subfolder, "region" here) -- pass one product's subfolder at a time
# (e.g. .../gpp/viirs_historic), since scanning several together would
# conflate their same-named batch folders into one group.
#
# Arguments:
#   gpp_dir         root folder to scan recursively for composite tifs --
#                   one product's subfolder, e.g.
#                   raw_data/predictor_variables/gpp/modis,
#                   .../gpp/viirs_historic or .../gpp/viirs_recent
#   mad_threshold   how many MADs below the group median counts as
#                   undersized (default 5 -- deliberately conservative,
#                   since it's an informational flag, not an action trigger)
#   min_group_n     minimum files in a (region, layer) group before the size
#                   check is attempted (default 5 -- MAD is meaningless on a
#                   handful of files)
#
# Returns a tibble (empty if nothing flagged): path, region, layer, size_mb,
# group_median_mb, mad_below, readable, reason ("unreadable" or
# "undersized"). Delete a flagged file and re-run download_gpp() to replace
# it -- it's resumable.

check_gpp_files <- function(gpp_dir = "raw_data/predictor_variables/gpp/viirs_historic",
                             mad_threshold = 5,
                             min_group_n   = 5) {

  files <- list.files(gpp_dir, pattern = "\\.tif$", recursive = TRUE, full.names = TRUE)

  checked <- tibble::tibble(
    path    = files,
    region  = basename(dirname(files)), # the batch subfolder each file sits in -- see header note on grouping
    layer   = dplyr::case_when(
      grepl("Gpp_500m", files)    ~ "Gpp_500m",
      grepl("Psn_QC_500m", files) ~ "Psn_QC_500m",
      TRUE                        ~ NA_character_ # a non-composite file (e.g. sidecar/metadata) -- dropped below
    ),
    size_mb = file.size(files) / 1e6
  ) |>
    dplyr::filter(!is.na(layer)) |> # only real composite files continue past here
    dplyr::mutate(
      group_median_mb = stats::ave(size_mb, region, layer, FUN = stats::median), # typical size for this (region, layer) group
      group_mad_mb    = stats::ave(size_mb, region, layer, FUN = stats::mad),    # robust spread for that same group
      group_n         = stats::ave(size_mb, region, layer, FUN = length),        # how many files are in that group
      mad_below       = ifelse(group_mad_mb > 0, (group_median_mb - size_mb) / group_mad_mb, 0), # how many MADs below the group median
      undersized      = group_n >= min_group_n & mad_below > mad_threshold       # flagged only if the group is big enough to trust
    )

  # the authoritative signal -- actually opens and reads each file's data, not just its header
  checked$readable <- vapply(checked$path, gpp_file_readable, logical(1))

  checked |>
    dplyr::filter(!readable | undersized) |>                              # keep only flagged files
    dplyr::mutate(reason = ifelse(!readable, "unreadable", "undersized")) |>
    dplyr::select(path, region, layer, size_mb, group_median_mb, mad_below, readable, reason) |>
    dplyr::arrange(readable, dplyr::desc(mad_below)) # unreadable files first, then worst undersized first

}

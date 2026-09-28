# Restrict a vector of downloaded GPP composite file paths (download_gpp()'s own output) to
# whichever ones fall in a given set of calendar years, parsed from each file's own
# "doy<YYYY><DDD>" AppEEARS filename token. Used by composite_gpp_rast_modis_block (_targets.R),
# which branches load_and_clamp_gpp_composites() over modis_year_blocks the same way VIIRS/SILO
# already branch their own equivalents, since gpp_files_modis itself is one single, unbranched
# download (MODIS is frozen, downloaded once as a whole archive -- see gpp_files_modis's own
# comment) -- this is what turns that one flat file list into each block's own subset.
#
# %in%, not == -- confirmed live: download_gpp()'s own output mixes in non-composite support files
# (study_area.gpkg, gpp_pending_check.rds, request/metadata files) alongside the real .tif
# composites, and their "year" extraction here is NA. == propagates that NA straight into the
# logical index (silently KEEPING an NA placeholder in the filtered result instead of dropping it),
# corrupting every downstream date parse; %in% treats NA as a clean non-match instead.
#
# Arguments:
#   files  character vector of file paths (e.g. download_gpp()/download_gpp_block()'s own output)
#   years  integer vector of calendar years to keep
#
# Returns files, restricted to whichever ones parse to a year in years.

filter_gpp_files_by_year <- function(files, years) {
  files[as.integer(sub(".*doy([0-9]{4}).*", "\\1", basename(files))) %in% years]
}

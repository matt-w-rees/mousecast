# Load AWRA-L v7's sm_pct.nc (rootzone soil moisture, % full -- downloaded by
# download_awra_data(), r/b_download_awra_data.R) and label its layers with
# this pipeline's own month_year convention.
#
# Adapted from tmp_workflow_part_2/r/cov_process_awra_layers.R (an earlier,
# un-integrated draft) -- fixed one real bug there: it built labels as
# "<month>-<year>" (e.g. "7-2026"), not this pipeline's actual "<month>_<year>"
# convention (attach_time_variables(), r/a_attach_time_variables.R) that
# attach_raster_covs() matches against, so those labels would never
# have joined onto survey/paddock data correctly. Also modernised to return
# the SpatRaster directly (for tar_terra_rast()) rather than writing to disk
# and returning a path -- matching this pipeline's current build_*_raster()
# convention (e.g. build_monthly_rainfall_raster(), r/b_build_monthly_rainfall_raster.R)
# rather than the file-target style the original draft predates.
#
# No unit conversion or masking needed here -- sm_pct is already a percentage
# (not a rate needing days-in-month adjustment like PML-V2's GPP, see
# build_pml_gpp_raster()'s header), and AWRA is a modelled national surface,
# not a remotely-sensed product with fill/ocean codes to clamp.
#
# sm_pct.nc carries no CRS in its own metadata, so terra::rast() guesses one
# at read time, emitting a "[rast] guessed crs" warning every run -- confirmed
# live (2026-08) the guess itself (WGS84 lon/lat) is correct (extent matches
# Australia, matches every other covariate raster in this pipeline), so the
# warning is suppressed at its source below (the guess happens the moment
# rast() reads the file, so setting crs() afterwards doesn't stop it being
# raised) -- the same intent as the explicit crs() raster_mean_series()
# already sets on its own points argument for the same reason
# (r/b_raster_mean_series.R's own header), just applied where this specific
# warning actually originates.
#
# Arguments:
#   file   path to sm_pct.nc (download_awra_data()'s output)
#
# Returns a SpatRaster, one layer per month_year.

build_awra_soil_moisture_raster <- function(file) {

  # Guard against an incomplete/failed AWRA download reaching terra::rast() as an empty or
  # missing path -- download_awra_data() deliberately drops a variable from its own return value
  # on a partial transfer (its own header comment: non-fatal, resumed on the next tar_make() call),
  # so a call site filtering awra_files by variable name (e.g. soil_moisture_raster, _targets.R)
  # can end up passing character(0) here instead of a real path. terra::rast(character(0)) throws
  # an opaque "[rast] filename is empty" error with no mention of AWRA/sm_pct at all -- confirmed
  # live 2026-09, traced back to a dropped mid-transfer connection (curl exit 18) leaving
  # sm_pct.nc.part on disk instead of a finished sm_pct.nc. This turns that into an actionable
  # message pointing at the real cause instead.
  if (length(file) == 0 || !file.exists(file)) {
    stop(
      "build_awra_soil_moisture_raster(): no AWRA sm_pct.nc file to read (got ",
      if (length(file) == 0) "no path at all" else paste0("'", file, "', which doesn't exist"),
      "). download_awra_data() has likely not finished downloading it yet -- check for a ",
      "'sm_pct.nc.part' file under its out_dir (a transfer still in progress or interrupted ",
      "partway through) and rerun tar_make() to resume it."
    )
  }

  r <- suppressWarnings(terra::rast(file)) # confirmed-correct guessed CRS -- see header
  terra::crs(r) <- "EPSG:4326" # set explicitly so it's no longer just a guess

  dates <- as.Date(terra::time(r))
  names(r) <- paste0(lubridate::month(dates), "_", lubridate::year(dates))

  r
}

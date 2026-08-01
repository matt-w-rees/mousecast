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
# Arguments:
#   file   path to sm_pct.nc (download_awra_data()'s output)
#
# Returns a SpatRaster, one layer per month_year.

build_awra_soil_moisture_raster <- function(file) {

  r <- terra::rast(file)

  dates <- as.Date(terra::time(r))
  names(r) <- paste0(lubridate::month(dates), "_", lubridate::year(dates))

  r
}

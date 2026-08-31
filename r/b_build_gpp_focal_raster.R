# Build the fine-scale, focal-smoothed GPP covariate used for burrow_count's own detection formula
# (ground cover obscures burrows, CLAUDE.md) -- a NATIVE-resolution (~463m MODIS/VIIRS, not resampled
# onto the shared 0.1 degree/~11km grid every transition-side GPP covariate uses) local average
# around each paddock, since a coarse national-grid cell is far too large to reflect one specific
# paddock's own crop cover. monthly_gpp_rast (MODIS/VIIRS only, not the PML-spliced gpp_raster_coarse)
# is enough on its own: rapid assessment (burrow/chewcard) has no records before 2012
# (r/c_build_survey_visit_grid.R), so PML-V2's pre-2000 extension buys nothing here.
#
# Cropped to the requested states' paddock footprint BEFORE focal()-smoothing -- both the correct
# scope (this covariate is only ever queried at the eastern-states rapid-assessment paddocks it's
# built for, unlike the transition-side covariates which need full national coverage for
# plague_risk_map) and a genuine performance requirement: focal() on the raster's full
# covariate_download_region extent took ~71 sec PER LAYER (confirmed live -- large enough that terra
# falls back to slow, chunked disk-backed processing), versus ~0.2 sec/layer once cropped to this
# smaller, in-memory-sized extent (also confirmed live) -- hours versus seconds across a multi-layer
# stack. Every step happens inside this one function call, not as separate upstream _targets.R
# targets, deliberately -- a standalone SpatExtent target doesn't survive a crew worker loading it
# fresh from disk (confirmed live: "NULL value passed as symbol address" -- terra's C++ objects wrap
# an external pointer that dies on serialization, the same class of issue tar_terra_rast() exists to
# solve for SpatRaster specifically); keeping the extent a transient in-function value sidesteps the
# problem entirely rather than working around it.
#
# Season-end months only (Feb/May/Aug/Nov) -- unlike gpp_rolling6/gpp_rolling3, this is a raw
# single-month snapshot with no rolling-average calculation needing every other month as input
# first, so it can be trimmed the same way soil_moisture's own raw value already is (_targets.R
# section 4).
#
# A pure raster build only -- no validation/warning side effect (2026-08 refactor, matching this
# pipeline's function-per-task convention, CLAUDE.md). compute_focal_raster()'s own
# na.policy = "all" (r/b_compute_focal_raster.R) already recovers a paddock whose centre pixel
# alone is masked (e.g. GPP's ocean mask clipping a paddock right at the coastline) by averaging in
# its valid neighbours instead -- but a paddock whose entire focal window is masked in every month
# genuinely has no data to recover, and stays NA. See warn_gpp_focal_coverage_gaps()
# (r/b_warn_gpp_focal_coverage_gaps.R), called separately in _targets.R right after this target, for
# the coverage check that flags those cases.
#
# Arguments:
#   monthly_gpp_rast   native-resolution monthly GPP stack (_targets.R section 2.vi), "<month>_<year>" layer names
#   paddocks_sf         sf paddock table with a "state" column (paddocks_sf_with_soil_type)
#   states              Australian state abbreviations to crop to (e.g. eastern_states)
#   window              focal window side length in cells, passed through to compute_focal_raster()
#                       (default 3 -- a 3x3-cell neighbourhood, ~1.4 x 1.4km at this raster's own
#                       ~463m native resolution -- smooths pixel noise while staying local to one
#                       paddock)
#
# Returns a SpatRaster, one layer per season-end month, native resolution, focal-smoothed.

build_gpp_focal_raster <- function(monthly_gpp_rast, paddocks_sf, states, window = 3) {
  season_end_layers <- names(monthly_gpp_rast)[as.integer(sub("_.*", "", names(monthly_gpp_rast))) %in% c(2, 5, 8, 11)]
  crop_extent <- terra::ext(sf::st_bbox(dplyr::filter(paddocks_sf, state %in% states))) + 0.1
  compute_focal_raster(terra::crop(monthly_gpp_rast[[season_end_layers]], crop_extent), window = window)
}

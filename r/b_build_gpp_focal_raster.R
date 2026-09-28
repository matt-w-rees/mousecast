# Build the fine-scale, focal-smoothed GPP covariate ("gpp_finescale") -- originally built purely for
# burrow_count's own detection formula (ground cover obscures burrows, CLAUDE.md), eastern-states-only
# at the time; now also a core predictor in quarto_reports/chewcard_count_exploration.qmd (Section D),
# which needs every state including WA -- so it's built for every state with at least one paddock
# (no separate state list to keep in sync as new states are surveyed). A NATIVE-resolution (~463m, not resampled onto the
# shared 0.1 degree/~11km grid every transition-side GPP covariate uses) local average around each
# paddock, since a coarse national-grid cell is far too large to reflect one specific paddock's own
# crop cover. Cropped to the paddocks' own footprint BEFORE focal()-smoothing -- both the
# correct scope (only ever queried at paddock locations) and a real
# performance win (focal() on the full national extent is orders of magnitude slower than on this
# smaller, in-memory-sized crop -- confirmed live). Season-end months only (Feb/May/Aug/Nov), a raw
# single-month snapshot with no rolling-average calculation needing every other month. A pure raster
# build with no validation side effect -- see warn_gpp_focal_coverage_gaps()
# (r/b_warn_gpp_focal_coverage_gaps.R), called separately right after this target, for the coverage
# check that flags paddocks with no valid data to recover.
#
# Arguments:
#   monthly_gpp_rast   native-resolution monthly GPP stack (_targets.R section 2.vi), "<month>_<year>" layer names
#   paddocks_sf         sf paddock table (paddocks_sf_with_soil_type) -- every paddock, whichever state
#   window              focal window side length in cells, passed through to compute_focal_raster()
#                       (default 3 -- a 3x3-cell neighbourhood, ~1.4 x 1.4km at this raster's own
#                       ~463m native resolution -- smooths pixel noise while staying local to one
#                       paddock)
#
# Returns a SpatRaster, one layer per season-end month, native resolution, focal-smoothed.

build_gpp_focal_raster <- function(monthly_gpp_rast, paddocks_sf, window = 3) {
  # Keep only the four season-end calendar months (Feb/May/Aug/Nov).
  season_end_layers <- names(monthly_gpp_rast)[as.integer(sub("_.*", "", names(monthly_gpp_rast))) %in% c(2, 5, 8, 11)]
  # Bounding box of every paddock, with a small buffer.
  crop_extent <- terra::ext(sf::st_bbox(paddocks_sf)) + 0.1
  # Crop to that footprint first (cheap on a disk-backed raster), then focal-smooth at native resolution.
  compute_focal_raster(terra::crop(monthly_gpp_rast[[season_end_layers]], crop_extent), window = window)
}

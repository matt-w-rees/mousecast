# Resample a raster onto another raster's exact grid (extent, resolution,
# CRS), via an area-weighted average of every source cell within each
# destination cell (terra::resample(method = "average")) -- the correct way
# to downsample a finer raster onto a coarser one, as opposed to point-vs-
# point extraction at mismatched resolutions (see r/b_compare_gpp_rasters.R's
# header for why that distinction matters: comparing a single fine pixel
# against a coarse pixel confounds a genuine covariate difference with a
# pure scale/heterogeneity artifact).
#
# Generalises the crop-then-resample pattern first written in an earlier,
# PML-specific bias-check function (check_gpp_pml_bias(), since renamed and
# generalised into compare_gpp_rasters(), r/b_compare_gpp_rasters.R) for
# reuse across every covariate this pipeline coarsens onto PML-V2's 0.1
# degree grid (rain, soil moisture, MOD17/VNP17 GPP -- see _targets.R's
# coarse/anomaly section). compare_gpp_rasters() itself no longer resamples
# inline -- every caller in _targets.R now calls this function explicitly
# first (2026-08 -- confirmed the original PML-specific function had never
# actually been updated to call this, despite this function's own header
# already claiming to generalise it out).
#
# template is cropped to source's own extent (+ margin) before resampling,
# so this only ever processes the relevant region -- matters most when
# template is a much larger raster than source actually needs (e.g. before
# pml_gpp_rast was itself cropped to covariate_download_region at its own build step,
# _targets.R's section 6.i, it was PML-V2's full near-global grid).
#
# Arguments:
#   source      SpatRaster to resample (the finer/native-resolution raster)
#   template    SpatRaster whose grid (extent, resolution, CRS) source is
#               resampled onto -- only its geometry is used, not its values
#               (only the first layer is read, since resample() needs just
#               one layer's grid definition)
#   margin_deg  degrees of margin added around source's own extent before
#               cropping template (default 0.5, generous relative to
#               PML's 0.1 degree cells)
#
# Returns a SpatRaster on template's grid, one layer per source layer
# (layer names preserved).

resample_to_grid <- function(source, template, margin_deg = 0.5) {

  # terra::resample() reprojects nothing -- it just maps source cells onto
  # template's own grid coordinates -- so a genuine CRS mismatch here would
  # silently misalign every cell rather than error. Both are expected to
  # already be lon/lat WGS84 (this pipeline's convention throughout), so this
  # should never actually fire; it's here so a future mismatch fails loudly
  # instead of producing quietly-wrong covariate values.
  if (!terra::same.crs(source, template)) {
    stop(
      "resample_to_grid(): source and template have different CRS (",
      terra::crs(source, describe = TRUE)$name, " vs ", terra::crs(template, describe = TRUE)$name,
      ") -- terra::resample() does not reproject, so this would silently misalign every cell."
    )
  }

  source_ext <- terra::ext(source)
  template_cropped <- terra::crop(template[[1]], terra::ext( # [[1]] -- only one layer's grid geometry is needed
    source_ext$xmin - margin_deg, source_ext$xmax + margin_deg,
    source_ext$ymin - margin_deg, source_ext$ymax + margin_deg
  ))

  terra::resample(source, template_cropped, method = "average") # area-weighted average onto template's own grid
}

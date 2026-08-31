# Recompress GPP (MODIS or VIIRS) composite files in place with proper GDAL
# compression (DEFLATE, a datatype-appropriate predictor, max compression
# level). Neither download_gpp() (AppEEARS delivers its files as-is) nor
# r_not_in_use/b_import_viirs_gpp_bundle.R's terra::writeRaster() calls
# specified any compression, so composites written by the latter were stored
# essentially uncompressed. Measured on real composites: a Gpp_500m file went
# 144.9 MB -> 98.0 MB, a Psn_QC_500m file 15.7 MB -> 10.5 MB (~32-33% smaller
# either way) -- no change to values, extent or dimensions, just how the same
# data sits on disk. (Masking to each region's actual ROI polygon, not just
# its bounding box, was considered as a further reduction on top of this, but
# only added ~5 more percentage points for the risk of trusting the polygon
# boundary exactly -- skipped in favour of compression alone.)
#
# The predictor is chosen from each file's actual on-disk datatype rather
# than assumed from its layer name -- both Gpp_500m and Psn_QC_500m
# composites turned out to already be stored as FLT4S (float32) here (not
# byte, despite Psn_QC_500m's documented AppEEARS datatype), likely promoted
# by whichever processing step (reprojection/resampling) wrote them.
#
# Each file is recompressed to a temp path first, then renamed over the
# original -- never written in place directly, so an interrupted run can't
# leave a half-written, corrupted file (see r_not_in_use/b_import_viirs_gpp_bundle.R's
# header notes on this exact failure mode). Not resumable/idempotent --
# re-running against already-recompressed files just recompresses them again
# (harmlessly, but wastefully); it's a supervised one-off cleanup, not part
# of the regular pipeline. Files delivered directly by download_gpp() may
# already be reasonably compressed by AppEEARS itself -- worth spot-checking
# a real file's size before assuming this needs to be run at all.
#
# Arguments:
#   region_dir   folder of composite tifs to recompress in place (e.g.
#                raw_data/predictor_variables/gpp/viirs/east)
#
# Returns list(size_before_gb, size_after_gb), invisibly.

recompress_gpp_files <- function(region_dir) {

  # Let GDAL parallelise each file's own read/compress/write across cores --
  # terra defaults to threads = 0 (off) in this installation (confirmed live
  # 2026-08, see load_and_clamp_gpp_composites()'s own header for the fuller
  # investigation) -- this function's per-file loop below is exactly the
  # shape of workload that setting helps most (thousands of independent
  # single-file read/write operations, not one big combined stack).
  terra::terraOptions(threads = TRUE)

  files <- list.files(region_dir, pattern = "\\.tif$", full.names = TRUE)
  size_before <- sum(file.size(files))

  for (i in seq_along(files)) {
    f <- files[i]
    if (i %% 100 == 0) message(basename(region_dir), ": ", i, "/", length(files), " files recompressed")

    r <- terra::rast(f)
    predictor <- if (terra::datatype(r) %in% c("FLT4S", "FLT8S")) "3" else "2" # floating-point vs integer predictor, per this file's real on-disk datatype

    tmp <- paste0(f, ".recompress_tmp.tif")
    terra::writeRaster(r, tmp, overwrite = TRUE,
                        gdal = c("COMPRESS=DEFLATE", paste0("PREDICTOR=", predictor), "ZLEVEL=9"))
    file.rename(tmp, f) # atomic swap -- original file is never touched until the recompressed copy is complete
  }

  size_after <- sum(file.size(files))
  message(
    basename(region_dir), ": ", round(size_before / 1e9, 1), " GB -> ",
    round(size_after / 1e9, 1), " GB"
  )

  invisible(list(size_before_gb = size_before / 1e9, size_after_gb = size_after / 1e9))
}

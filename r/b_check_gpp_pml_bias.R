# Compare PML-V2 (TPDC, 1982+) against this pipeline's own MOD17/VNP17-based
# GPP record at paddock locations, on the months they both cover, to check
# whether PML-V2's pre-2000 extension can be spliced in as-is or needs a bias
# correction first -- see r/b_download_gpp.R's header and CLAUDE.md-adjacent
# session notes on why the two are different algorithms (PML-V2 vs MOD17/
# VNP17's light-use-efficiency family), not just different resolutions, so a
# systematic offset between them wouldn't be surprising even where both
# should agree (their post-2000 overlap).
#
# Resolution-matched, not point-vs-point: our_gpp_rast (500m) is resampled
# onto pml_gpp_rast's exact 0.1 degree grid (area-weighted average of every
# 500m cell within each 0.1 degree cell, via terra::resample(method =
# "average")) before either side is extracted at paddock locations. Comparing
# a raw 500m pixel against a 0.1 degree (~100 km^2) pixel at the same point
# would confound a genuine model bias with a pure scale/heterogeneity
# artifact -- this is the exact same issue PML-V2's own README warns about
# for comparing its 0.1 degree grid against point-like flux towers ("a mixed
# grid average against a specific tower flux inevitably leads to systematic
# scaling biases... which are scaling effects, not model errors"). A single
# fine pixel at a paddock centroid is much closer to that point-like case
# than to PML's own coarse, heterogeneous footprint, so resampling first is
# what makes this a fair like-for-like comparison.
#
# Mirrors compare_viirs_gpp_products()'s (r/b_compare_viirs_gpp_products.R)
# paddock-centroid, non-raster-wide comparison philosophy and output shape,
# but matches by month_year (both rasters already monthly, same gC/m^2/day
# convention -- build_pml_gpp_raster() converts PML to it directly;
# build_seasonal_gpp_raster() converts our own MOD17/VNP17-based composites
# out of their raw kg C/m^2/8-day scale to match, see that function's own 2b
# step) rather than by exact composite date, since PML-V2 has no 8-day-
# composite equivalent to match against.
#
# Arguments:
#   paddocks_sf              sf polygon object with paddock_id and
#                             longitude_paddock/latitude_paddock (the
#                             pipeline's paddocks_sf target, after
#                             paddock_centroids())
#   structured_paddock_ids    paddock_ids with structured survey data -- see
#                             _targets.R's shared target of the same name
#   our_gpp_rast              this pipeline's own GPP SpatRaster, one layer
#                             per month_year, gC/m^2/day, 500m (e.g.
#                             build_seasonal_gpp_raster(..., summarise_by =
#                             "month")'s output)
#   pml_gpp_rast               PML-V2's GPP SpatRaster, one layer per
#                             month_year, gC/m^2/day, 0.1 degree
#                             (build_pml_gpp_raster()'s output)
#
# Returns a tibble, one row per overlapping month_year: month_year,
# n_paddocks, bias_mean (pml - ours, averaged over paired non-NA paddocks),
# bias_mad (median absolute bias, robust to outlier paddocks), correlation,
# pml_mean and our_mean (each side's own mean, averaged over the same paired
# paddocks -- kept alongside bias_mean so a calendar-month ratio (our_mean /
# pml_mean) can be aggregated too, see summarise_gpp_ratio_by_month(), r/
# b_summarise_gpp_ratio_by_month.R, and its own header for why a ratio
# correction is used instead of bias_mean's flat additive one). Multiple
# nearby paddocks that fall inside the same 0.1 degree PML cell will
# legitimately share identical values on both sides -- an expected
# consequence of comparing at PML's coarser resolution, not a bug.

check_gpp_pml_bias <- function(paddocks_sf, structured_paddock_ids, our_gpp_rast, pml_gpp_rast) {

  paddocks_sf <- dplyr::filter(paddocks_sf, paddock_id %in% structured_paddock_ids)
  points_vect <- terra::vect(
    sf::st_drop_geometry(paddocks_sf),
    geom = c("longitude_paddock", "latitude_paddock"),
    crs  = "EPSG:4326"
  )

  common <- intersect(names(our_gpp_rast), names(pml_gpp_rast))
  if (length(common) == 0) {
    stop("our_gpp_rast and pml_gpp_rast share no month_year layers in common.")
  }

  # ── Resample our fine (500m) raster onto PML's exact coarse (0.1 degree)
  # grid -- crop to just the paddocks' extent (+ margin) first so this only
  # ever processes the relevant region, not pml_gpp_rast's full global extent ──
  bbox       <- sf::st_bbox(paddocks_sf)
  margin_deg <- 0.5
  pml_template <- terra::crop(pml_gpp_rast[[1]], terra::ext(
    bbox["xmin"] - margin_deg, bbox["xmax"] + margin_deg,
    bbox["ymin"] - margin_deg, bbox["ymax"] + margin_deg
  ))
  our_gpp_rast_coarse <- terra::resample(our_gpp_rast[[common]], pml_template, method = "average")

  purrr::map_dfr(common, function(m) {
    our_vals <- terra::extract(our_gpp_rast_coarse[[m]], points_vect, ID = FALSE)[[1]]
    pml_vals <- terra::extract(pml_gpp_rast[[m]], points_vect, ID = FALSE)[[1]]

    paired <- stats::na.omit(data.frame(our = our_vals, pml = pml_vals))

    tibble::tibble(
      month_year  = m,
      n_paddocks  = nrow(paired),
      bias_mean   = mean(paired$pml - paired$our),
      bias_mad    = stats::median(abs(paired$pml - paired$our)),
      correlation = if (nrow(paired) >= 2) stats::cor(paired$pml, paired$our) else NA_real_,
      pml_mean    = mean(paired$pml),
      our_mean    = mean(paired$our)
    )
  })

}

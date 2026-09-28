# Merge a list of SpatRasters into one continuous stack, by layer name, most-
# robust-source-first: for each distinct layer name across every input, take
# it from the FIRST element of rast_list that actually has it. A genuine
# coalesce, not a plain concatenation -- rast_list's own order is the
# priority order, so a period covered by more than one input is resolved
# deterministically (its higher-priority source is used) instead of producing
# two same-named layers with no defined meaning for which one a later
# rast[["<name>"]] lookup would return.
#
# For most callers (min_temp_raster, monthly_gpp_rast_viirs_gf), rast_list's
# own inputs are non-overlapping year-blocks by construction, so a coalesce
# and a plain concatenation produce an identical result. GPP's own splice
# (monthly_gpp_rast_frozen/monthly_gpp_rast, _targets.R) genuinely needs
# overlap-aware precedence, though: MOD17 is downloaded in full (2000-
# present, not just its pre-2012 real contribution to the splice, see
# r/b_download_gpp.R's header), so it necessarily overlaps VNP17's own 2012+
# range. Resolving that precedence here, once, centrally, is what lets
# PML/MODIS both get corrected/processed over their own FULL available range
# without every caller having to separately pre-trim its own input to a
# non-overlapping window first (e.g. PML's own pre2000_layers trim, applied
# before apply_gpp_pixel_ratio_correction() rather than after) -- the merge
# itself decides precedence via rast_list's own order; see each call site in
# _targets.R for its own robustness ordering.
#
# One real consequence: if the most-robust source ever has a genuine gap
# (e.g. VIIRS gap-filled hasn't published a given recent month yet), the
# merge silently falls through to the next-listed source for that one
# period, rather than leaving that period entirely absent from the combined
# stack the way a plain concatenation would. For MODIS as VIIRS's own
# fallback, that source is a different product with its own real, quantified
# offset (gpp_modis_bias_check, ~15% higher for VIIRS -- see
# gpp_processing.qmd's "vi. Splice into one continuous stack") and gets no
# correction of its own (a deliberate choice -- diagnostic only) -- so a
# fallback month, if one is ever actually needed, carries that same
# uncorrected offset. Judged an acceptable, rare trade-off: a same-instrument-
# family value for one gap beats a hole in the covariate outright, and every
# other period is unaffected (VIIRS remains the only real source used
# wherever it actually publishes something).
#
# terra::rast(list_of_spatrasters) combines correctly where do.call(c,
# list_of_spatrasters) does not -- confirmed live that do.call(c, .) on a
# many-element list of SpatRasters silently returns a plain list instead of
# dispatching terra's own c.SpatRaster method (no error, no warning -- this
# went undetected until a downstream layer count came out wrong). terra::rast()
# has its own gotcha too: when the input list itself has element names (e.g.
# a gathered dynamic branch target, whose list elements are named after each
# branch's own target hash), it uses THOSE names (plus a per-layer index
# suffix) to construct new layer names, discarding every input raster's own
# meaningful names (e.g. summarise_silo_to_month()'s "<month>_<year>"
# convention) in the process -- confirmed live directly against
# min_temp_raster_block's own 10 branches. Selecting each source's own
# contribution out as one bulk chunk below and building the result from that
# explicit list sidesteps both gotchas regardless of whether rast_list
# itself happens to be named.
#
# Extracted ONE BULK CHUNK PER SOURCE, not one r[[<name>]] call per
# individual layer name: a disk-backed SpatRaster reopens its backing file
# on every single-layer `[[` subset, so looping name-by-name would mean
# ~1200+ separate opens of a many-GB file for GPP's own composite-grain
# splice alone (composite_gpp_rast, ~1200+ distinct composite dates across
# three sources) -- confirmed live as the dominant cost of that splice,
# visible via lsof as dozens of concurrent open handles on the same file,
# several hours in and still climbing. Claiming each source's own
# not-yet-taken names and pulling them out in one r[[<names>]] call instead
# cuts that down to one open per source (3, for the GPP splice) regardless
# of how many composite dates that source actually covers.
#
# Every downstream consumer of this function's output already treats a
# SpatRaster's own layer POSITION order as unreliable and re-sorts by parsed
# name before doing anything position-dependent (compute_rolling_mean_raster(),
# compute_whiplash_raster()) or selects layers by name entirely
# (raster_mean_series(), trim_to_season_end_months(), compute_loo_anomaly_raster(),
# compute_climatology_raster(), build_gpp_focal_raster(), build_gpp_pixel_ratio_raster(),
# apply_gpp_pixel_ratio_correction(), attach_raster_covs()) -- confirmed live
# across every one of them before relying on that here, since this
# function's own output layer order (each source's own block, in
# rast_list's priority order) is neither first-seen-across-all-inputs nor
# necessarily chronological. That same tolerance is what makes the
# per-source bulk-chunk order above safe to use as-is, with no extra
# reorder-to-match-all_names step needed (which would just reintroduce a
# per-layer indexing cost on the already-combined result).
#
# Arguments:
#   rast_list   list of SpatRasters, ordered MOST-ROBUST-SOURCE-FIRST (e.g. a
#               gathered dynamic branch target, optionally combined with c()
#               alongside plain, non-branched SpatRasters -- see _targets.R's
#               composite_gpp_rast_viirs_gf/composite_gpp_rast for both cases)
#
# Returns one SpatRaster: one layer per distinct name found across every
# input, each taken from the highest-priority (earliest in rast_list) source
# that actually has it.

combine_spatraster_list <- function(rast_list) {
  # every distinct layer name found across all inputs -- used only to know
  # what's actually needed, not to drive a per-name extraction loop
  all_names <- unique(unlist(lapply(rast_list, names)))

  # walk rast_list in priority order, claiming each source's own contribution
  # (its own layers not already claimed by a higher-priority source) as ONE
  # bulk subset call, not one call per layer -- see header for why this
  # matters
  claimed <- character(0)
  chunks  <- list()
  for (r in rast_list) {
    contribute <- intersect(names(r), setdiff(all_names, claimed))
    if (length(contribute) == 0) next # this source has nothing left to add
    chunks[[length(chunks) + 1]] <- r[[contribute]] # one bulk r[[<names>]] call for this whole source
    claimed <- c(claimed, contribute)
  }

  terra::rast(chunks)
}

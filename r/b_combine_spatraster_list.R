# Merge a list of SpatRasters into one continuous stack, by layer name, most-
# robust-source-first: for each distinct layer name across every input, take
# it from the FIRST element of rast_list that actually has it. A genuine
# coalesce, not a plain concatenation -- rast_list's own order is the
# priority order, so a period covered by more than one input is resolved
# deterministically (its higher-priority source wins) instead of producing
# two same-named layers with no defined meaning for which one a later
# rast[["<name>"]] lookup would return.
#
# Generalised (2026-08) from an earlier version that only ever concatenated
# (terra::rast(rast_list), no merge logic at all) -- safe for its two
# original callers (min_temp_raster, monthly_gpp_rast_viirs_gf), whose own
# inputs are non-overlapping year-blocks by construction, so this produces an
# identical result there. But GPP's own splice (monthly_gpp_rast_frozen/
# monthly_gpp_rast, _targets.R) needs genuine overlap-aware precedence: MOD17
# is now downloaded in full (2000-present, not just its pre-2012 real
# contribution to the splice, see r/b_download_gpp.R's header), so it
# necessarily overlaps VNP17's own 2012+ range -- the old design worked
# around that by manually pre-trimming MOD17 to a non-overlapping window
# before concatenating (monthly_gpp_rast_modis_pre2012, since retired) rather
# than teaching the merge step itself which source should win. That meant
# every product needing this same "which source wins" logic (PML's own
# pre2000_layers trim, applied before apply_gpp_pixel_ratio_correction()
# rather than after) had to re-derive it independently. This version handles
# it once, centrally: PML/MODIS now both get corrected/processed over their
# own FULL available range, and the merge itself decides precedence via
# rast_list's own order -- see each call site in _targets.R for its own
# robustness ordering.
#
# One real consequence, not hidden by this change: if the most-robust source
# ever has a genuine gap (e.g. VIIRS gap-filled hasn't published a given
# recent month yet), the merge now silently falls through to the next-listed
# source for that one period -- previously that period would just be
# entirely absent from the combined stack. For MODIS as VIIRS's own
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
# list_of_spatrasters) does not -- confirmed live this session that do.call(c,
# .) on a many-element list of SpatRasters silently returns a plain list
# instead of dispatching terra's own c.SpatRaster method (no error, no
# warning -- this went undetected until a downstream layer count came out
# wrong). terra::rast() has its own gotcha too: when the input list itself
# has element names (e.g. a gathered dynamic branch target, whose list
# elements are named after each branch's own target hash), it uses THOSE
# names (plus a per-layer index suffix) to construct new layer names,
# discarding every input raster's own meaningful names (e.g.
# summarise_silo_to_month()'s "<month>_<year>" convention) in the process --
# confirmed live directly against min_temp_raster_block's own 10 branches.
# Selecting each source's own contribution out as one bulk chunk below and
# building the result from that explicit list sidesteps both gotchas
# regardless of whether rast_list itself happens to be named.
#
# Extracted ONE BULK CHUNK PER SOURCE (2026-08), not one r[[<name>]] call per
# individual layer name (an earlier version's own design) -- confirmed live
# this session that the per-name version was the dominant cost of GPP's own
# composite-grain splice (composite_gpp_rast, ~1200+ distinct composite dates
# across three sources), not the merge logic itself: a disk-backed SpatRaster
# reopens its backing file on every single-layer `[[` subset, so looping
# name-by-name meant ~1200+ separate opens of a many-GB file, visible live via
# lsof as dozens of concurrent open handles on the same file, several hours in
# and still climbing. Claiming each source's own not-yet-taken names and
# pulling them out in one r[[<names>]] call instead cuts that down to one open
# per source (3, for the GPP splice) regardless of how many composite dates
# that source actually covers.
#
# Every downstream consumer of this function's output already treats a
# SpatRaster's own layer POSITION order as unreliable and re-sorts by parsed
# name before doing anything position-dependent (compute_rolling_mean_raster(),
# compute_whiplash_raster()) or selects layers by name entirely
# (raster_mean_series(), trim_to_season_end_months(), compute_loo_anomaly_raster(),
# compute_climatology_raster(), build_gpp_focal_raster(), build_gpp_pixel_ratio_raster(),
# apply_gpp_pixel_ratio_correction(), attach_raster_covs()) -- confirmed
# live across every one of them (2026-08) before relying on that here, since
# this function's own output layer order (each source's own block, in
# rast_list's priority order -- not first-seen-across-all-inputs the way an
# even earlier version documented, and not necessarily chronological either
# way) is a real change from the plain-concatenation version. That same
# tolerance is what makes the per-source bulk-chunk order above safe to use
# as-is, with no extra reorder-to-match-all_names step needed (which would
# just reintroduce a per-layer indexing cost on the already-combined result).
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

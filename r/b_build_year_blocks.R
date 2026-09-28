# Split a vector of years into consecutive blocks of at most `block_size`
# years, for dynamic branching (pattern = map()) over a long time series.
# Used two ways in this pipeline:
#
#  - min_temp_year_blocks (_targets.R section 4): so
#    summarise_silo_to_month() (r/b_summarise_silo_to_month.R) aggregates a
#    handful of years' daily SILO composites per branch instead of decades
#    of full-country daily layers in one tapp() call. That single-call
#    approach is what repeatedly got min_temp_raster's build OOM-killed
#    (~2.4h in). silo_daily_files_min_temp itself branches over these same
#    blocks too (pattern = map(min_temp_year_blocks) in _targets.R), and
#    download_silo_daily_data() returns only the files each call actually
#    requested rather than its whole out_dir (see that function's own
#    header) -- so each block's own files are tracked independently: a
#    single whole-range download target would otherwise invalidate every
#    block whenever any one file changed (e.g. the still-forming latest
#    year), not just the block whose own years actually changed, matching
#    gpp_viirs_blocks' own genuine per-block isolation below, not just this
#    function's bounded-memory branching.
#
#  - gpp_viirs_blocks (_targets.R section 2): so each block gets its own
#    download_gpp_block() call (r/b_download_gpp_block.R) into its own
#    out_dir. Because AppEEARS downloads (unlike SILO's) can be scoped to
#    an arbitrary out_dir per call, this case gets genuine per-block
#    isolation, not just bounded memory: changing one block's files leaves
#    every other block's own downstream branch untouched, not just cheap
#    to rebuild.
#
# Arguments:
#   years        integer vector of years to split (e.g. 1980:2025)
#   block_size   maximum years per block (default 5)
#
# Returns a named list of integer vectors, one element per block -- pass to
# tar_target(..., iteration = "list") so each list element becomes its own
# dynamic branch. Names are just each block's own first year, so
# tar_manifest()/tar_visnetwork() branch labels stay legible.

build_year_blocks <- function(years, block_size = 5) {
  blocks <- split(years, ceiling(seq_along(years) / block_size))
  names(blocks) <- vapply(blocks, function(x) as.character(x[1]), character(1))
  blocks
}

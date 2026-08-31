# Thin wrapper around download_gpp() (r/b_download_gpp.R) for one fixed
# calendar-year block of VIIRS composites (_targets.R's gpp_viirs_blocks,
# built by build_year_blocks(), r/b_build_year_blocks.R).
#
# Replaces this pipeline's old rolling gpp_recent_cutoff + prune_stale_before/
# archive_stale_before_to scheme (previously in _targets.R/r/b_download_gpp.R's
# gpp_prune_stale_dates()): that design kept ONE ever-growing "historic" folder
# that composites were migrated into every time the cutoff advanced (~every 3
# months), so monthly_gpp_rast_historic (a single non-branched aggregation
# over that whole, ever-growing folder) got fully invalidated and reprocessed
# on that same cadence -- a cost that only grows over time as more years
# accumulate in it. Fixed calendar blocks avoid this entirely: each block gets
# its own permanent out_dir, so a block already in the past never changes
# again (download_gpp()'s own incremental check finds nothing missing and
# exits immediately) and downstream aggregation branches stay genuinely
# independent (confirmed live against a throwaway `targets` pipeline: changing
# one block's files left every other block's own downstream branch skipped,
# not just cheap to rebuild). Only the block containing today keeps
# genuinely catching up, via download_gpp()'s existing incremental logic --
# no migration between folders ever needed.
#
# out_dir is derived from block_years (out_dir_base/"<year>" for a single-year
# block, e.g. ".../viirs/2026", or out_dir_base/"<first_year>_<last_year>" for
# a multi-year one) so each block's files physically live in their own
# folder, never a shared one -- this is what gives downstream dynamic
# branching (pattern = map()) real per-block cache isolation, not just a
# post-hoc filter over one shared list (see build_year_blocks()'s own header).
# _targets.R currently uses block_size = 1 (one AppEEARS request per calendar
# year) specifically so the current, still-open block never has to request
# more than one year of composites in a single task -- confirmed this matters
# in practice: this scheme's very first run has an empty out_dir for every
# block, so without yearly blocks the current block's first request would
# cover its whole multi-year span at once.
#
# Arguments:
#   block_years     integer vector, one block from gpp_viirs_blocks (e.g. 2012:2016)
#   roi             sf polygon -- passed straight through to download_gpp() (covariate_download_region)
#   out_dir_base    parent folder each block gets its own "<first>_<last>" subfolder under
#   earthdata_user  Earthdata Login username -- passed straight through
#   product         AppEEARS product string -- passed straight through
#   end_date_cap    latest date this block may request, as "YYYY-MM-DD" (_targets.R's
#                   gpp_viirs_end_date, which already accounts for Sys.Date() and any
#                   test_last_year override) -- clamps a block whose calendar range
#                   extends beyond what's actually requestable yet (i.e. the current,
#                   still-open block)
#   time_out        seconds to wait for AppEEARS -- passed straight through
#
# Returns download_gpp()'s own return value: every downloaded file path under
# this block's out_dir (for tar_file() tracking).

download_gpp_block <- function(block_years, roi, out_dir_base, earthdata_user, product,
                                end_date_cap = as.character(Sys.Date()), time_out = 28800) {

  start_date <- paste0(min(block_years), "-01-01")
  end_date   <- as.character(min(as.Date(paste0(max(block_years), "-12-31")), as.Date(end_date_cap))) # clamp to end_date_cap

  # single-year block -> "2026"; multi-year block -> "2012_2016"
  block_label <- if (min(block_years) == max(block_years)) as.character(min(block_years))
                 else paste0(min(block_years), "_", max(block_years))
  out_dir     <- file.path(out_dir_base, block_label) # this block's own permanent, isolated folder

  download_gpp(
    roi            = roi,
    out_dir        = out_dir,
    earthdata_user = earthdata_user,
    product        = product,
    start_date     = start_date,
    end_date       = end_date,
    job_name       = paste0("gpp_viirs_", block_label, "_", format(Sys.Date(), "%Y%m%d")),
    time_out       = time_out
  )
}

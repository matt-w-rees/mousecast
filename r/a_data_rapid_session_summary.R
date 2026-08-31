# Add survey effort and detection summary columns to the combined rapid assessment data.
# Summarises the wide active_burrows_tN and chewcard_percent_N columns into derived columns placed
# before the wide source columns:
#
#   burrow_transects_surveyed  number of transects searched (non-NA active_burrows_t*)
#   burrow_total_count         total active burrows counted across all transects
#   chewcards_deployed         number of chewcards deployed (non-NA chewcard_percent_*)
#   chewcards_detected         number of chewcards with chew percentage >= 1 (mouse presence) --
#                              matches chew_per10's own >= 1 threshold (r/a_combine_survey_data.R),
#                              so a card counted as "chewed" there is also counted as a detection here
#   chewcards_detected_first10 same >= 1 threshold, but restricted to chewcard_percent_1..10 only --
#                              added 2026-08 for the HMM's own chewcard observation stream
#                              (r/d_build_hmm_data.R), which needs one fixed trials count across
#                              surveys that deployed 10, 20 or 40 cards; using only the first 10
#                              keeps every survey on the same denominator without discarding
#                              surveys that happened to deploy more.
#   chewcards_deployed_block2/3/4, chewcards_detected_block2/3/4
#                              same pattern as chewcards_deployed/chewcards_detected_first10, but
#                              for cards 11-20/21-30/31-40 (transects 2/3/4) -- added 2026-08 so the
#                              HMM's own chewcard stream can use MULTIPLE fixed-trials=10 streams
#                              (one per transect actually deployed) instead of discarding cards past
#                              the first transect, see r/d_fit_plague_hmm.R's header for the
#                              block-stream mechanism (confirmed against hmmTMB's own
#                              crossbill_occupancy.R case study as the package's own standard
#                              pattern for varying replicate counts, not a workaround).
#   chewcards_any_detected     1 if ANY deployed card (across all up to 40, not just the first 10)
#                              was chewed, 0 otherwise -- added 2026-08 as an alternative "was the
#                              paddock occupied at all" response, collapsing the graded count down to
#                              a single Bernoulli trial with no fixed-trials denominator needed at
#                              all (unlike the count-based streams above); see r/d_fit_plague_hmm.R's
#                              header for why this is being compared against the block-stream count
#                              version, not adopted outright.
#
# All chewcard/burrow columns are NA (not 0) when nothing of that kind was deployed/surveyed --
# see individual definitions below for the exact per-column deployment gate.

data_rapid_session_summary <- function(data) {

  burrow_vars    <- grep("^active_burrows_t",  names(data), value = TRUE)
  chewcard_vars  <- grep("^chewcard_percent_", names(data), value = TRUE)
  # exact column names, not grep-ordered, so each block is genuinely "cards 1-10"/"11-20"/etc
  # regardless of how chewcard_vars above happens to be ordered
  chewcard_vars_block1 <- intersect(paste0("chewcard_percent_", 1:10),  names(data))
  chewcard_vars_block2 <- intersect(paste0("chewcard_percent_", 11:20), names(data))
  chewcard_vars_block3 <- intersect(paste0("chewcard_percent_", 21:30), names(data))
  chewcard_vars_block4 <- intersect(paste0("chewcard_percent_", 31:40), names(data))

  # Shared helper for "count of cards >= 1% chewed among this specific block's own columns, NA if
  # none of this block's cards were deployed" -- used identically for chewcards_detected_first10
  # (block1) and the new block2/3/4 columns below, so the same >= 1 threshold and NA convention
  # can't drift between them. Kept as a brief anonymous-style helper (CLAUDE.md), not a new file.
  block_detected <- function(vars) {
    deployed <- as.integer(rowSums(!is.na(as.matrix(dplyr::pick(dplyr::all_of(vars))))))
    dplyr::if_else(
      deployed == 0L,
      NA_integer_,
      as.integer(rowSums(as.matrix(dplyr::pick(dplyr::all_of(vars))) >= 1, na.rm = TRUE))
    )
  }

  data |>
    dplyr::mutate(

      # --- Burrow transect summaries ---
      burrow_transects_surveyed = as.integer(rowSums(!is.na(as.matrix(dplyr::pick(dplyr::all_of(burrow_vars)))))),
      # NA when no transects were surveyed
      burrow_total_count        = dplyr::if_else(
        burrow_transects_surveyed == 0L,
        NA_real_,
        rowSums(as.matrix(dplyr::pick(dplyr::all_of(burrow_vars))), na.rm = TRUE)
      ),

      # --- Chewcard summaries ---
      chewcards_deployed = as.integer(rowSums(!is.na(as.matrix(dplyr::pick(dplyr::all_of(chewcard_vars)))))),
      # NA when no chewcards were deployed
      chewcards_detected = dplyr::if_else(
        chewcards_deployed == 0L,
        NA_integer_,
        as.integer(rowSums(as.matrix(dplyr::pick(dplyr::all_of(chewcard_vars))) >= 1, na.rm = TRUE))
      ),
      chewcards_detected_first10 = block_detected(chewcard_vars_block1),

      # --- Per-block (per-transect) chewcard summaries, 2026-08 ---
      chewcards_deployed_block2 = as.integer(rowSums(!is.na(as.matrix(dplyr::pick(dplyr::all_of(chewcard_vars_block2)))))),
      chewcards_deployed_block3 = as.integer(rowSums(!is.na(as.matrix(dplyr::pick(dplyr::all_of(chewcard_vars_block3)))))),
      chewcards_deployed_block4 = as.integer(rowSums(!is.na(as.matrix(dplyr::pick(dplyr::all_of(chewcard_vars_block4)))))),
      chewcards_detected_block2 = block_detected(chewcard_vars_block2),
      chewcards_detected_block3 = block_detected(chewcard_vars_block3),
      chewcards_detected_block4 = block_detected(chewcard_vars_block4),

      # --- Collapsed "any card chewed" indicator, 2026-08 -- see header ---
      chewcards_any_detected = dplyr::if_else(
        chewcards_deployed == 0L,
        NA_integer_,
        as.integer(chewcards_detected > 0L)
      ),

      .after = "comments"
    )
}

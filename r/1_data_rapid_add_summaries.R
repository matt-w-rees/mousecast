# Add survey effort and detection summary columns to the combined rapid assessment data.
# Summarises the wide active_burrows_tN and chewcard_percent_N columns into four
# derived columns placed before the wide source columns:
#
#   burrow_transects_surveyed  number of transects searched (non-NA active_burrows_t*)
#   burrow_total_count         total active burrows counted across all transects
#   chewcards_deployed         number of chewcards deployed (non-NA chewcard_percent_*)
#   chewcards_detected         number of chewcards with chew percentage > 1 (mouse presence)
#
# All four columns are 0 when no survey was conducted (never NA).

data_rapid_add_summaries <- function(data) {

  burrow_vars   <- grep("^active_burrows_t",  names(data), value = TRUE)
  chewcard_vars <- grep("^chewcard_percent_", names(data), value = TRUE)

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
        as.integer(rowSums(as.matrix(dplyr::pick(dplyr::all_of(chewcard_vars))) > 1, na.rm = TRUE))
      ),

      .after = "comments"
    )
}

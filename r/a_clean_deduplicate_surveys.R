# Resolve a single (paddock_id, survey_date) duplicate group.
#
# If crop_group/crop_variety/crop_stage agree across every row in the group,
# the rows represent separate survey lines/subsites within the same paddock
# on the same day, so sum_cols (survey effort + results) are summed into one
# combined row.
#
# If the crop variables differ, the rows are treated as conflicting/duplicate
# entries for the same paddock-day - only one row is kept and the rest are
# dropped:
#   - traps: the row with the highest value_col (ties -> NA treated as -Inf,
#     so a real 0 always outranks a missing count)
#   - rapid: NOT simply the highest value_col -- ranked by strongest evidence
#     of mouse activity instead (detected > 0 first, then burrow count, then
#     chewcard count -- see the inline comment below for why)
resolve_duplicate_group <- function(group, crop_cols, sum_cols, value_col, survey_type) {

  same_crop <- all(vapply(crop_cols, function(col) dplyr::n_distinct(group[[col]]) == 1, logical(1)))

  if (same_crop) {
    other_cols <- setdiff(names(group), c(sum_cols, "comments"))

    combined <- group |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
        dplyr::across(dplyr::all_of(other_cols), dplyr::first),
        comments = if ("comments" %in% names(group)) {
          vals <- unique(comments[!is.na(comments)])
          if (length(vals) == 0) NA_character_ else paste(vals, collapse = "; ")
        } else NULL
      ) |>
      dplyr::select(dplyr::all_of(names(group)))

    if (survey_type == "rapid") {
      # Restore the "zero transects/cards surveyed -> NA count" convention
      # (see data_rapid_session_summary.R), which sum()-ing two NA totals would
      # otherwise turn into 0.
      combined <- combined |>
        dplyr::mutate(
          burrow_total_count = dplyr::if_else(burrow_transects_surveyed == 0L, NA_real_, burrow_total_count),
          chewcards_detected = dplyr::if_else(chewcards_deployed == 0L, NA_integer_, chewcards_detected)
        )

      combined <- merge_rapid_wide_columns(combined, group)
    }

    list(data = combined, action = "combined", n = nrow(group))

  } else {
    if (survey_type == "rapid") {
      # "Maximum value" = strongest evidence of mouse activity, matching
      # mice_detected in app.R/raw_data_update.qmd: a row where burrows or
      # chewcards detected mice outranks one that didn't, regardless of
      # whether its burrow_total_count happens to be NA (0 transects
      # surveyed) vs. an actual 0.
      burrows   <- dplyr::coalesce(group[["burrow_total_count"]], 0)
      chewcards <- dplyr::coalesce(group[["chewcards_detected"]], 0)
      detected  <- as.numeric(burrows > 0 | chewcards > 0)
      val <- detected * 1e6 + burrows * 1000 + chewcards
    } else {
      val <- group[[value_col]]
      val[is.na(val)] <- -Inf
    }

    chosen <- group[which.max(val), ]

    list(data = chosen, action = "kept_max", n = nrow(group))
  }
}

# For a combined rapid row, concatenate the per-transect (active_burrows_tN)
# and per-chewcard (chewcard_percent_N) values from every row in the group
# into the available slots, in order, so downstream summaries (rowMeans()/
# rowSums() over these columns) reflect every transect/card that was
# surveyed. Any values beyond the available slots are dropped - the totals in
# sum_cols (burrow_total_count, chewcards_deployed, etc.) remain correct
# regardless.
merge_rapid_wide_columns <- function(combined, group) {
  for (prefix in c("active_burrows_t", "chewcard_percent_")) {
    cols <- grep(paste0("^", prefix, "[0-9]+$"), names(group), value = TRUE)
    cols <- cols[order(as.integer(sub(prefix, "", cols)))]
    if (length(cols) == 0) next

    m <- as.matrix(group[, cols, drop = FALSE])
    vals <- as.vector(t(m))             # row-major: row 1's values, then row 2's, ...
    vals <- vals[!is.na(vals)]
    vals <- vals[seq_len(length(cols))] # pad/truncate to the available slots

    for (i in seq_along(cols)) combined[[cols[i]]] <- vals[i]
  }
  combined
}

# Resolve genuine duplicate surveys at the same paddock on the same day.
#
# data_list_clean_paddocks can contain multiple rows sharing (paddock_id,
# survey_date) - mostly leftover from the monitoring Access database, where
# the same paddock was sometimes entered as several differently-named
# subsites/lines. paddock_id is the only "site" identifier considered here
# (matching the project's paddock-based reporting), so duplicates are
# resolved purely by (paddock_id, survey_date):
#   - if crop_group/crop_variety/crop_stage agree across the group, the rows
#     are summed into one combined record (separate survey lines within the
#     same paddock on the same day)
#   - otherwise, only the row with the highest value_col is kept and the
#     rest are dropped (conflicting/duplicate entries)
#
# EXCEPTION: historic_walpeup, historic_roseworthy, historic_coleambally and
# historic_qld rows are grouped on (paddock_id, subsite_name, survey_date)
# instead. Walpeup/Roseworthy/Coleambally all use one fixed coordinate per
# site (see r/a_clean_historic_data_walpeup.R, r/a_clean_historic_data_roseworthy.R
# and r/a_clean_historic_data_coleambally.R), so multiple physically separate
# grids/traplines trapped concurrently always collapse to the same
# paddock_id (e.g. Walpeup's concurrent wheat/ryecorn grids; Roseworthy's
# Crop and Pasture traplines; Coleambally's concurrent crop grids at one
# Farm). Queensland (r/a_clean_historic_data_qld.R) is different -- most
# traplines have their own genuinely distinct coordinate -- but 11 of 81
# paddock traplines still happen to share a coordinate with a different
# paddock trapline, so the same collision risk applies there too. Grouping
# on paddock_id alone would treat them
# exactly like the monitoring database's same-paddock-different-subsite case
# below and merge/drop them, silently discarding whichever grid didn't have
# the highest capture count. subsite_name is what actually distinguishes
# them, so including it in the key here keeps them apart. Not applied to
# every source: for the monitoring database, differently-named subsites
# sharing one real paddock_id genuinely are the same survey split across
# named lines, and are meant to be merged -- widening the key for every
# source would stop that from happening.
#
# Run this after data has been joined to paddock_id (match_surveys_to_paddocks),
# so that downstream consumers (shiny app, qmd reports) never see duplicate
# (paddock_id, survey_date) rows and don't need their own dedup logic.
clean_deduplicate_surveys <- function(data, survey_type = c("traps", "rapid")) {

  survey_type <- match.arg(survey_type)

  crop_cols <- c("crop_group", "crop_variety", "crop_stage")

  if (survey_type == "traps") {
    sum_cols  <- c("number_traps_set", "number_phantoms", "number_functional_traps",
                   "number_mice_caught", "n_males_site", "n_females_site",
                   "number_unique_individuals_session", "number_functional_traps_session")
    value_col <- "number_mice_caught"
  } else {
    sum_cols  <- c("burrow_transects_surveyed", "burrow_total_count",
                   "chewcards_deployed", "chewcards_detected")
    value_col <- "burrow_total_count"
  }

  # dedup_id: paddock_id for every source except historic_walpeup/
  # historic_roseworthy/historic_coleambally/historic_qld, where subsite_name
  # is appended (see header comment) -- used only to find and group
  # duplicates below, never included in the returned data
  data <- data |>
    dplyr::mutate(dedup_id = dplyr::if_else(
      data_source %in% c("historic_walpeup", "historic_roseworthy", "historic_coleambally", "historic_qld"),
      paste(paddock_id, subsite_name),
      as.character(paddock_id)
    ))

  dup_keys <- data |>
    dplyr::count(dedup_id, survey_date, name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(dup_keys) == 0) return(dplyr::select(data, -dedup_id))

  unique_rows <- data |> dplyr::anti_join(dup_keys, by = c("dedup_id", "survey_date"))
  dup_rows    <- data |> dplyr::semi_join(dup_keys, by = c("dedup_id", "survey_date"))

  groups <- dup_rows |>
    dplyr::group_by(dedup_id, survey_date) |>
    dplyr::group_split()

  resolved <- purrr::map(groups, resolve_duplicate_group,
                          crop_cols = crop_cols, sum_cols = sum_cols,
                          value_col = value_col, survey_type = survey_type)

  resolved_data <- purrr::map_dfr(resolved, "data")
  actions       <- purrr::map_chr(resolved, "action")
  group_sizes   <- purrr::map_int(resolved, "n")

  n_combined <- sum(actions == "combined")
  n_kept_max <- sum(actions == "kept_max")
  n_merged   <- sum(group_sizes[actions == "combined"]) - n_combined
  n_dropped  <- sum(group_sizes[actions == "kept_max"]) - n_kept_max

  keep_desc <- if (survey_type == "rapid") {
    "the row with the strongest evidence of mouse activity"
  } else {
    sprintf("the row with the highest %s", value_col)
  }

  message(sprintf(
    "clean_deduplicate_surveys(%s): %d duplicate (paddock_id, survey_date) group(s) found - %d combined (same crop, %d row(s) merged into 1), %d resolved by keeping %s (%d row(s) dropped)",
    survey_type, length(groups), n_combined, n_merged, n_kept_max, keep_desc, n_dropped
  ))

  dplyr::bind_rows(unique_rows, resolved_data) |>
    dplyr::select(-dedup_id)
}

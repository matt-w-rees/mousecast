# Ensures every (survey_method, survey_night) combination used by a site is
# present at every time step in the combined model data. After binding the
# three per-type dataframes together with bind_rows(), a site that uses
# multiple survey methods (e.g. both chewcards and burrows) must have rows
# for each method x night combination at every time step so that mvgam
# receives equal-length series. Survey observation columns (n_success,
# n_trial, mice_prop) remain NA for time steps where no survey occurred;
# metadata and covariate columns are filled from the same site x time.
#
# A site "uses" a (survey_method, survey_night) combination if that
# combination appears at least once in the data for that site — i.e. it was
# actively monitored with that method (even if sparsely).
#
# @param data  Combined dataframe produced by dplyr::bind_rows() of all
#              three survey-type dataframes. Must contain columns:
#              state, region, farmer, site, subsite, longitude, latitude,
#              time, survey_method, survey_night, n_success, n_trial,
#              mice_prop.
#
# @return      The same dataframe with missing method x night x time rows
#              filled in (NA for observation columns; metadata/covariate
#              columns copied from the same site x time combination).
data_complete_survey_grid <- function(data,
                                      site_id_cols = c("state", "region", "farmer", "site", "subsite")) {

  # ---- Column classification -------------------------------------------

  # Expand the canonical site ID columns with coordinates so that spatial
  # attributes are part of the join key. intersect() guards against columns
  # absent in the data.
  site_cols <- intersect(c(site_id_cols, "longitude", "latitude"), names(data))

  # Columns that define which series a row belongs to within a site
  method_cols <- c("survey_method", "survey_night")

  # Observation columns: should remain NA when no survey occurred at that
  # time step, so they must NOT be copied from another row
  obs_cols <- intersect(
    c("n_success", "n_trial", "mice_prop",
      "session_start_date", "session_end_date",
      "crop_type", "crop_stage", "number_functional_traps"),
    names(data)
  )

  # All other columns are metadata or covariates. Covariate values are
  # derived from spatially explicit rasters, so they are the same for all
  # survey methods at a given site x time and can be safely copied to new rows.
  fill_cols <- setdiff(names(data), c(site_cols, "time", method_cols, obs_cols))

  n_before <- nrow(data)

  # ---- Build the complete grid -----------------------------------------

  # (a) Every (survey_method, survey_night) combination each site uses
  site_method_combos <- dplyr::distinct(data, across(all_of(c(site_cols, method_cols))))

  # (b) Every time step that is present for each site
  site_times <- dplyr::distinct(data, across(all_of(c(site_cols, "time"))))

  # (c) Cross-join: every site x time x (survey_method, survey_night).
  # relationship = "many-to-many" because each side has multiple rows per site.
  full_grid <- site_method_combos |>
    dplyr::left_join(site_times, by = site_cols, relationship = "many-to-many")

  # ---- Attach observation data -----------------------------------------

  join_cols <- c(site_cols, "time", method_cols)

  # Ensure uniqueness on join keys before joining (guards against upstream
  # duplicates producing a cartesian product in the result)
  obs_data <- data |>
    dplyr::select(all_of(c(join_cols, obs_cols))) |>
    dplyr::group_by(across(all_of(join_cols))) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  data_complete <- full_grid |>
    dplyr::left_join(obs_data, by = join_cols)

  # ---- Fill metadata / covariate columns for new rows ------------------

  # Build a reference table: one row per (site, time) with fill_cols values.
  # Used to populate newly added rows (which would otherwise have NA for
  # these columns after the grid join above).
  if (length(fill_cols) > 0) {
    site_time_ref <- data |>
      dplyr::select(all_of(c(site_cols, "time", fill_cols))) |>
      dplyr::group_by(across(all_of(c(site_cols, "time")))) |>
      dplyr::slice(1) |>
      dplyr::ungroup()

    data_complete <- data_complete |>
      dplyr::left_join(site_time_ref, by = c(site_cols, "time"))
  }

  # ---- Restore original column order -----------------------------------
  data_complete <- dplyr::select(data_complete, all_of(names(data)))

  # ---- Report ----------------------------------------------------------
  n_added <- nrow(data_complete) - n_before
  if (n_added > 0) {
    message(n_added, " rows added to complete survey method x night grid per site")
  } else {
    message("Survey method x night grid already complete (no rows added)")
  }

  return(data_complete)
}

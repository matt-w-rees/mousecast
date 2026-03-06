data_deduplicate_surveys_month <- function(data, survey_type, selection_method = c("random", "highest")) {
  
  # Match and validate selection method argument
  selection_method <- match.arg(selection_method)
  
  # Define grouping columns that identify unique survey sites
  group_cols <- intersect(c("region", "site", "subsite", "longitude", "latitude"), names(data))
  
  # Based on survey_type, compute total survey effort and total success for each row
  if (survey_type == "traps") {
    effort_cols <- c("number_functional_traps1", "number_functional_traps2", "number_functional_traps3")
    success_cols <- c("number_mice_caught1", "number_mice_caught2", "number_mice_caught3")
    
    # Check required effort and success columns exist in data
    if (!all(effort_cols %in% names(data))) stop("Missing trap effort columns")
    if (!all(success_cols %in% names(data))) stop("Missing trap success columns")
    
    # Sum effort and success columns row-wise, ignoring NAs
    data <- data %>%
      rowwise() %>%
      mutate(
        total_effort = sum(c_across(all_of(effort_cols)), na.rm = TRUE),
        total_success = sum(c_across(all_of(success_cols)), na.rm = TRUE)
      ) %>%
      ungroup()
    
  } else if (survey_type == "chewcards") {
    # Check required columns exist
    if (!("chewcards_deployed" %in% names(data))) stop("Missing chewcards_deployed column")
    if (!("chewcards_chewed" %in% names(data))) stop("Missing chewcards_chewed column")

    # Assign effort and success directly
    data <- data %>%
      mutate(
        total_effort = chewcards_deployed,
        total_success = chewcards_chewed
      )

  } else if (survey_type == "burrows") {
    # Check required columns exist
    if (!("burrow_transects_searched" %in% names(data))) stop("Missing burrow_transects_searched column")
    if (!("burrow_transects_present" %in% names(data))) stop("Missing burrow_transects_present column")

    # Assign effort and success directly
    data <- data %>%
      mutate(
        total_effort = burrow_transects_searched,
        total_success = burrow_transects_present
      )
    
  } else {
    stop("Unknown survey_type: ", survey_type)
  }
  
  # Record number of rows before deduplication
  n_before <- nrow(data)
  
  # Deduplicate surveys by grouping on site identifiers + month_year,
  # then selecting one row per group according to selection_method:
  if (selection_method == "random") {
    # Randomly select one survey row per group-season
    data_sampled <- data %>%
      group_by(across(all_of(c(group_cols, "month_year")))) %>%
      slice_sample(n = 1) %>%
      ungroup()
    
  } else if (selection_method == "highest") {
    # Select survey with highest total_effort first;
    # if ties, select one with highest total_success
    data_sampled <- data %>%
      group_by(across(all_of(c(group_cols, "month_year")))) %>%
      slice_max(order_by = total_effort, with_ties = TRUE) %>%
      slice_max(order_by = total_success, n = 1, with_ties = FALSE) %>%
      ungroup()
  }
  
  # Report how many duplicate rows were removed
  message(n_before - nrow(data_sampled), " duplicate survey rows removed")
  
  # remove added variables
  data_sampled <- dplyr::select(data_sampled, !(c(total_effort, total_success)))
  
  # Return deduplicated dataframe
  return(data_sampled)
}

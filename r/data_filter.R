data_remove_data <- function(data,
                        irrigated = FALSE,
                        state = NULL,
                        region = NULL,
                        site = NULL,
                        subsite = NULL,
                        crop_category = NULL,
                        trap_type = NULL,
                        remove_missing_crop_age = FALSE,
                        last_surveyed_before = NULL) {
  
  # Store initial row count for summary reporting
  n_before <- nrow(data)
  
  # Create a copy to avoid modifying the original data
  data_filtered <- data
  
  # ---- 1. Remove flood-irrigated subsites ----
  # These specific subsites use flood irrigation and should be excluded
  # when irrigated = TRUE
  if (isTRUE(irrigated)) {
    data_filtered <- data_filtered %>%
      dplyr::filter(!(subsite %in% c("comm farm", "dbela", "shane fras 1", "ian south 1")))
  }
  
  # ---- 2. Conditional filters based on user-specified criteria ----
  
  # Remove rows matching specified state(s)
  # Uses base R subsetting with $ to avoid column/parameter name conflicts
  if (!is.null(state)) {
    data_filtered <- data_filtered[!(data_filtered$state %in% state), ]
  }
  
  # Remove rows matching specified region(s)
  if (!is.null(region)) {
    data_filtered <- data_filtered[!(data_filtered$region %in% region), ]
  }
  
  # Remove rows matching specified site(s)
  if (!is.null(site)) {
    data_filtered <- data_filtered[!(data_filtered$site %in% site), ]
  }
  
  # Remove rows matching specified site(s)
  if (!is.null(subsite)) {
    data_filtered <- data_filtered[!(data_filtered$subsite %in% subsite), ]
  }
  
  # Remove rows matching specified trap type(s)
  # First checks if trap_type column exists in the data
  if (!is.null(trap_type) && "trap_type" %in% names(data_filtered)) {
    data_filtered <- data_filtered[!(data_filtered$trap_type %in% trap_type), ]
  }
  
  # Remove rows matching specified crop category/categories
  # Also removes rows where crop_category is NA
  # The | operator means "or", so this removes rows where crop_category matches
  # the specified values OR is missing
  if (!is.null(crop_category)) {
    data_filtered <- data_filtered[!(data_filtered$crop_category %in% crop_category | 
                                       is.na(data_filtered$crop_category)), ]
  }
  
  # ---- 3. Remove sites not surveyed since a given year ----
  # Removes entire sites where the most recent survey year is before the threshold
  if (!is.null(last_surveyed_before)) {
    data_filtered <- data_filtered |>
      dplyr::group_by(region, farmer, site, subsite) |>
      dplyr::filter(max(year) >= last_surveyed_before) |>
      dplyr::ungroup()
  }

  # ---- 4. Remove rows with missing crop age for specific crop types ----
  # For wheat, coarse grains, pulses, and oilseeds, crop_age should have a value
  # This filter removes rows where crop_age is NA for these specific crops
  # (indicating incomplete/invalid data), but keeps NA values for other crop types
  if (isTRUE(remove_missing_crop_age) && "crop_age" %in% names(data_filtered)) {
    data_filtered <- data_filtered[!(is.na(data_filtered$crop_age) & 
                                       data_filtered$crop_category %in% c("wheat", "coarse_grains", 
                                                                          "pulses", "oilseeds")), ]
  }
  
  # ---- 5. Print summary of filtering results ----
  # Calculate how many rows were removed and print informative message
  n_after <- nrow(data_filtered)
  n_removed <- n_before - n_after
  message(glue::glue("Filtered data: {n_removed} rows removed ({n_before} → {n_after})"))
  
  # Return the filtered dataframe
  return(data_filtered)
}
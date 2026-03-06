# Function to filter the data and report removals
remove_fenceline_sites_wierd_crops <- function(
    data,
    fence_subsites = c(
      "JW1StubFence", "JW2Edge", "JWAF1Crop", "JWAF2Crop",
      "JLAF1Scrub", "JWAF2Scrub", "JLBF2Crop",
      "GR2 FL 1 E-W", "GR2 FL 2 N-S", "TuckEastFL", "BTHB FL", "RK Murphy FL"
    ),
    fence_crop_types = c("fence_line", "pasture", "mown road verge (recent)", "unburned/unmown road verge"),
    rare_crops = c("corn", "cotton", "radish"),
    non_crop_types = c(
      "fence line", "mown road verge (recent)",
      "unburned/unmown road verge", "trees, shrubs & grass understorey"
    )
) {
  data <- data %>%
    mutate(
      crop_type = str_to_lower(crop_type),
      crop = str_to_lower(crop)
      # Note: subsite NOT lowercased here
    )
  
  removal_log <- tibble(step = character(), removed = integer())
  filter_step <- function(df, step_name, filter_expr) {
    before <- nrow(df)
    df <- df %>% filter({{ filter_expr }})
    removed <- before - nrow(df)
    removal_log <<- bind_rows(removal_log, tibble(step = step_name, removed = removed))
    df
  }
  
  # Crop surveys only filter combined
  data <- filter_step(
    data,
    "Crop surveys only filter",
    !(subsite %in% fence_subsites) &
      !(crop_type %in% str_to_lower(fence_crop_types))
  )
  
  # Remove rare crop types
  data <- filter_step(data, "Rare crop types", !(crop_type %in% rare_crops))
  
  # Remove non-crop/pasture crop types
  data <- filter_step(data, "Non-crop/pasture crop_type", !(crop_type %in% non_crop_types))
  
  # Remove subsites containing "scrub" or "fence"
  data <- filter_step(data, "Subsite contains 'scrub' or 'fence'",
                      !str_detect(str_to_lower(subsite), "scrub") & !str_detect(str_to_lower(subsite), "fence"))
  
  # Remove unknown crops
  data <- filter_step(data, "Unknown crops", crop != "unknown")
  
  # Remove crops missing stage info
  data <- filter_step(data, "Crops missing stage info", 
                      !crop %in% c("cereal", "legume", "oilseed"))
  
  print(removal_log)
  message("Total rows removed: ", sum(removal_log$removed),
          " (", round(sum(removal_log$removed) / (sum(removal_log$removed) + nrow(data)) * 100, 1), "%)")
  
  return(data)
}

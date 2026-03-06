get_current_and_next_season <- function(date = Sys.Date()) {
  
  # Extract month and year
  month <- as.numeric(format(date, "%m"))
  year <- as.numeric(format(date, "%Y"))
  
  # Determine current season and adjust year if needed
  current_season <- case_when(
    month %in% 12 ~ "Summer",
    month %in%  1:2 ~ "Summer",
    month %in%  3:5 ~ "Autumn",
    month %in%  6:8 ~ "Winter",
    month %in%  9:11 ~ "Spring"
  )
  
  # Adjust year for "Summer" (because Dec belongs to the next summer period)
  current_season_year <- ifelse(month == 12, year + 1, year)
  
  # Define season order
  seasons <- c("Summer", "Autumn", "Winter", "Spring")
  
  # Find index of current season
  current_index <- match(current_season, seasons)
  
  # Determine next season and year
  next_index <- ifelse(current_index == 4, 1, current_index + 1)
  next_season <- seasons[next_index]
  next_season_year <- ifelse(next_season == "Summer", current_season_year + 1, current_season_year)
  
  # Format results
  list(
    current_season = paste0(current_season, "-", current_season_year),
    next_season = paste0(next_season, "-", next_season_year)
  )
}
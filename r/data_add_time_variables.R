data_add_time_variables <- function(data) {
    
    season_levels <- c("Summer", "Autumn", "Winter", "Spring")
    
    data %>%
    
      # add month_year, midpoint, season, year_adj, month_year, season_year_adj columns 
      dplyr::mutate(
        
        # add year var
        year = year(session_start_date),

        # month_year based on session_start_date
        month_year = factor(
          paste0(month(session_start_date), "_", year(session_start_date)),
          levels = {
            yrs <- sort(unique(year(session_start_date)))
            mths <- 1:12
            expand.grid(mths, yrs) %>%
              arrange(Var2, Var1) %>%
              transmute(lvl = paste0(Var1, "_", Var2)) %>%
              pull(lvl)
          },
          ordered = TRUE),

        # midpoint date
        midpoint = session_start_date +
          (as.numeric(difftime(session_end_date, session_start_date, units = "days")) / 2),
        
          
        # raw season name
        season_chr = case_when(
          month(midpoint) %in% c(12, 1, 2) ~ "Summer",
          month(midpoint) %in% c(3, 4, 5)  ~ "Autumn",
          month(midpoint) %in% c(6, 7, 8)  ~ "Winter",
          month(midpoint) %in% c(9, 10, 11) ~ "Spring"
        ),
        
        # adjusted year so Dec belongs to next year
        year_adj = if_else(month(midpoint) == 12,
                           year(midpoint) + 1L,
                           year(midpoint)),
        
        # ordered factor for season
        season = factor(season_chr, levels = season_levels, ordered = TRUE),
        
        # combined season-year (ordered factor)
        season_year_adj = factor(
          paste0(season_chr, "-", year_adj),
          levels = {
            # build levels in chronological order
            yrs <- sort(unique(year_adj))
            expand.grid(season_levels, yrs) %>%
              arrange(Var2, match(Var1, season_levels)) %>%
              transmute(lvl = paste0(Var1, "-", Var2)) %>%
              pull(lvl)
          },
          ordered = TRUE
        )
      ) %>%
      
      # drop helper cols
      dplyr::select(-midpoint, -season_chr) 
}
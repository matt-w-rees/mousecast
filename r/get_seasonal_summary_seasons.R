# Return a character vector of all seasons that have at least one row of data,
# in chronological order as determined by the ordered factor levels in the data.
# Used by the pipeline to drive branching over seasonal summary reports.
#
# @param data Named list of survey data frames (traps, burrows, chewcards), each
#             containing a `season_year_adj` ordered factor column.
# @return Character vector of season labels, e.g. c("Autumn-2013", "Winter-2013", ...).
get_seasonal_summary_seasons <- function(data) {
  # factor levels encode the correct chronological order (shared across all survey types)
  all_factor_levels <- levels(data$traps$season_year_adj)

  # collect season labels that appear in at least one survey type
  seasons_with_data <- unique(c(
    as.character(na.omit(data$traps$season_year_adj)),
    as.character(na.omit(data$burrows$season_year_adj)),
    as.character(na.omit(data$chewcards$season_year_adj))
  ))

  # return in chronological order
  all_factor_levels[all_factor_levels %in% seasons_with_data]
}

# Season label and boundaries for the season a given date falls in.
#
# Matches attach_time_variables()'s season/year_adj convention exactly
# (r/a_attach_time_variables.R): Dec/Jan/Feb = Summer, Mar/Apr/May = Autumn,
# Jun/Jul/Aug = Winter, Sep/Oct/Nov = Spring; December belongs to the
# *following* year's Summer (year_adj), since Summer otherwise spans a
# calendar-year boundary.
#
# Returns a list:
#   label        e.g. "Winter 2026"
#   season       e.g. "Winter"
#   year_adj     e.g. 2026
#   season_start first day of the season
#   season_end   last day of the season
season_info <- function(date) {

  m <- lubridate::month(date)
  y <- lubridate::year(date)

  season <- dplyr::case_when(
    m %in% c(12, 1, 2)  ~ "Summer",
    m %in% c(3, 4, 5)   ~ "Autumn",
    m %in% c(6, 7, 8)   ~ "Winter",
    m %in% c(9, 10, 11) ~ "Spring"
  )
  year_adj <- if (m == 12) y + 1L else y

  season_start <- dplyr::case_when(
    m %in% 3:5   ~ as.Date(paste0(y, "-03-01")),
    m %in% 6:8   ~ as.Date(paste0(y, "-06-01")),
    m %in% 9:11  ~ as.Date(paste0(y, "-09-01")),
    m == 12      ~ as.Date(paste0(y, "-12-01")),
    TRUE         ~ as.Date(paste0(y - 1, "-12-01")) # Jan/Feb -> previous Dec
  )
  # season_start is always the 1st of a month, so plain + months() here can
  # never hit a month-end overflow (unlike %m+%, this doesn't need to handle
  # e.g. "Jan 31 + 1 month") -- always lands on a valid 1st of a month.
  season_end <- season_start + months(3) - 1

  list(
    label        = paste(season, year_adj),
    season       = season,
    year_adj     = year_adj,
    season_start = season_start,
    season_end   = season_end
  )
}

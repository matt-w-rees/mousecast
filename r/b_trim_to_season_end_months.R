# Restrict a monthly-grain SpatRaster to just its 4 season-end-month layers (Summer/Autumn/
# Winter/Spring's own last calendar month) -- used wherever a monthly covariate raster needs to be
# read once per season, at that season's own end month (attach_raster_covs()'s own
# period_col = "season_end_month_year" convention, r/b_attach_raster_covs.R), rather than carried
# downstream at full 12-month grain.
#
# Arguments:
#   rast               SpatRaster, one layer per month_year ("<month>_<year>", e.g. "7_2026")
#   season_end_months  integer vector of the 4 season-end calendar months (1-12) -- _targets.R's
#                      own season_end_months target (Summer/Autumn/Winter/Spring's own last month)
#
# Returns rast, subset to just the layers whose own calendar month is in season_end_months.

trim_to_season_end_months <- function(rast, season_end_months) {
  rast[[as.integer(sub("_.*", "", names(rast))) %in% season_end_months]]
}

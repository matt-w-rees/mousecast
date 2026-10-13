# Relabel a monthly raster's layers forward by a number of months, so that reading it at a given month returns
# the value from that many months earlier.
#
# Why: attach_survey_month_covs() (r/e_attach_survey_month_covs.R) reads rasters at each survey's own month;
# burrow detection's recent-rain terms want the month BEFORE the survey (the survey month itself is only partly
# over on the survey day). Shifting the labels forward by one lets the same function read the previous month.
#
# Arguments:
#   rast    SpatRaster, one layer per "<month>_<year>"
#   months  number of months to shift the labels forward (default 1)
#
# Returns rast with each layer renamed to the month `months` later.

shift_raster_months <- function(rast, months = 1) {
  # Each layer's month as a count of months since year 0, shifted, then back to "<month>_<year>".
  month_index <- as.integer(sub(".*_", "", names(rast))) * 12 + as.integer(sub("_.*", "", names(rast))) - 1 + months
  names(rast) <- paste0(month_index %% 12 + 1, "_", month_index %/% 12)
  rast
}

# Shared by all three ODK CSV readers (r/a_odk_read_submissions_ra_field.R,
# r/a_odk_read_submissions_ra_office.R, r/a_odk_read_submissions_traps.R) --
# drops submissions ODK Central's own review workflow has marked "rejected"
# (a reviewer has flagged the submission itself as bad, e.g. a duplicate or
# clearly bogus entry) before any further processing, so rejected data never
# reaches the rest of the pipeline. Applied to each reader's main submissions
# table right after it's read, before any repeat-group joins -- a rejected
# submission's own KEY then simply matches nothing in the burrow/chew/capture
# sub-tables, so its repeat-group rows disappear from the join along with it,
# with no separate filtering needed on those sub-tables themselves.
#
# A missing ReviewState column (an export from a form with the review
# workflow never enabled, or an older export predating this column) is
# treated as "nothing to drop", not an error. NA/blank ReviewState (the
# ODK Central default for a submission nobody has reviewed yet) is kept --
# only an explicit "rejected" is dropped.
.odk_filter_rejected_submissions <- function(data) {
  if (!"ReviewState" %in% names(data)) return(data)
  dplyr::filter(data, dplyr::coalesce(ReviewState, "") != "rejected")
}

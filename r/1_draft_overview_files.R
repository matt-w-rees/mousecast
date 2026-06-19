# Writes the markdown produced by generate_overview_text() to _overview.md
# in `dir`.
#
# _management.md is NOT (re)written here -- it's now a standalone,
# hand-maintained checklist; raw_data_update.qmd substitutes the current
# Moderate/High zone names into its "(list zones)" placeholder itself at
# render time (see the qmd's "management-text" chunk), so there's nothing
# data-driven left for this function to draft each cycle.
#
# raw_data_update.qmd includes _overview.md via {{< include >}} and, by
# default, does NOT call this function -- so any hand edits made by the
# National Mouse Group persist across renders. Call this function (or render
# the qmd with params$draft_overview = TRUE, which calls it for you) at the
# start of a new reporting cycle to overwrite _overview.md with a fresh
# draft generated from the current display_aez, ready for hand-editing.
#
# report_date is accepted only to match the call in raw_data_update.qmd's
# "draft_overview" chunk -- generate_overview_text() no longer needs it now
# that "## Overview" is no longer cropping-stage aware.
#
# Returns the file path (invisibly).
draft_overview_files <- function(display_aez, dir, report_date = Sys.Date()) {
  path <- file.path(dir, "_overview.md")
  writeLines(generate_overview_text(display_aez), path)
  invisible(path)
}

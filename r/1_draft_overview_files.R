# Writes the three markdown snippets produced by generate_overview_text()
# (in_short, in_detail, management) to standalone files in `dir`:
#   _overview_in_short.md, _overview_in_detail.md, _overview_management.md
#
# raw_data_update.qmd includes these files via {{< include >}} and, by
# default, does NOT call this function -- so any hand edits made by the
# National Mouse Group persist across renders. Call this function (or render
# the qmd with params$draft_overview = TRUE, which calls it for you) at the
# start of a new reporting cycle to overwrite the three files with a fresh
# draft generated from the current display_aez, ready for hand-editing.
#
# Returns the three file paths (invisibly).
draft_overview_files <- function(display_aez, dir, report_date = Sys.Date()) {
  overview_text <- generate_overview_text(display_aez, report_date = report_date)

  paths <- c(
    in_short   = file.path(dir, "_overview_in_short.md"),
    in_detail  = file.path(dir, "_overview_in_detail.md"),
    management = file.path(dir, "_overview_management.md")
  )

  writeLines(overview_text$in_short,   paths["in_short"])
  writeLines(overview_text$in_detail,  paths["in_detail"])
  writeLines(overview_text$management, paths["management"])

  invisible(paths)
}

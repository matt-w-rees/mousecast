# Builds the output path for this month's mouse update PDF inside `dir`,
# following the existing "Mouse Monitoring project Update #<N> <Mon> <Year>.pdf"
# naming convention (e.g. "Mouse Monitoring project Update #39 Mar 2026.pdf").
#
# If a PDF for the current month/year already exists in `dir`, its path is
# returned unchanged -- so re-running the target overwrites that file rather
# than incrementing the number again. Otherwise the new update number is one
# more than the highest "#<N>" found among existing files, so each new
# month's render starts a new numbered update and old months' PDFs are kept.
mouse_update_pdf_path <- function(dir, date = Sys.Date()) {
  month_label <- format(date, "%b %Y")
  existing    <- list.files(dir, pattern = "^Mouse Monitoring project Update #")

  this_month <- existing[grepl(paste0(" ", month_label, "\\.pdf$"), existing)]
  if (length(this_month) >= 1) {
    return(file.path(dir, this_month[1]))
  }

  numbers     <- as.integer(sub("^Mouse Monitoring project Update #([0-9]+).*", "\\1", existing))
  next_number <- max(numbers, na.rm = TRUE) + 1

  file.path(dir, sprintf("Mouse Monitoring project Update #%d %s.pdf", next_number, month_label))
}

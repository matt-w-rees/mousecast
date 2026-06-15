# Locates a Chrome or Chromium executable for mouse_update_pdf_from_html() to
# drive headlessly. Checks common macOS/Linux install locations plus PATH.
find_chrome <- function() {
  candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    Sys.which("google-chrome"),
    Sys.which("google-chrome-stable"),
    Sys.which("chromium"),
    Sys.which("chromium-browser")
  )
  chrome <- candidates[nchar(candidates) > 0 & file.exists(candidates)][1]
  if (is.na(chrome)) {
    stop(
      "No Chrome or Chromium executable found.\n",
      "Install Google Chrome (https://www.google.com/chrome/) and try again."
    )
  }
  chrome
}

# Converts a self-contained HTML report to PDF using Chrome headless, which
# applies the page's @media print CSS exactly as a browser's print dialog
# would -- used instead of wkhtmltopdf (discontinued) or pdflatex (which
# would need a separate LaTeX-based qmd and lose the HTML/CSS formatting).
#
# Returns `pdf_path`, suitable for use with format = "file" in a tar_target().
mouse_update_pdf_from_html <- function(html_path, pdf_path) {
  chrome <- find_chrome()

  html_abs <- normalizePath(html_path, mustWork = TRUE)
  pdf_abs  <- normalizePath(pdf_path,  mustWork = FALSE)

  # A scratch profile dir avoids "Multiple targets are not supported in
  # headless mode" when the user already has Chrome open with the default profile.
  profile_dir <- tempfile("chrome_profile_")
  on.exit(unlink(profile_dir, recursive = TRUE), add = TRUE)

  # Chrome headless writes the PDF and then can hang indefinitely instead of
  # exiting -- run via processx with a timeout and kill it once the file is
  # written rather than waiting on the process to exit on its own.
  processx::run(
    command = chrome,
    args    = c(
      "--headless",
      "--disable-gpu",
      "--no-sandbox",
      paste0("--user-data-dir=", profile_dir),
      "--no-pdf-header-footer",   # suppress Chrome's default date/title header and path/page footer
      paste0("--print-to-pdf=", pdf_abs),
      paste0("file://", html_abs)
    ),
    timeout          = 30,
    error_on_status  = FALSE
  )

  if (!file.exists(pdf_abs)) {
    stop("Chrome headless did not produce a PDF at: ", pdf_abs)
  }

  pdf_path
}

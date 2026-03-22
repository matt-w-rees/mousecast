# mouse_update_pdf_from_html.R
#
# Converts a self-contained HTML file to PDF using Chrome headless, which
# preserves all CSS styling and layout exactly as it appears in the browser.
# This is used instead of wkhtmltopdf (discontinued) or pdflatex (which
# requires a separate LaTeX-based QMD and loses HTML-specific formatting).


# ── Helper: locate Chrome or Chromium executable ─────────────────────────────

find_chrome <- function() {
  # Check common macOS and Linux install locations plus PATH
  candidates <- c(
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    Sys.which("google-chrome"),
    Sys.which("google-chrome-stable"),
    Sys.which("chromium"),
    Sys.which("chromium-browser")
  )
  # Keep only non-empty entries that actually exist on disk
  chrome <- candidates[nchar(candidates) > 0 & file.exists(candidates)][1]
  if (is.na(chrome)) {
    stop(
      "No Chrome or Chromium executable found.\n",
      "Install Google Chrome (https://www.google.com/chrome/) and try again."
    )
  }
  chrome
}


# ── Main function ─────────────────────────────────────────────────────────────

#' Convert a self-contained HTML file to PDF using Chrome headless.
#'
#' @param html_path  Path to the input HTML file (should be self-contained).
#' @param pdf_path   Path for the output PDF.  Defaults to the same location
#'                   as `html_path` but with a .pdf extension.
#' @return The `pdf_path` string, suitable for use with `format = "file"` in a
#'         `tar_target()`.
mouse_update_pdf_from_html <- function(html_path,
                                       pdf_path = sub("\\.html$", ".pdf", html_path)) {

  chrome   <- find_chrome()

  # Chrome requires absolute paths for both the input URL and output file
  html_abs <- normalizePath(html_path, mustWork = TRUE)
  pdf_abs  <- normalizePath(pdf_path,  mustWork = FALSE)

  # Run Chrome in headless mode and print the page to PDF.
  # --disable-gpu       avoids GPU-related crashes in headless mode on some systems.
  # --no-sandbox        needed in some CI/restricted environments.
  # --run-all-compositor-stages-before-draw  ensures the full page renders before capture.
  system2(
    command = chrome,
    args    = c(
      "--headless",
      "--disable-gpu",
      "--no-sandbox",
      "--run-all-compositor-stages-before-draw",
      "--no-pdf-header-footer",   # suppress Chrome's default date/title header and path/page footer
      paste0("--print-to-pdf=", pdf_abs),
      paste0("file://", html_abs)
    ),
    stdout = FALSE,
    stderr = FALSE
  )

  if (!file.exists(pdf_abs)) {
    stop("Chrome headless did not produce a PDF at: ", pdf_abs)
  }

  pdf_path
}

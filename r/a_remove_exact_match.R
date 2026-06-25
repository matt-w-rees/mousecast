# Remove rows from `data` where `column` exactly matches one of `values`
# (e.g. a hardcoded list of fenceline/pasture subsite names to exclude).
# str_squish() is applied to both sides so stray leading/trailing/double
# internal whitespace in either the raw data or the hardcoded list can't
# silently break a match (case sensitivity is not handled here -- it relies
# on the caller having already lowercased both). Warns if any value in
# `values` matched zero rows, since that usually means the hardcoded name has
# drifted from the data (typo, renamed site, etc.) and the intended row
# removal silently did nothing.
remove_exact_match <- function(data, column, values) {
  if (is.null(values)) return(data)

  col_clean    <- stringr::str_squish(data[[column]])
  values_clean <- stringr::str_squish(values)

  unmatched <- setdiff(values_clean, col_clean)
  if (length(unmatched) > 0) {
    warning(sprintf(
      "remove_exact_match(): %d %s value(s) not found in the data, so had no effect: %s",
      length(unmatched), column, paste(unmatched, collapse = ", ")
    ))
  }

  data[!(col_clean %in% values_clean), ]
}

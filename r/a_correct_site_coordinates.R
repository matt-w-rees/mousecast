# Replace the recorded coordinates of specific survey sites with corrected ones, from a hand-maintained
# table (raw_data/survey_data/site_coordinate_corrections.csv), before surveys are matched to paddocks.
#
# Some historic sites carry one coordinate for every visit that isn't in the surveyed paddock -- e.g. a
# gate or roadside position in the gap between paddocks, which match_surveys_to_paddocks()
# (r/a_match_surveys_to_paddocks.R) then snaps into whichever paddock edge is nearest, not necessarily the
# right one. Where a later, more precise record of the same site exists (e.g. ODK GPS), the table moves the
# old coordinate there.
#
# A row only changes records matching all of its data_source, site_name and old coordinates (to 6 decimal
# places), so a correction can't spill onto another source or site that happens to share a name or a
# coordinate. An optional subsite_name narrows it to one subsite -- for grids that were all entered at one
# site coordinate (e.g. Trifel's east and west grids); left blank, it applies to every subsite. Each
# correction that matches nothing is reported, since it usually means the source data or the table has
# changed.
#
# Arguments:
#   data_list    named list of survey data frames (traps, rapid, observations), each with longitude and
#                latitude; data_source and site_name are used where present
#   corrections  data frame with data_source, site_name, subsite_name (blank for any), old_longitude,
#                old_latitude, new_longitude, new_latitude (and an optional reason, not used here)
#
# Returns data_list with the matching rows' coordinates replaced.

correct_site_coordinates <- function(data_list, corrections) {
  # Matching keys: coordinates rounded to 6 decimal places (about 0.1 m), so float noise can't stop a match.
  # Site-wide corrections (blank subsite) key on the site; subsite corrections on the site plus subsite.
  corrections <- corrections |>
    dplyr::mutate(subsite_name = dplyr::na_if(as.character(subsite_name), ""),
                  site_key = paste(data_source, site_name, round(old_longitude, 6), round(old_latitude, 6)),
                  key = ifelse(is.na(subsite_name), site_key, paste(site_key, subsite_name)))
  n_matched <- stats::setNames(integer(nrow(corrections)), corrections$key)

  corrected <- purrr::map(data_list, function(data) {
    # Sources without site names (e.g. MouseAlert observations) can't match a correction.
    if (!all(c("data_source", "site_name") %in% names(data))) return(data)
    site_key <- paste(data$data_source, data$site_name, round(data$longitude, 6), round(data$latitude, 6))
    # A subsite correction takes precedence over a site-wide one for the same site.
    subsite <- if ("subsite_name" %in% names(data)) data$subsite_name else NA_character_
    row <- match(paste(site_key, subsite), corrections$key)
    row[is.na(row)] <- match(site_key[is.na(row)], corrections$key)
    hit <- !is.na(row)
    data$longitude[hit] <- corrections$new_longitude[row[hit]]
    data$latitude[hit] <- corrections$new_latitude[row[hit]]
    # Tally each correction's matches across every data frame (<<- updates the counter outside this function).
    counts <- table(corrections$key[row[hit]])
    n_matched[names(counts)] <<- n_matched[names(counts)] + as.integer(counts)
    data
  })

  for (i in seq_len(nrow(corrections))) {
    message(sprintf("correct_site_coordinates(): %s / %s%s -- %d record(s) moved", corrections$data_source[i],
                    corrections$site_name[i], ifelse(is.na(corrections$subsite_name[i]), "", paste0(" / ", corrections$subsite_name[i])),
                    n_matched[i]))
  }
  if (any(n_matched == 0)) {
    warning("correct_site_coordinates(): ", sum(n_matched == 0), " correction(s) matched no records -- check the table against the source data.")
  }
  corrected
}

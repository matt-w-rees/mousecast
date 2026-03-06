data_alter_subsites <- function(data) {

  # Rename specific subsites where the farmer moved to an adjacent paddock that
  # should be treated as the same monitoring location
  data_renamed <- data |>
    mutate(

      site = case_when(
       farmer == "brett tucker" ~ "brett tucker",
       subsite %in% c("jla tg", "jlb tg") ~ "paul lush m",
       site == "watts west pingally" ~ "watts west pingelly",
       site == "tfawcett2" & subsite == "tfawcett1" ~ "tfawcett1",
       TRUE ~ site
      ),

      subsite = case_when(
        # jwc crop is an alternative for either a or b depending on survey period;
        # they are very close so it is fine to assume the same site
        subsite == "jwc crop" & session_start_date < ymd("2015-01-01") ~ "jwa tgcrop",
        subsite == "jwc crop" & session_start_date >= ymd("2015-01-01") ~ "jwb tgcrop",
        # Wimmera Kevin Munroe - farmer switched to a neighbour's paddock across
        # the road at end of lease; close enough to treat as same location
        subsite == "kmun" ~ "kmun2",
        TRUE ~ subsite
      )
    )

  # For rows where site or subsite was renamed, replace the coordinates with the
  # canonical coordinates of the target site/subsite (derived from rows that were
  # not renamed). This prevents GPS coordinates from the original paddock
  # propagating into the renamed site's coordinate record.
  changed <- (data$site != data_renamed$site | data$subsite != data_renamed$subsite)
  changed[is.na(changed)] <- FALSE

  if (any(changed)) {
    # Canonical coordinates: mean of all unchanged rows per site/subsite
    coord_lookup <- data_renamed[!changed, ] |>
      dplyr::distinct(site, subsite, longitude, latitude) |>
      dplyr::group_by(site, subsite) |>
      dplyr::summarise(
        longitude = mean(longitude, na.rm = TRUE),
        latitude  = mean(latitude,  na.rm = TRUE),
        .groups = "drop"
      )

    # Overwrite coordinates for renamed rows by joining to the lookup.
    # coalesce() keeps original coordinates where no canonical match exists
    # (e.g. a site-grouping rename where each subsite has its own unique coords).
    new_coords <- data_renamed[changed, ] |>
      dplyr::left_join(
        dplyr::rename(coord_lookup, lon_new = longitude, lat_new = latitude),
        by = c("site", "subsite")
      ) |>
      dplyr::mutate(
        longitude = dplyr::coalesce(lon_new, longitude),
        latitude  = dplyr::coalesce(lat_new, latitude)
      ) |>
      dplyr::select(longitude, latitude)

    data_renamed[changed, c("longitude", "latitude")] <- new_coords
  }

  data_renamed

}

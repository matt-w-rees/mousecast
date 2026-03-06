data_fix_coords <- function(data) {

  # Check for multiple coordinates per region/site/subsite
  coord_check <- data |>
    distinct(region, site, subsite, longitude, latitude) |>
    group_by(region, site, subsite) |>
    mutate(n_coords = n()) |>
    filter(n_coords > 1) |>
    ungroup()

  if (nrow(coord_check) == 0) {
    message("✅ No conflicting coordinates found.")
    return(data)
  }

  message("⚠️  Multiple coordinates found for the following region-site-subsite combinations:\n")
  print(coord_check |> arrange(region, site, subsite))

  # Build a single canonical coordinate per region/site/subsite by averaging.
  # Keyed on region + site + subsite to prevent cross-site name clashes.
  coords_canonical <- data |>
    group_by(region, site, subsite) |>
    summarise(
      longitude = mean(longitude, na.rm = TRUE),
      latitude  = mean(latitude,  na.rm = TRUE),
      .groups = "drop"
    )

  # Replace coordinates with canonical values (1-to-1 join, no row duplication)
  data |>
    select(-longitude, -latitude) |>
    left_join(coords_canonical, by = c("region", "site", "subsite"))

}

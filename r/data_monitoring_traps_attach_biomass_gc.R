# Add biomass and ground cover information from rapid assessment (chewcard) data into trapping data.
# The chewcard_data argument uses the CSV-format column names (site_name, date_out)
# which are renamed on-the-fly to match the trap data columns for the join.

data_monitoring_traps_attach_biomass_gc <- function(data, chewcard_data){

  # rename chewcard columns to match trap data for joining
  # site = farmer name in the monitoring project (same as site in trap data)
  chewcard_subset <- chewcard_data |>
    dplyr::transmute(region, farmer, site = farmer, longitude, latitude,
                     date = date_out, biomass, ground_cover_percent)

  # add biomass information from chewcard data (chewcards are done more often than burrows) into trapping data
  left_join(data, chewcard_subset,
            by = c("region", "farmer", "site", "longitude", "latitude", "date"))
}

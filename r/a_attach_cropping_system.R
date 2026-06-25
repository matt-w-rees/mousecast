# Classify each paddock's GRDC agro-ecological zone (ae_zone) into a broad
# cropping system:
#   "single" — winter-only, one crop per paddock per year (most of southern
#              Australia + all of WA)
#   "dual"   — summer + winter, paddocks north of ~Dubbo (northern NSW + Qld)
#              can carry either a winter or a summer crop
#
# Derived from aez_zone_info()'s region_label grouping (r/a_aez_zone_info.R) --
# the same grouping flag_crop_stage_anomalies() (r/a_flag_crop_stage_anomalies.R)
# uses to decide which crop-calendar(s) apply to a row.
#
# WA Ord (irrigated scheme, no fixed rain-fed calendar) and paddocks with no
# ae_zone are left NA.
#
# Call on paddocks_sf after attach_aez(), alongside attach_grdc_subregion()/
# attach_soil_type()/attach_state() -- a plain lookup on the already-attached
# ae_zone column, not a spatial join, so it just needs a mutate.
attach_cropping_system <- function(data) {

  dual_zones <- aez_zone_info() |>
    dplyr::filter(region_label %in% c(
      "Northern NSW & southern Queensland", "Central & northern Queensland"
    )) |>
    dplyr::pull(ae_zone)

  dplyr::mutate(
    data,
    cropping_system = dplyr::case_when(
      is.na(ae_zone) | ae_zone == "WA Ord" ~ NA_character_,
      ae_zone %in% dual_zones ~ "dual",
      TRUE ~ "single"
    )
  )
}

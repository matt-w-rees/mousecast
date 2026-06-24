# Joins a character vector into natural-language prose: "A", "A and B", or
# "A, B, and C". Used to combine AE zone / region-group names in
# generate_overview_text(). Unlike list_names() in raw_data_update.qmd, no
# case conversion is applied -- ae_zone and region_label values are already
# formatted for display (see aez_zone_info()).
join_names <- function(x) {
  x <- unique(as.character(x))
  if (length(x) == 1) return(x)
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

# Builds one "+ [<region_label>]{.underline}: ..." paragraph for a region,
# stating which of its zones (if any) currently show High / Moderate / Low
# activity. A plain statement of fact only -- no management actions (those
# live in _management.md, a standalone hand-maintained file -- see
# generate_overview_text()) and no trend/anecdotal commentary (that's added
# by hand by the National Mouse Group). High-activity zones are called out in
# red, matching the rest of the report's colour convention (see cat_colours
# in raw_data_update.qmd).
region_overview_paragraph <- function(region_label, zone_rows) {
  high <- dplyr::filter(zone_rows, activity_category == "High")
  mod  <- dplyr::filter(zone_rows, activity_category == "Moderate")
  low  <- dplyr::filter(zone_rows, activity_category == "Low")

  sentences <- c(
    if (nrow(high) > 0) paste0(
      "[**High mouse activity has been recorded in ", join_names(high$ae_zone), ".**]{style=\"color: red;\"}"
    ),
    if (nrow(mod) > 0) paste0(
      "Moderate mouse activity has been recorded in ", join_names(mod$ae_zone), "."
    ),
    if (nrow(low) > 0) paste0(
      "Mouse abundance in ", join_names(low$ae_zone), " appears to be low."
    )
  )

  paste0("+ [", region_label, "]{.underline}: ", paste(sentences, collapse = " "))
}

# Generates draft markdown for raw_data_update.qmd's "## Overview" section
# from display_aez (one row per ever-surveyed AE zone, with
# activity_category -- see compute_aez_summary()): one paragraph per region
# (aez_zone_info()'s region_label) that has at least one Moderate/High zone
# -- High-activity regions first -- followed by a closing "Elsewhere ..."
# paragraph naming any remaining regions that are entirely Low. AE zones not
# present in aez_zone_info() are dropped with no warning -- aez_zone_info()
# should be kept in sync with aez_adj.
#
# Management actions are deliberately NOT generated here -- _management.md
# is now a standalone, hand-maintained checklist; raw_data_update.qmd
# substitutes the current Moderate/High zone names into its "(list zones)"
# placeholder itself at render time (see the qmd's "management-text" chunk).
#
# This is a *draft*: it captures which zones currently need a mention, but
# trend/anecdotal nuance (rising/falling activity, specific town names,
# rapid-assessment reports) isn't in display_aez and should still be added by
# the National Mouse Group before publishing.
generate_overview_text <- function(display_aez) {
  zone_info <- aez_zone_info()

  zones <- display_aez |>
    dplyr::left_join(zone_info, by = "ae_zone") |>
    dplyr::filter(!is.na(region_label))

  flagged <- dplyr::filter(zones, activity_category %in% c("High", "Moderate"))

  if (nrow(flagged) == 0) {
    return("+ No GRDC agro-ecological zones currently show moderate or high mouse activity: mouse numbers likely remain low across all monitored regions.")
  }

  ## Regions with at least one Moderate/High zone, High-containing regions
  ## first (within each severity, alphabetical) -- distinct() keeps each
  ## region's first occurrence, i.e. its highest severity.
  region_order <- flagged |>
    dplyr::mutate(activity_category = factor(activity_category, levels = c("High", "Moderate"))) |>
    dplyr::arrange(activity_category, region_label) |>
    dplyr::distinct(region_label) |>
    dplyr::pull(region_label)

  ## All zones (any activity_category) belonging to a flagged region, so a
  ## region's Low zones are mentioned alongside its Moderate/High ones in the
  ## same paragraph (see region_overview_paragraph()).
  zones_flagged <- dplyr::filter(zones, region_label %in% region_order)
  by_region <- split(zones_flagged, zones_flagged$region_label)
  region_lines <- vapply(
    region_order,
    function(r) region_overview_paragraph(r, by_region[[r]]),
    character(1), USE.NAMES = FALSE
  )

  ## Regions with no Moderate/High zone at all, but with at least one Low
  ## (i.e. surveyed and not "No data") zone, get folded into one closing
  ## "Elsewhere ..." paragraph.
  elsewhere_labels <- zones |>
    dplyr::filter(activity_category == "Low", !region_label %in% region_order) |>
    dplyr::distinct(region_label) |>
    dplyr::pull(region_label)

  elsewhere_line <- if (length(elsewhere_labels) > 0) {
    paste0("+ Elsewhere (", join_names(elsewhere_labels), "), mouse numbers likely remain low.")
  } else {
    character(0)
  }

  paste(c(region_lines, elsewhere_line), collapse = "\n\n")
}

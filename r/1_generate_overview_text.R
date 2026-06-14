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

# Classifies a month (1-12) into a broad cropping-calendar stage, used to
# pick the management action that applies to a Moderate/High AE zone right
# now (see action_label() / detail_sentence() / generate_overview_text()):
#   - "sowing":  Mar-May. Autumn sowing of the winter crop (all systems --
#                see aez_zone_info()). For "dual" cropping zones (northern
#                NSW toward the Qld border, and Qld), this window is *also*
#                when the summer crop is harvested.
#   - "in_crop": Jun-Sep. The winter crop is in the ground and maturing
#                toward grain fill -- the last window where zinc phosphide
#                bait remains effective.
#   - "harvest": Oct-Feb. Winter crop harvest (all systems). For "dual"
#                zones, this window is *also* when the summer crop is sown.
#
# Western Australia ("wa" cropping system) runs a similar single winter-crop
# cycle to "winter_only" zones but tends to start harvest a couple of weeks
# earlier and can finish sowing a little later after a dry start -- not
# enough to warrant separate month boundaries here, but worth bearing in mind
# when reviewing generated text close to these transitions.
cropping_stage <- function(month) {
  dplyr::case_when(
    month %in% 3:5 ~ "sowing",
    month %in% 6:9 ~ "in_crop",
    TRUE           ~ "harvest"
  )
}

# Maps an AE zone's activity_category ("Low"/"Moderate"/"High"/"No data" --
# see classify_with_nodata() in raw_data_update.qmd) and the current
# cropping_stage() to the short, ALL-CAPS "In short" action label used in
# generate_overview_text(). "Low"/"No data" zones return NA (they're
# summarised collectively, not individually -- see generate_overview_text()).
# "dual" cropping zones (see aez_zone_info()) get an extra sowing/harvest
# clause folded in for the "harvest" stage, since both the winter and summer
# crop cycles overlap in that window.
action_label <- function(activity_category, cropping_system, stage) {
  is_dual <- cropping_system == "dual"
  dplyr::case_when(
    activity_category == "High"     & stage == "sowing"  ~ "PREPARE TO ACT AT SOWING",
    activity_category == "Moderate" & stage == "sowing"  ~ "WATCH AND ACT AT SOWING",
    activity_category == "High"     & stage == "in_crop" ~ "ACT BEFORE CROP MATURES",
    activity_category == "Moderate" & stage == "in_crop" ~ "WATCH AND PREPARE TO ACT",
    activity_category == "High"     & stage == "harvest" & is_dual  ~ "HARVEST CLEANLY & ACT AT SOWING",
    activity_category == "High"     & stage == "harvest"            ~ "ACT NOW & HARVEST CLEANLY",
    activity_category == "Moderate" & stage == "harvest" & is_dual  ~ "HARVEST CLEANLY & WATCH AT SOWING",
    activity_category == "Moderate" & stage == "harvest"            ~ "MONITOR & HARVEST CLEANLY",
    TRUE ~ NA_character_
  )
}

# Builds one "In detail" sentence (the part after "<region>: ") for a region
# with at least one Moderate/High AE zone. zone_names is the already-joined
# (join_names()) list of AE zone names with Moderate/High activity in this
# region; activity_category/stage/cropping_system come from those rows (one
# region may need two sentences if it has both a High and a Moderate zone --
# generate_overview_text() groups by activity_category so each call here is
# for a single severity level).
detail_sentence <- function(activity_category, stage, cropping_system, zone_names) {
  is_dual <- cropping_system == "dual"
  focus <- dplyr::case_when(
    stage == "sowing"  & activity_category == "High"             ~ "consider baiting before or at sowing to protect the freshly sown crop",
    stage == "sowing"                                             ~ "there is some potential for an outbreak depending on conditions over the coming weeks — monitor closely in the lead-up to sowing",
    stage == "in_crop" & activity_category == "High"             ~ "with crops maturing, consider baiting before grain fill, as zinc phosphide becomes less effective once grain develops on the head",
    stage == "in_crop"                                            ~ "continue monitoring as crops mature, and be ready to act before grain fill if activity increases",
    stage == "harvest" & is_dual  & activity_category == "High"  ~ "harvest the maturing crop as cleanly as possible and be prepared to bait at sowing of the next crop",
    stage == "harvest" & is_dual                                  ~ "harvest cleanly and monitor closely ahead of the next sowing window",
    stage == "harvest" & activity_category == "High"             ~ "harvest as cleanly as possible to minimise carryover food for mice, and consider baiting before harvest if damage is evident",
    stage == "harvest"                                            ~ "harvest as cleanly as possible to limit food carryover into next season",
    TRUE ~ "monitor closely and act if activity increases"
  )
  sprintf("%s mouse activity has been recorded in %s; %s.", activity_category, zone_names, focus)
}

# Builds the "## Management recomendations" numbered list (markdown) for the
# current cropping_stage(). any_flagged is FALSE when no AE zone currently
# shows Moderate/High activity, in which case a short monitoring-only list is
# returned instead. has_dual adds an extra point for "dual" cropping zones
# (northern NSW toward the Qld border, and Qld -- see aez_zone_info()), where
# the sowing/harvest window of one crop overlaps the other crop's cycle.
#
# The "sowing" wording mirrors Mouse Monitoring project Update #39 (Mar
# 2026); "in_crop"/"harvest" wording is adapted from Updates #34/#37 (Aug
# 2024/2025) and #29/#32/#35 (Nov 2022/2023/2024) respectively.
management_text <- function(stage, has_dual, any_flagged) {
  if (!any_flagged) {
    return(paste(
      "1. Continue to [monitor mouse activity](https://grdc.com.au/resources-and-publications/resources/mouse-management/monitor-manage/know-your-mouse-numbers) by walking through paddocks and searching for active burrows (chew cards could also be used). Mouse numbers are currently low across the monitored network, but activity can increase quickly when conditions are favourable.",
      "2. [Report mouse sightings (and a lack of sightings)](https://www.feralscan.org.au/mousealert/) via MouseAlert so other growers can see what is being observed in their local area.",
      sep = "\n\n"
    ))
  }

  base <- switch(stage,
    sowing = paste(
      "1. Monitor mouse activity by walking through paddocks and [searching for active burrows (chew cards could also be used)](https://grdc.com.au/resources-and-publications/resources/mouse-management/monitor-manage/know-your-mouse-numbers). Two or three active burrows per 100 square metres are cause for concern prior to sowing.",
      "2. Where you have identified high mouse activity, [consider applying mouse bait](https://grdc.com.au/resources-and-publications/resources/mouse-management/monitor-manage/mouse-baiting) at seeding to prevent damage to the freshly sown crop. Monitor mouse activity after application to assess the effectiveness of baiting. ZnP25 is the only currently registered bait available; use in accordance with the label instructions and report any adverse effects (including a lack of efficacy) via the [APVMA website](https://www.apvma.gov.au/regulation/adverse-experience-reporting-program).",
      "3. Reduce available mouse food (e.g. spilt grain) where possible. High rates of background food directly reduce the effectiveness of poison baiting, as mice are less likely to consume the baits.",
      sep = "\n\n"
    ),
    in_crop = paste(
      "1. Monitor crops for signs of mouse activity using [chew cards or a walk-through of crops](https://grdc.com.au/resources-and-publications/resources/mouse-management/monitor-manage/know-your-mouse-numbers).",
      "2. Where mouse damage is evident in maturing crops, [apply zinc phosphide bait](https://grdc.com.au/resources-and-publications/resources/mouse-management/monitor-manage/mouse-baiting) before grain fill — once grain has developed on the head, mice are less likely to take ZnP bait. ZnP25 is the only currently registered bait available; use in accordance with the label instructions and report any adverse effects via the [APVMA website](https://www.apvma.gov.au/regulation/adverse-experience-reporting-program).",
      "3. Plan for a clean harvest (minimal grain loss) to reduce mouse food carryover into next season, and control weeds and grasses along fence lines and crop margins before seed set.",
      sep = "\n\n"
    ),
    harvest = paste(
      "1. [Harvest as cleanly as possible](https://grdc.com.au/resources-and-publications/resources/mouse-management/monitor-manage/know-your-mouse-numbers) to minimise grain loss — spilt grain provides high-quality food that can sustain mouse breeding over summer and lead to higher numbers at the next sowing.",
      "2. Monitor mouse activity through the stubble phase using chew cards or active burrow counts; any paddocks with activity now could develop into a problem at the next sowing.",
      "3. Where mouse damage is evident before harvest, [consider applying mouse bait](https://grdc.com.au/resources-and-publications/resources/mouse-management/monitor-manage/mouse-baiting). ZnP25 is the only currently registered bait available; use in accordance with the label instructions and report any adverse effects via the [APVMA website](https://www.apvma.gov.au/regulation/adverse-experience-reporting-program).",
      sep = "\n\n"
    )
  )

  extra <- if (has_dual) {
    switch(stage,
      sowing  = "4. In northern dual-cropping zones, this window is also the summer-crop harvest — harvest as cleanly as possible to avoid carrying mouse food (and mice) into the newly sown winter crop.",
      in_crop = "4. In northern dual-cropping zones, watch for mice moving into newly sown summer crops as the winter crop matures — damage to summer-crop seedlings can occur even while the winter crop remains the main focus.",
      harvest = "4. In northern dual-cropping zones, this window is also the sowing of the summer crop — be prepared to bait at sowing if mice remain active after harvest of the winter crop."
    )
  }

  paste(c(base, extra), collapse = "\n\n")
}

# Generates draft markdown for raw_data_update.qmd's "## Overview" and
# "## Management recomendations" sections from display_aez (one row per
# ever-surveyed AE zone, with activity_category -- see compute_aez_summary())
# and the current date.
#
# Returns a list of two markdown strings:
#   - overview:   "In short" bullet list (one line per (region, action,
#                 severity) combination among Moderate/High AE zones, High
#                 shown in red, matching cat_colours) followed by an "In
#                 detail" paragraph per region with Moderate/High activity,
#                 describing the activity level and the cropping-stage-
#                 appropriate management focus (see detail_sentence()), plus
#                 a closing "Elsewhere ..." paragraph for regions that are
#                 otherwise Low.
#   - management: numbered list for "## Management recomendations" (see
#                 management_text()).
#
# report_date drives cropping_stage() (defaults to Sys.Date(), i.e. the
# render date -- the same "now" used for the dynamic subtitle in the
# set_subtitle chunk). AE zones not present in aez_zone_info() are dropped
# with no warning -- aez_zone_info() should be kept in sync with aez_adj.
#
# This is a *draft*: it captures the data-driven pattern (region, season,
# abundance -> recommended action) seen across past Mouse Updates, but
# zone-level nuance that isn't in display_aez (e.g. specific town names,
# rising/falling trends, anecdotal rapid-assessment reports) should still be
# reviewed and added by the National Mouse Group before publishing.
generate_overview_text <- function(display_aez, report_date = Sys.Date()) {
  zone_info <- aez_zone_info()
  stage <- cropping_stage(as.integer(format(as.Date(report_date), "%m")))

  zones <- display_aez |>
    dplyr::left_join(zone_info, by = "ae_zone") |>
    dplyr::filter(!is.na(region_label)) |>
    dplyr::mutate(action = action_label(activity_category, cropping_system, stage))

  flagged <- zones |> dplyr::filter(!is.na(action))

  ## ---- "In short" ----------------------------------------------------------
  if (nrow(flagged) == 0) {
    in_short <- "+ No GRDC agro-ecological zones currently show moderate or high mouse activity: **WATCH**"
  } else {
    short_rows <- flagged |>
      dplyr::mutate(activity_category = factor(activity_category, levels = c("High", "Moderate"))) |>
      dplyr::arrange(activity_category, region_label, ae_zone) |>
      dplyr::group_by(region_label, state, action, activity_category) |>
      dplyr::summarise(zone_names = join_names(ae_zone), .groups = "drop") |>
      # group_by()/summarise() re-sorts rows by the grouping keys
      # (region_label first), undoing the arrange() above -- re-arrange by
      # severity so High-activity regions come before Moderate ones.
      dplyr::arrange(activity_category, region_label) |>
      dplyr::mutate(
        line = paste0(zone_names, " (", state, "): **", action, "**"),
        # The "+ " bullet marker must sit outside the {style=...} span for
        # pandoc to recognise the line as a list item (cf. the High-activity
        # bullet in the original Mar 2026 prose this replaces).
        line = dplyr::if_else(activity_category == "High",
                               paste0("[", line, "]{style=\"color: red;\"}"), line),
        line = paste0("+ ", line)
      )
    in_short <- paste(short_rows$line, collapse = "\n")
  }

  ## ---- "In detail" ----------------------------------------------------------
  if (nrow(flagged) == 0) {
    in_detail <- paste(
      "Mouse numbers are low across all monitored GRDC agro-ecological zones.",
      "Continue routine monitoring — there is always a chance of isolated patches of higher activity, particularly where background food (e.g. spilt grain) is plentiful."
    )
  } else {
    detail_rows <- flagged |>
      dplyr::mutate(activity_category = factor(activity_category, levels = c("High", "Moderate"))) |>
      dplyr::arrange(activity_category, region_label, ae_zone) |>
      dplyr::group_by(region_label, cropping_system, activity_category) |>
      dplyr::summarise(zone_names = join_names(ae_zone), .groups = "drop") |>
      # group_by()/summarise() re-sorts rows by the grouping keys
      # (region_label first), undoing the arrange() above -- re-arrange by
      # severity so High-activity regions come before Moderate ones.
      dplyr::arrange(activity_category, region_label) |>
      dplyr::mutate(
        line = paste0(
          "+ [", region_label, "]{.underline}: ",
          detail_sentence(as.character(activity_category), stage, cropping_system, zone_names)
        )
      )

    ## Regions with no Moderate/High zone at all, but with at least one Low
    ## (i.e. surveyed and not "No data") zone, get folded into one closing
    ## "Elsewhere ..." paragraph.
    elsewhere_labels <- zones |>
      dplyr::filter(activity_category == "Low", !region_label %in% flagged$region_label) |>
      dplyr::distinct(region_label) |>
      dplyr::pull(region_label)

    elsewhere_line <- if (length(elsewhere_labels) > 0) {
      paste0("+ Elsewhere (", join_names(elsewhere_labels), "), mouse numbers likely remain low.")
    } else {
      character(0)
    }

    in_detail <- paste(c(detail_rows$line, elsewhere_line), collapse = "\n\n")
  }

  ## ---- Management recommendations --------------------------------------------
  management <- management_text(stage, any(flagged$cropping_system == "dual"), nrow(flagged) > 0)

  ## Combine "In short" and "In detail" into a single block -- both are part
  ## of "## Overview" and are written to one file by draft_overview_files().
  overview <- paste(in_short, in_detail, sep = "\n\n")

  list(overview = overview, management = management)
}

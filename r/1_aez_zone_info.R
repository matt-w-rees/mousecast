# Static lookup mapping each GRDC agro-ecological zone (ae_zone, as it
# appears in aez_adj / display_aez) to:
#   - state:           state(s) the zone sits in, for "(WA)"-style suffixes
#                       in generate_overview_text()'s "In short" bullets.
#   - cropping_system: the broad cropping calendar that governs sowing/
#                       harvest timing for this zone -- one of:
#       "wa":           Western Australian grain belt. A single winter-crop
#                       cycle, similar to "winter_only" but tends to start
#                       harvest a little earlier and can finish sowing a
#                       little later in a dry start.
#       "winter_only":  single winter-crop cycle, sown autumn (Mar-May) and
#                       harvested late spring/summer (Oct-Feb) -- Tas, Vic,
#                       SA, and NSW roughly south of Dubbo.
#       "dual":         winter crop (as above) PLUS a summer crop sown
#                       Oct-Dec and harvested Mar-May -- northern NSW toward
#                       the Qld border, and Qld.
#   - region_label:    broad region grouping used to combine zones into
#                       "In detail" paragraphs in generate_overview_text().
#
# Used by generate_overview_text() (r/generate_overview_text.R). Includes all
# 18 zones in aez_adj, not just the 11 currently in aez_surveyed, so the
# lookup keeps working as the monitoring network expands.
aez_zone_info <- function() {
  tibble::tribble(
    ~ae_zone,                       ~state,      ~cropping_system, ~region_label,
    "WA Northern",                  "WA",        "wa",           "Western Australia",
    "WA Central",                   "WA",        "wa",           "Western Australia",
    "WA Sandplain",                 "WA",        "wa",           "Western Australia",
    "WA Eastern",                   "WA",        "wa",           "Western Australia",
    "WA Mallee",                    "WA",        "wa",           "Western Australia",
    "WA Ord",                        "WA",        "dual",         "Ord River (WA)",
    "SA Midnorth-Lower Yorke Eyre", "SA",        "winter_only",  "Adelaide Plains, Yorke & Eyre Peninsulas (SA)",
    "SA Vic Bordertown-Wimmera",    "SA & Vic",  "winter_only",  "Mallee & Wimmera (Vic/SA)",
    "SA Vic Mallee",                "SA & Vic",  "winter_only",  "Mallee & Wimmera (Vic/SA)",
    "Vic High Rainfall",            "Vic",       "winter_only",  "Victoria & southern/central NSW",
    "NSW Vic Slopes",               "NSW & Vic", "winter_only",  "Victoria & southern/central NSW",
    "NSW Central",                  "NSW",       "winter_only",  "Victoria & southern/central NSW",
    "NSW NE Qld SE",                "NSW & Qld", "dual",         "Northern NSW & southern Queensland",
    "NSW NW Qld SW",                "NSW & Qld", "dual",         "Northern NSW & southern Queensland",
    "Qld Central",                  "Qld",       "dual",         "Central & northern Queensland",
    "Qld Burdekin",                 "Qld",       "dual",         "Central & northern Queensland",
    "Qld Atherton",                 "Qld",       "dual",         "Central & northern Queensland",
    "Tas Grain",                    "Tas",       "winter_only",  "Tasmania"
  )
}

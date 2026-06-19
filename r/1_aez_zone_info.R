# Static lookup mapping each GRDC agro-ecological zone (ae_zone, as it
# appears in aez_adj / display_aez) to:
#   - state:        state(s) the zone sits in.
#   - region_label: broad region grouping used to combine zones into one
#                    paragraph per region in generate_overview_text().
#
# Used by generate_overview_text() (r/1_generate_overview_text.R). Includes
# all 18 zones in aez_adj, not just the 11 currently in aez_surveyed, so the
# lookup keeps working as the monitoring network expands.
aez_zone_info <- function() {
  tibble::tribble(
    ~ae_zone,                       ~state,      ~region_label,
    "WA Northern",                  "WA",        "Western Australia",
    "WA Central",                   "WA",        "Western Australia",
    "WA Sandplain",                 "WA",        "Western Australia",
    "WA Eastern",                   "WA",        "Western Australia",
    "WA Mallee",                    "WA",        "Western Australia",
    "WA Ord",                       "WA",        "Ord River (WA)",
    "SA Midnorth-Lower Yorke Eyre", "SA",        "Adelaide Plains, Yorke & Eyre Peninsulas (SA)",
    "SA Vic Bordertown-Wimmera",    "SA & Vic",  "Mallee & Wimmera (Vic/SA)",
    "SA Vic Mallee",                "SA & Vic",  "Mallee & Wimmera (Vic/SA)",
    "Vic High Rainfall",            "Vic",       "Victoria & southern/central NSW",
    "NSW Vic Slopes",               "NSW & Vic", "Victoria & southern/central NSW",
    "NSW Central",                  "NSW",       "Victoria & southern/central NSW",
    "NSW NE Qld SE",                "NSW & Qld", "Northern NSW & southern Queensland",
    "NSW NW Qld SW",                "NSW & Qld", "Northern NSW & southern Queensland",
    "Qld Central",                  "Qld",       "Central & northern Queensland",
    "Qld Burdekin",                 "Qld",       "Central & northern Queensland",
    "Qld Atherton",                 "Qld",       "Central & northern Queensland",
    "Tas Grain",                    "Tas",       "Tasmania"
  )
}

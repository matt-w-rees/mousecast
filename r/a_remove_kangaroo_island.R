# Remove Kangaroo Island's own disjoint polygon piece from the AE zone shapefile's
# "SA Midnorth-Lower Yorke Eyre" zone.
#
# Confirmed live (2026-09): Kangaroo Island is a genuinely separate island within that zone's
# own MULTIPOLYGON (not merged with the mainland Yorke Peninsula/mid-north SA coastline) --
# st_cast(..., "POLYGON") on that one zone's geometry splits cleanly into 5 disjoint pieces, one
# of which (~4.4 degrees^2, bbox 136.5-138.1 lon / -36.1 to -35.6 lat) matches the island's own
# real extent exactly. Zero real survey paddocks exist there (checked against hmm_data), so any
# model prediction reaching the island is a pure, ungrounded extrapolation from the mainland
# zone's own pooled coefficients -- the same underlying concern already documented for the
# far-west SA coast (rain_whiplash_trough_to_peak:ae_zoneSA Vic Mallee, see CLAUDE.md and
# quarto_reports/plague_model_summary.qmd's own Known Limitations), just found for a second zone
# via a different route (a known offshore island, not a data-driven coefficient audit).
#
# Two smaller offshore islets also showed up as their own disjoint pieces of the same zone during
# this investigation (~0.04 and ~0.02 degrees^2, likely Thistle Island and Wardang Island) --
# left untouched here since only Kangaroo Island was asked for; the same technique (a reference
# point + st_intersects() against st_cast()'s own pieces) would extend to them if ever wanted.
#
# Identifies the island piece by a reference point (its own approximate centroid, 137.2, -35.8)
# rather than a hardcoded polygon INDEX or bounding box -- st_cast()'s own piece order isn't
# guaranteed stable across re-reads of the source file, and a reference point is more robust to
# genuinely small differences in how the source shapefile happens to be re-vectorised than an
# exact bbox match would be.
#
# Arguments:
#   aez_adj   sf polygon object with an ae_zone column (_targets.R), native CRS already WGS 84
#
# Returns aez_adj with Kangaroo Island's own polygon piece removed from "SA Midnorth-Lower
# Yorke Eyre"'s geometry -- every other zone, and the rest of that zone's own mainland geometry,
# is untouched (confirmed live: row count, other zones' own geometry, and column set are all
# unchanged; only that one zone's own area shrinks, by exactly the island's own area).

remove_kangaroo_island <- function(aez_adj) {

  zone_name <- "SA Midnorth-Lower Yorke Eyre"
  ki_point <- sf::st_sfc(sf::st_point(c(137.2, -35.8)), crs = sf::st_crs(aez_adj))

  zone_idx <- which(aez_adj$ae_zone == zone_name)
  if (length(zone_idx) != 1) {
    stop("remove_kangaroo_island(): expected exactly one '", zone_name, "' row, found ", length(zone_idx))
  }

  # Split that one zone's own MULTIPOLYGON into its disjoint pieces, find the one piece the
  # island reference point falls inside, then recombine everything else back into one
  # MULTIPOLYGON -- the rest of the zone (mainland, plus the two smaller islets above) is
  # untouched by this, only the matched piece is dropped.
  pieces <- sf::st_cast(sf::st_geometry(aez_adj)[zone_idx], "POLYGON", warn = FALSE)
  ki_piece <- which(lengths(sf::st_intersects(pieces, ki_point)) > 0)
  if (length(ki_piece) != 1) {
    stop("remove_kangaroo_island(): expected exactly one polygon piece containing the Kangaroo Island reference point, found ", length(ki_piece))
  }

  sf::st_geometry(aez_adj)[zone_idx] <- sf::st_combine(pieces[-ki_piece])

  aez_adj
}

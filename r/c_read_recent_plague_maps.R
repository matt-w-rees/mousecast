# Reads and combines the hand-digitised recent plague-occurrence maps
# (raw_data/plague_occurrence/recent_plague_maps/) into a single sf object,
# one polygon feature per reported area of mouse activity.
#
# Layout: one gpkg per surveyed season, both the gpkg and its containing
# folder named "<Season>_<YYYY>" (e.g. "Spring_2021/Spring_2021.gpkg"),
# matching this project's raw_data folder-naming convention. Some seasons
# were originally digitised as more than one gpkg (e.g. a separate WA/East
# extent) -- those have already been merged into a single gpkg per season
# (2026-09), so this function itself no longer needs to group by folder, just
# read each gpkg and combine.
#
# Folder/file year follows this project's own year_adj convention (r/a_season_info.R), not the
# raw calendar year -- matters only for Summer, since a Summer season spans a calendar-year
# boundary (December belongs to the FOLLOWING year's Summer): "Summer_2018" means Dec 2017 + Jan/Feb
# 2018, i.e. year_adj = 2018, the same convention every other target in this pipeline already uses
# for season_year_adj. Every other season (Autumn/Winter/Spring) doesn't cross a year boundary, so
# their own folder year is unambiguous either way.
#
# Summer_2018/Summer_2018.gpkg deliberately duplicates Autumn_2018's own polygon content byte-for-
# byte (2026-09, confirmed with the project owner directly, not a leftover/copy-paste error despite
# looking exactly like one) -- there is no independent Summer 2018 source map, but Spring 2017 and
# Autumn 2018 both show confirmed plague activity, so the intervening, otherwise-undocumented Summer
# 2018 season is assumed to be a continuation of the same outbreak rather than left unmapped. Do NOT
# "clean this up" by deleting it as a duplicate -- it's the intended anchor for that season, matching
# this project's own established view (CLAUDE.md's "Positive anchor persistence" paragraph, in the
# superseded (a)+(b) rule) that plagues typically outlast a single season.
#
# Summer_2021/Summer_2021.gpkg is the same kind of deliberate bridge (2026-09, flagged live by
# warn_recent_plague_maps_gaps.R, then confirmed with the project owner as a genuine oversight, not
# left unmapped): geometry duplicated byte-for-byte from Autumn_2021's own 9 features, but severity
# downgraded to "moderate" across all of them (Autumn_2021 itself has a 6 moderate / 3 severe mix) --
# unlike Summer_2018's exact duplicate, this reflects lower confidence that a season with no
# independent source map of its own was genuinely at the SAME severity as the confirmed season either
# side of it, not just that mouse activity was present at all. Same "do NOT clean up" warning applies.
#
# Each feature carries a "severity" attribute (minor/moderate/severe,
# case-inconsistent in the raw files -- some gpkgs use "Severity", lowercased
# and coalesced here). "season"/"year" are parsed back out of the file's own
# "<Season>_<YYYY>" label, and "season_year" is rebuilt as "<Season> <YYYY>"
# (a space, not an underscore) to match this project's season_year_adj/label
# convention elsewhere (e.g. r/a_season_info.R's `paste(season, year_adj)`
# -> "Winter 2026"), ordered chronologically (Southern Hemisphere season
# order within each year) rather than the alphabetical order a plain factor
# would default to.
#
# Every file's own geometry column is renamed to "geometry" and cast to
# MULTIPOLYGON individually, BEFORE combining -- both steps matter and both
# were confirmed live as real, silent data-loss traps, not defensive
# boilerplate:
#   - geometry column NAME: sf::st_write()'s own GPKG default geometry
#     column name is "geom", not "geometry" (the name QGIS itself uses, which
#     every hand-digitised file here has) -- the two seasons merged from a
#     split WA/East gpkg (2026-09) picked up "geom" purely from going through
#     st_write() once, and dplyr::bind_rows() treats a differently-named
#     geometry column as a completely separate column rather than matching it
#     up, silently emptying that file's own rows' geometry in the combined
#     result. No warning or error anywhere in the chain flags this --
#     st_is_valid() even reports TRUE, since an empty geometry is trivially
#     valid -- caught only by directly checking st_area()/st_bbox() against a
#     real spatial join downstream. Both raw files have since been rewritten
#     with an explicit GEOMETRY_NAME="geometry" layer option so this
#     shouldn't recur, but this function renames per file anyway rather than
#     trusting every future digitised file to use the same column name.
#   - geometry TYPE: files are a mix of POLYGON and MULTIPOLYGON. Casting to
#     a common type AFTER combining (rather than per file, before) hit the
#     identical silent-data-loss shape: combining mismatched sfc types first
#     left some features mislabelled GEOMETRYCOLLECTION EMPTY, and casting a
#     GEOMETRYCOLLECTION straight to MULTIPOLYGON does not error, it silently
#     returns an empty geometry (confirmed live). Casting each file to
#     MULTIPOLYGON individually, before combining, avoids the ambiguous
#     mixed-type combination step entirely.
#
# This is a finer-grained (paddock-scale polygon, not state/subregion) spatial
# complement to yearly_plague_occurrence.csv / regional_mouse_activity_ratings.csv
# -- REPLACES both as r/c_anchor_plague_state.R's plague-state anchor source
# (2026-09); see that function's own header for the anchoring rule itself.
#
# Arguments:
#   files  every path under raw_data/plague_occurrence/recent_plague_maps/
#          (the whole folder, tracked as one tar_file() target in _targets.R
#          -- see CLAUDE.md's convention for a folder-of-related-files raw
#          source); filtered here to just the "*.gpkg" files it actually needs.
#
# Returns an sf object (MULTIPOLYGON, WGS84), one row per digitised feature:
#   season_year  ordered factor, "<Season> <YYYY>", chronological
#   season, year the same, split into their own columns
#   severity     "minor"/"moderate"/"severe", lowercased
#   geometry     the digitised extent

read_recent_plague_maps <- function(files) {

  gpkg_files <- files[grepl("\\.gpkg$", files, ignore.case = TRUE)]

  season_labels <- tools::file_path_sans_ext(basename(gpkg_files))

  maps_combined <- gpkg_files |>
    rlang::set_names(season_labels) |>
    purrr::map(function(f) {
      x <- sf::st_read(f, quiet = TRUE)
      names(x)[names(x) == attr(x, "sf_column")] <- "geometry"
      sf::st_geometry(x) <- "geometry"
      sf::st_cast(x, "MULTIPOLYGON")
    }) |>
    dplyr::bind_rows(.id = "season_year") |>
    dplyr::mutate(severity = tolower(dplyr::if_else(is.na(severity), Severity, severity))) |>
    dplyr::select(-dplyr::any_of("Severity"))

  season_order <- c(Summer = 1, Autumn = 2, Winter = 3, Spring = 4)

  maps_combined |>
    dplyr::mutate(
      season      = stringr::str_extract(season_year, "^[A-Za-z]+"),
      year        = as.integer(stringr::str_extract(season_year, "[0-9]{4}")),
      season_year = paste(season, year)
    ) |>
    dplyr::mutate(season_year = forcats::fct_reorder(season_year, year * 10 + season_order[season]))
}

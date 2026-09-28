# Flags a season with no recent_plague_maps polygon of its own that sits directly between two
# MAPPED seasons exactly one season apart whose own extents overlap spatially -- exactly the
# situation Summer 2018 (r/c_read_recent_plague_maps.R's header) was, a season with no independent
# source map at all, sandwiched between Spring 2017 and Autumn 2018's own overlapping extents, and
# deliberately bridged by hand rather than left unanchored. Surfaced automatically here, as its own
# pipeline step (CLAUDE.md's function-per-task convention, same reasoning as
# r/b_warn_gpp_focal_coverage_gaps.R), so every new season folder added over time (the stated plan
# going forward) gets the same review rather than silently staying unanchored.
#
# Deliberately restricted to a SINGLE missing season between two mapped seasons exactly one season
# apart -- not any longer unmapped stretch bounded by two mapped, overlapping seasons, even though
# that's a superficially similar shape. A single in-between season plausibly reflects one
# continuous, still-unfolding outbreak simply not mapped that one time; a multi-season/multi-year
# gap is a fundamentally weaker signal, and blanket-assuming continuity across a long unobserved
# stretch is the exact failure mode this project's own anchoring history already hit and reverted
# (CLAUDE.md's "State-level fallback for the pre-coverage gap" -- a positive-only continuity
# assumption over a long gap produced a near-absorbing-state decode collapse). This check
# deliberately doesn't repeat that mistake at the raw-data level: e.g. Autumn 2018 and Autumn 2021
# (11 seasons apart) also spatially overlap, but that's not treated as "an in-between season" the
# way Summer 2018 is -- confirmed live while building this function, and the reason the check is
# scoped to gap length 1, not "any gap bounded by overlapping seasons".
#
# "Same area" is read as genuine spatial overlap between the two bordering seasons' own combined
# extents (st_intersects() on each season's union), not merely "a map exists that season somewhere
# in the country" -- confirmed live against the real Summer 2018 case this is modelled on: Spring
# 2017's and Autumn 2018's own extents do genuinely overlap (0m apart), so a plain intersects test
# (no artificial buffer) already catches the motivating case.
#
# message(), not warning() or an error -- this is a prompt for a human decision (dig up a real
# source map for that season, or deliberately bridge it the way Summer 2018 was, or confirm the
# gap is genuine and leave it), not a pipeline-halting data-quality bug.
#
# Arguments:
#   recent_plague_maps r/c_read_recent_plague_maps.R's output
#
# Returns (invisibly) a character vector of flagged "<Season> <YYYY>" labels (empty if none) --
# also message()s them, naming each gap's own bordering mapped seasons, when any are found.

warn_recent_plague_maps_gaps <- function(recent_plague_maps) {

  season_rank <- c(Summer = 0L, Autumn = 1L, Winter = 2L, Spring = 3L)

  maps_with_quarter <- recent_plague_maps |>
    dplyr::mutate(quarter = year * 4L + season_rank[season])

  mapped_quarters <- sort(unique(maps_with_quarter$quarter))

  quarter_label <- function(q) {
    season <- names(season_rank)[season_rank == q %% 4L]
    year   <- (q - q %% 4L) / 4L
    paste(season, year)
  }

  quarter_union <- function(q) {
    sf::st_union(sf::st_geometry(dplyr::filter(maps_with_quarter, quarter == q)))
  }

  flagged <- character(0)

  # only consecutive PAIRS of mapped seasons exactly one season apart (a single missing season in
  # between) count as "an in-between season" -- see header for why a longer unmapped stretch isn't
  # treated the same way even when it's also bounded by two overlapping mapped seasons.
  for (i in seq_len(length(mapped_quarters) - 1)) {
    before_q <- mapped_quarters[i]
    after_q  <- mapped_quarters[i + 1]
    if (after_q - before_q != 2L) next

    gap_q    <- before_q + 1L
    overlaps <- length(sf::st_intersects(quarter_union(before_q), quarter_union(after_q))[[1]]) > 0
    if (overlaps) {
      flagged <- c(flagged, sprintf(
        "%s (between %s and %s)", quarter_label(gap_q), quarter_label(before_q), quarter_label(after_q)
      ))
    }
  }

  if (length(flagged) > 0) {
    message(
      "warn_recent_plague_maps_gaps(): ", length(flagged), " season(s) have no recent_plague_maps polygon ",
      "of their own, sandwiched directly between two mapped seasons whose own extents spatially overlap -- ",
      "worth reviewing whether a real source map exists to add, or a deliberate bridging polygon (like ",
      "Summer 2018, r/c_read_recent_plague_maps.R) is warranted: ", paste(flagged, collapse = "; ")
    )
  }

  invisible(flagged)
}

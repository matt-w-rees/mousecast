# Saves a faceted map of every recent_plague_maps polygon, coloured by severity, laid out as a
# grid -- one row per year_adj, one column per season (Summer/Autumn/Winter/Spring, chronological
# left-to-right) -- as a visual companion to warn_recent_plague_maps_gaps.R's own text-only
# message() -- the pipeline's own version of the ad-hoc plotting code recent_plague_maps.R (project
# root) already used interactively. Regenerated every time recent_plague_maps itself changes (e.g. a
# new season folder added, per the stated plan to keep doing this as plagues happen), so it stays a
# current, at-a-glance reference without needing to remember to re-run that script by hand.
#
# facet_grid(rows = year, cols = season), not facet_wrap(~season_year) -- a grid keeps every
# season in the SAME column across every row, so scanning down a column reads as "how did this one
# season look across every mapped year", and a year with only 1-2 mapped seasons still gets its own
# full row with the other columns rendered empty (just the Australia outline, no fill) rather than
# reflowing -- an empty panel is itself informative (a season that year without a map), not a gap to
# hide. season is re-levelled to Summer/Autumn/Winter/Spring here (not read off season_year, which
# is built for chronological SORTING as one combined label, not for use as its own facet variable).
#
# Arguments:
#   recent_plague_maps r/c_read_recent_plague_maps.R's output
#   aus_shp             national outline sf object, for spatial context
#   output_path         full path to write the PNG to
#
# Returns output_path, invisibly (tar_file()-compatible).

plot_recent_plague_maps <- function(recent_plague_maps, aus_shp, output_path) {

  bb             <- sf::st_bbox(recent_plague_maps)
  aus_shp_coarse <- sf::st_simplify(aus_shp, dTolerance = 10000)

  plot_data <- recent_plague_maps |>
    dplyr::mutate(season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring")))

  # one row per distinct mapped year -- height scales with that, not with the (now fixed at 4) column count.
  n_years     <- dplyr::n_distinct(plot_data$year)
  plot_height <- n_years * 2.6 + 1

  p <- ggplot2::ggplot(plot_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = severity), color = "black", linewidth = 0.2) +
    ggplot2::geom_sf(data = aus_shp_coarse, fill = NA, colour = "grey60", linewidth = 0.3) +
    ggplot2::scale_fill_viridis_d(name = "Severity") +
    ggplot2::facet_grid(rows = ggplot2::vars(year), cols = ggplot2::vars(season)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::coord_sf(xlim = c(bb["xmin"], bb["xmax"]), ylim = c(bb["ymin"], bb["ymax"]))

  ggplot2::ggsave(output_path, p, width = 11, height = plot_height, limitsize = FALSE)

  invisible(output_path)
}

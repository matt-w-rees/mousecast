# Weights for spreading surveyed paddocks' evidence to target locations (grid cells, or other paddocks
# when scoring): a Gaussian kernel in distance, exp(-d^2 / (2 * bandwidth_km^2)), set to zero where the
# paddock's state isn't one the target's zone names -- cropping and survey programmes differ across
# borders, while a cross-border zone (e.g. "NSW NE Qld SE") shares with every state it names.
#
# Arguments:
#   target_x, target_y        target longitudes and latitudes
#   target_zone               target AE zone names
#   source_x, source_y        paddock longitudes and latitudes
#   source_state              paddock states (aus_state, e.g. "QLD")
#   bandwidth_km              kernel scale (km)
#
# Returns a target x paddock matrix of weights.

spread_weights <- function(target_x, target_y, target_zone, source_x, source_y, source_state, bandwidth_km) {
  # Distances in km, equirectangular -- accurate enough at these scales.
  km_per_degree <- 111.32
  dx <- outer(target_x, source_x, "-") * km_per_degree * cos(target_y * pi / 180)
  dy <- outer(target_y, source_y, "-") * km_per_degree
  weights <- exp(-(dx^2 + dy^2) / (2 * bandwidth_km^2))

  # The states each target's zone names, upper-cased to match aus_state ("Qld" -> "QLD").
  zone_states <- strsplit(toupper(as.character(target_zone)), " ")
  same_state <- t(vapply(zone_states, function(z) source_state %in% z, logical(length(source_state))))
  weights * same_state
}

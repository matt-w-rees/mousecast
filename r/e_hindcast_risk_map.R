# The forecast map from a grid hindcast (hindcast_plague_occurrence(), r/e_hindcast_plague_occurrence.R):
# onset risk at its latest season, as a raster on the covariate grid. Used for the 3-state model, whose
# forecast -- Pr(plague next season | not plague now) -- weights each non-plague state's entry risk by how
# likely a cell is to be in it now, which needs the state distribution the hindcast carries forward from
# season to season (predict_plague_risk() maps a single transition probability instead).
#
# Arguments:
#   plague_hindcast  hindcast_plague_occurrence()'s output
#   template         a single-layer SpatRaster on the covariate grid the hindcast was built on (its cell
#                    column indexes this grid)
#
# Returns a single-layer SpatRaster of onset risk at the hindcast's latest period, named by that period.
hindcast_risk_map <- function(plague_hindcast, template) {
  template <- template[[1]]
  # The latest period, in "<month>_<year>" form.
  period_key <- as.integer(sub(".*_", "", plague_hindcast$period)) * 100 + as.integer(sub("_.*", "", plague_hindcast$period))
  latest <- plague_hindcast[period_key == max(period_key), ]
  # Onset at each of its cells, NA elsewhere.
  risk <- terra::setValues(template, NA_real_)
  risk[latest$cell] <- latest$onset
  names(risk) <- latest$period[1]
  risk
}

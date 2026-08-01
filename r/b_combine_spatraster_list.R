# Combine a list of SpatRasters into one continuous stack, preserving each
# element's own layer names.
#
# terra::rast(list_of_spatrasters) combines correctly where do.call(c,
# list_of_spatrasters) does not -- confirmed live this session that do.call(c,
# .) on a many-element list of SpatRasters silently returns a plain list
# instead of dispatching terra's own c.SpatRaster method (no error, no
# warning -- this went undetected until a downstream layer count came out
# wrong). But terra::rast() has its own gotcha: when the input list itself
# has element names (e.g. a gathered dynamic branch target, whose list
# elements are named after each branch's own target hash), it uses THOSE
# names (plus a per-layer index suffix) to construct new layer names,
# discarding every input raster's own meaningful names (e.g.
# summarise_silo_to_month()'s "<month>_<year>" convention) in the process --
# confirmed live directly against min_temp_raster_block's own 10 branches.
# This restores each element's own names explicitly afterward, sidestepping
# that behaviour regardless of whether rast_list itself happens to be named.
#
# Arguments:
#   rast_list   list of SpatRasters (e.g. a gathered dynamic branch target,
#               optionally combined with c() alongside plain, non-branched
#               SpatRasters -- see _targets.R's monthly_gpp_rast/
#               monthly_gpp_rast_frozen for both cases)
#
# Returns one SpatRaster: every input layer concatenated in list order, with
# each element's own original layer names restored.

combine_spatraster_list <- function(rast_list) {
  combined <- terra::rast(rast_list)
  names(combined) <- unlist(lapply(rast_list, names))
  combined
}

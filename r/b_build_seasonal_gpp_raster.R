# Build a seasonal (Summer/Autumn/Winter/Spring) mean Gross Primary
# Productivity (GPP) raster from 8-day Gpp_500m composites, spanning two
# spliced satellite products (downloaded via r/b_download_gpp.R into
# raw_data/predictor_variables/gpp/{modis,viirs}/): MODIS (MOD17A2HGF.061,
# 2000-01-01 to 2011-12-31) for the historical record and VIIRS
# (VNP17A2GF.002, 2012-01-01+) for recent/ongoing composites -- a
# well-established, NASA-endorsed continuous record (VIIRS's GPP algorithm
# is the designed operational continuation of MODIS's; confirmed live via
# appeears::rs_products()/rs_layers() that both products share identical
# layer names, fill-value codes, scale factor and 8-day cadence).
#
# Kept as its own raster-processing step -- decode composites, mask fill
# codes, aggregate to period means -- so it's its own cached target: it only
# needs to re-run when the raw GPP files change, not every time some
# downstream consumer's own inputs change. Mirrors this project's existing
# raster-builder/extractor split (see r_not_in_use/covariate_processing/
# cov_summarise_silo_to_season.R + extract_lagged_seasonal_raster_values.R).
# Only summarise_by = "month" is currently wired into _targets.R (feeding the
# coarse GPP/rolling-average pipeline, this pipeline's only remaining use of
# GPP) -- "season" was originally built for a native-resolution seasonal
# gpp_mean attach (attach_seasonal_gpp(), r_not_in_use/b_attach_seasonal_gpp.R,
# retired once the rolling average became GPP's sole covariate) and is kept
# as a general capability, not currently called.
#
# A period (season or month, see summarise_by) is only summarised if it has
# at least `min_composites_per_period` composites, so an in-progress period
# isn't reported on a fraction of the data (a full season has ~11 8-day
# composites; a full month has ~3-4). Each output layer is named to match
# attach_time_variables()'s own convention (r/a_attach_time_variables.R)
# exactly -- "Season-Year" (e.g. "Winter-2026") for summarise_by = "season",
# "Month_Year" (e.g. "7_2026") for "month" -- so a monthly or seasonal GPP
# covariate can be left_join()'d/attach_raster_covs()'d straight onto survey
# or paddock data by that same column. Each layer carries its composite count
# as a "n_composites" layer metag (see terra::metags()), read back off by
# whichever consumer wants it (retired attach_seasonal_gpp() did; the current
# "month" pathway doesn't, but the tag costs nothing to keep populated).
#
# gpp_files spans two products (MODIS and VIIRS, each downloaded as a single
# combined national AppEEARS task, see _targets.R) whose requested date
# ranges are non-overlapping by construction (MODIS's end_date sits one day
# before VIIRS's start_date), so for any given date there's only ever one
# file to use, not two competing satellites' independent estimates needing
# merged. The per-date merge/extend below is kept anyway as a defensive
# no-op for the single-file case (length(rasters) == 1), in case a date is
# ever split across more than one file again -- confirmed live this session
# this isn't just theoretical: gf_files_reference_year (_targets.R) can
# genuinely hand two files for the same boundary date (one from each of two
# adjacent yearly VIIRS block folders, e.g. a Dec 31-anchored 8-day
# composite downloaded independently by both the just-frozen and the
# still-open block's own requests). Reduce(terra::merge, rasters), not
# do.call(terra::merge, rasters) -- confirmed live this session that
# do.call() with 2+ SpatRasters can silently mis-dispatch to
# base::merge.default() instead of terra's own S4 method when called from
# inside a nested closure (purrr::map()'s own anonymous function here),
# raising "argument 'x' is missing" instead of merging; Reduce() calls
# terra::merge() pairwise with two explicit positional arguments each time,
# sidestepping that dispatch failure, and (unlike do.call()) is also
# correct for genuinely merging 3+ same-date files, since terra::merge()'s
# own signature takes exactly two SpatRasters (x, y), not an arbitrary
# number.
#
# Arguments:
#   gpp_files                  file paths under raw_data/predictor_variables/gpp
#                               (the whole tracked folder(s), across both
#                               modis/ and viirs/ -- Gpp_500m_*.tif composites
#                               are picked out by filename; the Psn_QC_500m
#                               and supporting_files/ files are ignored)
#   aus_shp                    sf polygon object of the Australian coastline
#                               (the pipeline's aus_shp target), used to mask
#                               every off-shore cell to NA -- see note below on
#                               why valid_max alone doesn't catch the sea
#   summarise_by               "season" (default) or "month" -- which period
#                               8-day composites are collapsed into
#   min_composites_per_period  minimum 8-day composites required to summarise
#                               a period; defaults to 8 for summarise_by =
#                               "season" (a full season has ~11) or 3 for
#                               "month" (a full month has ~3-4) if left NULL
#   valid_max                  GPP values (kg C/m^2/8-day) at or above this are
#                               masked to NA before averaging (default 3). The
#                               VNP17A2/MOD17 GPP layer reserves scaled values
#                               >= 3.2761 (raw int16 code >= 32761) as sentinel
#                               codes -- fill, perennial water/salt, urban,
#                               missing input, not-calculated -- not real GPP;
#                               confirmed against this dataset, where every
#                               composite's true domain-wide max is ~0.1-0.3.
#                               This does NOT reliably mask ocean/water -- an
#                               earlier whole-raster investigation (see
#                               r/b_compare_viirs_gpp_products.R's header)
#                               found ocean pixels report small non-fill
#                               "noise" GPP values rather than the sentinel
#                               code, so they pass straight through this
#                               clamp. Several structured-survey subregions sit
#                               directly on the coast (Yorke/Eyre Peninsula,
#                               Esperance, Albany, Geraldton, Kwinana), and
#                               resample_to_grid()'s area-weighted averaging
#                               (r/b_resample_to_grid.R, used to coarsen this
#                               raster onto PML's 0.1 degree grid downstream)
#                               would otherwise blend unmasked ocean noise
#                               straight into a coastal cell's coarse value.
#   fallback_files              optional non-gap-filled VIIRS (VNP17A2.002)
#                               composite file paths, for filling in any
#                               composite date gpp_files doesn't have yet --
#                               used only for _targets.R's two most recent
#                               VIIRS blocks (gpp_viirs_recent_blocks: the
#                               current, still-open year plus the one that
#                               just froze), whose gap-filled (VNP17A2GF.002)
#                               composites lag real-time by several months
#                               (confirmed live this session). Only dates NOT
#                               already present in gpp_files are taken from
#                               here -- gap-filled composites always take
#                               priority where both exist. NULL (default)
#                               disables this entirely (unchanged behaviour
#                               for MODIS and every truly frozen VIIRS block,
#                               which never need it).
#   fallback_ratio_rast         SpatRaster, 12 layers named "1"-"12" (calendar
#                               month), one gap-filled/non-gap-filled ratio
#                               per pixel per month (_targets.R's
#                               gpp_gf_pixel_ratio_rast target,
#                               build_gpp_pixel_ratio_raster()'s output),
#                               same grid as fallback_files -- MULTIPLIED
#                               (per pixel, by its own calendar month) into
#                               each fallback_files composite before use --
#                               shifts a raw non-gap-filled value up (or
#                               down) to gap-filled's own typical level at
#                               that pixel, so an uncorrected non-gap-filled/
#                               gap-filled offset doesn't show up as a
#                               discontinuity right at the point a forecast's
#                               covariate matters most. A ratio (not an
#                               additive offset) so a low-value pixel can't
#                               be pushed negative -- confirmed empirically
#                               this session that the flat additive version
#                               of this correction still pushed a small
#                               fraction of pixels negative nationally (up to
#                               ~0.24% in the worst month) despite being ~100x
#                               smaller than PML's own additive correction
#                               (which failed the same way, far more
#                               severely -- see build_gpp_pixel_ratio_raster()'s
#                               own header). Required if fallback_files
#                               supplies any date gpp_files doesn't already have.
#
# Returns a SpatRaster, one layer per complete period (season_year_adj or
# month_year, see summarise_by), values in gC/m^2/day (converted from the
# raw kg C/m^2/8-day composites -- see 2b, below -- to match PML-V2's own
# convention, build_pml_gpp_raster()) -- or NULL if gpp_files has no
# composites at all, or none of them add up to even one complete period. Confirmed live
# this session: VIIRS's gap-filled product (VNP17A2GF.002) can lag several
# months behind real-time, so a block scoped to just the current calendar
# year (_targets.R's gpp_viirs_blocks, one block per year) can genuinely have
# zero complete months for most of the year -- terra's own `[[` errors
# ("[subset] no (valid) layer selected") rather than returning an empty
# raster, so this is checked explicitly instead of letting that happen.
# Callers combining several calls' outputs (_targets.R's monthly_gpp_rast/
# monthly_gpp_rast_frozen) don't need to filter NULLs out first -- confirmed
# live that terra's c() silently skips NULL entries even via
# do.call(c, list(...)), so a NULL block just contributes zero layers.

build_seasonal_gpp_raster <- function(gpp_files,
                                       aus_shp,
                                       summarise_by = c("season", "month"),
                                       min_composites_per_period = NULL,
                                       valid_max = 3,
                                       fallback_files = NULL,
                                       fallback_ratio_rast = NULL) {

  summarise_by <- match.arg(summarise_by)
  if (is.null(min_composites_per_period)) {
    min_composites_per_period <- if (summarise_by == "season") 8 else 3
  }

  # ── 1. Load the Gpp_500m composites, one layer per date (skip Psn_QC_500m /
  # supporting files; mosaic same-date files from different regions -- see
  # header note) ─────────────────────────────────────────────────────────────
  gpp_tif <- gpp_files[grepl("Gpp_500m.*\\.tif$", gpp_files)]

  # composite date, parsed from the AppEEARS "doy<YYYY><DDD><HHMMSS>" filename token
  gpp_tif_dates <- as.Date(sub(".*doy([0-9]{4})([0-9]{3}).*", "\\1-\\2", gpp_tif), format = "%Y-%j")

  # ── 1b. Fill in any date gpp_files doesn't have yet, from fallback_files --
  # see fallback_files' own argument note above. Only dates missing from
  # gpp_files are taken from here (setdiff), so gap-filled composites always
  # win where both products actually cover the same date.
  fallback_rast_by_date <- list()
  if (!is.null(fallback_files)) {
    fallback_tif   <- fallback_files[grepl("Gpp_500m.*\\.tif$", fallback_files)]
    fallback_dates <- as.Date(sub(".*doy([0-9]{4})([0-9]{3}).*", "\\1-\\2", fallback_tif), format = "%Y-%j")
    fill_dates     <- sort(unique(setdiff(fallback_dates, gpp_tif_dates)))

    if (length(fill_dates) > 0) {
      if (is.null(fallback_ratio_rast)) {
        stop("fallback_files supplies date(s) gpp_files doesn't have, but fallback_ratio_rast is NULL -- ",
             "can't safely splice in an uncorrected product.")
      }
      skipped_dates <- as.Date(character())
      for (i in seq_along(fill_dates)) {
        d <- fill_dates[i] # single-bracket indexing, not `for (d in fill_dates)` -- confirmed live that a for-loop's own implicit iteration over a Date vector strips its class (unlike lapply()/vapply(), which use `[[` and preserve it), silently turning `d` into a plain number and routing lubridate::month(d) into the wrong S3 method entirely.
        month_num  <- lubridate::month(d)
        # fallback_ratio_rast is built from a single recent reference year (its own
        # gap-filled/non-gap-filled overlap, see _targets.R) -- right after that
        # reference year itself rolls over to a new one, its own late months can
        # still be missing gap-filled data too (same lag, one year removed), so
        # this specific calendar month may genuinely have no ratio entry yet. That's
        # a transient, self-correcting gap (resolves once the reference year's own
        # gap-filled data catches up), not a reason to fail the whole raster build --
        # skip just this date, leaving it unavailable, same as if fallback_files
        # hadn't had it at all.
        if (!as.character(month_num) %in% names(fallback_ratio_rast)) {
          skipped_dates <- c(skipped_dates, d)
          next
        }
        ratio_layer <- fallback_ratio_rast[[as.character(month_num)]]
        fallback_rast_by_date[[as.character(d)]] <- terra::rast(fallback_tif[fallback_dates == d][1]) * ratio_layer
      }
      if (length(skipped_dates) > 0) {
        message(length(skipped_dates), " date(s) skipped -- fallback_ratio_rast has no entry yet for their ",
                "calendar month(s): ", paste(skipped_dates, collapse = ", "))
      }
      if (length(fallback_rast_by_date) > 0) {
        message(length(fallback_rast_by_date), " date(s) filled from the ratio-corrected fallback product ",
                "(the primary product had nothing for these yet): ", paste(names(fallback_rast_by_date), collapse = ", "))
      }
    }
  }

  gpp_dates <- sort(unique(c(gpp_tif_dates, as.Date(names(fallback_rast_by_date)))))
  if (length(gpp_dates) == 0) {
    message("No Gpp_500m composites found in gpp_files or fallback_files -- returning NULL.")
    return(NULL)
  }

  per_date_rast <- purrr::map(gpp_dates, function(d) {
    if (as.character(d) %in% names(fallback_rast_by_date)) {
      return(fallback_rast_by_date[[as.character(d)]])
    }
    rasters <- lapply(gpp_tif[gpp_tif_dates == d], terra::rast)
    if (length(rasters) == 1) rasters[[1]] else Reduce(terra::merge, rasters)
  })

  # dates covered by only one region are smaller than dates covered by both
  # -- extend every date out to the shared union extent (NA outside whatever
  # region(s) actually delivered it) so they can be stacked into one raster.
  full_ext <- Reduce(terra::union, lapply(per_date_rast, terra::ext))
  gpp_rast <- terra::rast(lapply(per_date_rast, terra::extend, y = full_ext))
  names(gpp_rast) <- as.character(gpp_dates)

  # mask everything outside the Australian coastline to NA -- see valid_max's
  # header note on why the sentinel-code clamp below can't be relied on to
  # exclude the sea. aus_shp is already EPSG:4326 (matching this raster,
  # since appeears::rs_build_task() hardcodes area requests to geographic
  # projection), so no reprojection is needed.
  gpp_rast <- terra::mask(gpp_rast, terra::vect(aus_shp))

  # mask sentinel/fill codes (see valid_max above) to NA before any averaging
  gpp_rast <- terra::clamp(gpp_rast, upper = valid_max, values = FALSE)

  # ── 2. Collapse 8-day composites to period means ──────────────────────────
  # season_info() (r/a_season_info.R) gives the same Summer/Autumn/Winter/
  # Spring, December-rolls-into-next-year convention used everywhere else in
  # the pipeline. The month case needs no such year-boundary adjustment --
  # plain calendar month/year -- and matches attach_time_variables()'s own
  # month_year format ("<month>_<year>", e.g. "7_2026") exactly.
  period_lbl <- vapply(gpp_dates, function(d) {
    if (summarise_by == "season") {
      s <- season_info(d)
      paste0(s$season, "-", s$year_adj)
    } else {
      paste0(lubridate::month(d), "_", lubridate::year(d))
    }
  }, character(1))

  composite_counts <- table(period_lbl)
  complete_periods <- names(which(composite_counts >= min_composites_per_period))
  keep       <- period_lbl %in% complete_periods
  if (!any(keep)) {
    message("No period in gpp_files has >= ", min_composites_per_period,
            " composites (", length(gpp_dates), " composite(s) found, ",
            min(gpp_dates), " to ", max(gpp_dates), ") -- returning NULL.")
    return(NULL)
  }
  gpp_rast   <- gpp_rast[[keep]]
  period_lbl <- period_lbl[keep]

  period_rast <- terra::tapp(gpp_rast, index = period_lbl, fun = "mean", na.rm = TRUE)
  # tapp() mangles index labels into syntactic names (e.g. "Winter-2026" ->
  # "Winter.2026", or "7_2026" untouched but not guaranteed to stay so);
  # restore the clean labels, in the same first-appearance order tapp() used
  # to build the layers (gpp_dates was already sorted, so this is
  # chronological for both season and month).
  period_lvls <- unique(period_lbl)
  names(period_rast) <- period_lvls

  # ── 2b. Unit conversion: kg C/m^2/8-day -> gC/m^2/day ─────────────────────
  # Raw AppEEARS Gpp_500m composites (and so period_rast, their per-period
  # mean) are kg C/m^2/8-day (confirmed against the source files -- see
  # valid_max's own note above). PML-V2's own build_pml_gpp_raster() converts
  # to gC/m^2/day (mean daily rate); apply the equivalent conversion here
  # (kg->g: x1000, per-8-day total->per-day mean: /8, i.e. x125) so both GPP
  # sources share the same convention before any comparison, correction or
  # splice. Confirmed empirically this session that leaving this unconverted
  # (as an earlier version of this function did) meant the PML bias
  # "correction" (check_gpp_pml_bias()/_targets.R's gpp_pml_bias_correction)
  # was actually dominated by this missing ~125x factor rather than a
  # genuine model difference -- e.g. raw PML's own January mean (~1.28
  # gC/m^2/day) was smaller than the "correction" (~2.16) being subtracted
  # from it, driving the corrected pre-2000 splice negative, which isn't
  # physically possible for GPP. Applied after tapp()'s period-mean (not to
  # gpp_rast beforehand) since it's a linear rescaling either way, and this
  # keeps the raw-unit fallback correction addition (1b, above) dimensionally
  # consistent with the raw composites it's added to.
  period_rast <- period_rast * (1000 / 8)

  # ── 3. Tag each layer with the composite count it was built from ─────────
  for (i in seq_along(period_lvls)) {
    terra::metags(period_rast, layer = i) <- c(
      n_composites = as.character(composite_counts[[period_lvls[i]]])
    )
  }

  period_rast

}

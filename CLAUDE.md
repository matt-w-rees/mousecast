You are situated inside of an R project source directory. 

The purpose of this project is to integrate data sources and forecast house mouse abundance and plagues in Australian grain growing regions. 
The main management action is to undertake poison-baiting of mice using zinc phosphide (either 25 or 50 dose). 
Providing farmers with early warning of moderate-high or increasing mouse activity is key to help them decide when to bait. 

Mouse data comprises of structured surveys:
a) live-trapping surveys where grids of traps are deployed, usually for 3 or more nights.
b) "rapid assessment" surveys, comprising of active burrow counts (in 100 m transects) and chew card assessments (cards a deployed on same transects, 10 m apart, percentage of card chewed by mice is recorded). 
The same paddock is generally surveyed repeatedly, but there are many cases where another paddock is surveyed instead. 

We also consider citizen observation data of categorical abundance estimates: no, low, moderate of high mouse abundance observed, via the "MouseAlert" website. We consider citizen observation to be less reliable than structured surveys, so often weight it less.

Each survey data type has different detection rates, and mouse detection also likely varies throughout the year for each data type too (namely active burrow counts as soil cracking or dense crop cover can obscure burrows). 

Mice population are generally driven by weather and crop growth patterns. 
It is important to consider there are different cropping schedules across Australia, notably winter-only crop schedule south of Dubbo (approximately) versus summer + winter crops north, in the eastern states. 
Western Australia has winter-only crops, but with slightly different sowing and harvesting dates. 

We consider a crop paddock as a unique "survey site". 
Data sometimes has "site" and "subsite" names, but we use GPS coordinates to determine which paddock the survey was in (although historic data often has coordinates taken from the roadside or a central farm house which makes this challenging, we can use site/subsite names to determine when this is likely the case).
There is also data along fence lines in between paddocks, but we want to discard this data and only keep surveys from within the crop. 
We use the "ePaddock" remotely sensed dataset to determine paddock boundaries, although it is not alway accurate so have to append with hand-drawn paddocks. 

Survey data is currently entered using "ODK" software. Previously "Microsoft Access" was used as the database. Data has also been entered into CSV files, as is MouseAlert data.
ODK Central form definitions (.xlsx, used to build the forms in ODK Central) live in "odk_forms/". Submissions are currently ingested by manually exporting each form from ODK Central ("Export to CSV (with media)") into "raw_data/survey_data/odk/<form_name>.csv/" and reading those files; the live ODK Central API (the "ruODK" package) was used previously but is not currently used.
The "Monitoring" project is the main data source we consider, but we also collate data from an "Ecology" project (live-trap data only in a microsoft access database, mostly experimental studies). Baiting is accounted for explicitly (bait_history/bait_dosage) rather than by excluding treated plots, so both control and treated Ecology data are used. 
There is also additional data sources, such as "GRDC" funded monitoring (via subcontractors). 

This project uses the r-package "targets"; the targets pipeline is "_targets.R" with functions contained within the "r/" folder. It is important to "track" raw files so the pipeline is only re-run (relevant sections of which) when the raw data changes.
When a raw data source is a folder of multiple related files (e.g. an ODK CSV export with main + repeat-group sub-tables), track the whole folder with tar_file(list.files(folder, full.names = TRUE, recursive = TRUE), cue = tar_cue(mode = "always")) -- the always-cue is required, since without it targets only re-hashes the files it already listed and silently never sees a newly added file (found live: recent_plague_maps missed Summer_2021/Autumn_2022/Winter_2022, 2026-09) -- and have the downstream read function pick out the files it needs by filename pattern, rather than tracking and passing each file separately.
There is also a "r_not_in_use" folder to store old scripts no longer used, in case they will be useful in the future. 
It is often helpful to review the "r_not_in_use" files when asked to create a new function. 
When a script or function is fully superseded (e.g. replaced by a new approach), move it into "r_not_in_use" rather than deleting it outright. 

The pipeline has two main sections: 
(1) cleaning and combining mouse survey datasets, saving and summarising the resulting dataset through a shiny app for data exploration, as well as a html/pdf simple communication deployed via a website (updated when pushed to github). 
(2) time-series statistical forecast modelling of the cleaned mouse survey dataset, which firstly includes attaching temporally-varying and static covariates to the integrated dataset. 
Section (2) is itself split into lettered pipeline sections in "_targets.R": B) downloads and processes the covariates; C) merges Section A's survey data with Section B's covariates into a seasonal, model-ready panel (one row per paddock x season, framework-agnostic); D) explores each raw observation stream's own distributional structure independent of any downstream model (one Quarto report per data type -- chewcard_count, trap_count, burrow_count); E) fits and predicts from the forecast model itself, described below.

Section B's large predictor-download/reprocessing targets each have their own real-world update cadence, gated in code rather than left to `targets`' default "any dependency changed" rule -- useful for sanity-checking whether a given re-run reflects genuinely new upstream data or is an artefact of an unrelated change (see the note below the table: `targets` hashes a function's full source text, comments included, so a pure comment edit anywhere in a target's dependency chain is enough to force a rebuild with zero data-freshness reason behind it).

| Target(s) | Source's own update cadence | Pipeline's re-run gate | What "normal" looks like |
|---|---|---|---|
| `gpp_files_modis`, `composite_gpp_rast_modis_block` | MODIS MOD17, frozen 2000 to a fixed `gpp_modis_end_date` | Fixed end date + `recheck_after_days = 36500` (r/b_download_gpp.R) | Should never auto-rebuild once built. Only a deliberate `gpp_modis_end_date` extension, or a genuine code change to `download_gpp()`/`load_and_clamp_gpp_composites()`, should invalidate it. |
| `gpp_files_viirs_recent_block`, `gpp_files_viirs_nongf` | VIIRS VNP17A2(GF), ~8-day composites, near-real-time | `end_date`/`end_date_cap = todays_date` (`_targets.R`) -- eligible to check every `tar_make()` | Expected to run every `tar_make()` call, but a genuine run should only fetch the handful of composite dates published since last time. A multi-GB, tens-of-minutes rebuild (the whole archive re-transferring) is a sign something upstream (e.g. a comment edit) invalidated it, not that ~2 years of new data suddenly appeared. |
| `composite_gpp_rast_viirs_frozen_block` | Same VIIRS source, once a calendar-year block ages out of `gpp_viirs_recent_blocks` | Frozen -- no longer re-requested once a block leaves the recent window | Should never re-run for a given block once it's frozen. |
| `pml_gpp_files` | PML-V2 historic archive (TPDC), fixed 1982-2020 | No auto re-check beyond a remote file-size match (r/b_download_pml_data.R) | Should essentially never re-run. The only legitimate reason is TPDC extending its archive, a manual `years` argument change. |
| `silo_files` (monthly rainfall) | SILO `monthly_rain`, published only once a calendar month fully elapses | Current month always re-requested; past months only if missing (r/b_download_silo_monthly_data.R) | Attempts every `tar_make()` call (a cheap request), but a real new file only appears once a month completes -- most attempts should be near-instant 404-skips. Uses `cue = tar_cue(mode = "always")`, matching `awra_files` below. |
| `awra_files` (feeds `soil_moisture_raster`/`_coarse`/`_nowcast`) | AWRA-L v7 `sm_pct` (NCI), one rolling file, appended as new months become available | `cue = tar_cue(mode = "always")`, but gated internally by a HEAD Last-Modified check against the local file's mtime (r/b_download_awra_data.R) | Dispatches every `tar_make()` call, but the ~1.3GB re-download only actually happens once NCI's own file timestamp has moved -- no fixed schedule is documented upstream, so treat any observed cadence as empirical, not guaranteed. |
| `silo_daily_files_min_temp`, `min_temp_raster_block` | SILO daily min-temp, current calendar year mutable | `refresh_after = latest_complete_season_end` (`_targets.R`) -- re-download eligibility resets once per completed season | Should reprocess at most ~4x/year, at a Feb/May/Aug/Nov season boundary. A rebuild mid-season with no code change is worth double-checking. |
| `*_nowcast` variants (`gpp_rolling_raster_6_nowcast`, `soil_moisture_raster_coarse_nowcast`) | Same underlying GPP/AWRA sources as their season-end-trimmed namesakes above, just without the trim | Tracks whichever of `gpp_files_*`/`awra_files` it's built from | Same cadence as its own untrimmed source row above -- no separate schedule of its own. |

This table only tells you whether a rebuild's *timing* makes sense against real-world data cadence -- it doesn't mean a rebuild outside that cadence is necessarily wrong, since a genuine `_targets.R`/function-code change should always take priority over it.

Section D explores each of the three observation streams (chewcard_count, trap_count, burrow_count) on its own terms, independent of Section E's HMM or any other downstream model -- one self-contained Quarto report per data type (quarto_reports/<type>_exploration.qmd), covering the raw data, candidate distribution families, and predictor covariates, ending in recommendations for future modelling of that data type. chewcard_count_exploration.qmd is built first; trap_count and burrow_count reports are planned to follow the same template. Each report uses Section C's model-ready survey data (e.g. survey_model_data_rapid for chew cards) as its population -- all states and every deployed card/trap-night/transect, deliberately not Section E's narrower HMM subset. Modelling uses the `gamlss2` package; the reports themselves, not this file, are the record of current findings and methodological details.

Section E forecasts mouse plagues with a two-state (plague / not-plague) hidden Markov model, fit with the "hmmTMB" R package, at Section C's paddock x season grain. Each paddock is its own time series (hmmTMB's ID mechanism); repeat within-season visits are collapsed first by resolve_survey_visits(mode = "first") (r/c_resolve_survey_visits.R), which keeps visit-varying fields (e.g. bait_history, crop_stage) paired with their own visit's counts.

- Observations: live-trap, active-burrow and chew-card counts enter as conditionally-independent streams given the hidden state. A paddock-season missing a stream simply drops it from the likelihood (no imputation). hmmTMB constraints shape how each stream is specified -- e.g. offset() terms are silently ignored and binomial trials are fixed per state -- see r/e_fit_plague_hmm.R's header for the current families, effort offsets and which observation covariates are shared vs. free across states.
- Transitions: covariate-driven (not paddock-specific), with ae_zone entering as random effects. Use contemporaneous (current-season) covariates: the transition already links one season's covariates to the FOLLOWING season's state, so "_lag1" versions would test a two-season delay instead. The current transition formula is in _targets.R (plague_hmm).
- Anchoring (semi-supervised): known plague occurrence fixes the hidden state via hmmTMB's "state" column (value where known, NA elsewhere), built by anchor_plague_state() (r/c_anchor_plague_state.R) from the hand-digitised plague-extent maps in "raw_data/plague_occurrence/recent_plague_maps/" -- see that function's header for the rule. plague_hmm_unsupervised fits the same model with no anchors, as a comparison.
- Scope: the states and excluded zones are set by build_hmm_data()'s state_filter/exclude_zones arguments in _targets.R.
- Prediction: predict_plague_risk() (r/e_predict_plague_risk.R) maps Pr(not-plague -> plague) on the 0.1 degree covariate grid, masked to the AE zones the model was fit to. The forecast is one season ahead only, since every covariate is built from realised observations.

The detailed history of what has been tried, adopted or reverted for this model lives in the headers of r/e_fit_plague_hmm.R, r/c_anchor_plague_state.R and r/e_predict_plague_risk.R, and in quarto_reports/plague_model_summary.qmd -- not in this file.

This workflow is based on function-orientated programming, where each function generally acheives one task. 
The priority is therefore on readability, even if it requires more code. It is important all code is explained using hashed comments. 
Each script should be named the same as the function name, except with a prefix of "a_", "b_", "c_", "d_" or "e_" corresponding to which major pipeline section it is used in (matching the "A)"/"B)"/"C)"/"D)"/"E)" section headers in _targets.R). Section D (the observation-stream exploration reports) has no supporting r/ functions -- its reports are qmd-resident -- so there are no "r/d_*.R" files; the plague HMM's functions are "r/e_*.R".


AI assistants should read every .R file.
AI assistants should never commit or push the "docs/" folder to GitHub -- Cloudflare Pages deploys it straight to the public website (mouseforecast.com), so updates to it are pushed manually/deliberately, not swept up into an ordinary commit.
AI assistants should always explain created code using comments.
Place each comment directly above the specific line (or tightly-related group of lines) it explains, not as one paragraph bundling several distinct explanations before a block of otherwise-unrelated code -- a reader shouldn't have to hold several ideas in mind while scanning down to find which line each one was about. This applies inside multi-line function calls too (e.g. a mutate() with several unrelated fields, or a multi-term model formula) -- a comment on its own line between arguments/terms is valid R and keeps the explanation next to the specific thing it's about. Exception: a single comment above a block of genuinely homogeneous, repeated lines (e.g. five near-identical group_by()/summarise() calls on different columns) is fine, since splitting it would just repeat the same sentence.
AI assistants should put new functions at the top of the r script, although AI assistants should generally not define functions inside of functions unless they are very brief, anonymous functions. 
AI assistants should ensure all created quarto documents are self-contained. 
In "_targets.R", keep comments terse: one short sentence per line, not a wrapped paragraph spanning several "#" lines. Detailed rationale (why a function works the way it does, edge cases, etc.) belongs in that function's own header comment in "r/", not duplicated in "_targets.R" -- "_targets.R" comments should just say what a target does and point to the relevant function for detail.
Within each major section ("A)"/"B)"/"C)"/"D)"/"E)"), subdivide with numbered "1)", "2)", "3)"... subheadings, and within those, lettered "i.", "ii.", "iii."... sub-subheadings where a further split is useful -- matching section A's existing style.
Blank lines around these headings (any level -- "A)", "1)", "i."): one blank line between a heading and its own first sub-heading/target (introducing its children); two blank lines between the end of one heading's content and the next heading of any level (a sibling, or a step back up).
In "_targets.R", a target's call collapses onto one line if it has 3 or fewer arguments (counting whichever call -- the tar_file()/tar_terra_rast() wrapper or the function it wraps -- actually carries the meaningful configuration); with 4+ arguments, give each its own line with the "=" signs vertically aligned. A pipe chain ("|>") always gets one step per line regardless of argument counts. An inline "{ }" block keeps the opening brace on the assignment's own line, indents the body one level, one statement per line, and closes with "}" back at the target's own indentation.

Future AI assistants working on this project should read these help pages via btw tools:
hmmTMB::HMM
hmmTMB::MarkovChain
hmmTMB::Observation
targets::tar_plan()


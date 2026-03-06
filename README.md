# mousecast

Forecasting house mouse abundance in Australian grain-growing regions.

## Overview

This project uses the [`targets`](https://docs.ropensci.org/targets/) R package to manage a reproducible analysis pipeline. The pipeline downloads environmental covariates, cleans field survey data, fits Bayesian time-series models via [`mvgam`](https://nicholasjclark.github.io/mvgam/), and produces reports and forecasts.

To run the full pipeline:

```r
targets::tar_make()
```

To visualise what is up-to-date and what needs to run:

```r
targets::tar_visnetwork()
```

## Pipeline stages (`_targets.R`)

The pipeline runs in the following order:

1. **Set-up** — determine the current and next forecast season.
2. **Download covariates** — fetch daily climate data from SILO and monthly water balance data from AWRA (BOM).
3. **Process covariates** — aggregate daily rasters to monthly and seasonal summaries (rainfall, temperature, soil moisture, evapotranspiration).
4. **Load survey data** — extract and clean mouse trapping data from two Microsoft Access databases (Ecology project, Monitoring project), DPIRD data, and new rapid-assessment CSV files submitted from 2026 onwards.
5. **Integrate data** — combine trap, burrow, and chew-card survey types; add time variables and crop information; filter out unreliable sites.
6. **Attach covariates** — extract raster values at each survey location and month, then summarise to seasonal averages and percent deviations from long-term means.
7. **Prepare model data** — deduplicate sessions, add lagged covariate columns, and format data for `mvgam`.
8. **Fit model** — fit a Bayesian state-space model with shared latent trends across sites.
9. **Predict** — extract posterior draws and summarise predictions by site trend, region, and agro-ecological zone.
10. **Reports** — render Quarto HTML reports covering data summaries, model evaluation, hindcasts, and forecasts.

## Folder structure

```
mousecast/
|-- _targets.R              # Pipeline definition (the main entry point)
|-- run.R / run.sh          # Convenience scripts to launch tar_make()
|-- r/                      # R functions used by the pipeline (one file per function)
|-- r_other/                # Work-in-progress and archived scripts (not part of pipeline)
|-- raw_data/               # Input data (not modified by the pipeline)
|   |-- survey_data/        # Mouse survey databases and CSVs
|   `-- predictor_variables/ # Shapefiles, average climate grids, etc.
|-- derived_data/           # Pipeline outputs (generated, can be rebuilt)
|   |-- cleaned_raw_dataset/ # Cleaned survey data with metadata
|   |-- model_fits/         # Saved model objects
|   `-- ethics_reports/     # Annual ethics reporting summaries
|-- quarto_reports/         # Quarto (.qmd) source files and rendered HTML reports
|-- _targets/               # targets cache (auto-managed, do not edit)
`-- docs/                   # Additional project documentation
```

## Functions (`r/`)

Each `.R` file in `r/` contains a single function with the same name as the file. Functions follow a naming convention that reflects their role:

- `data_*` — load, clean, filter, or reshape survey data
- `cov_*` — download or process environmental covariate rasters
- `model_*` / `predict_*` — model specification, fitting, and prediction
- `plot_*` — visualisation helpers

## Key dependencies

- [`targets`](https://docs.ropensci.org/targets/) / [`tarchetypes`](https://docs.ropensci.org/tarchetypes/) — pipeline management
- [`mvgam`](https://nicholasjclark.github.io/mvgam/) — Bayesian multivariate GAM time-series models
- [`terra`](https://rspatial.org/terra/) — raster processing
- [`sf`](https://r-spatial.github.io/sf/) — vector spatial data
- [`tidyverse`](https://www.tidyverse.org/) — data wrangling
- [`quarto`](https://quarto.org/) — report rendering

## Notes

- Reading Microsoft Access databases (`.accdb`) on macOS requires `mdbtools` installed via Homebrew and the `Hmisc` package. See the original README for setup instructions.
- Pipeline targets are stored in `qs` format for fast serialisation.
- Covariate raster files are saved under `derived_data/predictor_variables/` and are tracked as file targets so the pipeline only reruns affected steps when source files change.

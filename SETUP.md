# System-level dependencies

`renv.lock` pins every R package this pipeline uses (see `renv::restore()`). It does not, and cannot,
pin the layer underneath R packages -- the OS-level geospatial libraries `sf`/`terra` link against, and
the C++ compiler toolchain `hmmTMB` (via TMB) and `rstan`/`StanHeaders` need at install time. This file
is a known-good reference snapshot of that layer, captured on the machine this pipeline is actively
developed on -- not an enforced constraint, just something concrete to match against if this pipeline
ever needs to run somewhere else (another machine, an HPC cluster).

Captured 2026-09-05, macOS 26.5.2 (arm64), R 4.6.1.

## Geospatial libraries (GDAL / GEOS / PROJ)

What `sf` and `terra` actually report themselves linked against -- this is the number that matters for
reproducing their behaviour, checked via:

```r
sf::sf_extSoftVersion()
terra::gdal()
```

| Library | Version |
|---|---|
| GDAL | 3.8.5 (sf) / 3.13.1 (terra) |
| GEOS | 3.13.0 |
| PROJ | 9.5.1 |

**Important asymmetry, confirmed live:** on macOS, CRAN's binary builds of `sf`/`terra` bundle their own
statically-linked copies of GDAL/GEOS/PROJ -- they do NOT use whatever the system package manager has
installed. Proof: this machine's Homebrew has gdal 3.13.1_4 / geos 3.14.1 / proj 9.8.1 installed
(`brew list --versions gdal geos proj`) -- newer, and different, from what `sf`/`terra` themselves
report above. Homebrew's copies are unused by R here.

This means the table above -- not Homebrew's versions -- is the real target to match elsewhere. On
Linux (e.g. an HPC cluster), `sf`/`terra` typically DO dynamically link the system's installed
libgdal/libgeos/libproj instead of bundling their own, so matching there means finding modules/packages
that provide these same versions, not just "the newest available."

## Compiler toolchain

Needed for `hmmTMB`'s TMB model compilation (happens at runtime, not just install) and for building
`rstan`/`StanHeaders` from source at install time.

- Compiler: Apple clang 21.0.0 (`clang-2100.1.1.101`), target `arm64-apple-darwin25.5.0`
- No custom `~/.R/Makevars` -- default R toolchain flags, nothing hand-tuned

## R itself

- R version 4.6.1 (2026-06-24)
- Platform: aarch64-apple-darwin (arm64 macOS)

## If this pipeline needs to run somewhere else

This file is the cheap, static version of environment parity -- a reference to check against by hand.
The complete version is a container (Docker to build, Apptainer/Singularity to actually run on a
shared HPC cluster without root) that pins this whole layer plus `renv::restore()` for the R packages,
in one artifact. Worth building only once there's a real run to justify it -- see the HPC investigation
discussion (2026-09) for the full reasoning on why that wasn't done pre-emptively.

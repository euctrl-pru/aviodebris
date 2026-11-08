# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**aviodebris** is an R package that estimates the collision and casualty risk between aircraft traffic and reentering space debris. It supports the DASC 2025 paper "Preparing for Potential Closure of European Airspaces due to Re-entering Space Objects".

The package combines:
- Orbital mechanics calculations for debris reentry probability distributions
- H3 hexagonal grid spatial analysis over EUROCONTROL airspace
- Aircraft trajectory processing and traffic density computation
- Risk expectation calculations based on effective exposed area

## Package Architecture

```
aviodebris/
├── R/                          # Package source code
│   ├── orbitals.R              # Kepler-to-Cartesian conversion, latitude weights
│   ├── vallado.R               # Ports of Vallado astrodynamics algorithms (coe2rv, rv2coe, ecef2ll, ...)
│   ├── constants.R             # Internal astrodynamic constants (EGM-08, km/s/rad units)
│   ├── hex_spatial.R           # H3 hexagon grid operations, bounding box utilities
│   ├── hexagonize.R            # Assign H3 cells to trajectory points (DuckDB)
│   ├── resample.R              # Trajectory resampling
│   ├── traffic_density.R       # Hourly traffic density per cell/aircraft type
│   ├── collision_and_casualty.R # Risk expectation calculations
│   ├── aircraft_performance_scrapers.R # Web scrapers for aircraft data
│   ├── data.R                  # Dataset documentation
│   └── aviodebris-package.R    # Package-level documentation
├── data/                       # Exported .rda datasets
├── data-raw/                   # Scripts to generate datasets
│   ├── gcat.R                  # Process GCAT debris reentry catalog
│   ├── weighting_function.R    # Generate probability weights
│   └── gcat/                   # Raw GCAT TSV files
├── tests/testthat/             # Unit tests (testthat, edition 3)
│   ├── test-orbitals.R         # kepler_to_cartesian(), latitude_weights()
│   └── test-vallado.R          # Vallado ports against published reference values
└── man/                        # Auto-generated roxygen2 documentation
```

## Key Functions

| Function | Purpose |
|----------|---------|
| `kepler_to_cartesian()` | Convert orbital elements to geocentric-equatorial coordinates |
| `latitude_weights()` | Compute latitude probability distribution for debris reentry |
| `bbox_nm()` | Return EUROCONTROL/NM area bounding box |
| `hexes_for_bbox_at_res()` | Generate H3 hexagons covering a bounding box |
| `resample_traffic()` | Resample trajectory parquet files at specified interval |
| `hexagonize_traffic()` | Assign H3 cell indices using DuckDB H3 extension |
| `traffic_density_hourly()` | Calculate hourly traffic density per cell/aircraft type |
| `collision_and_casualty_risk_expectation_hourly()` | Compute risk expectation combining debris weights and traffic |
| `scrape_aircraft_type_info_*()` | Scrape aircraft data from EUROCONTROL, Skybrary, doc8643 |
| `coe2rv()` | Classical orbital elements → ECI position/velocity (Vallado) |
| `rv2coe()` | ECI position/velocity → classical orbital elements and orbit type (Vallado) |
| `ecef2ll()` / `ecef2llb()` | ECEF position → geocentric/geodetic latitude, longitude, height (Astronomical Almanac / Borkowski) |

### Vallado ports (`R/vallado.R`)

Ports of algorithms from D. Vallado, *Fundamentals of Astrodynamics and Applications* (2022), following the CelesTrak Python implementation (https://github.com/CelesTrak/fundamentals-of-astrodynamics, `software/python/src/valladopy/`).

- Plain numerics in **km, km/s, rad** (no `units` objects); `mu` defaults to `GRAVITATIONAL_CONSTANT_EARTH` (398600.4415 km³/s²).
- Orbit types (from `determine_orbit_type()`): `"circular_equatorial"`, `"circular_inclined"`, `"eph_equatorial"`, `"eph_inclined"` ("eph" = elliptical, parabolic or hyperbolic). Orbit-type tolerance is `SMALL = 1e-10` (Python port); Vallado's Fortran uses 1e-8.
- Undefined elements are `NA` (e.g. `nu`/`m` for circular, `raan`/`argp` for equatorial orbits). For circular orbits `coe2rv()` needs `arglat` (inclined) or `truelon` (equatorial) instead of `nu`.
- Internal helpers: `rot1()`, `rot3()`, `newtonnu()`, `determine_orbit_type()`, `angle()`, `clip()`, `adjust_angle()`, `interval_modulus()`; `fmod()`/`rebound()` adapted from StratigrapheR.
- Known upstream bug: the Python `newtone()` drops the minus sign on sin(ν) in the hyperbolic branch (Vallado Alg. 6), so its `test_newtone` hyperbolic expected ν has the wrong sign. Don't port it literally.

### Test reference values (`tests/testthat/test-vallado.R`)

| Source | Covers |
|---|---|
| CelesTrak Python tests (`test_frame_conversions.py`, `test_newton.py`) | Ex. 2-5/2-6/5-5 (`rv2coe`/`coe2rv`, heliocentric `mu`), Ex. 3-3 (`ecef2ll`, `ecef2llb`), `newtonnu` |
| Vallado Fortran `software/fortran/TESTASTF.OUT` (ELORB cases) | Elliptical inclined/equatorial (prograde + retrograde); near-circular vectors (printed to 7 decimals, so compared at 1e-6 and give ecc ≈ 1e-8) |
| `coe2rv()` → `rv2coe()` round trips | Exact circular inclined/equatorial types; hyperbolic and parabolic orbits with anomalies from `test_newtonm` |

## Datasets

| Dataset | Description |
|---------|-------------|
| `reentries_gcat` | 25 years of debris reentries (2000-2025) from GCAT |
| `weightings_half_degree_latitude` | Latitude-based debris probability weights |
| `weightings_half_degree_hourly` | Half-degree grid hourly weights |
| `weightings_h3_resolution_3_hourly` | H3 resolution 3 cell hourly weights |
| `aircraft_types` | Aircraft type characteristics (322 types) |
| `effective_exposed_area` | Effective exposed area and max passengers per aircraft type |

## Common Commands

```r
# Install package locally
devtools::install()

# Run R CMD check
devtools::check()

# Regenerate documentation
devtools::document()

# Regenerate datasets (run from project root)
source("data-raw/gcat.R")
source("data-raw/weighting_function.R")

# Load package for development
devtools::load_all()
```

## Data Processing Pipeline

1. **Trajectory data** (external parquet files in `data-raw/trjs/`)
2. **Resample** → `resample_traffic(day)` → `trajectories_<date>_resampled_30s.parquet`
3. **Hexagonize** → `hexagonize_traffic(day)` → `trajectories_<date>_resampled_30s_bbox_res_3.parquet`
4. **Density** → `traffic_density_hourly(day)` → `traffic_density_<date>_res_3_hourly.parquet`
5. **Risk** → `collision_and_casualty_risk_expectation_hourly(day)` → `collision_and_casualty_<date>_res_3_hourly.parquet`

## Code Style Guidelines

This package follows tidyverse style conventions:

- Use `|>` (native pipe) not `%>%`
- Use `.by` argument in `summarise()`/`mutate()` instead of `group_by() |> ... |> ungroup()`
- Use snake_case for function and variable names
- Prefix external package functions with namespace (e.g., `dplyr::filter()`)
- Use the `.data` pronoun in data-masking contexts (`filter()`, `mutate()`, `summarise()`, ...) for R CMD check compliance
- In tidyselect contexts (`select()`, `unnest()`, `pivot_*()`, `.by`, ...) use quoted column names (e.g. `select(-"x")`), not `.data$x` (deprecated since tidyselect 1.2.0)
- In roxygen, escape unit brackets (`\[km\]`, `\[rad\]`), otherwise markdown turns them into broken links
- Tests use testthat edition 3: `tolerance` is a mean *relative* difference; use explicit absolute bounds (`expect_lt(max(abs(x - y)), eps)`) when values are tiny or near zero
- Call `coe2rv()` / `rv2coe()` with named arguments in tests

## Dependencies

Key dependencies:
- **arrow**: Parquet I/O
- **duckdb/DBI**: SQL processing with H3 extension for hexagonization
- **h3jsr**: H3 hexagonal grid operations
- **sf**: Spatial features
- **trrrj**: Trajectory resampling (custom package)
- **pracma** (removed): row-wise `row_dot()`/`row_norm()`/`row_cross()` helpers in `R/vallado.R` replace it
- **units** (Suggests): only used in tests

## Known Issues and TODOs

### Bugs to Fix
- ~~`collision_and_casualty.R`: Typo in output filename `casuality` → `casualty`~~ ✓ Fixed
- ~~`hexagonize.R`: Hardcoded extension path `/Users/spi/.duckdb/extensions`~~ ✓ Fixed (uses `DUCKDB_EXTENSION_DIR` env var)
- ~~`hexagonize.R`: DuckDB connection not closed (should use `withr::local_db_connection()`)~~ ✓ Fixed
- ~~`hexagonize.R`: Parameter `day` is shadowed by `lubridate::day(date)` assignment~~ ✓ Fixed (renamed to `day_of_month`)

- ~~`rv2coe()`: orbit-type names mismatched `determine_orbit_type()` (`cir_*` vs `circular_*`), so circular elements were never computed~~ ✓ Fixed

### Vallado TODOs
- No independent reference values for hyperbolic/parabolic `rv2coe()` (only round trips + `test_newtonm` anomalies)
- Untested edge cases: hyperbolic ν beyond asymptote (`m = Inf`), parabolic ν > 168°, ecc between 1e-10 and 1e-8
- `h_mag < 0` early return in `rv2coe()` is unreachable (a norm can't be negative)

### Style Improvements
- ~~Replace `group_by() |> summarise() |> ungroup()` with `.by` argument~~ ✓ Finished
- ~~Add missing `.groups = "drop"` to `summarise()` calls~~ ✓ Finished
- ~~Fix dataset name inconsistency: `effective_expose_area` → `effective_exposed_area`~~ ✓ Finished

### Design Improvements
- ~~Functions write side-effects only; consider returning data invisibly~~ ✓ Fixed (pipeline functions write parquet and explicitly return `invisible(out)`)
- Scraper functions have significant duplication; consider factory pattern
- Add input validation with `cli::cli_abort()`
- ~~Add unit tests for `kepler_to_cartesian()` and `latitude_weights()`~~ ✓ Done (`tests/testthat/test-orbitals.R`)

### Performance Improvements
- `plot_hexes_map()`: `hexes_for_bbox_at_res()` called twice with same args
- ~~`kepler_to_cartesian()`: Already vectorizable, but called row-by-row via `purrr::partial()`~~ ✓ Fixed (`latitude_weights()` now uses vectorised `coe2rv()`; identical output, ~4x faster)

### Documentation
- Add vignette showing full workflow
- Add `@examples` with `\dontrun{}` for file I/O functions
- Move `janitor`, `zoo`, `fs` to `Suggests:` (only used in data-raw scripts)

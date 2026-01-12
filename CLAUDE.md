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
- Use `.data` pronoun in tidyselect contexts for R CMD check compliance

## Dependencies

Key dependencies:
- **arrow**: Parquet I/O
- **duckdb/DBI**: SQL processing with H3 extension for hexagonization
- **h3jsr**: H3 hexagonal grid operations
- **sf**: Spatial features
- **trrrj**: Trajectory resampling (custom package)

## Known Issues and TODOs

### Bugs to Fix
- ~~`collision_and_casualty.R`: Typo in output filename `casuality` → `casualty`~~ ✓ Fixed
- ~~`hexagonize.R`: Hardcoded extension path `/Users/spi/.duckdb/extensions`~~ ✓ Fixed (uses `DUCKDB_EXTENSION_DIR` env var)
- ~~`hexagonize.R`: DuckDB connection not closed (should use `withr::local_db_connection()`)~~ ✓ Fixed
- ~~`hexagonize.R`: Parameter `day` is shadowed by `lubridate::day(date)` assignment~~ ✓ Fixed (renamed to `day_of_month`)

### Style Improvements
- Replace `group_by() |> summarise() |> ungroup()` with `.by` argument
- ~~Add missing `.groups = "drop"` to `summarise()` calls~~ ✓ Fixed in `collision_and_casualty.R`
- ~~Fix dataset name inconsistency: `effective_expose_area` → `effective_exposed_area`~~ ✓ Fixed

### Design Improvements
- Functions write side-effects only; consider returning data invisibly
- Scraper functions have significant duplication; consider factory pattern
- Add input validation with `cli::cli_abort()`
- Add unit tests for `kepler_to_cartesian()` and `latitude_weights()`

### Performance Improvements
- `plot_hexes_map()`: `hexes_for_bbox_at_res()` called twice with same args
- `kepler_to_cartesian()`: Already vectorizable, but called row-by-row via `purrr::partial()`

### Documentation
- Add vignette showing full workflow
- Add `@examples` with `\dontrun{}` for file I/O functions
- Move `janitor`, `zoo`, `fs` to `Suggests:` (only used in data-raw scripts)

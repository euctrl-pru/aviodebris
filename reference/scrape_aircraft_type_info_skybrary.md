# Scraper for EUROCONTROL's Skybrary aircraft info

Scraper for EUROCONTROL's Skybrary aircraft info

## Usage

``` r
scrape_aircraft_type_info_skybrary(ac_type, session)
```

## Arguments

- ac_type:

  ICAO aircraft type

- session:

  \`polite\` session

## Value

a dataframe of aircraft info

## Examples

``` r
if (FALSE) { # \dontrun{
host <- "https://skybrary.aero/"
session <- polite::bow(host, force = TRUE)
scraper <- purrr::partial(scrape_aircraft_type_info_skybrary, session = session)

 ac_types |>
  map(.f = scraper) |>
  bind_rows()
} # }
```

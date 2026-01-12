# Scraper for EUROCONTROL's Aircraft Performance DB

Scraper for EUROCONTROL's Aircraft Performance DB

## Usage

``` r
scrape_aircraft_type_info_perfdb(ac_type, session)
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
host <- "https://learningzone.eurocontrol.int/ilp/customs/ATCPFDB/details.aspx"
session <- polite::bow(host, force = TRUE)
scraper <- purrr::partial(scrape_aircraft_type_info_perfdb,
                          session = session)

ac_types |>
  map(.f = scraper) |>
  bind_rows()
} # }
```

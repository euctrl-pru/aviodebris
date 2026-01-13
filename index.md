# `aviodebris` package

The `aviodebris` package provides facilities for the writing of the DASC
2025 paper “Preparing for Potential Closure of European Airspaces due to
Re-entering Space Objects”

## Installation

You can install the development version of `aviodebris` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("espinielli/aviodebris")
```

## Data

Some of the datasets used in this repo/paper have been scraped from the
web. In particular aircraft type details have been collected from

1.  [EUROCONTROL Aircraft Performance
    DB](https://learningzone.eurocontrol.int/ilp/customs/ATCPFDB/details.aspx)
2.  [SkyBrary](https://skybrary.aero/)
3.  [doc8643](https://doc8643.com/)

The `screape_....R` scripts in `data-raw/` take care of this data
collection and rely on a list of ICAO aircraft types from the paper
data.

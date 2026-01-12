# reasample a day of trajectories at 30s

reasample a day of trajectories at 30s

## Usage

``` r
resample_traffic(day, interval = 30L)
```

## Arguments

- day:

  the date for the trajectories, it refers to the relevant parquet file
  in \`data-raw/trjs/trjs\_\<YYYY-MM-DD\>.parquet\`

- interval:

  number of seconds between samples \[30\]

## Value

a data frame of resampled points is saved in
\`data/trajectories\_\<YYYY-MM-DD\>\_resampled_30s.parquet\`

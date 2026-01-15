# resample a day of trajectories at interval

resample a day of trajectories at interval

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
\`data/trajectories\_\<YYYY-MM-DD\>\_resampled\_\<interval\>s.parquet\`

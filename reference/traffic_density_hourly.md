# Calculated the hourly traffic density per aircraft type

Calculated the hourly traffic density per aircraft type

## Usage

``` r
traffic_density_hourly(day, resolution = 3L, interval = 30L)
```

## Arguments

- day:

  the date to calculate the traffic density for; it refers to the file
  `trajectories_<YYYY-MM-DD>_resampled_<interval>s_bbox_res_<resolution>.parquet`
  in `data/`

- resolution:

  the resolution of the H3 hexagonal cells

- interval:

  resampling interval

## Value

save the result in a parquet file
`traffic_density_<YYYY-MM-DD>_hourly.parquet`

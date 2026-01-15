# assign H3 cell index at specified resolution to all trajectory point

Keep only portions in the BBOX of the study

## Usage

``` r
hexagonize_traffic(
  day,
  resolution = 3L,
  interval = 30L,
  bbox = c(xmin = -40.01297, ymin = 16.99059, xmax = 46.76206, ymax = 82.00901)
)
```

## Arguments

- day:

  the date for the trajectories, it refers to the relevant parquet file
  in
  \`data/trajectories\_\<YYYY-MM-DD\>\_resampled\_\<interval\>s.parquet\`

- resolution:

  the H3 resolution

- interval:

  resampling interval

- bbox:

  a bounding box with names \`xmin\`, \`xmax\`, \`ymin\` and \`ymax\`

## Value

a new parquet file as
\`data/trajectories\_\<YYYY-MM-DD\>\_resampled\_\<interval\>s_bbox_res\_\<resolution\>.parquet\`

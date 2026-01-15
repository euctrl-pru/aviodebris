# Calculate hourly collision and casualty risk expectation

Calculate hourly collision and casualty risk expectation

## Usage

``` r
collision_and_casualty_risk_expectation_hourly(day, resolution = 3L)
```

## Arguments

- day:

  date to calculate for

- resolution:

  H3 hex resolution

## Value

a dataframe with hourly `collision_expectation` and `casualty_risk`,
saved in a parquet file `collision_and_casualty_hourly.parquet`

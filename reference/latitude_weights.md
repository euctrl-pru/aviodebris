# latitude weight for a space object circularly orbiting at \`inclination\`

latitude weight for a space object circularly orbiting at
\`inclination\`

## Usage

``` r
latitude_weights(delta_lat, altitude, inclination_deg, n = 10001L)
```

## Arguments

- delta_lat:

  delta altitude \[decimal degrees\]

- altitude:

  altitude of the orbiting space object \[m\]

- inclination_deg:

  inclination \[decimal degrees\]

- n:

  number of samples of the orbit

## Value

a tibble of latitudes and values for the density

## Examples

``` r
if (FALSE) { # \dontrun{
rE  <- 6378e3 # Earth equatorial radius
alt <- 550e3 # altitude of satellite, i.e. 500 km for LEO
latitude_weights(0.5, rE + alt, 50)
} # }
```

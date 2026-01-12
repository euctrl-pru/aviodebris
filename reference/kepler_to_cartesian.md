# convert from orbital elements to Cartesian coordinates

convert from orbital elements to Cartesian coordinates

## Usage

``` r
kepler_to_cartesian(nu, a, e, i, Omega, w, M = 5.9722e+24, m = 0)
```

## Arguments

- nu:

  the true anomaly, i.e. the angular displacement measured from
  periapsis to the position vector along the direction of motion
  \[radians\]

- a:

  the semi-major axis of the orbit \[m\]

- e:

  the eccentricity of the orbit

- i:

  the inclination of the orbit \[radians\]

- Omega:

  the Right ascension of the ascending node \[radians\]

- w:

  the argument of perigee \[radians\]

- M:

  the mass of the first body (more massive one), i.e. Earth, \[kg\]

- m:

  the mass of the orbiting body (less massive), i.e. satellite, \[kg\].
  In the case of m \<\< M, m can be passed as 0

## Value

a vector with I, J, K, Vi, Vj, Vk of the values of position and velocity
in a geocentric-equatorial reference system

## Examples

``` r
if (FALSE) { # \dontrun{
rE  <- 6378e3 # Earth equatorial radius
alt <- 550e3 # altitude of satellite, i.e. 500 km for LEO
ecc <- 0 # circular orbit, i.e. when uncontrolled reentry of debris
m   <- 0 # mass of debris/satellite << mass Earth, so it can be 0
o   <- 0
w   <- 0
i <- 10
kepler_to_cartesian(
  nu = 0.5,
  a = rE + alt,
  e = ecc,
  i = i, Omega = o,
  w = w,
  m = m)
} # }
```

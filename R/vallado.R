# implementation of some of the algorithms in
# David Vallado "Fundamentals of Astrodynamics and Applications"

#' Find the position and velocity vectors in geocentric equatorial (ijk) system
#' given the classical orbit elements
#'
#' The additional orbital elements provide calculations for perfectly circular
#' and equatorial orbits.
#'
#' @param p semilatus rectum, `p`, [km]
#' @param ecc eccentricity, `e`, [unitless]
#' @param incl inclination, `i`, \eqn{[0, \pi]} [rad]
#' @param raan right ascension of ascending node, i.e. the longitude of
#'             the ascending node, `\eqn{\Omega}`
#' @param argp argument of perigee, `\eqn{\omega}`, \eqn{[0, 2\pi]} [rad]
#' @param nu true anomaly, `\eqn{\nu}`, \eqn{[0, 2\pi]} [rad]
#' @param arglat argument of latitude (for circular inclined orbit),
#'               \eqn{[0, 2\pi]} [rad]
#' @param truelon true longitude (for circular equatorial orbit),
#'               \eqn{[0, 2\pi]} [rad]
#' @param lonper longitude of periapsis (for elliptical equatorial orbit),
#'               \eqn{[0, 2\pi]} [rad]
#'
#' @returns a list of 2 vectors, `r` [km] and `v` [km/s] with named components
#'          `i`, `j`, and `k`, and `vi`, `vj` and `vk` respectively.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- 11067.790 |> units::set_units("km") # km
#' ecc <- 0.83285 |> units::as_units()
#' incl <- 87.87 |> units::set_units("degree")
#' raan <- 227.89 |> units::set_units("degree")
#' argp <- 53.38 |> units::set_units("degree")
#' nu <- 92.335 |> units::set_units("degree")
#'
#' coe2rv(p, ecc, incl, raan, argp, nu)
#' }
coe2rv <- function(
  p,
  ecc,
  incl,
  raan,
  argp,
  nu,
  arglat = NA,
  truelon = NA,
  lonper = NA
) {
  # determine what type of orbit is involved and set up the
  # angles for the special cases.

  if (ecc < units::set_units(small)) {
    # circular
    if (
      (incl < units::set_units(small, "rad")) ||
        (abs(incl - units::set_units(pi, "rad")) <
          units::set_units(small, "rad"))
    ) {
      # circular equatorial
      argp <- units::set_units(0.0, "rad")
      raan <- units::set_units(0.0, "rad")
      nu <- truelon
    } else {
      # circular inclined
      argp <- units::set_units(0.0, "rad")
      nu <- arglat
    }
  } else {
    # elliptical equatorial
    if (
      (incl < units::set_units(small, "rad")) ||
        (abs(incl - units::set_units(pi, "rad")) <
          units::set_units(small, "rad"))
    ) {
      argp <- lonper
      raan <- units::set_units(0.0, "rad")
    }
  }

  # PQW position and velocity vectors
  cosnu <- cos(nu)
  sinnu <- sin(nu)

  temp <- p / (units::set_units(1.0) + (ecc * cosnu))

  r_pqw <- temp * c(cosnu, sinnu, units::set_units(0.0))

  if (abs(p) < units::set_units(0.0001, "km")) {
    p = units::set_units(0.0001, "km")
  }

  v_pqw <- sqrt(mu / p) * c(-sinnu, (ecc + cosnu), units::set_units(0.0))

  # transform to IJK
  r_ijk <- rot3(r_pqw, -argp) |>
    rot1(-incl) |>
    rot3(-raan) |>
    `names<-`(c("i", "j", "k"))

  v_ijk <- rot3(v_pqw, -argp) |>
    rot1(-incl) |>
    rot3(-raan) |>
    `names<-`(c("vi", "vj", "vk"))

  return(
    list(r = r_ijk, v = v_ijk)
  )
}

#' Rotate a vector about the 1st axis
#'
#' @param vec a 3D vector to rotate
#' @param theta the angle of rotation [rad]
#'
#' @returns a 3D vector
#'
#' @examples
#' \dontrun{
#' c(0, 1, 0) |> rot1(pi/2)
#' }
rot1 <- function(vec, theta) {
  temp <- vec[3]
  c <- cos(theta)
  s <- sin(theta)

  outvec_3 <- c * vec[3] - s * vec[2]
  outvec_2 <- c * vec[2] + s * temp
  outvec_1 <- vec[1]

  c(outvec_1, outvec_2, outvec_3)

  # matrix(
  #   # fmt: skip
  #   c( 1,           0,           0,
  #      0,  cos(theta),  sin(theta),
  #      0, -sin(theta),  cos(theta)),
  #   nrow = 3
  # ) %*%
  #   matrix(vec, ncol = 1) |>
  #   as.vector()
}


#' Rotate a vector about the 3rd axis
#'
#' @inherit rot1
#' @returns a 3D vector
#'
#' @examples
#' \dontrun{
#' c(0, 1, 0) |> rot3(-pi/2)
#' }
rot3 <- function(vec, theta) {
  temp <- vec[2]
  c <- cos(theta)
  s <- sin(theta)

  outvec_2 <- c * vec[2] - s * vec[1]
  outvec_1 <- c * vec[1] + s * temp
  outvec_3 <- vec[3]

  c(outvec_1, outvec_2, outvec_3)

  # rotation about a generic axis
  # https://en.wikipedia.org/wiki/Rotation_matrix#Rotation_matrix_from_axis_and_angle
  # matrix(
  #   # fmt: skip
  #   c( cos(theta),  sin(theta), 0,
  #     -sin(theta),  cos(theta), 0,
  #               0,           0, 1),
  #   nrow = 3
  # ) %*%
  #   matrix(vec, ncol = 1) |>
  #   as.vector()
}


knorm <- function(x, k) {
  max(abs(x)) * (sum((abs(x) / max(abs(x)))^k))^(1 / k)
}

mag <- function(x) {
  knorm(x, 2)
}

#' convert a geocentric equatorial position vector into latitude and longitude
#'
#' This function finds both geodetic and geocentric latitude.
#'
#' @param r ECEF (I, J, K) position vector [km]
#'
#' @returns a list of the following named values
#'          * `latgc`: geocentric latitude of satellite, \eqn{[-\pi/2, \pi/2]} [rad]
#'          * `latgd`: geodetic latitude of satellite, \eqn{[-\pi/2, \pi/2]} [rad]
#'          * `lon`: longitude of satellite, \eqn{[-\pi, \pi]} [rad]
#'          * `hellp`: height above the ellipsoid, [km]
#' @export
#'
#' @examples
#' \dontrun{
#' r <- c(
#'   i = 6524.834,
#'   j = 6862.875,
#'   k = 6448.296
#' ) |>
#'   units::set_units("km")
#'
#' res <- ecef2ll(r)
#' }
ecef2ll <- function(r) {
  pi <- pi |> units::set_units("rad")
  small <- 0.00000001 # small value for tolerances
  re <- 6378.1363 |> units::set_units("km")
  eesqrd <- 0.006694385000 |> units::set_units() # eccentricity of earth sqrd

  magr <- mag(r)

  # find the longitude value
  names(r) <- NULL
  temp <- sqrt(r[1] * r[1] + r[2] * r[2])
  if (abs(temp) < units::set_units(small, "km")) {
    rtasc <- sign(r[3]) * pi * 0.5
  } else {
    rtasc = atan2(r[2], r[1]) |>
      units::set_units(NULL) |>
      units::set_units("rad")
  }

  lon <- rtasc

  if (abs(lon) >= pi) {
    if (lon < 0.0) {
      lon <- 2 * pi + lon
    } else {
      lon <- lon - 2 * pi
    }
  }

  decl <- asin(r[3] / magr)
  latgd <- decl

  # iterate to find geodetic latitude
  i <- 1
  olddelta <- latgd + units::set_units(10.0, "rad")

  while (
    (abs(olddelta - latgd) >= units::set_units(small, "rad")) && (i < 10)
  ) {
    olddelta = latgd
    sintemp <- sin(latgd)
    c <- re / (sqrt(units::set_units(1.0) - eesqrd * sintemp * sintemp))
    latgd <- atan((r[3] + c * eesqrd * sintemp) / temp)
    i <- i + 1
  }

  # calculate the height
  # if less of 1 degree
  if ((pi * 0.5 - abs(latgd)) < pi / 180.0) {
    hellp <- (temp / cos(latgd)) - c
  } else {
    s <- c * (units::set_units(1.0) - eesqrd)
    hellp <- r[3] / sin(latgd) - s
  }

  latgc <- asin(r[3] / magr)

  return(
    list(latgc = latgc, latgd = latgd, lon = lon, hellp = hellp)
  )
}

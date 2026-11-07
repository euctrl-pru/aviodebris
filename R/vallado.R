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
#' @param mu gravitational parameter, default to Earth's, [km^3/s^2]
#'
#' @returns a list of 2 vectors, `r` [km] and `v` [km/s] with named components
#'          `i`, `j`, and `k`, and `vi`, `vj` and `vk` respectively.
#' @export
#'
#' @examples
#' \dontrun{
#' p <- 11067.790 # km
#' ecc <- 0.83285
#' incl <- 87.87 * pi / 180 # rad
#' raan <- 227.89 * pi / 180 # rad
#' argp <- 53.38 * pi / 180
#' nu <- 92.335 * pi / 180
#'
#' coe2rv(p, ecc, incl, raan, argp, nu)
#' }
coe2rv <- function(
  p,
  ecc,
  incl,
  raan,
  argp,
  nu = 0.0, # rad
  arglat = 0.0, # rad
  truelon = 0.0, # rad
  lonper = 0.0, # rad
  mu = gravitational_constant_earth # km^3/s^2
) {
  # determine what type of orbit is involved and set up the
  # angles for the special cases.
  if (ecc < small) {
    # circular
    if ((incl < small) || (abs(incl - pi) < small)) {
      # circular equatorial
      argp <- 0.0
      raan <- 0.0
      nu <- truelon
    } else {
      # circular inclined
      argp <- 0.0
      nu <- arglat
    }
  } else {
    # elliptical equatorial
    if ((incl < small) || (abs(incl - pi) < small)) {
      argp <- lonper
      raan <- 0.0
    }
  }
  # Compute position and velocity in the perifocal coordinate system (PQW)
  cosnu <- cos(nu)
  sinnu <- sin(nu)

  temp <- p / (1.0 + (ecc * cosnu))
  r_pqw <- temp * c(cosnu, sinnu, 0.0)

  if (abs(p) < 0.0001) {
    p <- 0.0001
  }

  v_pqw <- sqrt(mu / p) * c(-sinnu, (ecc + cosnu), 0.0)

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

#' Converts position and velocity vectors into classical orbital elements.
#'
#' From Vallado, Algorithm 9
#'
#' @param r Position vector [km]
#' @param v Velocity vector [km/s]
#' @param mu gravitational parameter, default to Earth's, [km^3/s^2]
#'
#' @returns a list of named items, (`p`, `a`, `ecc`, `incl`, `raan`, `argp`,
#'          `nu`, `m`, `arglat`, `truelon`, `lonper`, `orbit_type`), where
#'          * `p`  = semilatus rectum [km]
#'          * `a` = semimajor axis [km]
#'          * `ecc` = eccentricity
#'          * `incl` = inclination [rad (0—2π)]
#'          * `raan` = right ascension of the ascending node [rad (0—2π)]
#'          * `argp` = argument of perigee [rad (0—2π)]
#'          * `nu` = true anomaly [rad (0—2π)]
#'          * `m` = mean anomaly [rad (0—2π)]
#'          * `arglat` = argument of latitude [rad (0—2π)]
#'          * `truelon` = true longitude [rad (0—2π)]
#'          * `lonper` = longitude of periapsis [rad (0—2π)]
#'          * `orbit_type` = type of orbit such as
#'             - "circular_equatorial"
#'             - "circular_inclined"
#'             - "eph_equatorial" (elliptical, parabolic, hyperbolic equatorial)
#'             - "eph_inclined" (elliptical, parabolic, hyperbolic inclined)
#'
#' @export
#' @examples
#' \dontrun{
#' # Vallado, 2022, Ex. 2-5
#' # Position and velocity in km and km/s
#' r = c(6524.834, 6862.875, 6448.296)
#' v = c(4.901327, 5.533756, -1.976341)
#' rv2coe(r = r, v = v)
#' }
rv2coe <- function(r, v, mu = gravitational_constant_earth) {
  # Initialize variables
  # p, a, ecc, incl, raan, argp, nu, m, arglat, truelon, lonper = (np.nan,) * 11
  p <- NA_real_
  a <- NA_real_
  ecc <- NA_real_
  incl <- NA_real_
  raan <- NA_real_
  argp <- NA_real_
  nu <- NA_real_
  m <- NA_real_
  arglat <- NA_real_
  truelon <- NA_real_
  lonper <- NA_real_
  # orbit_type = None
  orbit_type <- NA_character_
  # Make sure position and velocity vectors are numpy arrays
  # r, v = np.array(r), np.array(v)
  # Get magnitude of position and velocity vectors
  # r_mag, v_mag = np.linalg.norm(r), np.linalg.norm(v)
  r_mag <- r |> as.matrix() |> norm("F")
  v_mag <- v |> as.matrix() |> norm("F")
  # # Get angular momentum
  # h = np.cross(r, v)
  h <- pracma::cross(r, v)
  # h_mag = np.linalg.norm(h)
  h_mag <- h |> as.matrix() |> norm("F")
  # Elements are undefined for negative angular momentum
  # if h_mag < 0:
  #     return p, a, ecc, incl, raan, argp, nu, m, arglat, truelon, lonper, orbit_type
  if (h_mag < 0) {
    return
    list(
      p = p,
      a = a,
      ecc = ecc,
      incl = incl,
      raan = raan,
      argp = argp,
      nu = nu,
      m = m,
      arglat = arglat,
      truelon = truelon,
      lonper = lonper,
      orbit_type = orbit_type
    )
  }
  # Define line of nodes vector
  # n_vec = np.array([-h[1], h[0], 0])
  n_vec <- c(-h[2], h[1], 0.0)
  # n_mag = np.linalg.norm(n_vec)
  n_mag <- n_vec |> as.matrix() |> norm("F")
  # Get eccentricity vector
  # e_vec = ((v_mag**2 - mu / r_mag) * r - np.dot(r, v) * v) / mu
  e_vec <- ((v_mag^2 - mu / r_mag) * r - pracma::dot(r, v) * v) / mu
  # ecc = np.linalg.norm(e_vec)
  ecc <- e_vec |> as.matrix() |> norm("F")
  # find a, e, and p (semi-latus rectum)
  # sme = (v_mag**2 / 2) - (mu / r_mag)
  sme <- (v_mag^2 / 2) - (mu / r_mag)
  # a = -mu / (2 * sme) if abs(sme) > const.SMALL else np.inf
  if (abs(sme) > small) {
    a <- -mu / (2 * sme)
  } else {
    a <- Inf
  }

  # Semi-latus rectum
  # p = h_mag**2 / mu
  p <- h_mag^2 / mu
  # Find inclination
  # incl = np.arccos(h[2] / h_mag)
  incl <- acos(h[3] / h_mag)
  # Determine orbit type
  # orbit_type = utils.determine_orbit_type(ecc, incl, tol=const.SMALL)
  orbit_type <- determine_orbit_type(ecc, incl, tol = small)
  # Find right ascension of ascending node
  # if n_mag > const.SMALL:
  #     raan = np.arccos(np.clip(n_vec[0] / n_mag, -1, 1))
  #     if n_vec[1] < 0:
  #         raan = adjust_angle(raan)
  if (n_mag > small) {
    raan <- acos(clip(n_vec[1] / n_mag, -1, 1))
    if (n_vec[2] < 0) {
      raan <- adjust_angle(raan)
    }
  }
  # Find argument of periapsis
  # if orbit_type is utils.OrbitType.EPH_INCLINED:
  #     argp = angle(n_vec, e_vec)
  #     if e_vec[2] < 0:
  #         argp = adjust_angle(argp)
  if (orbit_type == "eph_inclined") {
    argp <- angle(n_vec, e_vec)
    if (e_vec[3] < 0) {
      argp <- adjust_angle(argp)
    }
  }
  # Find true anomaly at epoch
  # if orbit_type in [utils.OrbitType.EPH_INCLINED, utils.OrbitType.EPH_EQUATORIAL]:
  #     nu = angle(e_vec, r)
  #     if np.dot(r, v) < 0:
  #         nu = adjust_angle(nu)
  if (orbit_type %in% c("eph_inclined", "eph_equatorial")) {
    nu <- angle(e_vec, r)
    if (pracma::dot(r, v) < 0) {
      nu <- adjust_angle(nu)
    }
  }
  # Find argument of latitude (inclined cases)
  # if orbit_type in [utils.OrbitType.CIR_INCLINED, utils.OrbitType.EPH_INCLINED]:
  #     arglat = angle(n_vec, r)
  #     if r[2] < 0:
  #         arglat = adjust_angle(arglat)
  if (orbit_type %in% c("cir_inclined", "eph_inclined")) {
    arglat <- angle(n_vec, r)
    if (r[3] < 0) {
      arglat <- adjust_angle(arglat)
    }
  }
  # Find longitude of periapsis
  # if ecc > const.SMALL and orbit_type is utils.OrbitType.EPH_EQUATORIAL:
  #     lonper = np.arccos(np.clip(e_vec[0] / ecc, -1, 1))
  #     if e_vec[1] < 0:
  #         lonper = adjust_angle(lonper)
  #     if incl > np.pi / 2:
  #         lonper = adjust_angle(lonper)
  if (ecc > small && orbit_type == "eph_equatorial") {
    lonper <- acos(clip(e_vec[1] / ecc, -1, 1))
    if (e_vec[2] < 0) {
      lonper <- adjust_angle(lonper)
    }
    if (incl > (pi / 2)) {
      lonper <- adjust_angle(lonper)
    }
  }
  # Find true longitude
  # if r_mag > const.SMALL and orbit_type is utils.OrbitType.CIR_EQUATORIAL:
  #     truelon = np.arccos(np.clip(r[0] / r_mag, -1, 1))
  #     if r[1] < 0:
  #         truelon = adjust_angle(truelon)
  #     if incl > np.pi / 2:
  #         truelon = adjust_angle(truelon)
  if (r_mag > small && orbit_type == "cir_equatorial") {
    truelon <- acos(clip(r[1] / r_mag, -1, 1))
    if (r[2] < 0) {
      truelon <- adjust_angle(truelon)
    }
    if (incl > (pi / 2)) {
      truelon <- adjust_angle(truelon)
    }
  }
  # Find mean anomaly for eccentric orbits
  # if np.isreal(ecc):
  #     _, m = newtonnu(ecc, nu)
  if (is.double(ecc)) {
    m <- newtonnu(ecc, nu) |> magrittr::extract("m") |> as.numeric()
  }
  # return p, a, ecc, incl, raan, argp, nu, m, arglat, truelon, lonper, orbit_type
  return(list(
    p = p,
    a = a,
    ecc = ecc,
    incl = incl,
    raan = raan,
    argp = argp,
    nu = nu,
    m = m,
    arglat = arglat,
    truelon = truelon,
    lonper = lonper,
    orbit_type = orbit_type
  ))
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
#' )
#' res <- ecef2ll(r)
#' }
ecef2ll <- function(r) {
  small <- 1e-8 # small value for tolerances
  re <- 6378.1363 # km
  eesqrd <- 0.006694385000 # eccentricity of earth sqrd

  magr <- r |> as.matrix() |> norm("F") # km

  # find the longitude value
  names(r) <- NULL
  temp <- sqrt(r[1] * r[1] + r[2] * r[2])
  if (abs(temp) < small) {
    rtasc <- sign(r[3]) * pi * 0.5
  } else {
    rtasc <- atan2(r[2], r[1])
  }

  lon <- rtasc

  if (abs(lon) >= pi) {
    if (lon < 0.0) {
      lon <- lon + 2 * pi
    } else {
      lon <- lon - 2 * pi
    }
  }

  decl <- asin(r[3] / magr)
  latgd <- decl

  # iterate to find geodetic latitude
  i <- 1
  olddelta <- latgd + 10.0

  while ((abs(olddelta - latgd) >= small) && (i < 10)) {
    olddelta <- latgd
    sintemp <- sin(latgd)
    c <- re / (sqrt(1.0 - eesqrd * sintemp * sintemp))
    latgd <- atan((r[3] + c * eesqrd * sintemp) / temp)
    i <- i + 1
  }

  # calculate the height
  # if less of 1 degree
  if ((pi * 0.5 - abs(latgd)) < pi / 180.0) {
    hellp <- (temp / cos(latgd)) - c
  } else {
    s <- c * (1.0 - eesqrd)
    hellp <- r[3] / sin(latgd) - s
  }

  latgc <- asin(r[3] / magr)

  return(
    list(latgc = latgc, latgd = latgd, lon = lon, hellp = hellp)
  )
}


#' convert a geocentric equatorial position vector into latitude and longitude
#'
#' This function finds both geodetic and geocentric latitude.
#' Algorithm by Borkowski, K. M. 1989. "Accurate Algorithms to Transform
#' Geocentric to Geodetic Coordinates." Bulletin Geodesique Vol. 63: 50-56.
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
#' res <- ecef2llb(r)
#' }
ecef2llb <- function(r) {
  r <- r |> unname()
  # Constants
  third <- 1.0 / 3.0
  # Compute magnitude of the position vector
  magr <- r |> as.matrix() |> norm("F") # km

  # Earth semi-major and semi-minor axes
  a <- mean_equatorial_radius_earth
  b <- sign(r[3]) * a * sqrt(1.0 - squared_eccentricity_earth)
  # Compute longitude (right ascension approximation)
  names(r) <- NULL
  temp <- sqrt(r[1] * r[1] + r[2] * r[2])
  if (abs(temp) < small) {
    lon <- sign(r[3]) * pi * 0.5
  } else {
    lon <- atan2(r[2], r[1])
  }
  # Adjust longitude to be within [-2π, 2π] range
  if (abs(lon) >= pi) {
    if (lon < 0.0) {
      lon <- lon + 2 * pi
    } else {
      lon <- lon - 2 * pi
    }
  }

  # Intermediate variables for polynomial solution
  atemp <- 1 / (a * temp)
  e <- (b * r[3] - a * a + b * b) * atemp
  f <- (b * r[3] + a * a - b * b) * atemp
  p <- 4 * third * (e * f + 1.0)
  q <- 2 * (e * e - f * f)
  d <- p^3 + q^2
  # Solve the cubic equation based on the discriminant `d`
  if (d > 0.0) {
    nu <- (sqrt(d) - q)^third - (sqrt(d) + q)^third
  } else {
    sqrtp <- sqrt(-p)
    nu <- 2 * sqrtp * cos(third * acos(q / (p * sqrtp)))
  }
  # Intermediate variables for latitude and height computations
  g <- 0.5 * (sqrt(e^2 + nu) + e)
  t <- sqrt(g^2 + (f - nu * g) / (2 * g - e)) - g
  # Compute geodetic latitude and height above the ellipsoid
  latgd <- atan(a * (1.0 - t^2) / (2 * b * t))
  hellp <- (temp - a * t) * cos(latgd) + (r[3] - b) * sin(latgd)
  # Compute geocentric latitude
  latgc <- asin(r[3] / magr)
  return(
    list(latgc = latgc, latgd = latgd, lon = lon, hellp = hellp)
  )
}

interval_modulus <- function(x, a, b) {
  if (a == b) {
    return(x)
  }
  a + ((x - a) %% (b - a))
}


#' Determine the type of orbit based on eccentricity and inclination
#'
#' @param ecc the eccentricity of the orbit
#' @param incl the inclination of the orbit [rad]
#' @param tol small value for tolerance
#'
#' @returns a type of orbit such as
#'          - "circular_equatorial"
#'          - "circular_inclined"
#'          - "eph_equatorial" (elliptical, parabolic, hyperbolic equatorial)
#'          - "eph_inclined" (elliptical, parabolic, hyperbolic inclined)
determine_orbit_type <- function(ecc, incl, tol = small) {
  if (ecc < tol) {
    if ((incl < tol) || (abs(incl - pi) < tol)) {
      return
      "circular_equatorial"
    } else {
      return
      "circular_inclined"
    }
  } else if ((incl < tol) || (abs(incl - pi) < tol)) {
    return
    "eph_equatorial"
  } else {
    return
    "eph_inclined"
  }
}

# equivalent to numpy clip()
clip <- function(x, lower, upper) {
  pmax(pmin(x, upper), lower)
}

# # inspired by matlib
# angle <- function(x, y) {
#   acos(x %*% y / (len(x^2) * len(y^2))) |> as.numeric()
# }

#' Calculate the angle between two vectors in radians
#'
#' This function computes the angle between two vectors using the dot product
#' and the magnitudes of the vectors. The function handles cases where the dot
#' product might slightly exceed the interval [-1, 1] due to numerical
#' precision issues by clipping the value. If either vector has zero
#' magnitude, the function returns `NaN`to indicate that the angle is not
#' computable.
#'
#' @param v1 a vector
#' @param v2 a vector
#'
#' @returns the angle between two vectors [rad], NaN if either vector is zero
#'
#' @export
#' @examples
#' \dontrun{
#' }
angle <- function(v1, v2) {
  #  mag_v1 = np.linalg.norm(v1)
  mag_v1 <- v1 |> as.matrix() |> norm("F")
  #  mag_v2 = np.linalg.norm(v2)
  mag_v2 <- v2 |> as.matrix() |> norm("F")
  #  if mag_v1 * mag_v2 > SMALL**2:
  #      cos_angle = np.dot(v1, v2) / (mag_v1 * mag_v2)
  #      cos_angle = np.clip(cos_angle, -1, 1)  # keep cosine within domain
  #      return np.arccos(cos_angle)
  #  else:
  #      return np.nan
  if (mag_v1 * mag_v2 > small^2) {
    cos_angle <- pracma::dot(v1, v2) / (mag_v1 * mag_v2)
    # keep cosine within domain
    cos_angle <- clip(cos_angle, -1, 1)
  } else {
    cos_angle <- NaN
  }

  acos(cos_angle)
}


len <- function(X) {
  if (!is.numeric(X)) {
    stop("X must be numeric")
  }
  if (is.vector(X)) {
    X <- matrix(X, ncol = 1)
  }
  sqrt(colSums(X^2))
}

#' Solves for the eccentric anomaly and mean anomaly given the true anomaly
#'
#' This function solves Kepler's equation when the true anomaly is known.
#' The mean and eccentric, parabolic, or hyperbolic anomaly is also found.
#' The default parabolic limit at 168 deg is arbitrary.
#' The hyperbolic anomaly is also limited.
#' The hyperbolic sine is used because it's not double-valued.
#'
#' References:
#'   Vallado: 2022, p. 78, Algorithm 5
#'
#' @param ecc eccentricity of the orbit
#' @param nu true anomaly [rad]
#' @param parabolic_lim_deg paraboloic limit [deg], default is 168
#'
#' @returns list(e0, m)
#'            * `e0` = eccentric anomaly [rad]
#'            * `m`` = mean anomaly [rad]
#'
newtonnu <- function(ecc, nu, parabolic_lim_deg = 168) {
  e0 <- Inf
  m <- Inf

  if (abs(ecc) < small) {
    # Circular case
    e0 <- nu
    m <- nu
  } else if (ecc < (1 - small)) {
    # Elliptical case
    sine <- (sqrt(1 - ecc^2) * sin(nu)) / (1 + ecc * cos(nu))
    cose <- (ecc + cos(nu)) / (1 + ecc * cos(nu))
    e0 <- atan2(sine, cose)
    m <- e0 - ecc * sin(e0)
  } else if (ecc > (1 + small)) {
    # Hyperbolic case
    if ((ecc > 1) && (abs(nu) < (pi - acos(1 / ecc)))) {
      sine <- (sqrt(ecc^2 - 1) * sin(nu)) / (1 + ecc * cos(nu))
      e0 <- asinh(sine)
      m <- ecc * sinh(e0) - e0
    }
  } else {
    # Parabolic case``
    if (abs(nu) < (parabolic_lim_deg * pi / 180)) {
      e0 <- tan(nu / 2)
      m <- e0 + (e0^3) / 3
    }
  }

  # Update eccentric and mean anomaly to be within (0, 2pi) range
  if (ecc < 1) {
    m <- fmod(m, 2 * pi)
    if (m < 0) {
      m <- m + 2 * pi
    }
    e0 <- fmod(e0, 2 * pi)
  }

  return(list(e0 = e0, m = m))
}

#' Universal remainder function
#'
#' @description Given a [xmin,xmax[ or ]xmin,xmax] interval, this function
#' determines the remainder of each numeric relative to this interval. In other
#' words if the interval was repeated over the whole numeric domain, this
#' function determines where each value would be positioned in a given
#' repetition.
#'
#' @param x vector of floating point numbers
#' @param xmax,xmin the limits of the interval
#' @param bounds how to deal with boundaries (right- or left-open; '[['
#' or ']]')
#' @seealso \code{\link{incfix}}, \code{\link{dipfix}} and \code{\link{transphere}}
#' @examples
#' fmod(c(1260.23,360),360)
#'
#' fmod(c(1260.23,360),360,bounds = "]]")
#'
#' fmod(c(1260.23,360),360 + 180, 180)
#'
#' @export
fmod <- function(x, xmax, xmin = 0, bounds = "[[") {
  b <- rebound(bounds)

  if (b == "][" | b == "[]") {
    stop(paste(
      "The 'bounds' parameter should be '[[' or ']]' or equivalent",
      ", i.e. boundaries should be open only on one side",
      sep = ""
    ))
  }

  # res <- rep(NA,length(x))

  m <- xmax - xmin

  if (m <= 0) {
    stop("The 'xmin' parameter should be less than the 'xmax' one.")
  }

  f <- floor((x - xmin) / m)
  res <- ((x - xmin) - m * f) + xmin

  if (bounds == "]]") {
    res[x == xmax | x == xmin] <- xmax
  }

  return(res)
}


#' @title Simplifies boundary indicators for lim objects
#'
#' @description Simplifies boundary indicators for lim objects: from the wide
#' range supported by R ("[]", "[)", "(]", "()", "[[", "]]", "][", "open",
#' "closed", "left-open", "right-open", "left-closed", "right-closed") to "[]",
#' "[[", "]]" and "][" only
#'
#' @param b a vector of boundary indicators
#' @param na.errors whether to replace all other values by NA (rather
#' than simply stopping the function)
#' @return a simplified vector of boundary indicators (\code{"[]"}, \code{"[["},
#' \code{"]]"} and \code{"]["} only)
#'
#' @seealso \code{\link{as.lim}}
#'
#' @examples
#' bounds <- c("[]", "[)", "(]", "()",
#'             "[[", "]]", "][",
#'             "open", "closed",
#'             "left-open", "right-open",
#'             "left-closed", "right-closed")
#'
#' rebound(bounds)
#'
#' @export

rebound <- function(b, na.errors = F) {
  cond <- (isTRUE(na.errors) | isFALSE(na.errors))

  if (!cond) {
    stop("The 'na.errors' parameter should be T or F")
  }

  if (all(b == "[]" | b == "[[" | b == "]]" | b == "][" | is.na(b))) {
    return(b)
  }

  b[b == "closed"] <- "[]"
  b[b == "()" | b == "open"] <- "]["
  b[b == "[)" | b == "right-open" | b == "left-closed"] <- "[["
  b[b == "(]" | b == "left-open" | b == "right-closed"] <- "]]"

  if (na.errors) {
    b[!(b == "[]" | b == "][" | b == "[[" | b == "]]")] <- NA
    return(b)
  } else {
    if (!all(b == "[]" | b == "][" | b == "[[" | b == "]]")) {
      stop("Incorrect 'bounds' argument")
    } else {
      return(b)
    }
  }
}

# adjust angle by subtracting from 2π
adjust_angle <- function(ang) {
  2 * pi - ang
}

# implementation of some of the algorithms in
# David Vallado "Fundamentals of Astrodynamics and Applications"

#' Find the position and velocity vectors in geocentric equatorial (ijk) system
#' given the classical orbit elements
#'
#' The additional orbital elements provide calculations for perfectly circular
#' and equatorial orbits.
#'
#' @param p semilatus rectum, `p`, \[km\]
#' @param ecc eccentricity, `e`, \[unitless\]
#' @param incl inclination, `i`, \eqn{[0, \pi]} \[rad\]
#' @param raan right ascension of ascending node, i.e. the longitude of
#'             the ascending node, `\eqn{\Omega}`
#' @param argp argument of perigee, `\eqn{\omega}`, \eqn{[0, 2\pi]} \[rad\]
#' @param nu true anomaly, `\eqn{\nu}`, \eqn{[0, 2\pi]} \[rad\]
#' @param arglat argument of latitude (for circular inclined orbit),
#'               \eqn{[0, 2\pi]} \[rad\]
#' @param truelon true longitude (for circular equatorial orbit),
#'               \eqn{[0, 2\pi]} \[rad\]
#' @param lonper longitude of periapsis (for elliptical equatorial orbit),
#'               \eqn{[0, 2\pi]} \[rad\]
#' @param mu gravitational parameter, default to Earth's, \[km^3/s^2\]
#'
#' @returns a list with `r` \[km\] and `v` \[km/s\]. For a single orbit these are
#'          named vectors (`i`, `j`, `k` and `vi`, `vj`, `vk`); when any element has
#'          length n > 1 they are n x 3 matrices with those column names.
#'          All elements are recycled to a common length (1 or n).
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
  mu = GRAVITATIONAL_CONSTANT_EARTH # km^3/s^2
) {
  # recycle all element inputs to a common length (length 1 or n)
  els <- list(
    p = p,
    ecc = ecc,
    incl = incl,
    raan = raan,
    argp = argp,
    nu = nu,
    arglat = arglat,
    truelon = truelon,
    lonper = lonper
  )
  lens <- lengths(els)
  n <- max(lens)
  if (any(!lens %in% c(1L, n))) {
    cli::cli_abort(
      "Orbital elements must have length 1 or a common length {n}."
    )
  }
  els <- lapply(els, rep_len, length.out = n)

  # determine what type of orbit is involved and set up the
  # angles for the special cases, element-wise.
  circ <- els$ecc < SMALL
  equat <- (els$incl < SMALL) | (abs(els$incl - pi) < SMALL)

  # circular equatorial: nu <- truelon; circular inclined: nu <- arglat
  nu <- ifelse(circ & equat, els$truelon, ifelse(circ, els$arglat, els$nu))
  # circular: argp <- 0; elliptical equatorial: argp <- lonper
  argp <- ifelse(circ, 0.0, ifelse(equat, els$lonper, els$argp))
  # equatorial (circular or not): raan <- 0
  raan <- ifelse(equat, 0.0, els$raan)
  incl <- els$incl
  ecc <- els$ecc
  p <- els$p

  # Compute position and velocity in the perifocal coordinate system (PQW)
  cosnu <- cos(nu)
  sinnu <- sin(nu)

  temp <- p / (1.0 + (ecc * cosnu))
  r_pqw <- cbind(temp * cosnu, temp * sinnu, 0.0)

  p <- ifelse(abs(p) < 0.0001, 0.0001, p)
  vf <- sqrt(mu / p)
  v_pqw <- cbind(-vf * sinnu, vf * (ecc + cosnu), 0.0)

  # transform to IJK
  to_ijk <- function(m) {
    m |>
      rot3(-argp) |>
      rot1(-incl) |>
      rot3(-raan)
  }
  r_ijk <- to_ijk(r_pqw)
  v_ijk <- to_ijk(v_pqw)
  colnames(r_ijk) <- c("i", "j", "k")
  colnames(v_ijk) <- c("vi", "vj", "vk")

  # a single orbit keeps the named-vector return
  if (n == 1L) {
    return(list(r = r_ijk[1, ], v = v_ijk[1, ]))
  }
  list(r = r_ijk, v = v_ijk)
}

#' Converts position and velocity vectors into classical orbital elements.
#'
#' From Vallado, Algorithm 9
#'
#' @param r Position vector \[km\]: a 3-vector, or an n x 3 matrix (one per row)
#' @param v Velocity vector \[km/s\]: same shape as `r`
#' @param mu gravitational parameter, default to Earth's, \[km^3/s^2\]
#'
#' @returns a list of named length-n vectors (n = number of state vectors),
#'          (`p`, `a`, `ecc`, `incl`, `raan`, `argp`,
#'          `nu`, `m`, `arglat`, `truelon`, `lonper`, `orbit_type`), where
#'          * `p`  = semilatus rectum \[km\]
#'          * `a` = semimajor axis \[km\]
#'          * `ecc` = eccentricity
#'          * `incl` = inclination \[rad (0—2π)\]
#'          * `raan` = right ascension of the ascending node \[rad (0—2π)\]
#'          * `argp` = argument of perigee \[rad (0—2π)\]
#'          * `nu` = true anomaly \[rad (0—2π)\]
#'          * `m` = mean anomaly \[rad (0—2π)\]
#'          * `arglat` = argument of latitude \[rad (0—2π)\]
#'          * `truelon` = true longitude \[rad (0—2π)\]
#'          * `lonper` = longitude of periapsis \[rad (0—2π)\]
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
rv2coe <- function(r, v, mu = GRAVITATIONAL_CONSTANT_EARTH) {
  # one state vector per row; a plain 3-vector becomes a single row
  r <- matrix(r, ncol = 3)
  v <- matrix(v, ncol = 3)
  if (nrow(r) != nrow(v)) {
    cli::cli_abort("{.arg r} and {.arg v} must have the same number of rows.")
  }

  r_mag <- row_norm(r)
  v_mag <- row_norm(v)
  rdotv <- row_dot(r, v)

  # angular momentum and line of nodes
  h <- row_cross(r, v)
  h_mag <- row_norm(h)
  n_vec <- cbind(-h[, 2], h[, 1], 0.0)
  n_mag <- row_norm(n_vec)

  # eccentricity vector
  e_vec <- ((v_mag^2 - mu / r_mag) * r - rdotv * v) / mu
  ecc <- row_norm(e_vec)

  # semimajor axis (Inf for parabolic) and semilatus rectum
  sme <- (v_mag^2 / 2) - (mu / r_mag)
  a <- ifelse(abs(sme) > SMALL, -mu / (2 * sme), Inf)
  p <- h_mag^2 / mu

  incl <- acos(h[, 3] / h_mag)
  orbit_type <- determine_orbit_type(ecc, incl, tol = SMALL)
  ei <- orbit_type == "eph_inclined"
  ee <- orbit_type == "eph_equatorial"
  ci <- orbit_type == "circular_inclined"
  ce <- orbit_type == "circular_equatorial"

  # quadrant fix: 2 * pi - x where cond holds
  flip <- function(x, cond) ifelse(cond, adjust_angle(x), x)

  # right ascension of ascending node
  raan <- ifelse(
    n_mag > SMALL,
    flip(acos(clip(n_vec[, 1] / n_mag, -1, 1)), n_vec[, 2] < 0),
    NA_real_
  )
  # argument of periapsis
  argp <- ifelse(ei, flip(angle(n_vec, e_vec), e_vec[, 3] < 0), NA_real_)
  # true anomaly
  nu <- ifelse(ei | ee, flip(angle(e_vec, r), rdotv < 0), NA_real_)
  # argument of latitude (inclined cases)
  arglat <- ifelse(ci | ei, flip(angle(n_vec, r), r[, 3] < 0), NA_real_)
  # longitude of periapsis (eccentric equatorial)
  lonper <- ifelse(
    ecc > SMALL & ee,
    flip(
      flip(acos(clip(e_vec[, 1] / ecc, -1, 1)), e_vec[, 2] < 0),
      incl > (pi / 2)
    ),
    NA_real_
  )
  # true longitude (circular equatorial)
  truelon <- ifelse(
    r_mag > SMALL & ce,
    flip(
      flip(acos(clip(r[, 1] / r_mag, -1, 1)), r[, 2] < 0),
      incl > (pi / 2)
    ),
    NA_real_
  )

  # mean anomaly; nu is undefined (NA) for circular orbits, so m stays NA
  m <- rep(NA_real_, length(nu))
  has_nu <- !is.na(nu)
  m[has_nu] <- newtonnu(ecc[has_nu], nu[has_nu])$m

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


#' Rotate a vector about the 1st axis
#'
#' @param vec a 3D vector, or an n x 3 matrix with one vector per row
#' @param theta the angle of rotation \[rad\], length 1 or n
#'
#' @returns an n x 3 matrix of rotated vectors (one row per input vector)
#'
#' @examples
#' \dontrun{
#' c(0, 1, 0) |> rot1(pi/2)
#' }
rot1 <- function(vec, theta) {
  m <- matrix(vec, ncol = 3)
  c <- cos(theta)
  s <- sin(theta)

  cbind(
    m[, 1],
    c * m[, 2] + s * m[, 3],
    c * m[, 3] - s * m[, 2]
  )
}


#' Rotate a vector about the 3rd axis
#'
#' @inherit rot1
#'
#' @examples
#' \dontrun{
#' c(0, 1, 0) |> rot3(-pi/2)
#' }
rot3 <- function(vec, theta) {
  m <- matrix(vec, ncol = 3)
  c <- cos(theta)
  s <- sin(theta)

  cbind(
    c * m[, 1] + s * m[, 2],
    c * m[, 2] - s * m[, 1],
    m[, 3]
  )
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
#' @param r ECEF (I, J, K) position vector \[km\]
#'
#' @returns a list of the following named values
#'          * `latgc`: geocentric latitude of satellite, \eqn{[-\pi/2, \pi/2]} \[rad\]
#'          * `latgd`: geodetic latitude of satellite, \eqn{[-\pi/2, \pi/2]} \[rad\]
#'          * `lon`: longitude of satellite, \eqn{[-\pi, \pi]} \[rad\]
#'          * `hellp`: height above the ellipsoid, \[km\]
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
  SMALL <- 1e-8 # SMALL value for tolerances
  re <- 6378.1363 # km
  eesqrd <- 0.006694385000 # eccentricity of earth sqrd

  magr <- r |> as.matrix() |> norm("F") # km

  # find the longitude value
  names(r) <- NULL
  temp <- sqrt(r[1] * r[1] + r[2] * r[2])
  if (abs(temp) < SMALL) {
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

  while ((abs(olddelta - latgd) >= SMALL) && (i < 10)) {
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
#' @param r ECEF (I, J, K) position vector \[km\]
#'
#' @returns a list of the following named values
#'          * `latgc`: geocentric latitude of satellite, \eqn{[-\pi/2, \pi/2]} \[rad\]
#'          * `latgd`: geodetic latitude of satellite, \eqn{[-\pi/2, \pi/2]} \[rad\]
#'          * `lon`: longitude of satellite, \eqn{[-\pi, \pi]} \[rad\]
#'          * `hellp`: height above the ellipsoid, \[km\]
#' @export
#'
#' @examples
#' \dontrun{
#' r <- c(
#'   i = 6524.834,
#'   j = 6862.875,
#'   k = 6448.296
#' ) # km
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
  a <- MEAN_EQUATORIAL_RADIUS_EARTH # km
  b <- sign(r[3]) * a * sqrt(1.0 - SQUARED_ECCENTRICITY_EARTH)
  # Compute longitude (right ascension approximation)
  names(r) <- NULL
  temp <- sqrt(r[1] * r[1] + r[2] * r[2])
  if (abs(temp) < SMALL) {
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
#' @param incl the inclination of the orbit \[rad\]
#' @param tol SMALL value for tolerance
#'
#' @returns a type of orbit such as
#'          - "circular_equatorial"
#'          - "circular_inclined"
#'          - "eph_equatorial" (elliptical, parabolic, hyperbolic equatorial)
#'          - "eph_inclined" (elliptical, parabolic, hyperbolic inclined)
determine_orbit_type <- function(ecc, incl, tol = SMALL) {
  equat <- (incl < tol) | (abs(incl - pi) < tol)
  ifelse(
    ecc < tol,
    ifelse(equat, "circular_equatorial", "circular_inclined"),
    ifelse(equat, "eph_equatorial", "eph_inclined")
  )
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
#' product might slightly exceed the interval \[-1, 1\] due to numerical
#' precision issues by clipping the value. If either vector has zero
#' magnitude, the function returns `NaN`to indicate that the angle is not
#' computable.
#'
#' @param v1 a 3-vector or an n x 3 matrix
#' @param v2 a 3-vector or an n x 3 matrix
#'
#' @returns the row-wise angles \[rad\], NaN where either vector is zero
#'
#' @noRd
angle <- function(v1, v2) {
  mags <- row_norm(v1) * row_norm(v2)
  # keep cosine within domain; NaN if either vector is zero
  cos_angle <- ifelse(
    mags > SMALL^2,
    clip(row_dot(v1, v2) / mags, -1, 1),
    NaN
  )
  acos(cos_angle)
}

# row-wise vector helpers: inputs are 3-vectors or n x 3 matrices
row_dot <- function(a, b) {
  rowSums(matrix(a, ncol = 3) * matrix(b, ncol = 3))
}

row_norm <- function(a) {
  sqrt(row_dot(a, a))
}

row_cross <- function(a, b) {
  a <- matrix(a, ncol = 3)
  b <- matrix(b, ncol = 3)
  cbind(
    a[, 2] * b[, 3] - a[, 3] * b[, 2],
    a[, 3] * b[, 1] - a[, 1] * b[, 3],
    a[, 1] * b[, 2] - a[, 2] * b[, 1]
  )
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
#' @param ecc eccentricity of the orbit (vectorised with `nu`)
#' @param nu true anomaly \[rad\]
#' @param parabolic_lim_deg paraboloic limit \[deg\], default is 168
#'
#' @returns list(e0, m)
#'            * `e0` = eccentric anomaly \[rad\]
#'            * `m`` = mean anomaly \[rad\]
#'
newtonnu <- function(ecc, nu, parabolic_lim_deg = 168) {
  n <- max(length(ecc), length(nu))
  ecc <- rep_len(ecc, n)
  nu <- rep_len(nu, n)
  e0 <- rep(Inf, n)
  m <- rep(Inf, n)

  circ <- abs(ecc) < SMALL
  ell <- !circ & ecc < (1 - SMALL)
  hyp <- ecc > (1 + SMALL)
  par <- !circ & !ell & !hyp

  # Circular case
  e0[circ] <- nu[circ]
  m[circ] <- nu[circ]

  # Elliptical case
  if (any(ell)) {
    e <- ecc[ell]
    v <- nu[ell]
    sine <- (sqrt(1 - e^2) * sin(v)) / (1 + e * cos(v))
    cose <- (e + cos(v)) / (1 + e * cos(v))
    e0[ell] <- atan2(sine, cose)
    m[ell] <- e0[ell] - e * sin(e0[ell])
  }

  # Hyperbolic case, only within the asymptotes
  hyp_ok <- hyp & (abs(nu) < (pi - acos(1 / pmax(ecc, 1))))
  if (any(hyp_ok)) {
    e <- ecc[hyp_ok]
    v <- nu[hyp_ok]
    sine <- (sqrt(e^2 - 1) * sin(v)) / (1 + e * cos(v))
    e0[hyp_ok] <- asinh(sine)
    m[hyp_ok] <- e * sinh(e0[hyp_ok]) - e0[hyp_ok]
  }

  # Parabolic case, only below the parabolic limit
  par_ok <- par & (abs(nu) < (parabolic_lim_deg * pi / 180))
  if (any(par_ok)) {
    e0[par_ok] <- tan(nu[par_ok] / 2)
    m[par_ok] <- e0[par_ok] + (e0[par_ok]^3) / 3
  }

  # Update eccentric and mean anomaly to be within (0, 2pi) range.
  # Non-finite values (outside the hyperbolic/parabolic limits) stay Inf.
  wrap <- ecc < 1 & is.finite(m)
  if (any(wrap)) {
    m[wrap] <- fmod(m[wrap], 2 * pi)
    m[wrap] <- ifelse(m[wrap] < 0, m[wrap] + 2 * pi, m[wrap])
    e0[wrap] <- fmod(e0[wrap], 2 * pi)
  }

  list(e0 = e0, m = m)
}

#' Universal remainder function
#'
#' @description Given a `[xmin, xmax[` or `]xmin, xmax]` interval, this function
#' determines the remainder of each numeric relative to this interval. In other
#' words if the interval was repeated over the whole numeric domain, this
#' function determines where each value would be positioned in a given
#' repetition.
#'
#' @param x vector of floating point numbers
#' @param xmax,xmin the limits of the interval
#' @param bounds how to deal with boundaries (right- or left-open; `"[["`
#' or `"]]"`)
#' Adapted from `StratigrapheR::fmod()`.
#' @examples
#' fmod(c(1260.23,360),360)
#'
#' fmod(c(1260.23,360),360,bounds = "]]")
#'
#' fmod(c(1260.23,360),360 + 180, 180)
#'
#' @noRd
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
#' Adapted from `StratigrapheR::rebound()`.
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
#' @noRd

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

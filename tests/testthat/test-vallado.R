library(units)

deg <- pi / 180

test_that("coe2rv 2.6 (normal case)", {
  # Vallado, 2007, Ex. 2-6
  p <- 11067.790 # km
  ecc <- 0.83285
  incl <- 87.87 * deg # rad
  raan <- 227.89 * deg # rad
  argp <- 53.38 * deg # rad
  nu <- 92.335 * deg # rad

  # expected values from Python tests in
  # python/tests/astro/twobody/test_frame_conversions.py in repo
  # https://github.com/CelesTrak/fundamentals-of-astrodynamics/
  r_exp <- c(
    i = 6525.368120986091,
    j = 6861.531834896055,
    k = 6449.118614160162
  ) # km

  v_exp <- c(
    vi = 4.902278644574153,
    vj = 5.533139566279278,
    vk = -1.9757100987916154
  ) # km/s

  res <- coe2rv(
    p,
    ecc,
    incl,
    raan,
    argp,
    nu,
    arglat = NA_real_, # rad
    truelon = NA_real_, # rad
    lonper = NA_real_, # rad
  )

  expect_equal(res$r, r_exp)
  expect_equal(res$v, v_exp)
})


test_that("ecef2ll ex 3.3", {
  # Vallado, 2007, Ex. 3-3
  r <- c(
    i = 6524.834,
    j = 6862.875,
    k = 6448.296
  ) # km

  res <- ecef2ll(r)

  # expected values from Python tests in
  # python/tests/astro/twobody/test_frame_conversions.py in repo
  # https://github.com/CelesTrak/fundamentals-of-astrodynamics/
  expect_equal(res$latgc, 0.597826066235814) # rad
  expect_equal(res$latgd, 0.5995641464668334) # rad
  expect_equal(res$lon, 0.8106428999047803) # rad
  expect_equal(res$hellp, 5085.219430346959) # km
})

test_that("ecef2llb ex 3.3", {
  # Vallado, 2007, Ex. 3-3 using ecef2llb
  r <- c(
    i = 6524.834,
    j = 6862.875,
    k = 6448.296
  ) # km

  res <- ecef2llb(r)

  # expected values from Python tests in
  # python/tests/astro/twobody/test_frame_conversions.py in repo
  # https://github.com/CelesTrak/fundamentals-of-astrodynamics/
  expect_equal(res$latgc, 0.597826066235814) # rad
  expect_equal(res$latgd, 0.5995641464668334) # rad
  expect_equal(res$lon, 0.8106428999047803) # rad
  expect_equal(res$hellp, 5085.219430346959) # km
})

test_that("interval modulus works for angles and radians", {
  expect_equal(interval_modulus(180, -90, 90), 0)
  expect_equal(interval_modulus(120, -90, 90), -60)
  expect_equal(interval_modulus(1.25, 0, 1), 0.25)
  expect_equal(interval_modulus(pi / 3, -0.5 * pi, 0.5 * pi), pi / 3)
  expect_equal(interval_modulus(3 * pi / 4, -0.5 * pi, 0.5 * pi), -pi / 4)
  expect_equal(
    interval_modulus(
      set_units(180, "degree"),
      set_units(-0.5 * pi, "rad"),
      set_units(0.5 * pi, "rad")
    ),
    set_units(0, "rad")
  )
})

# Expected values from the Python tests in
# https://github.com/CelesTrak/fundamentals-of-astrodynamics/
# (software/python/tests/astro/twobody/), which check to 1e-10.
# Units: km, km/s, rad.

test_that("rv2coe ex 2.5", {
  r <- c(6524.834, 6862.875, 6448.296)
  v <- c(4.901327, 5.533756, -1.976341)
  res <- rv2coe(r, v)

  expect_equal(res$p, 11067.798350991814)
  expect_equal(res$a, 36127.337763974785)
  expect_equal(res$ecc, 0.8328533990836885)
  expect_equal(res$incl, 1.5336055626394494)
  expect_equal(res$raan, 3.9775750028016947)
  expect_equal(res$argp, 0.9317428111437854)
  expect_equal(res$nu, 1.6115524999414736)
  expect_equal(res$m, 0.1327277817291186)
  expect_equal(res$arglat, 2.5432953110852594)
  expect_true(is.na(res$truelon))
  expect_true(is.na(res$lonper))
  expect_equal(res$orbit_type, "eph_inclined")
})

test_that("coe2rv ex 5.5 (heliocentric)", {
  au <- 149597870.7 # km
  mu_sun <- 1.32712428e11 # km^3/s^2
  deg <- deg
  res <- coe2rv(
    5.190373 * au,
    0.048486,
    1.303382 * deg,
    100.454515 * deg,
    -86.135317 * deg,
    206.890953 * deg,
    mu = mu_sun
  )

  expect_equal(
    unname(res$r) / au,
    c(-4.080005705218284, -3.573871937183541, 0.10604294791849682)
  )
  expect_equal(
    unname(res$v) * 86400 / au,
    c(0.004882993701463404, -0.0053257663706772936, -8.726723359488976e-05)
  )
})

test_that("ecef2llb ex 3.3 (Borkowski values)", {
  r <- c(i = 6524.834, j = 6862.875, k = 6448.296)
  res <- ecef2llb(r)

  expect_equal(res$latgc, 0.597826066235814)
  expect_equal(res$latgd, 0.5995641464669065)
  expect_equal(res$lon, 0.8106428999047803)
  expect_equal(res$hellp, 5085.2194303451715)
})

test_that("newtonnu elliptical case", {
  res <- newtonnu(0.1, 45 * deg)
  expect_equal(res$e0 * 180 / pi, 41.078960346507934)
  expect_equal(res$m * 180 / pi, 37.31406335764441)
})

test_that("rv2coe - coe2rv roundtrip", {
  r <- c(i = 6524.834, j = 6862.875, k = 6448.296)
  v <- c(vi = 4.901327, vj = 5.533756, vk = -1.976341)
  res1 <- rv2coe(r, v)
  res2 <- coe2rv(
    p = res1$p,
    ecc = res1$ecc,
    incl = res1$incl,
    raan = res1$raan,
    argp = res1$argp,
    nu = res1$nu, # rad
    arglat = res1$arglat, # rad
    truelon = res1$truelon, # rad
    lonper = res1$lonper # rad
  )
  expect_equal(res2$r, r)
  expect_equal(res2$v, v)
})


# rv2coe orbit-type coverage ------------------------------------------------
# State vectors and expected elements (deg) from Vallado's Fortran test suite
# output, software/fortran/TESTASTF.OUT ("xx 150 ELORB" cases) in
# https://github.com/CelesTrak/fundamentals-of-astrodynamics/
# Inputs are printed to 7 decimals, so compare at 1e-6 relative tolerance.

test_that("rv2coe elliptical inclined (TESTASTF)", {
  s <- rv2coe(
    r = c(7253.7557112, -6718.9042768, -5453.4308709),
    v = c(5.1467799, 3.5740224, 0.3012640)
  )
  expect_equal(s$orbit_type, "eph_inclined")
  expect_equal(s$p, 12246.02301693, tolerance = 1e-6)
  expect_equal(s$ecc, 0.19999999, tolerance = 1e-6)
  expect_equal(unname(s$incl) / deg, 29.9999997, tolerance = 1e-6)
  expect_equal(unname(s$raan) / deg, 29.9999963, tolerance = 1e-6)
  expect_equal(unname(s$argp) / deg, 220.0000078, tolerance = 1e-6)
  expect_equal(unname(s$nu) / deg, 64.9999940, tolerance = 1e-6)
  expect_equal(unname(s$m) / deg, 45.5811664, tolerance = 1e-6)
})

test_that("rv2coe elliptical inclined retrograde (TESTASTF)", {
  s <- rv2coe(
    r = c(6736.5513062, -5709.0206792, -525.3690581),
    v = c(-4.7282517, -4.9753015, 1.1606611)
  )
  expect_equal(s$orbit_type, "eph_inclined")
  expect_equal(s$p, 9471.40649760, tolerance = 1e-6)
  expect_equal(s$ecc, 0.10000006, tolerance = 1e-6)
  expect_equal(unname(s$incl) / deg, 170.0000001, tolerance = 1e-6)
  expect_equal(unname(s$raan) / deg, 299.9999875, tolerance = 1e-6)
  expect_equal(unname(s$argp) / deg, 24.9999797, tolerance = 1e-6)
  expect_equal(unname(s$nu) / deg, 315.0000077, tolerance = 1e-6)
  expect_equal(unname(s$m) / deg, 322.6859477, tolerance = 1e-6)
})

test_that("rv2coe elliptical equatorial (TESTASTF)", {
  s <- rv2coe(
    r = c(-22739.1086596, -22739.1086596, 0),
    v = c(2.4851400, -2.0200411, 0)
  )
  expect_equal(s$orbit_type, "eph_equatorial")
  expect_equal(s$p, 26328.95469976, tolerance = 1e-6)
  expect_equal(s$ecc, 0.2, tolerance = 1e-6)
  expect_equal(unname(s$incl), 0)
  expect_equal(unname(s$nu) / deg, 205.0, tolerance = 1e-6)
  expect_equal(unname(s$m) / deg, 216.1768739, tolerance = 1e-6)
  expect_equal(unname(s$lonper) / deg, 20.0, tolerance = 1e-6)
  expect_true(is.na(s$raan))
  expect_true(is.na(s$argp))
})

test_that("rv2coe elliptical equatorial retrograde (TESTASTF)", {
  s <- rv2coe(
    r = c(28242.3662822, 2470.8868808, 0),
    v = c(0.6657522, -3.6253302, 0)
  )
  expect_equal(s$orbit_type, "eph_equatorial")
  expect_equal(s$p, 27152.11430028, tolerance = 1e-6)
  expect_equal(s$ecc, 0.1, tolerance = 1e-6)
  expect_equal(unname(s$incl) / deg, 180)
  expect_equal(unname(s$nu) / deg, 115.0, tolerance = 1e-6)
  expect_equal(unname(s$m) / deg, 104.2906161, tolerance = 1e-6)
  expect_equal(unname(s$lonper) / deg, 240.0, tolerance = 1e-6)
})

# The printed circular vectors give ecc ~ 1e-8 > SMALL (1e-10), so rv2coe
# treats them as eccentric. Check the type-independent elements here; the
# circular orbit types are covered by the exact round trips below.

test_that("rv2coe circular inclined (TESTASTF, near-circular inputs)", {
  s1 <- rv2coe(
    r = c(-2693.3455501, 6428.4342536, 4491.3778205),
    v = c(-3.9548471, -4.2809659, 3.7556710)
  )
  expect_equal(s1$p, 8291.64350005, tolerance = 1e-6)
  expect_equal(s1$ecc, 0, tolerance = 1e-7)
  expect_equal(unname(s1$incl) / deg, 50, tolerance = 1e-6)
  expect_equal(unname(s1$raan) / deg, 80, tolerance = 1e-6)
  expect_equal(unname(s1$arglat) / deg, 45, tolerance = 1e-6)

  s2 <- rv2coe(
    r = c(-7079.6883448, 3167.8771882, -2931.5386730),
    v = c(1.7760808, 6.2377093, 2.4513402)
  )
  expect_equal(s2$p, 8291.64350001, tolerance = 1e-6)
  expect_equal(s2$ecc, 0, tolerance = 1e-7)
  expect_equal(unname(s2$incl) / deg, 150, tolerance = 1e-6)
  expect_equal(unname(s2$raan) / deg, 115, tolerance = 1e-6)
  expect_equal(unname(s2$arglat) / deg, 315, tolerance = 1e-6)
})

test_that("rv2coe circular equatorial (TESTASTF, near-circular inputs)", {
  # true longitude = longitude of periapsis + true anomaly
  # `s1` (prograde, i = 0°)
  s1 <- rv2coe(
    r = c(6199.6905946, 13295.2793851, 0),
    v = c(-4.7242592, 2.2029583, 0)
  )
  expect_equal(s1$p, 14669.71769998, tolerance = 1e-6)
  expect_equal(s1$ecc, 0, tolerance = 1e-7)
  expect_equal(unname(s1$incl), 0)
  expect_equal(((s1$lonper + s1$nu) %% (2 * pi)) / deg, 65, tolerance = 1e-5)

  # `s2` (retrograde, i = 180°)
  s2 <- rv2coe(
    r = c(6199.6905946, -13295.2793851, 0),
    v = c(-4.7242592, -2.2029583, 0)
  )
  expect_equal(s2$p, 14669.71769998, tolerance = 1e-6)
  expect_equal(s2$ecc, 0, tolerance = 1e-7)
  expect_equal(unname(s2$incl) / deg, 180)
  expect_equal(((s2$lonper + s2$nu) %% (2 * pi)) / deg, 65, tolerance = 1e-5)
})

test_that("rv2coe round trip: circular inclined", {
  for (el in list(c(50, 80, 45), c(150, 115, 315))) {
    sv <- coe2rv(
      p = 8291.6435,
      ecc = 0,
      incl = el[1] * deg,
      raan = el[2] * deg,
      argp = 0,
      nu = 0,
      arglat = el[3] * deg
    )
    s <- rv2coe(sv$r, sv$v)
    expect_equal(s$orbit_type, "circular_inclined")
    expect_equal(s$p, 8291.6435)
    expect_equal(unname(s$incl) / deg, el[1])
    expect_equal(unname(s$raan) / deg, el[2])
    expect_equal(unname(s$arglat) / deg, el[3])
    expect_true(is.na(s$nu))
    expect_true(is.na(s$m))
  }
})

test_that("rv2coe round trip: circular equatorial", {
  for (incl in c(0, pi)) {
    sv <- coe2rv(
      p = 14669.7177,
      ecc = 0,
      incl = incl,
      raan = 0,
      argp = 0,
      nu = 0,
      truelon = 65 * deg
    )
    s <- rv2coe(sv$r, sv$v)
    expect_equal(s$orbit_type, "circular_equatorial")
    expect_equal(s$p, 14669.7177)
    expect_equal(unname(s$incl), incl)
    expect_equal(unname(s$truelon) / deg, 65)
    expect_true(is.na(s$raan))
    expect_true(is.na(s$nu))
  }
})

# Hyperbolic and parabolic orbits -------------------------------------------
# test_kepler.py has no orbital-element references, so these use the
# (ecc, m, nu) triples from test_newton.py::test_newtonm in
# https://github.com/CelesTrak/fundamentals-of-astrodynamics/
# For ecc >= 1, m is Vallado's hyperbolic/parabolic mean-anomaly analogue.

test_that("newtonnu hyperbolic and parabolic (test_newtonm inverse)", {
  expect_equal(newtonnu(1.5, 1.8916928725677562)$m, pi / 2)
  expect_equal(newtonnu(1.5, 1.8916928725677562)$e0, 1.449742150875263)
  expect_equal(newtonnu(1, 1.1949025292471007)$m, pi / 4)
  expect_equal(newtonnu(1, 1.1949025292471007)$e0, 0.6804016596037228)
})

test_that("rv2coe hyperbolic inclined via coe2rv (test_newtonm values)", {
  p <- 20000
  ecc <- 1.5
  nu <- 1.8916928725677562
  incl <- 30 * deg
  raan <- 40 * deg
  argp <- 50 * deg

  sv <- coe2rv(
    p = p,
    ecc = ecc,
    incl = incl,
    raan = raan,
    argp = argp,
    nu = nu
  )
  s <- rv2coe(sv$r, sv$v)

  expect_equal(s$orbit_type, "eph_inclined")
  expect_equal(s$p, p)
  expect_equal(s$ecc, ecc)
  expect_equal(s$a, p / (1 - ecc^2))
  expect_lt(s$a, 0)
  expect_equal(unname(s$incl), incl)
  expect_equal(unname(s$raan), raan)
  expect_equal(unname(s$argp), argp)
  expect_equal(unname(s$nu), nu)
  expect_equal(s$m, pi / 2)
})

test_that("rv2coe parabolic inclined via coe2rv (test_newtonm values)", {
  p <- 20000
  nu <- 1.1949025292471007
  ecc <- 1
  incl <- 30 * deg
  raan <- 40 * deg
  argp <- 50 * deg

  sv <- coe2rv(
    p = p,
    ecc = ecc,
    incl = incl,
    raan = raan,
    argp = argp,
    nu = nu
  )
  s <- rv2coe(sv$r, sv$v)

  expect_equal(s$orbit_type, "eph_inclined")
  expect_equal(s$p, p)
  expect_equal(s$ecc, ecc)
  # specific energy is ~0 but rounds to slightly nonzero, so |a| is huge
  expect_gt(abs(s$a), 1e9)
  expect_equal(unname(s$incl), incl)
  expect_equal(unname(s$raan), raan)
  expect_equal(unname(s$argp), argp)
  expect_equal(unname(s$nu), nu)
  expect_equal(s$m, pi / 4)
})

test_that("rv2coe hyperbolic equatorial via coe2rv", {
  ecc <- 1.5
  nu <- 1.8916928725677562
  lonper <- 70
  sv <- coe2rv(
    p = 20000,
    ecc = ecc,
    incl = 0,
    raan = 0,
    argp = 0,
    nu = nu,
    lonper = 70 * deg
  )
  s <- rv2coe(sv$r, sv$v)

  expect_equal(s$orbit_type, "eph_equatorial")
  expect_equal(s$ecc, ecc)
  expect_equal(unname(s$lonper) / deg, lonper)
  expect_equal(unname(s$nu), nu)
  expect_equal(s$m, pi / 2)
})

test_that("newtonnu hyperbolic (test_newtone case, corrected sign)", {
  # Upstream test_newtone expects nu = -2.7523; its newtone() drops the
  # minus sign on sin(nu) (Vallado Alg. 6), so the correct nu is positive.
  res <- newtonnu(1.05, 2.752304611108218)
  expect_equal(res$e0, 123.45 * pi / 180)
  expect_equal(res$m, 2.3123894903995055)
})

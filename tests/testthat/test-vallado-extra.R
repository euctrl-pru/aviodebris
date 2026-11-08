
# rv2coe orbit-type coverage ------------------------------------------------
# State vectors and expected elements (deg) from Vallado's Fortran test suite
# output, software/fortran/TESTASTF.OUT ("xx 150 ELORB" cases) in
# https://github.com/CelesTrak/fundamentals-of-astrodynamics/
# Inputs are printed to 7 decimals, so compare at 1e-6 relative tolerance.

deg <- pi / 180
rv2coe_deg <- function(x) rv2coe(x[1:3], x[4:6])

test_that("rv2coe elliptical inclined (TESTASTF)", {
  s <- rv2coe_deg(c(
    7253.7557112, -6718.9042768, -5453.4308709,
    5.1467799, 3.5740224, 0.3012640
  ))
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
  s <- rv2coe_deg(c(
    6736.5513062, -5709.0206792, -525.3690581,
    -4.7282517, -4.9753015, 1.1606611
  ))
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
  s <- rv2coe_deg(c(
    -22739.1086596, -22739.1086596, 0,
    2.4851400, -2.0200411, 0
  ))
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
  s <- rv2coe_deg(c(
    28242.3662822, 2470.8868808, 0,
    0.6657522, -3.6253302, 0
  ))
  expect_equal(s$orbit_type, "eph_equatorial")
  expect_equal(s$p, 27152.11430028, tolerance = 1e-6)
  expect_equal(s$ecc, 0.1, tolerance = 1e-6)
  expect_equal(unname(s$incl) / deg, 180)
  expect_equal(unname(s$nu) / deg, 115.0, tolerance = 1e-6)
  expect_equal(unname(s$m) / deg, 104.2906161, tolerance = 1e-6)
  expect_equal(unname(s$lonper) / deg, 240.0, tolerance = 1e-6)
})

# The printed circular vectors give ecc ~ 1e-8 > small (1e-10), so rv2coe
# treats them as eccentric. Check the type-independent elements here; the
# circular orbit types are covered by the exact round trips below.

test_that("rv2coe circular inclined (TESTASTF, near-circular inputs)", {
  s1 <- rv2coe_deg(c(
    -2693.3455501, 6428.4342536, 4491.3778205,
    -3.9548471, -4.2809659, 3.7556710
  ))
  expect_equal(s1$p, 8291.64350005, tolerance = 1e-6)
  expect_equal(s1$ecc, 0, tolerance = 1e-7)
  expect_equal(unname(s1$incl) / deg, 50, tolerance = 1e-6)
  expect_equal(unname(s1$raan) / deg, 80, tolerance = 1e-6)
  expect_equal(unname(s1$arglat) / deg, 45, tolerance = 1e-6)

  s2 <- rv2coe_deg(c(
    -7079.6883448, 3167.8771882, -2931.5386730,
    1.7760808, 6.2377093, 2.4513402
  ))
  expect_equal(s2$p, 8291.64350001, tolerance = 1e-6)
  expect_equal(s2$ecc, 0, tolerance = 1e-7)
  expect_equal(unname(s2$incl) / deg, 150, tolerance = 1e-6)
  expect_equal(unname(s2$raan) / deg, 115, tolerance = 1e-6)
  expect_equal(unname(s2$arglat) / deg, 315, tolerance = 1e-6)
})

test_that("rv2coe circular equatorial (TESTASTF, near-circular inputs)", {
  for (x in list(
    c(6199.6905946, 13295.2793851, 0, -4.7242592, 2.2029583, 0),
    c(6199.6905946, -13295.2793851, 0, -4.7242592, -2.2029583, 0)
  )) {
    s <- rv2coe_deg(x)
    expect_equal(s$p, 14669.71769998, tolerance = 1e-6)
    expect_equal(s$ecc, 0, tolerance = 1e-7)
    # true longitude = longitude of periapsis + true anomaly
    expect_equal(
      ((s$lonper + s$nu) %% (2 * pi)) / deg, 65,
      tolerance = 1e-5
    )
  }
  expect_equal(unname(rv2coe_deg(c(
    6199.6905946, 13295.2793851, 0, -4.7242592, 2.2029583, 0
  ))$incl), 0)
  expect_equal(unname(rv2coe_deg(c(
    6199.6905946, -13295.2793851, 0, -4.7242592, -2.2029583, 0
  ))$incl) / deg, 180)
})

test_that("rv2coe round trip: circular inclined", {
  for (el in list(c(50, 80, 45), c(150, 115, 315))) {
    sv <- coe2rv(8291.6435, 0, el[1] * deg, el[2] * deg, 0, 0,
      arglat = el[3] * deg)
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
    sv <- coe2rv(14669.7177, 0, incl, 0, 0, 0, truelon = 65 * deg)
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
  sv <- coe2rv(p, ecc, 30 * deg, 40 * deg, 50 * deg, nu)
  s <- rv2coe(sv$r, sv$v)

  expect_equal(s$orbit_type, "eph_inclined")
  expect_equal(s$p, p)
  expect_equal(s$ecc, ecc)
  expect_equal(s$a, p / (1 - ecc^2))
  expect_lt(s$a, 0)
  expect_equal(unname(s$incl) / deg, 30)
  expect_equal(unname(s$raan) / deg, 40)
  expect_equal(unname(s$argp) / deg, 50)
  expect_equal(unname(s$nu), nu)
  expect_equal(s$m, pi / 2)
})

test_that("rv2coe parabolic inclined via coe2rv (test_newtonm values)", {
  p <- 20000
  nu <- 1.1949025292471007
  sv <- coe2rv(p, 1, 30 * deg, 40 * deg, 50 * deg, nu)
  s <- rv2coe(sv$r, sv$v)

  expect_equal(s$orbit_type, "eph_inclined")
  expect_equal(s$p, p)
  expect_equal(s$ecc, 1)
  # specific energy is ~0 but rounds to slightly nonzero, so |a| is huge
  expect_gt(abs(s$a), 1e9)
  expect_equal(unname(s$nu), nu)
  expect_equal(s$m, pi / 4)
})

test_that("rv2coe hyperbolic equatorial via coe2rv", {
  p <- 20000
  ecc <- 1.5
  nu <- 1.8916928725677562
  sv <- coe2rv(p, ecc, 0, 0, 0, nu, lonper = 70 * deg)
  s <- rv2coe(sv$r, sv$v)

  expect_equal(s$orbit_type, "eph_equatorial")
  expect_equal(s$ecc, ecc)
  expect_equal(unname(s$lonper) / deg, 70)
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

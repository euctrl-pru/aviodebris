library(units)

test_that("coe2rv 2.6 (normal case)", {
  # Vallado, 2007, Ex. 2-6
  p <- 11067.790 # km
  ecc <- 0.83285
  incl <- 87.87 * pi / 180 # rad
  raan <- 227.89 * pi / 180 # rad
  argp <- 53.38 * pi / 180 # rad
  nu <- 92.335 * pi / 180 # rad

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
  deg <- pi / 180
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
  res <- newtonnu(0.1, 45 * pi / 180)
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

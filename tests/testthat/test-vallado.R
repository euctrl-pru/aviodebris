library(units)

test_that("coe3rv 2.6 (normal case)", {
  # Vallado, 2007, Ex. 2-6
  p <- 11067.790 |> set_units("km") # km
  ecc <- 0.83285 |> as_units()
  incl <- 87.87 |> set_units("degree")
  raan <- 227.89 |> set_units("degree")
  argp <- 53.38 |> set_units("degree")
  nu <- 92.335 |> set_units("degree")

  # expected values from Python tests in
  # python/tests/astro/twobody/test_frame_conversions.py in repo
  # https://github.com/CelesTrak/fundamentals-of-astrodynamics/
  r_exp <- c(
    i = 6525.368120986091,
    j = 6861.531834896055,
    k = 6449.118614160162
  ) |>
    set_units("km")
  v_exp <- c(
    vi = 4.902278644574153,
    vj = 5.533139566279278,
    vk = -1.9757100987916154
  ) |>
    set_units("km/s")

  res <- coe2rv(
    p,
    ecc,
    incl,
    raan,
    argp,
    nu,
    arglat = NA,
    truelon = NA,
    lonper = NA
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
  ) |>
    set_units("km")

  res <- ecef2ll(r)

  # expected values from Python tests in
  # python/tests/astro/twobody/test_frame_conversions.py in repo
  # https://github.com/CelesTrak/fundamentals-of-astrodynamics/
  expect_equal(res$latgc, set_units(0.597826066235814, "rad"))
  expect_equal(res$latgd, set_units(0.5995641464668334, "rad"))
  expect_equal(res$lon, set_units(0.8106428999047803, "rad"))
  expect_equal(res$hellp, set_units(5085.219430346959, "km"))
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
      set_units(0.5 * pi, "rad"),
      set_units(0.5 * pi, "rad")
    ),
    set_units(0.5 * pi, "rad"),
    set_units(0, "rad")
  )
})

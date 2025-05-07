test_that("coe3rv normal case", {
  # Vallado, 2007, Ex. 2-6
  p <- 11067.790 |> units::set_units("km") # km
  ecc <- 0.83285 |> units::as_units()
  incl <- 87.87 |> units::set_units("degree")
  raan <- 227.89 |> units::set_units("degree")
  argp <- 53.38 |> units::set_units("degree")
  nu <- 92.335 |> units::set_units("degree")

  # expected values from Python tests in
  # python/tests/astro/twobody/test_frame_conversions.py in repo
  # https://github.com/CelesTrak/fundamentals-of-astrodynamics/
  r_exp <- c(
    i = 6525.368120986091,
    j = 6861.531834896055,
    k = 6449.118614160162
  ) |>
    units::set_units("km")
  v_exp <- c(
    vi = 4.902278644574153,
    vj = 5.533139566279278,
    vk = -1.9757100987916154
  ) |>
    units::set_units("km/s")

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

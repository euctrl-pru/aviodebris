# Evidence-based tests for orbital mechanics functions

test_that("kepler_to_cartesian matches textbook values for circular equatorial orbit", {
  mu <- 3.986004418e14
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0
  i <- 0
  Omega <- 0
  w <- 0
  nu <- 0
  expected_pos <- c(i = a, j = 0, k = 0)
  expected_vel <- c(vi = 0, vj = sqrt(mu / a), vk = 0)
  result <- kepler_to_cartesian(nu, a, e, i, Omega, w)
  expect_equal(result["i"], expected_pos["i"], tolerance = 1e-6)
  expect_equal(result["j"], expected_pos["j"], tolerance = 1e-6)
  expect_equal(result["k"], expected_pos["k"], tolerance = 1e-6)
  expect_equal(result["vi"], expected_vel["vi"], tolerance = 1e-3)
  expect_equal(result["vj"], expected_vel["vj"], tolerance = 1e-3)
  expect_equal(result["vk"], expected_vel["vk"], tolerance = 1e-6)
})

test_that("latitude_weights for i=0 is a spike at equator", {
  rE <- 6378e3
  alt <- 500e3
  lw <- latitude_weights(0.5, rE + alt, 0)
  expect_equal(sum(lw$val > 0), 1)
  expect_equal(lw$lat[which.max(lw$val)], 0, tolerance = 0.5)
})

test_that("latitude_weights for i=90 is symmetric about poles", {
  rE <- 6378e3
  alt <- 500e3
  lw <- latitude_weights(0.5, rE + alt, 90)
  idx <- seq_len(nrow(lw) / 2)
  expect_equal(lw$val[idx], rev(lw$val[-idx]), tolerance = 1e-4)
  expect_equal(abs(lw$lat[which.max(lw$val)]), 90, tolerance = 0.5)
})

test_that("COE3RV test case from Vallado works", {
  # values from Vallado, 4th edition, Example 2-3, p. 128.
  nu = 92.335 |>
    set_units("degrees") |>
    # our API uses radians
    set_units("radians") |>
    as.numeric()
  p = 11067.790 |>
    set_units("km") |>
    # our API uses meters
    set_units("m") |>
    as.numeric()
  e = 0.83285
  a = p / (1 - e^2)
  i = 87.87 |> set_units("degrees")
  # our API uses radians
  i = i |> set_units("radians") |> as.numeric()
  Omega = 227.89 |>
    set_units("degrees") |>
    # our API uses radians
    set_units("radian") |>
    as.numeric()
  w = 53.38 |>
    set_units("degrees") |>
    # our API uses radians
    set_units("radians") |>
    as.numeric()
  m = 0 |> set_units("kg") |> as.numeric()

  expected_pos <- c(
    i = 6525.344 |> set_units("km"),
    j = 6861.535 |> set_units("km"),
    k = 6449.125 |> set_units("km")
  ) |>
    set_units("m") |>
    drop_units()

  expected_vel <- c(
    vi = 4.902276 |> set_units("km/s"),
    vj = 5.533124 |> set_units("km/s"),
    vk = -1.975709 |> set_units("km/s")
  ) |>
    set_units("m/s") |>
    drop_units()

  result <- kepler_to_cartesian(
    nu = nu,
    a = a,
    e = e,
    i = i,
    Omega = Omega,
    w = w,
    m = m
  )

  expect_equal(result["i"], expected_pos["i"], tolerance = 1e-3)
  expect_equal(result["j"], expected_pos["j"], tolerance = 1e-3)
  expect_equal(result["k"], expected_pos["k"], tolerance = 1e-3)

  expect_equal(result["vi"], expected_vel["vi"], tolerance = 1e-3)
  expect_equal(result["vj"], expected_vel["vj"], tolerance = 1e-3)
  expect_equal(result["vk"], expected_vel["vk"], tolerance = 1e-3)
})

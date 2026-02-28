# Evidence-based tests for orbital mechanics functions
library(units)

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

test_that("kepler_to_cartesian quarter-orbit position on circular equatorial
           orbit", {
  mu <- 3.986004418e14
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0
  i <- 0
  Omega <- 0
  w <- 0
  nu <- pi / 2
  result <- kepler_to_cartesian(nu, a, e, i, Omega, w)
  expect_equal(unname(result["i"]), 0, tolerance = 1e-6)
  expect_equal(unname(result["j"]), a, tolerance = 1e-6)
  expect_equal(unname(result["k"]), 0, tolerance = 1e-6)
  expect_equal(unname(result["vi"]), -sqrt(mu / a), tolerance = 1e-3)
  expect_equal(unname(result["vj"]), 0, tolerance = 1e-3)
  expect_equal(unname(result["vk"]), 0, tolerance = 1e-6)
})

test_that("kepler_to_cartesian inclined orbit at ascending node
           has zero K position", {
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0
  i <- pi / 4 # 45 degrees
  Omega <- 0
  w <- 0
  nu <- 0
  result <- kepler_to_cartesian(nu, a, e, i, Omega, w)
  expect_equal(unname(result["k"]), 0, tolerance = 1e-6)
  expect_gt(abs(unname(result["vk"])), 0) # velocity should have K component
})

test_that("kepler_to_cartesian half-orbit symmetry for circular orbit", {
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0
  i <- pi / 6
  Omega <- pi / 3
  w <- 0
  nu1 <- pi / 4
  nu2 <- nu1 + pi
  result1 <- kepler_to_cartesian(nu1, a, e, i, Omega, w)
  result2 <- kepler_to_cartesian(nu2, a, e, i, Omega, w)
  r1 <- sqrt(result1["i"]^2 + result1["j"]^2 + result1["k"]^2)
  r2 <- sqrt(result2["i"]^2 + result2["j"]^2 + result2["k"]^2)
  expect_equal(r1, r2, tolerance = 1e-6)
  expect_equal(result1["i"], -result2["i"], tolerance = 1e-6)
  expect_equal(result1["j"], -result2["j"], tolerance = 1e-6)
  expect_equal(result1["k"], -result2["k"], tolerance = 1e-6)
})

test_that("kepler_to_cartesian radius magnitude equals semi-major axis
           for circular orbit", {
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0
  i <- pi / 3
  Omega <- pi / 4
  w <- 0
  nus <- seq(0, 2 * pi, length.out = 20)
  for (nu in nus) {
    result <- kepler_to_cartesian(nu, a, e, i, Omega, w)
    r <- sqrt(result["i"]^2 + result["j"]^2 + result["k"]^2)
    expect_equal(unname(r), a, tolerance = 1e-6)
  }
})

test_that("kepler_to_cartesian radius at periapsis and apoapsis for eccentric orbit", {
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0.3
  i <- 0
  Omega <- 0
  w <- 0
  # periapsis
  result_peri <- kepler_to_cartesian(0, a, e, i, Omega, w)
  r_peri <- sqrt(result_peri["i"]^2 + result_peri["j"]^2 + result_peri["k"]^2)
  expect_equal(unname(r_peri), a * (1 - e), tolerance = 1e-6)
  # apoapsis
  result_apo <- kepler_to_cartesian(pi, a, e, i, Omega, w)
  r_apo <- sqrt(result_apo["i"]^2 + result_apo["j"]^2 + result_apo["k"]^2)
  expect_equal(unname(r_apo), a * (1 + e), tolerance = 1e-6)
})

test_that("kepler_to_cartesian velocity magnitude satisfies vis-viva equation", {
  mu <- 3.986004418e14
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0.2
  i <- pi / 4
  Omega <- 0
  w <- 0
  nus <- seq(0, 2 * pi, length.out = 20)
  for (nu in nus) {
    result <- kepler_to_cartesian(nu, a, e, i, Omega, w)
    r <- unname(sqrt(result["i"]^2 + result["j"]^2 + result["k"]^2))
    v <- unname(sqrt(result["vi"]^2 + result["vj"]^2 + result["vk"]^2))
    v_expected <- sqrt(mu * (2 / r - 1 / a))
    expect_equal(v, v_expected, tolerance = 1e-3)
  }
})

test_that("kepler_to_cartesian Omega rotation changes equatorial plane position", {
  rE <- 6378e3
  a <- rE + 500e3
  e <- 0
  i <- 0
  Omega1 <- 0
  Omega2 <- pi / 2
  w <- 0
  nu <- 0
  result1 <- kepler_to_cartesian(nu, a, e, i, Omega1, w)
  result2 <- kepler_to_cartesian(nu, a, e, i, Omega2, w)
  # rotation by 90° should swap I and J coordinates
  expect_equal(unname(result1["i"]), unname(result2["j"]), tolerance = 1e-6)
  expect_equal(unname(result1["j"]), -unname(result2["i"]), tolerance = 1e-6)
  expect_equal(unname(result1["k"]), unname(result2["k"]), tolerance = 1e-6)
})

test_that("latitude_weights sum to 1 for various inclinations", {
  rE <- 6378e3
  alt <- 500e3
  inclinations <- c(0, 30, 45, 60, 90)
  for (inc in inclinations) {
    lw <- latitude_weights(0.5, rE + alt, inc)
    expect_equal(sum(lw$val), 1, tolerance = 1e-6, label = paste("i =", inc))
  }
})

test_that("latitude_weights are bounded by inclination", {
  rE <- 6378e3
  alt <- 500e3
  inc <- 45
  lw <- latitude_weights(0.5, rE + alt, inc)
  nonzero_lats <- lw$lat[lw$val > 1e-6]
  expect_true(all(abs(nonzero_lats) <= inc + 0.5))
})

test_that("latitude_weights have higher density near turning latitudes", {
  rE <- 6378e3
  alt <- 500e3
  inc <- 45
  lw <- latitude_weights(0.5, rE + alt, inc)
  # find weights near inclination limit and near equator
  idx_limit <- which.min(abs(lw$lat - inc))
  idx_equator <- which.min(abs(lw$lat - 0))
  expect_gt(lw$val[idx_limit], lw$val[idx_equator])
})

test_that("latitude_weights converge with increasing sample size", {
  rE <- 6378e3
  alt <- 500e3
  inc <- 45
  lw1 <- latitude_weights(0.5, rE + alt, inc, n = 10001)
  lw2 <- latitude_weights(0.5, rE + alt, inc, n = 100001)
  expect_equal(lw1$val, lw2$val, tolerance = 1e-3)
})

test_that("latitude_weights are consistent across different resolutions", {
  rE <- 6378e3
  alt <- 500e3
  inc <- 45
  lw_fine <- latitude_weights(0.5, rE + alt, inc)
  lw_coarse <- latitude_weights(1.0, rE + alt, inc)
  # aggregate fine to coarse by grouping pairs of fine bins
  lw_fine_agg <- lw_fine |>
    dplyr::mutate(
      lat_coarse = lw_coarse$lat[findInterval(.data$lat, lw_coarse$lat - 0.5)]
    ) |>
    dplyr::summarise(val = sum(.data$val), .by = "lat_coarse") |>
    dplyr::filter(!is.na(.data$lat_coarse)) |>
    dplyr::arrange(.data$lat_coarse)
  lw_coarse_sorted <- lw_coarse |>
    dplyr::filter(.data$lat %in% lw_fine_agg$lat_coarse) |>
    dplyr::arrange(.data$lat)
  expect_equal(lw_fine_agg$val, lw_coarse_sorted$val, tolerance = 5e-3)
})

#' Calculate hourly collision and casuality risk expectation
#'
#' @param days dates to calculate for
#' @param resolution H3 hex resolution
#'
#' @returns save the result in a parquet file `collision_and_casuality_hourly.parquet`
#' @export
#'
collision_and_casuality_risk_expectation_hourly <- function(days, resolution) {
  hourly_weights_h3_resolution_3 <- here::here(
    "data",
    "weightings_h3__resolution_3_hourly_2000-01-01_2025-01-01.csv"
  ) |>
    readr::read_csv()

  ac_types <- readr::read_csv(here::here("data", "aircraft_type_info.csv"))
  effective_expose_area <- ac_types |>
    dplyr::mutate(
      eea = ((.data$cruise_tas_kt * .data$wing_span_m * .data$height_m) +
        (145 * .data$wing_span_m * .data$length_m)) /
        145,
      eea = dplyr::if_else(is.na(.data$eea), 1000, .data$eea),
      NULL
    ) |>
    dplyr::select("icao", "eea", "pax")

  arrow::read_parquet(
    here::here("data", "traffic_density_hourly.parquet")
  ) |>
    dplyr::filter(.data$h3_resolution == 3) |>
    dplyr::left_join(
      hourly_weights_h3_resolution_3,
      by = c("cell" = "h3_resolution_3")
    ) |>
    dplyr::left_join(effective_expose_area, by = c("aircraft_type" = "icao")) |>
    dplyr::mutate(
      eea = dplyr::if_else(is.na(.data$eea), 1000, .data$eea),
      pax = dplyr::if_else(is.na(.data$pax), 10, .data$pax)
    ) |>
    dplyr::mutate(
      collision_expectation = .data$.data$w * .data$occupancy * .data$eea,
      casuality_risk = .data$collision_expectation * .data$pax,
      .by = c("year", "month", "day", "hour", "cell", "aircraft_type")
    ) |>
    arrow::write_parquet(
      here::here("data", "collision_and_casuality_hourly.parquet"),
      compression = "gzip"
    )
}

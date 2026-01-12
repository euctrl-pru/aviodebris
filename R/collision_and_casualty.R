#' Calculate hourly collision and casualty risk expectation
#'
#' @param day date to calculate for
#' @param resolution H3 hex resolution
#'
#' @returns save the result in a parquet file `collision_and_casualty_hourly.parquet`
#' @export
#'
collision_and_casualty_risk_expectation_hourly <- function(
  day,
  resolution = 3L
) {
  date <- day |> lubridate::as_date()

  arrow::read_parquet(
    here::here(
      "data",
      stringr::str_glue(
        "traffic_density_{date}_res_{resolution}_hourly.parquet"
      )
    )
  ) |>
    dplyr::filter(.data$h3_resolution == resolution) |>
    dplyr::left_join(
      aviodebris::weightings_h3_resolution_3_hourly,
      by = c("cell" = stringr::str_glue("h3_resolution_{resolution}"))
    ) |>
    dplyr::summarise(
      # take the mean of the half degree values
      w = mean(.data$w),
      # just take the mean, but values should all be the same
      # this is done to keep the column
      occupancy = mean(.data$occupancy),
      density_m2 = mean(.data$density_m2),
      .by = c("year", "month", "day", "hour", "cell", "aircraft_type")
    ) |>
    dplyr::left_join(
      aviodebris::effective_exposed_area,
      by = c("aircraft_type" = "icao")
    ) |>
    dplyr::mutate(
      eea = dplyr::if_else(is.na(.data$eea), 500, .data$eea),
      pax = dplyr::if_else(is.na(.data$pax), 7, .data$pax)
    ) |>
    dplyr::summarise(
      collision_expectation = .data$w * .data$density_m2 * .data$eea,
      .by = c("year", "month", "day", "hour", "cell", "aircraft_type")
    ) |>
    dplyr::summarise(
      collision_expectation = sum(.data$collision_expectation),
      .by = c("year", "month", "day", "hour", "cell")
    ) |>
    arrow::write_parquet(
      here::here(
        "data",
        stringr::str_glue(
          "collision_and_casualty_{date}_res_{resolution}_hourly.parquet"
        )
      ),
      compression = "gzip"
    )
}

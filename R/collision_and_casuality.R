#' Calculate hourly collision and casuality risk expectation
#'
#' @param day date to calculate for
#' @param resolution H3 hex resolution
#'
#' @returns save the result in a parquet file `collision_and_casuality_hourly.parquet`
#' @export
#'
collision_and_casuality_risk_expectation_hourly <- function(
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
    dplyr::left_join(
      aviodebris::effective_expose_area,
      by = c("aircraft_type" = "icao")
    ) |>
    dplyr::mutate(
      eea = dplyr::if_else(is.na(.data$eea), 1000, .data$eea),
      pax = dplyr::if_else(is.na(.data$pax), 10, .data$pax)
    ) |>
    dplyr::mutate(
      collision_expectation = .data$w * .data$occupancy * .data$eea,
      casuality_risk = .data$collision_expectation * .data$pax,
      .by = c("year", "month", "day", "hour", "cell", "aircraft_type")
    ) |>
    arrow::write_parquet(
      here::here(
        "data",
        stringr::str_glue(
          "collision_and_casuality_{date}_res_{resolution}_hourly.parquet"
        )
      ),
      compression = "gzip"
    )
}

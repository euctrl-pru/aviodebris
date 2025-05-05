#' Calculated the hourly traffic density per aircraft type
#'
#' @param day the date to calculate the traffic density for;
#'            it refers to the file
#'            `trajectories_<YYYY-MM-DD>_resampled_<interval>s_bbox_res_<resolution>.parquet`
#'            in `data/`

#' @param resolution the resolution of the H3 hexagonal cells
#' @param interval resampling interval
#'
#' @export
#'
#' @returns save the result in a parquet file
#'          `traffic_density_<YYYY-MM-DD>_hourly.parquet`
#'
traffic_density_hourly <- function(day, resolution = 3L, interval = 30L) {
  date <- day |> lubridate::as_date()
  here::here(
    "data",
    stringr::str_glue(
      "trajectories_{date}_resampled_{interval}s_bbox_res_{resolution}.parquet",
      date = format(date, "%Y-%m-%d")
    )
  ) |>
    arrow::read_parquet() |>
    dplyr::summarise(
      # NOTE: of course 3600 are the seconds in 1 hour
      # occupancy of the cell is `interval` seconds the number of
      # trajectory points
      # (last point could be less than `interval` seconds away,
      # so we are slightly overestimating)
      occupancy = dplyr::n() * interval / 3600,
      density_m2 = .data$occupancy /
        h3jsr::cell_area(.data$cell, units = c("m2")),
      .by = c(
        .data$year,
        .data$month,
        .data$day,
        .data$hour,
        .data$h3_resolution,
        .data$cell,
        .data$aircraft_type
      )
    ) |>
    dplyr::arrange(.data$occupancy) |>
    arrow::write_parquet(
      here::here(
        "data",
        stringr::str_glue(
          "traffic_density_{date}_res_{resolution}_hourly.parquet",
          date = format(date, "%Y-%m-%d")
        )
      ),
      compression = "gzip"
    )
}

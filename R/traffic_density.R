#' Calculated the hourly traffic density per aircraft type
#'
#' @param days a vector of dates which correspond to parquet files
#'             in `data/trajectories_<YYYY-MM-DD>_resampled_<interval>s_bbox_res_<resolution>.parquet`

#' @param resolution the resolution of the H3 hexagonal cells
#' @param interval resampling interval
#'
#' @export
#'
#' @returns save the result in a parquet file `traffic_density_hourly.parquet`
#'
traffic_density_hourly <- function(days, resolution = 3L, interval = 30L) {
  date <- days |> lubridate::as_date()
  stringr::str_glue(
    "data/trajectories_{date}_resampled_{interval}s_bbox_res_{resolution}.parquet"
  ) |>
    purrr::map(.f = arrow::read_parquet) |>
    dplyr::bind_rows() |>
    dplyr::summarise(
      # NOTE: `30` is because of sampling at 30s
      # NOTE: of course 3600 are the seconds in 1 hour
      # occupancy of the cell is 30s the number of trajectory points
      # (last point could be less than 30s away, so we are slightly overestimating)
      occupancy = dplyr::n() * interval / 3600,
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
      here::here("data", "traffic_density_hourly.parquet"),
      compression = "gzip"
    )
}

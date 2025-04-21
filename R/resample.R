#' reasample a day of trajectories at 30s
#'
#' @param day the date for the trajectories, it refers to the relevant
#'            parquet file in `data-raw/trjs/trjs_<YYYY-MM-DD>.parquet`
#'
#' @param interval number of seconds between samples [30]
#'
#' @export
#'
#' @returns a data frame of resampled points is saved in
#'          `data/trajectories_<YYYY-MM-DD>_resampled_30s.parquet`
#'
resample_traffic <- function(day, interval = 30L) {
  date <- day |> lubridate::as_date()

  here::here(
    "data-raw",
    "trjs",
    stringr::str_glue("trjs_{date}.parquet", date = format(date, "%Y-%m-%d"))
  ) |>
    arrow::read_parquet() |>
    dplyr::select(-c("callsign", "icao24")) |>
    trrrj::resample(interval) |>
    arrow::write_parquet(here::here(
      "data",
      stringr::str_glue("trajectories_2023-09-01_resampled_{interval}s.parquet")
    ))
}

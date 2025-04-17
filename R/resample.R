#' reasample a day of trajectories at 30s
#'
#' @param day the date for the trajectories, it refers to the relevant
#'            parquet file in `data-raw/trjs/trjs_<YYYY-MM-DD>.parquet`
#'
#' @export
#'
#' @returns a data frame of resampled points is saved in
#'          `data/trajectories_<YYYY-MM-DD>_resampled_30s.parquet`
#'
resample_traffic <- function(day) {
  date <- day |> lubridate::as_date()
  stringr::str_glue(
    "data-raw/trjs/trjs_{date}.parquet",
    date = format(date, "%Y-%m-%d")
  ) |>
    arrow::read_parquet() |>
    dplyr::select(-c("callsign", "icao24")) |>
    trrrj::resample(30L) |>
    arrow::write_parquet(here::here(
      "data",
      "trajectories_2023-09-01_resampled_30s.parquet"
    ))
}

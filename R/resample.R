#' resample a day of trajectories at interval
#'
#' @param day the date for the trajectories, it refers to the relevant
#'            parquet file in `data-raw/trjs/trjs_<YYYY-MM-DD>.parquet`
#'
#' @param interval number of seconds between samples [30]
#'
#' @export
#' @noMd
#' @returns (invisibly) a data frame of resampled points. Called for its side
#'          effect of writing
#'          `data/trajectories_<YYYY-MM-DD>_resampled_<interval>s.parquet`
#'
resample_traffic <- function(day, interval = 30L) {
  date <- day |> lubridate::as_date() |> format("%Y-%m-%d")
  fi <- stringr::str_glue("trjs_{date}.parquet")
  fo <- stringr::str_glue("trajectories_{date}_resampled_{interval}s.parquet")

  out <- here::here("data-raw", "trjs", fi) |>
    arrow::read_parquet() |>
    trrrj::resample(interval)

  arrow::write_parquet(out, here::here("data", fo))
  invisible(out)
}

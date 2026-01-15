#' assign H3 cell index at specified resolution to all trajectory point
#'
#' Keep only portions in the BBOX of the study
#'
#' @param day the date for the trajectories, it refers to the relevant
#'            parquet file in `data/trajectories_<YYYY-MM-DD>_resampled_<interval>s.parquet`
#' @param resolution the H3 resolution
#' @param interval resampling interval
#' @param bbox a bounding box with names `xmin`, `xmax`, `ymin` and `ymax`
#'
#' @export
#'
#' @returns a new parquet file as
#'   `data/trajectories_<YYYY-MM-DD>_resampled_<interval>s_bbox_res_<resolution>.parquet`
#'
hexagonize_traffic <- function(
  day,
  resolution = 3L,
  interval = 30L,
  bbox = c(xmin = -40.01297, ymin = 16.99059, xmax = 46.76206, ymax = 82.00901)
) {
  date <- day |> lubridate::as_date() |> format("%Y-%m-%d")
  xmin <- bbox["xmin"] |> unname()
  xmax <- bbox["xmax"] |> unname()
  ymin <- bbox["ymin"] |> unname()
  ymax <- bbox["ymax"] |> unname()

  fn_in <- "trajectories_{date}_resampled_{interval}s.parquet" |>
    stringr::str_glue()
  fn_out <- "trajectories_{date}_resampled_{interval}s_bbox_res_{resolution}.parquet" |>
    stringr::str_glue()

  here::here("data", fn_in) |>
    arrow::read_parquet() |>
    # fmt: skip
    dplyr::filter(
        xmin <= .data$longitude, .data$longitude <= xmax,
        ymin <= .data$latitude,  .data$latitude  <= ymax,
      ) |>
    dplyr::mutate(
      hex = h3o::h3_from_xy(
        y = .data$latitude,
        x = .data$longitude,
        resolution = resolution
      ),
      hex = as.character(.data$hex)
    ) |>
    arrow::write_parquet(here::here("data", fn_out))
}

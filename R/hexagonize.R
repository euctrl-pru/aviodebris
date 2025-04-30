#' assign H3 cell index at specified resolution to all trajectory point
#'
#' Keep only portions in the BBOX of the study
#'
#' @param day the date for the trajectories, it refers to the relevant
#'            parquet file in `data/trajectories_<YYYY-MM-DD>_resampled_30s.parquet`
#' @param resolution the H3 resolution
#' @param interval resampling interval
#' @param bbox a bounding box with names `xmin`, `xmax`, `ymin` and `ymax`
#'
#' @export
#'
#' @returns a new parquet file such as
#'          `data/trajectories_<YYYY-MM-DD>_resampled_30s_bbox_res_<resolution>.parquet`
#'
hexagonize_traffic <- function(
  day,
  resolution = 3L,
  interval = 30L,
  bbox = c(xmin = -40.01297, ymin = 16.99059, xmax = 46.76206, ymax = 82.00901)
) {
  date <- day |> lubridate::as_date()
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  day <- lubridate::day(date)
  date <- date |> format("%Y-%m-%d")
  xmin = bbox["xmin"] |> unname()
  xmax = bbox["xmax"] |> unname()
  ymin = bbox["ymin"] |> unname()
  ymax = bbox["ymax"] |> unname()

  withr::local_envvar(c(TZ = "UTC", ORA_SDTZ = "UTC"))
  # con <- |>  withr::local_db_connection(DBI::dbConnect(duckdb(), path = ":memory:"))
  con <- DBI::dbConnect(duckdb::duckdb(), path = ":memory:")

  DBI::dbExecute(
    con,
    "SET extension_directory = '/Users/spi/.duckdb/extensions';"
  )
  DBI::dbExecute(con, "LOAD H3;")
  # fmt: skip
  query <- stringr::str_glue("
    CREATE OR REPLACE TABLE TRAJECTORY AS
    SELECT
      *,
      ROW_NUMBER() OVER(PARTITION BY flight_id ORDER BY timestamp) AS seq_id,
      date_part('day',   timestamp) AS day,
      date_part('month', timestamp) AS month,
      date_part('year',  timestamp) AS year,
      date_part('hour',  timestamp) AS hour,
      h3_h3_to_string(h3_latlng_to_cell(latitude, longitude, {resolution})) AS cell,
      {resolution} AS h3_resolution
    FROM
        'data/trajectories_{date}_resampled_{interval}s.parquet';
    ALTER TABLE TRAJECTORY DROP COLUMN sequence_id;
    ALTER TABLE TRAJECTORY RENAME seq_id TO sequence_id;
    COPY(
      SELECT
        *
      FROM
        TRAJECTORY
      WHERE
        -- filter on bounding box (all HEX cells at res 3 containing NM area)
        -- xmin = -27.04161, ymin = 25.98853,
        -- xmax =  46.60269, ymax = 72.31645
        (
          ({xmin} <= longitude AND longitude < {xmax})
            AND ({ymin} <= latitude AND latitude < {ymax})
        )
        AND (year = {year} AND month = {month} AND day = {day})
    )
    TO
      'data/trajectories_{date}_resampled_{interval}s_bbox_res_{resolution}.parquet'
    (FORMAT 'parquet')
    ;")

  DBI::dbExecute(con, query)
}

# ---- weighting function ----
# build reentry probability density from past debris reentries
# for a grid of 0.5 degrees on a hourly basis
library(conflicted)
library(tidyverse)
library(magrittr)
library(aviodebris)

conflicts_prefer(dplyr::filter)

# Earth equatorial radius
rE <- 6378e3
# altitude of satellite from (spherical) Earth surface, i.e. 500 km for LEO
alt <- 550e3

gcat <- reentries_gcat |>
  # not strictly needed, because there are unique rows
  filter(row_number() == 1, .by = c("jcat", "piece")) |>
  select(jcat, piece, inclination, reentry_date) |>
  mutate(
    id = str_c(jcat, "_", piece)
  )

fff <- purrr::partial(
  latitude_weights,
  delta_lat = 0.5,
  altitude = rE + alt
)

weightings_half_degree_latitude <- gcat |>
  pull(inclination) |>
  purrr::map(
    \(.x) fff(inclination_deg = .x)
  ) |>
  bind_cols() |>
  rename(lll = `lat...1`) |>
  select(-starts_with("lat")) |>
  mutate(
    vvv = rowSums(pick(starts_with("val"))),
    nnn = length(pick(starts_with("val"))),
    vvv = vvv / nnn
  ) |>
  select(-starts_with("val"), -nnn) |>
  rename(lat = "lll", val = "vvv")

# weightings_half_degree_latitude |>
#   write_csv(here::here(
#     "data",
#     "weightings_half_degree_latitude_2000-01-01_2025-01-01.csv"
#   ))

weightings_half_degree_latitude |>
  usethis::use_data(overwrite = TRUE)

#---- calculate weights per hex cell ----
library(here)
library(h3jsr)
library(sf)

# longitudes at half degrees
lons <- seq(0L, 719L) / 2
# interval of debris reentry considered
wef <- "2000-01-01" |> as_datetime()
til <- "2025-01-01" |> as_datetime()
hs <- difftime(til, wef, units = "hours") |> as.double()

weightings_half_degree_hourly <- weightings_half_degree_latitude |>
  mutate(lon = list(lons)) |>
  unnest(lon) |>
  mutate(
    # spread by longitude
    val = val / length(lons),
    # spread by hour
    val = val / hs,
    NULL
  ) |>
  relocate(lon, .before = lat)

weightings_half_degree_hourly |>
  usethis::use_data(overwrite = TRUE)

weightings_h3_resolution_3_hourly <- weightings_half_degree_hourly |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  point_to_cell(res = 3, simple = FALSE) |>
  # integrate (sum) over the ones in each cell and divide by the cell area
  reframe(
    w = mean(val),
    .by = h3_resolution_3
  ) |>
  as_tibble()

weightings_h3_resolution_3_hourly |>
  usethis::use_data(overwrite = TRUE)

# prepare aircraft type data
library(conflicted)
library(tidyverse)
library(here)
library(purrr)

conflict_prefer("filter", "dplyr", quiet = TRUE)

missing_cruise_speed <- readr::read_csv(here::here(
  "data-raw",
  "aircraft_types",
  "acts_missing_cruise_tas.csv"
))


#---- scrape aircraft info ----
if (FALSE) {
  library(arrow)
  library(aviodebris)
  library(polite)

  source(here::here("data-raw", "aircraft_types", "act_passengers.R"))

  # read traffic data and extract all aircraft types
  extract_aircraft_types <- function(fns, aircraft_type_columns) {
    fns |>
      map(.f = read_parquet) |>
      bind_rows() |>
      pull(aircraft_type_columns) |>
      unique() |>
      sort() |>
      setdiff("ZZZZ")
  }

  types_all <- fs::dir_ls(
    "~/repos/flight_density_space_debris/data/",
    regexp = "resampled_30s.parquet$"
  ) |>
    extract_aircraft_types("aircraft_type")

  # ---- EUROCONTROL Perfromance DB ----
  host <- "https://contentzone.eurocontrol.int/"
  session <- polite::bow(host, force = TRUE)
  scraper <- purrr::partial(scrape_aircraft_type_info_perfdb, session = session)

  # some aircraft types will be missing, i.e. helicopters
  acts_perfdb <- types_all |>
    map(.f = scraper) |>
    bind_rows()

  mising_acts_perfdb <- acts_perfdb |>
    filter(if_all(c(-icao), is.na)) |>
    pull(icao)

  acts_perfdb_clean <- acts_perfdb |>
    filter(!icao %in% mising_acts_perfdb) |>
    mutate(across(
      c(
        "mtow_kg",
        "cruise_tas_kt",
        "cruise_mach",
        "cruise_range_nm",
        "cruise_ceiling_fl"
      ),
      as.numeric
    )) |>
    left_join(all_pax) |>
    mutate(pax = crew + pax_max) |>
    select(-c("accomodation", "crew", "pax_min", "pax_typical", "pax_max")) |>
    write_csv("data/ectrl_acts.csv")

  # ---- SkyBrary aircraft pages ----
  host <- "https://skybrary.aero/"
  session <- polite::bow(host, force = TRUE)
  scraper <- purrr::partial(
    scrape_aircraft_type_info_skybrary,
    session = session
  )

  # scrape only missing type from perf DB
  acts_sky <- all_acts_missing |>
    map(.f = scraper) |>
    bind_rows()

  acts_sky_missing <- acts_sky |>
    filter(if_all(c(-icao), is.na)) |>
    pull(icao)

  acts_sky |>
    filter(!icao %in% acts_sky_missing) |>
    left_join(all_pax) |>
    mutate(pax = crew + pax_max) |>
    select(-c("accomodation", "crew", "pax_min", "pax_typical", "pax_max")) |>
    write_csv("data/sky_acts.csv")

  # ---- doc8643.com ----
  host <- "https://doc8643.com/"
  session <- polite::bow(host, force = TRUE)
  scraper <- purrr::partial(
    scrape_aircraft_type_info_doc8643,
    session = session
  )

  acts_doc8643 <- acts_sky_missing |>
    map(.f = scraper) |>
    bind_rows() |>
    mutate(
      type = case_when(
        icao == "C700" ~ "L2J",
        icao == "C68A" ~ "L2J",
        .default = .data$type
      )
    ) |>
    separate_wider_delim(
      manufacturer,
      " ",
      names = c("manufacturer", "other"),
      too_many = "drop"
    ) |>
    mutate(
      manufacturer = case_when(
        manufacturer == "AIR" ~ str_c(manufacturer, other, sep = " "),
        manufacturer == "AVIONES" ~ str_c(manufacturer, other, sep = " "),
        manufacturer == "GULFSTREAM" ~ str_c(manufacturer, other, sep = " "),
        other == "HELICOPTERS" & manufacturer == "AIRBUS" ~
          str_c(manufacturer, other, sep = " "),
        other == "GRUMMAN" & manufacturer == "NORTHROP" ~
          str_c(manufacturer, other, sep = " "),
        other == "MARTIN" & manufacturer == "LOCKHEED" ~
          str_c(manufacturer, other, sep = " "),
        .default = manufacturer
      )
    ) |>
    select(-other)

  acts_doc8643 |>
    left_join(all_pax) |>
    mutate(pax = crew + pax_max) |>
    select(-accomodation) |>
    write_csv("data/doc8643_acts.csv")
}

#---- unite scraped data ----
aircraft_types <- here::here(
  "data-raw",
  "aircraft_types",
  c(
    "ectrl_acts.csv",
    "sky_acts.csv",
    "doc8643_acts.csv"
  )
) |>
  purrr::map(.f = function(fn) {
    # fmt: skip
    cols <- cols(
      icao              = col_character(),
      name              = col_character(),
      manufacturer      = col_character(),
      type              = col_character(),
      wtc               = col_character(),
      recat_eu          = col_character(),
      mtow_kg           = col_double(),
      cruise_tas_kt     = col_double(),
      cruise_mach       = col_double(),
      cruise_range_nm   = col_double(),
      cruise_ceiling_fl = col_double(),
      wing_span_m       = col_double(),
      length_m          = col_double(),
      height_m          = col_double(),
      pax               = col_double()
    )
    read_csv(fn, col_types = cols)
  }) |>
  bind_rows() |>
  rows_patch(missing_cruise_speed) |>
  filter(is.na(cruise_tas_kt))

aircraft_types |>
  usethis::use_data(overwrite = TRUE)

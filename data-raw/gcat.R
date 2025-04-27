# extract debris reentry data from
# J. C. McDowell, “General Catalog of Artificial Space Objects.” 2020.
# Available: https://planet4589.org/space/gcat.

library(conflicted)
library(tidyverse)
library(here)
library(fs)

conflicts_prefer(dplyr::filter)
conflicts_prefer(purrr::map)

read_catalogue <- function(path) {
  fn <- fs::path_file(path) |> fs::path_ext_remove()
  gcat_file <- path
  gcat_date <- read_lines(gcat_file, skip = 1, n_max = 1) |>
    str_sub(start = 10L) |>
    as_datetime(format = "%Y %b  %e  %H%M:%S")

  gcat_col_names <- names(read_tsv(gcat_file, n_max = 0))
  gcat <- read_tsv(
    gcat_file,
    col_names = gcat_col_names,
    skip = 2,
    trim_ws = FALSE,
    quote = ""
  )
  gcat |>
    mutate(
      catalogue = fn,
      update_date = gcat_date
    )
}

# NOTE: use only
# * satcat	Standard Satellite Catalog (S) Objects from the US Satellite Catalog
# * auxcat	Auxiliary Satellite Catalog	(A) Objects omitted from the US catalog
# * tmpcat	Temporary Catalog	(T)	Objects expected to receive an S catalog number soon
gcat <- here("data-raw", "gcat", c("satcat.tsv", "auxcat.tsv", "tmpcat.tsv")) |>
  map(.f = read_catalogue) |>
  bind_rows() |>
  janitor::clean_names()


is_upper_stage <- function(type) {
  str_detect(type, "R1|R2|R3|R4|R5")
}

is_rocket_component <- function(type) {
  str_detect(type, "^C..[A|M].*$")
}

# 25 years of reentries
end_date <- '2025 Jan 01' |> as_datetime()
start_date <- '2000 Jan 01' |> as_datetime()

# try to determine (vague) d_date uncertainty
# uncertainty if "\?$"

# the specs say there could be date with hour 24 and minute 60
# I do not see any occurrences.
gcat |>
  select(number_jcat, d_date) |>
  filter(str_length(d_date) >= 19) |>
  mutate(
    hour = str_sub(d_date, 13L, 14L),
    minute = str_sub(d_date, 15L, 16L),
    second = str_sub(d_date, 18L, 19L) |> as.numeric()
  ) |>
  filter(minute == "60" | hour == "24" | second > 59)


# # fmt: skip
# vdate_test <- tribble(
#   ~precision,                          ~vdate, ~range,  ~width,
#   "Millisec",        "2016 Jun 8 2355:57.345", "2016 Jun 8 2355:57.345 to 2016 Jun 8 2355:57.346", "1ms",
#   "Second",              "2016 Jun 8 2355:57", "2016 Jun 8 2355:57.0   to 2016 Jun 8 2355:58.0",    "1s",
#   "Seconds",            "2016 Jun 8 2355:57?", "2016 Jun 8 2355:56.0   to 2355:59.0",               "3s",
#   "Minute",                 "2016 Jun 8 2355", "2016 Jun 8 2355:00     to 2016 Jun 8 2356:00",      "1m",
#   "Minutes",               "2016 Jun 8 2355?", "2016 Jun 8 2354:00     to 2016 Jun 8 2357:00",      "3m",
#   "Centiday",                 "2016 Jun 8.98", "2016 Jun 8 2331:12     to 2016 Jun 8 2345:36",   "0.01d",
#   "Centidays",               "2016 Jun 8.98?", "2016 Jun 8 2316:48     to 2016 Jun 9 0000:00",   "0.03d",
#   "Hour",                    "2016 Jun 8 23h", "2016 Jun 8 2300:00     to 2016 Jun 9 0000:00",      "1h",
#   "Hours",                     "2016 Jun 8.9", "2016 Jun 8 2136:00     to 2016 Jun 9 0000:00",    "2.4h",
#   "Day",                         "2016 Jun 8", "2016 Jun 8 0000        to 2016 Jun 9 0000",         "1d",
#   "Day (scheduled)",            "2016 Jun 8s", "2016 Jun 8 0000        to 2016 Jun 9 0000",         "1d",
#   "Days",                       "2016 Jun 8?", "2016 Jun 7 0000        to 2016 Jun 10 0000",        "3d",
#   "Month",                         "2016 Jun", "2016 Jun 1 0000        to 2016 Jul 1 0000",        "1mo",
#   "Months",                       "2016 Jun?", "2016 May 1 0000        to 2016 Aug 1 0000",        "3mo",
#   "Quarter",                        "2016 Q2", "2016 Apr 1 0000        to 2016 Jul 1 0000",        "3mo",
#   "Quarters",                      "2016 Q2?", "2016 Jan 1 0000        to 2016 Oct 1 0000",        "9mo",
#   "Year",                              "2016", "2016 Jan 1 0000        to 2017 Jan 1 0000",         "1y",
#   "Years",                            "2016?", "2015 Jan 1 0000        to 2018 Jan 1 0000",         "3y",
#   "Decade",                           "2010s", "2010 Jan 1 0000        to 2020 Jan 1 0000",        "10y",
#   "Decades",                         "2010s?", "2000 Jan 1 0000        to 2030 Jan 1 0000",        "30y",
#   "Century",                            "21C", "2001 Jan 1 0000        to 2101 Jan 1 0000",       "100y",
#   "Centuries",                         "21C?", "1901 Jan 1 0000        to 2201 Jan 1 0000",       "300y",
#   "Millenium",                           "3M", "2001 Jan 1 0000        to 3001 Jan 1 0000",      "1000y",
#   "Millenia",                           "3M?", "1001 Jan 1 0000        to 4001 Jan 1 0000",      "3000y"
# )
#
# vdate_test |>
#   mutate(
#     date_string = vdate,
#     uncertainty = str_sub(vdate, -1L, -1L) == "?",
#     date_length = str_length(date_string),
#     date_length = if_else(
#       uncertainty == TRUE,
#       date_length - 1,
#       date_length
#     ),
#     date_string = str_sub(date_string, 1L, date_length),
#     scheduled = str_sub(date_string, -1L, -1L) == "s",
#     date_length = if_else(
#       scheduled == TRUE,
#       date_length - 1,
#       date_length
#     ),
#     date_string = str_sub(date_string, 1L, date_length),
#     conficence_factor = case_when(
#       uncertainty ~ 3,
#       scheduled ~ 1,
#       .default = 1
#     ),
#     confidence_width = "1ms",
#     confidence_width = case_when(
#       date_length == 8 ~ dmonths(conficence_factor),
#       date_length <= 4 & scheduled ~ 10 * dyears(conficence_factor),
#       date_length <= 4 & !scheduled ~ dyears(conficence_factor),
#       .default = dmilliseconds(1)
#     )
#   )
#
# vdate_test |>
#   mutate(
#     uncertainty = str_sub(vdate, -1L, -1L) == "?",
#     scheduled = str_sub(vdate, -1L, -1L) == "s" |
#       str_sub(vdate, -2L, -2L) == 's',
#     resolution = "1ms",
#     string_date = case_when(
#       uncertainty == TRUE & scheduled == TRUE ~ str_sub(vdate, 1L, -3L),
#       uncertainty == TRUE | scheduled == TRUE ~ str_sub(vdate, 1L, -2L),
#       .default = vdate
#     ),
#     d_date_length = str_length(string_date),
#     reentry_date = case_when(
#       18 <= d_date_length ~
#         fast_strptime(string_date, format = "%Y %b %d %H%M:%OS"),
#       15 <= d_date_length & d_date_length <= 16 ~
#         fast_strptime(string_date, format = "%Y %b %d %H%M"),
#       14 <= d_date_length & d_date_length <= 15 ~
#         fast_strptime(string_date, format = "%Y %b %d %H"),
#       10 <= d_date_length & d_date_length <= 12 ~
#         fast_strptime(string_date, format = "%Y %b %d"),
#       d_date_length == 7 ~ as.Date(as.yearqtr(string_date, format = "%Y Q%q")),
#       d_date_length == 8 ~ fast_strptime(string_date, format = "%Y %b"),
#       d_date_length == 4 ~ fast_strptime(string_date, format = "%Y"),
#       .default = NA
#     ),
#     NULL
#   ) |>
#   mutate(
#     resolution = case_when(
#       # default resolution in gcat is '1s'
#       d_date_length == 22 ~ "ms",
#       d_date_length == 21 ~ "cs",
#       d_date_length == 20 ~ "ds",
#       d_date_length == 19 ~ "s",
#       16 <= d_date_length & d_date_length <= 17 ~ "min",
#       10 <= d_date_length & d_date_length <= 12 ~ "day",
#       d_date_length == 8 ~ "month",
#       d_date_length == 7 ~ "quarter",
#       d_date_length == 4 ~ "year",
#       .default = NA
#     ),
#     factor = case_when(
#       uncertainty == FALSE & scheduled == FALSE ~ 1,
#       uncertainty == FALSE & scheduled == TRUE & d_date_length == 4 ~ 10,
#       uncertainty == TRUE & scheduled == TRUE & d_date_length == 4 ~ 30,
#       uncertainty == FALSE & scheduled == TRUE ~ 1,
#       uncertainty == TRUE & scheduled == FALSE ~ 3,
#       uncertainty == TRUE & scheduled == TRUE ~ 30,
#       .default = 1
#     )
#   ) |>
#   View()

library(zoo)

gcat_base <- gcat |>
  # a reentry type
  filter(status == "R") |>
  mutate(
    string_date = str_sub(d_date, 1L, 11L),
    # fix wrongly encoded vague dates (reported to Jonathan)
    d_date = str_replace(d_date, pattern = "\\?\\?$", replacement = "\\?"),
  ) |>
  mutate(
    uncertainty = str_sub(d_date, -1L, -1L) == "?",
    scheduled = str_sub(d_date, -1L, -1L) == "s" |
      str_sub(d_date, -2L, -2L) == 's',
    resolution = "1ms",
    string_date = case_when(
      uncertainty == TRUE & scheduled == TRUE ~ str_sub(d_date, 1L, -3L),
      uncertainty == TRUE | scheduled == TRUE ~ str_sub(d_date, 1L, -2L),
      .default = d_date
    ),
    d_date_length = str_length(string_date),
    reentry_date = case_when(
      d_date_length == 19 ~
        fast_strptime(string_date, format = "%Y %b %d %H%M:%OS"),
      16 <= d_date_length & d_date_length <= 17 ~
        fast_strptime(string_date, format = "%Y %b %d %H%M"),
      10 <= d_date_length & d_date_length <= 12 ~
        fast_strptime(string_date, format = "%Y %b %d"),
      d_date_length == 7 ~ as.Date(as.yearqtr(string_date, format = "%Y Q%q")),
      d_date_length == 8 ~ fast_strptime(string_date, format = "%Y %b"),
      d_date_length == 4 ~ fast_strptime(string_date, format = "%Y"),
      .default = NA
    ),
    object = if_else(is_upper_stage(type), "upper stage", NA),
    object = if_else(is_rocket_component(type), "rocket component", object)
  ) |>
  # keep only reentries whih are 'upper stage' or 'rocket component'
  filter(!is.na(object)) |>
  mutate(
    resolution = case_when(
      # default resolution in gcat is '1s'
      d_date_length == 19 ~ "s",
      16 <= d_date_length & d_date_length <= 17 ~ "min",
      10 <= d_date_length & d_date_length <= 12 ~ "day",
      d_date_length == 8 ~ "month",
      d_date_length == 7 ~ "quarter",
      d_date_length == 4 ~ "year",
      .default = NA
    ),
    factor = case_when(
      uncertainty == FALSE & scheduled == FALSE ~ 1,
      uncertainty == FALSE & scheduled == TRUE & d_date_length == 4 ~ 10,
      uncertainty == TRUE & scheduled == TRUE & d_date_length == 4 ~ 30,
      uncertainty == FALSE & scheduled == TRUE ~ 1,
      uncertainty == TRUE & scheduled == FALSE ~ 3,
      uncertainty == TRUE & scheduled == TRUE ~ 30,
      .default = 1
    )
  ) |>
  rename(
    jcat = number_jcat,
    u = uncertainty,
    s = scheduled,
    r = resolution,
    f = factor
  ) |>
  mutate(
    # fmt: skip
    wef = case_when(
      (r == "year")    & (u == TRUE) & (s == TRUE) ~ reentry_date - years(10),
      (r == "year")    & (u == TRUE)               ~ reentry_date - years(1),
      (r == "quarter") & (u == TRUE)               ~ reentry_date - months(3),
      (r == "month")   & (u == TRUE)               ~ reentry_date - months(1),
      (r == "day")     & (u == TRUE)               ~ reentry_date - days(1),
      (r == "min")     & (u == TRUE)               ~ reentry_date - minutes(1),
      (r == "s")       & (u == TRUE)               ~ reentry_date - seconds(f),
      .default = reentry_date
    ),
    # fmt: skip
    til = case_when(
      (r == "day")     & (u == FALSE)              ~ reentry_date + days(1),
      (r == "day")     & (u == TRUE)               ~ reentry_date + days(1),
      (r == "month")   & (u == FALSE)              ~ reentry_date + months(1),
      (r == "month")   & (u == TRUE)               ~ reentry_date + months(2),
      (r == "quarter") & (u == TRUE)               ~ reentry_date + months(6),
      (r == "year")    & (u == TRUE) & (s == TRUE) ~ reentry_date + years(20),
      (r == "year")    & (u == TRUE)               ~ reentry_date + years(2),
      (r == "year")                                ~ reentry_date + years(1),
      (r == "min")     & (u == TRUE)               ~ reentry_date + minutes(2),
      (r == "min")     & (u == FALSE)              ~ reentry_date + minutes(1),
      (r == "s")       & (u == TRUE)               ~ reentry_date + seconds(2),
      (r == "s")       & (u == FALSE)              ~ reentry_date + seconds(1),
      .default = reentry_date
    ),
    NULL
  )


# keep the objects that reentered between start_date and end_date
reentries_gcat <- gcat_base |>
  filter(!(til < start_date), !(end_date < wef)) |>
  rename(inclination = `inc`) |>
  mutate(
    inclination = as.numeric(inclination),
    # normalize latitude to [-90, 90] range
    inclination = ((inclination + 90) %% 180) - 90,
    NULL
  ) |>
  select(jcat, piece, type, inclination, reentry_date, dry_mass)

reentries_gcat |>
  usethis::use_data(overwrite = TRUE)

#' Debris reentry data
#'
#' A subset of data from the following catalogues
#' * satcat	Standard Satellite Catalog (S) Objects from the US Satellite Catalog
#' * auxcat	Auxiliary Satellite Catalog	(A) Objects omitted from the US catalog
#' * tmpcat	Temporary Catalog	(T)	Objects expected to receive an S catalog number soon
#' consisting of only reentries occurred in the 25 years from
#' 2000-01-01 (included) till 2025-01-01 (excluded)
#' `satcat`, `auxcat` and `tmpcat` from
#' J. C. McDowell, “General Catalog of Artificial Space Objects.” 2020.
#'
#' @format ## `reentries_gcat`
#' A data frame with 1968 rows and 6 columns:
#' * `jcat`: A space object tag; a single prefix letter followed by a sequence
#'   number with 5 digits.
#'   Possible prefixes are:
#'   - `A` for auxcat (Auxiliary catalog)
#'   - `S` for stdcat (Standard catalog)
#'   - `T` for tmpcat (Temporary catalog)
#' * `piece`: The piece designation for the object
#' * `type`: This field tells you something about what kind of object:
#'   - `R1`--`R5`: Stage number for launch vehicle stage.
#'   - `C..[A|M]`: a component, i.e. a functional part of the spacecraft or
#'         rocket that is deliberately or accidentally ejected.
#' * `inclination`: the orbit inclination in the [-90, 90] interval.
#' * `reentry_date`: the date of reentry in ["vague date" format](https://planet4589.org/space/gcat/web/intro/vague.html).
#' * `dry_mass`: the (poosibly estimated) dry mass.
#'
#' @source <https://planet4589.org/space/gcat>
#'
"reentries_gcat"


#' Debris weighting function per latitude
#'
#' weighting functions for reentered debris in 25 years from
#' 2000-01-01 till 2025-01-01 (excluded), at half degree resolution.
#'
#' @format ## `weightings_half_degree_latitude`
#' A data frame with 360 rows (latitude from -90 to 90 at half degree)
#' and 2 columns:
#' * `lat`: the latitude.
#' * `val`: the value of the weighting function.
#'
"weightings_half_degree_latitude"


#' Debris weighting function for half degree grid
#'
#' Hourly weighting functions for reentered debris in 25 years from
#' 2000-01-01 till 2025-01-01 (excluded), at half degree resolution
#' worldwide.
#'
#' @format ## `weightings_half_degree_hourly`
#' A data frame with 360 * 720 rows and 3 columns:
#' * `lon`: the longitude.
#' * `lat`: the latitude.
#' * `val`: the value of the hourly weighting function.
#'
"weightings_half_degree_hourly"


#' Debris hourly density function per H3 hex cell at resolution 3
#'
#' Hourly weighting functions for reentered debris in 25 years from
#' 2000-01-01 till 2025-01-01 (excluded), for H3 cells at resolution 3.
#'
#' @format ## `weightings_half_degree_hourly`
#' A data frame with 360 * 720 rows and 1 column:
#' * `h3_resolution_3`: the H3 cell id
#' * `w`: the value of the hourly weighting function.
#'
"weightings_h3_resolution_3_hourly"


#' Aircraft type info
#'
#' @format ## `aircraft_types`
#' A data frame with 322 rows and 15 columns:
#' * `icao`: the ICAO aircraft type code.
#' * `name`: the aircraft type name.
#' * `manufacturer`: the manufacturer.
#' * `type`: the type, i.e. L2J for **L**andplane **2** **J**et engines craft
#' * `wtc`: Wake Turbulence Category
#' * `...`: the rest
"aircraft_types"


#' Effective Exposed Area
#'
#' @format ## `effective_expose_area`
#' A data frame with columns:
#' * `icao`: the ICAO aircraft type code.
#' * `eea`: the effective exposed area.
#' * `pax`: the max number of passengers.
"effective_expose_area"

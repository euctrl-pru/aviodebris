#' Approximate bounding box for NM area
#'
#' @export
#' @returns bounding box for NM area
bbox_nm <- function() {
  # bbox Europe/EUROCONTROL area derived from:
  # ms <- "BI|E.|L.|UD|UG|GM|UK|GC"
  # fir_ctry <- country_fir(
  #   firs = pruatlas::firs_nm_406,
  #   icao_id = "BI|E.|L.|UD|UG|GM|UK|GC",
  #   fl = 200,
  #   merge = TRUE
  # ) |>
  #   sf::st_transform(crs = sf::st_crs(3035))
  #
  # bbox <- fir_ctry |>
  #   sf::st_convex_hull() |>
  #   sf::st_buffer(1000) |>
  #   sf::st_transform(crs = sf::st_crs(4326)) |>
  #   sf::st_bbox()

  # -40.01297  16.99059  46.76206  82.00901
  # fmt: skip
  c(
    xmin = -40.01297,
    ymin =  16.99059,
    xmax =  46.76206,
    ymax =  82.00901
  )
}


#' H3 hexagons covering a bounding box.
#'
#' @param bbox bounding box to be covered by H3 hexagons
#' @param resolution H3 hexagons' resolution
#'
#' @export
#' @returns simple features of H3 hexagons polygons and relevant addresses
#'
hexes_for_bbox_at_res <- function(bbox, resolution) {
  bbox |>
    sf::st_bbox(crs = 4326) |>
    sf::st_as_sfc(crs = 4326) |>
    # increase points to the bbox polygon
    smoothr::densify(n = 300L) |>
    sf::st_as_sf(resolution = resolution) |>
    sf::st_transform(crs = 4326) |>
    h3jsr::polygon_to_cells(res = resolution, simple = FALSE) |>
    dplyr::pull(.data$h3_addresses) |>
    unlist() |>
    h3jsr::cell_to_polygon(simple = FALSE)
}


#' Return the bounding box of the H3 hexagons covering a user bounding box.
#'
#' @param bbox the user bounding box
#' @param resolution the resolution of the H3 hexagons covering `bbox`
#'
#' @export
#' @returns the bounding box containing the H3 hexagons covering `bbox`
#'
bbox_of_hexes_for_bbox_at_res <- function(bbox, resolution) {
  hexes_for_bbox_at_res(bbox, resolution) |>
    sf::st_union() |>
    sf::st_exterior_ring() |>
    sf::st_as_sf() |>
    dplyr::mutate(resolution = resolution)
}


#' Plot H3 hex map
#'
#' @param resolution cells resolution
#'
#' @returns a plot
#' @export
#'
#' @examples
#' \dontrun{
#' plot_hexes_map(2L)
#' }
plot_hexes_map <- function(resolution) {
  eur_hex <- bbox_nm() |>
    hexes_for_bbox_at_res(resolution = resolution)

  eur_hex_union <- bbox_nm() |>
    # hex union at res 2
    hexes_for_bbox_at_res(resolution = resolution) |>
    sf::st_union() |>
    sf::st_exterior_ring() |>
    sf::st_as_sf() |>
    dplyr::mutate(resolution = resolution)

  eur_centreoid <- eur_hex |>
    dplyr::pull(.data$h3_address) |>
    unlist() |>
    h3jsr::cell_to_point(simple = FALSE)

  gisco_RG <- giscoR::gisco_get_countries(
    resolution = "20",
    epsg = "4326",
    spatialtype = "RG",
    region = c("Europe", "Africa", "Asia")
  ) |>
    dplyr::mutate(res = "20M")

  gisco <- gisco_RG

  EUR_res20 <- gisco |>
    sf::st_intersection(eur_hex_union) |>
    dplyr::select(.data$res)

  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = EUR_res20,
      fill = 'lightgrey',
      linewidth = 0.4
    ) +
    ggplot2::geom_sf(
      data = eur_hex,
      fill = NA,
      colour = 'red',
      linewidth = 0.3
    ) +
    ggplot2::geom_sf(
      data = eur_centreoid,
      fill = NA,
      colour = 'blue',
      size = 0.05
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_sf(crs = "ESRI:102013", datum = NA)
}

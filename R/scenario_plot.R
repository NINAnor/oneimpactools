#' Plot vectors to used in a scenario
#'
#' @param vects List of vectors with points, lines, and polygons to be changed in
#' one or more scenarios.
#' @param add Add all plots in a single one? Only that is implemented. Alternatively,
#' it could be facets.
#' @param col_pts point colors
#'
#' Ideas: Add subset to conditions (only change == "remove")
#' Add all features of a scenario set, and highlight only the ones in a scenario
#'
#' @export
scenario_plot_vect <- function(vects, add = TRUE,
                               col_pts= "red", col_lin = "black", col_pol = "grey",
                               pal_pts = "cat", pal_lin = "div", pal_pol = "div",
                               crs = 25833,
                               ...) {

  # identify NAs
  isthere <- !is.na(vects)
  # identify tables with zero rows
  zero_rows <- sapply(vects, nrow)
  zero_rows <- sapply(zero_rows, function(x) ifelse(is.null(x), TRUE, x < 1))

  # update
  isthere <- isthere & !zero_rows

  lims <- sapply(vects[isthere], function(x) x |> sf::st_as_sf() |>
           st_bbox())
  lim_min <- apply(lims, 1, min)
  lim_max <- apply(lims, 1, max)
  e <- terra::ext(lim_min[1], lim_max[3], lim_min[2], lim_max[4]) |>
    st_bbox(crs = crs)

  # map
  m <- NULL

  # polygons
  if(isthere[3]) {
    m <- vects$pol |>
      sf::st_as_sf(crs = crs) |>
      tmap::tm_shape(bbox = e) +
      tmap::tm_borders(col = col_pol, palette = pal_pol, title.col = "Polygons")
  }

  # lines
  if(isthere[2]) {

    m2 <- vects$lin |>
      sf::st_as_sf(crs = crs) |>
      tmap::tm_shape(bbox = e) +
      tmap::tm_lines(col = col_lin, col.scale = tm_scale_categorical(pal_lin))

    # initialize
    if(is.null(m)) {
      m <- m2
    } else {
      # add to existing
      m <- m + m2
    }

  }

  # points
  if(isthere[1]) {

    m3 <- vects$pts |>
      sf::st_as_sf(crs = crs) |>
      tmap::tm_shape(bbox = e) +
      tmap::tm_dots(fill = col_pts, size = 0.3, col.scale = tm_scale_categorical(pal_pts))

    # initialize
    if(is.null(m)) {
      m <- m3
    } else {
      # add to existing
      m <- m + m3 + tm_layout(...)
    }
  }

  m
}

# scenario_plot_rast

#' Set the region in a GRASS GIS session
#'
#' To know more about the arguments, check: https://grass.osgeo.org/grass82/manuals/g.region.html
#'
#' @export
grass_set_region <- function(flags = "", ...) {
  rgrass::execGRASS("g.region", parameters = list(...), flags = flags)
}

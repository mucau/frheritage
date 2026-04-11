#' Retrieve INSEE department codes from an sf object
#'
#' This function retrieves the INSEE administrative department codes
#' (`code_insee`) intersecting the features of a given `sf` object.
#' It relies on the *Admin Express COG* WFS service provided by IGN,
#' accessed via the **happign** package.
#'
#' @param x An `sf` object defining the area(s) of interest.
#'
#' @return A `character` vector of unique INSEE department codes intersecting
#' the input geometries. Returns `NULL` if no intersection is found or if the
#' WFS request fails.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Transforms the input geometries to CRS:4326.
#'   \item Computes centroids of the geometries.
#'   \item Queries the IGN WFS service (*Admin Express COG*) to retrieve departments.
#'   \item Performs a spatial join using [sf::st_intersects()].
#'   \item Extracts and returns unique `code_insee` values.
#' }
#'
#' Notes:
#' \itemize{
#'   \item The function depends on the availability of the IGN WFS service.
#'   \item Large or complex geometries may cause the request to fail.
#'   \item In such cases, consider simplifying or reducing the spatial extent of `x`.
#' }
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'
#'   # Minimal sf object (point in Paris)
#'   x <- sf::st_sf(
#'     geometry = sf::st_sfc(
#'       sf::st_point(c(2.35, 48.85)),
#'       crs = 4326
#'     )
#'   )
#'
#'   # Retrieve department code
#'   get_deps(x)
#'
#'   # Multiple locations (Paris and Rouen)
#'   x2 <- sf::st_sf(
#'     geometry = sf::st_sfc(
#'       sf::st_point(c(2.35, 48.85)),  # 75
#'       sf::st_point(c(1.09, 49.44)),  # 76
#'       crs = 4326
#'     )
#'   )
#'
#'   get_deps(x2)
#'
#' }
#' }
#'
#' @importFrom sf st_transform st_centroid st_geometry st_sf st_join st_intersects
#' @importFrom happign get_wfs intersects
#'
#' @export
get_deps <- function(x) {
  # Coordinate transformation to CRS:4326
  x <- sf::st_transform(x, 4326)
  x <- quiet(sf::st_centroid(x))

  # Try to get departments safely
  dep <- tryCatch({
    happign::get_wfs(
      x = x,
      layer = "ADMINEXPRESS-COG-CARTO.LATEST:departement",
      predicate = happign::intersects()
    )
  }, error = function(e) {
    message("happign::get_wfs() failed: Please try to reduce the spatial size")
    return(NULL)
  })

  # If no departments were retrieved, return NA vector
  if (is.null(dep) || nrow(dep) == 0) {
    message("No departments intersect x: Please try to reduce the spatial size")
    return(NULL)
  }

  # Join spatially to get INSEE codes
  joined <- sf::st_join(sf::st_sf(sf::st_geometry(x)),
                        dep[, "code_insee", drop = FALSE],
                        join = sf::st_intersects, left = TRUE)
  unique(joined$code_insee)
}

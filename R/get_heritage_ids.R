#' Retrieve heritage layer IDs for a given sf object
#'
#' This function retrieves available layer identifiers from the French Ministry
#' of Culture's "Atlas du Patrimoine" GeoSource service feed, based on the
#' spatial extent and department(s) of a given `sf` object.
#'
#' @param x An `sf` object defining the area of interest.
#' @param buffer A `numeric` value (default = 2500). Buffer distance in meters
#' used to slightly expand geometries before querying.
#' @param crs An `integer` or `sf::st_crs` object (default = 2154). Coordinate
#' reference system used for spatial processing.
#' @param verbose Logical. If `TRUE` (default), prints progress and diagnostic messages.
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Aggregates nearby geometries using `buffer` input.
#'   \item Determines the corresponding INSEE department code for each geometry, using `happign::get_wfs()`.
#'   \item Computes the bounding box of each geometry.
#'   \item Queries the "Atlas du Patrimoine" GeoSource service feed for all available metadata
#'         records (IDs, titles, GUIDs) within each bounding box.
#' }
#' Progress is shown for each request.
#'
#' @return
#' A `data.frame` with the following columns:
#' \describe{
#'   \item{id}{Numeric identifier extracted from the record GUID.}
#'   \item{title}{Record title as published in the GeoSource service feed.}
#'   \item{guid}{Full GUID (unique resource identifier).}
#'   \item{code}{Internal code associated with the layer.}
#' }
#' Returns an empty `data.frame` if no records are found or the request fails.
#'
#' @examples
#' \dontrun{
#' # Retrieve available heritage records for an sf object
#' ids <- get_heritage_ids(my_sf_layer)
#' }
#'
#' @importFrom sf st_bbox
#'
#' @export
#'
get_heritage_ids <- function(x,
                             buffer = 2500,
                             crs = 2154,
                             verbose = TRUE) {

  # --- Step 1: Prepare input geometry ---
  geo_too_large(x, verbose = verbose)  # Stop if geometry is too large
  x <- geo_prepare(x, crs)
  y <- geo_aggregate_light(x, buffer)  # Light aggregation with buffer

  # --- Step 2: Compute extents and departments ---
  extents <- geo_extent(y)
  deps <- silent_run(geo_dep(x))
  if (verbose) {
    message(sprintf("%d department(s) detected.",
                    length(deps)))
  }

  # --- Step 3: Build request URLs (vectorized) ---
  urls <- mapply(
    ids_build_url,
    split(as.data.frame(extents), seq_len(nrow(extents))),
    deps,
    SIMPLIFY = TRUE
  )

  # --- Step 4: Download and parse metadata sequentially ---
  all_ids <- lapply(seq_along(urls), function(i) {
    ids_fetch(urls[i], dep[i], verbose)
  })

  # --- Step 5: Combine all results and remove empty entries ---
  combined <- do.call(rbind, Filter(NROW, all_ids))
  if (is.null(combined) || nrow(combined) == 0) {
    warning("No heritage identifiers were retrieved.")
    return(data.frame())
  }

  # --- Step 6: Remove duplicates and assign typological codes ---
  combined <- combined[!duplicated(combined$id), , drop = FALSE]
  combined$code <- ids_to_codes(combined$title)

  if (verbose) {
    message(sprintf("%d unique heritage ID(s) retrieved.",
                    nrow(combined)))
  }

  combined
}

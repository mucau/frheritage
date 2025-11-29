#' Retrieve and download heritage spatial data for a given sf object
#'
#' This function retrieves and downloads spatial heritage datasets from the
#' French Ministry of Culture’s "Atlas du Patrimoine" GeoSource service,
#' based on the spatial extent and department(s) of a given `sf` object.
#' It first identifies relevant dataset IDs and then downloads corresponding
#' shapefiles for each requested heritage code.
#'
#' @param x An `sf` object defining the area of interest.
#' @param data_code A `character` vector of heritage dataset codes to retrieve.
#' Valid codes can be obtained with [get_heritage_layernames()].
#' @param buffer A `numeric` value (default = 2500). Buffer distance in meters
#' used to slightly expand geometries before querying.
#' @param crs An `integer` or `sf::st_crs` object (default = 2154). Coordinate
#' reference system used for spatial processing.
#' @param spatial_filter A `character` string (default = `"intersects"`). Spatial
#' predicate to filter downloaded features.
#' @param verbose Logical. If `TRUE` (default), prints progress and diagnostic messages.
#'
#' @details
#' This functions only works for the 96 departments of metropolitan France.
#'
#' Internally, the function:
#' \enumerate{
#'   \item Validates the requested heritage codes.
#'   \item Checks the spatial filter.
#'   \item Prepares the geometry and aggregates nearby geometries using `buffer` input.
#'   \item Determines the corresponding INSEE department code for each geometry, using `happign::get_wfs()`.
#'   \item Computes the bounding box of each geometry.
#'   \item Filters layers ids for the requested `data_code` by using `frheritage::all_ids`.
#'   \item Builds the URL and downloads the zip archive containing the shapefiles.
#'   \item Reads and merges shapefiles into `sf` objects.
#' }
#'
#' @return
#' Either:
#' \itemize{
#'   \item A single `sf` object if one heritage code was processed.
#'   \item A named list of `sf` objects if multiple codes were processed.
#' }
#' Returns an empty result or stops with an informative error if no matching data is found.
#'
#' @examples
#' \dontrun{
#'   # Retrieve spatial data for a given sf geometry and selected heritage codes
#'   heritage_data <- get_heritage(
#'     x = my_sf_layer,
#'     data_code = c("IMMH", "SICI"),
#'     buffer = 2000,
#'     spatial_filter = "intersects"
#'   )
#' }
#'
#' @importFrom sf st_bbox st_crs
#'
#' @export
#'
get_heritage <- function(x,
                         data_code,
                         buffer = 2500,
                         crs = 2154,
                         spatial_filter = "intersects",
                         verbose = TRUE) {

  # Step 1: Validate inputs
  data_check(data_code)
  if (length(data_code) != 1L)
    stop("`data_code` must be a single heritage code.")

  spatial_filter <- geo_spatial_check(spatial_filter)

  # Step 2: Prepare input geometry
  geo_too_large(x, verbose = verbose)
  y <- geo_prepare(x, crs = crs, buffer = buffer)

  # Step 3: Compute extent & department
  extents <- geo_extent(y)
  if (is.null(extents) || length(extents) != 4L)
    stop("Invalid extent.")

  deps <- silent_run(geo_dep(x))

  # Step 4: Filter metadata for this code
  ids <- data_filter(department = deps, data_code = data_code)

  if (nrow(ids) == 0L)
    stop("No matching IDs found for code ", data_code, ".")

  # Step 5: Only rows corresponding to the code
  code_rows <- ids[ids$code == data_code, , drop = FALSE]

  if (nrow(code_rows) == 0L)
    stop("No matching IDs found for code ", data_code, ".")

  if (verbose) message("\nProcessing code ", data_code)

  # Utility: download + read 1 row
  process_row <- function(row) {
    url <- zip_query_build(
      id = row$id,
      title = row$title,
      guid = row[["guid"]],
      extent_vals = extents,
      crs = crs
    )

    if (verbose) message("Requesting ID ", row$id, " ...")

    zip_tmp <- zip_download(url, row$id, verbose)
    if (is.null(zip_tmp)) return(NULL)

    geo_shapefiles_read(zip_tmp, crs = crs)
  }

  # Step 6: Process all rows
  code_sf <- lapply(seq_len(nrow(code_rows)), function(i) {
    process_row(code_rows[i, ])
  })

  # Step 7: Merge outputs
  merged <- geo_sf_bind(code_sf)

  if (is.null(merged))
    stop("No spatial data could be retrieved for code ", data_code, ".")

  if (verbose) message("\nDone! Returned one sf object.")

  merged
}

#' Retrieve heritage layer IDs for a given sf object
#'
#' This function retrieves available heritage dataset identifiers from the
#' French Ministry of Culture’s "Atlas du Patrimoine" service.
#'
#' It uses the spatial extent and inferred department(s) of the input `sf`
#' object to query the service.
#'
#' @param x An `sf` object defining the area of interest.
#' @param buffer A `numeric` value (default = 2500). Buffer distance in meters
#'   used to slightly expand geometries before querying.
#' @param crs An `integer` or `sf::st_crs` object (default = 2154). Coordinate
#'   reference system used for spatial processing.
#' @param verbose Logical. If `TRUE` (default), prints progress and diagnostic messages.
#'
#' @details
#' Internally, the function:
#' \enumerate{
#'   \item Aggregates nearby geometries using `buffer` input.
#'   \item Determines the corresponding INSEE department code for each geometry, using `happign::get_wfs()`.
#'   \item Computes the bounding box of each geometry.
#'   \item Queries the "Atlas du Patrimoine" service feed for all available metadata
#'         records (IDs, titles, GUIDs) within each bounding box.
#' }
#'
#' @return
#' A `data.frame` with the following columns:
#' \describe{
#'   \item{id}{Numeric identifier extracted from the record GUID.}
#'   \item{title}{Record title as published in the service feed.}
#'   \item{guid}{Full GUID (unique resource identifier).}
#'   \item{departement}{Department code associated with the spatial query.}
#'   \item{code}{Internal code associated with the layer.}
#' }
#' Returns an empty `data.frame` if no records are found or the request fails.
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'   # Create a minimal sf object defining the area of interest.
#'   # A simple point geometry is sufficient to trigger spatial queries.
#'   my_sf_layer <- sf::st_sf(
#'     geometry = sf::st_sfc(
#'       sf::st_point(c(2.21, 48.82)),
#'       crs = 4326
#'     )
#'   )
#'
#'   # Retrieve available heritage dataset identifiers intersecting
#'   # the spatial extent derived from the sf object.
#'   ids <- get_heritage_ids(my_sf_layer)
#' }
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

  # --- Step 0: Check service availability ----

  # Check whether the external Atlas service is available before any processing.
  # This avoids running expensive spatial operations if the backend is unreachable.
  if (!isTRUE(atlas_ok())) {
    stop("Atlas service is not available.", call. = FALSE)
  }

  # --- Step 1: Prepare + validate geometry ----

  # Prepare input geometry:
  # - reproject to target CRS
  # - apply buffer to extend spatial search area
  # - perform internal cleaning/normalisation of geometry
  y <- geo_prepare(x, crs = crs, buffer = buffer)

  # Validate that resulting object is a valid sf object with proper geometry
  geo_object_check(y)

  # Defensive check: ensure preprocessing did not remove all features
  if (is.null(y) || nrow(y) == 0L) {
    stop("Empty geometry after preprocessing.", call. = FALSE)
  }

  # --- Step 2.1: Compute extents ----

  # Compute bounding boxes (extent) for each feature.
  # These extents will be used to query the external spatial service.
  y_extents <- geo_extent(y)

  # Validate extent structure to ensure downstream query construction is safe
  if (!is.data.frame(y_extents) || nrow(y_extents) == 0L) {
    stop("Invalid extents.", call. = FALSE)
  }

  # Ensure expected extent schema is present (strict contract)
  required_cols <- c("left", "bottom", "right", "top")

  if (!all(required_cols %in% names(y_extents))) {
    stop("Invalid extent structure.", call. = FALSE)
  }

  y_extents <- y_extents[, required_cols, drop = FALSE]

  # --- Step 2.2: Compute departments (per geometry) ----

  # Extract individual geometries to compute administrative department membership
  # Each geometry is mapped to one or more department codes
  y_geoms <- sf::st_geometry(y)

  y_deps <- lapply(seq_along(y_geoms), function(i) {

    # Wrap single geometry into an sf object for spatial lookup
    quiet(get_deps(
      sf::st_sf(
        geometry = sf::st_sfc(y_geoms[[i]], crs = sf::st_crs(y))
      )
    ))
  })

  # Ensure spatial department mapping is consistent with input geometries
  if (length(y_deps) != length(y_geoms)) {
    stop("Internal error: mismatch between geometries and departments.", call. = FALSE)
  }

  # Fail fast if any geometry cannot be assigned to a department
  if (any(vapply(y_deps, function(d) length(d) == 0L, logical(1)))) {
    stop("At least one geometry has no department.", call. = FALSE)
  }

  # Ensure no missing values exist in department assignment results
  if (any(vapply(y_deps, function(d) any(is.na(d)), logical(1)))) {
    stop("NA detected in department assignment.", call. = FALSE)
  }

  # Optional progress information: number of department assignments generated
  if (verbose) {
    message(sum(lengths(y_deps)), " group dep(s) detected")
  }

  # --- Step 3: Build query table (extent × dep) ----

  # Build query input combining spatial extent and department filters
  # Each geometry can generate multiple department-based queries
  z <- Map(function(i, deps_y) {

    # Informational log per processing group (extent + departments)
    if (verbose) {
      message(sprintf("Processing group dep(s): %s",
                      paste(unique(deps_y), collapse = ", ")))
    }

    ext_y <- y_extents[i, , drop = FALSE]

    # Skip group if no valid departments exist (defensive guard)
    if (length(deps_y) == 0L) return(NULL)

    # Construct query table for API request generation
    data.frame(
      dep = deps_y,
      left = ext_y$left,
      bottom = ext_y$bottom,
      right = ext_y$right,
      top = ext_y$top,
      stringsAsFactors = FALSE
    )

  }, seq_along(y_deps), y_deps)

  # Remove empty groups resulting from filtering or invalid mappings
  z <- Filter(Negate(is.null), z)
  names(z) <- NULL

  # Flatten grouped structure into a single query table
  queries <- do.call(rbind, z)

  # Fail gracefully if no valid queries can be constructed
  if (is.null(queries) || nrow(queries) == 0L) {
    warning("No valid extent/department combinations.", call. = FALSE)
    return(data.frame())
  }

  # Build request URLs for each query row
  # Each URL encodes spatial extent + department filter
  queries$url <- vapply(seq_len(nrow(queries)), function(i) {

    extent <- list(
      left   = queries$left[i],
      bottom = queries$bottom[i],
      right  = queries$right[i],
      top    = queries$top[i]
    )

    ids_url_build(extent, queries$dep[i])

  }, character(1))

  # --- Step 4: Download ----

  # Execute API requests sequentially for each query
  results <- lapply(seq_len(nrow(queries)), function(i) {

    df <- ids_download(queries$url[i])

    # Handle empty or failed API responses gracefully
    if (!is.data.frame(df) || nrow(df) == 0L) {
      if (verbose) {
        message(sprintf("No result for dep %s", queries$dep[i]))
      }
      return(NULL)
    }

    # Attach department metadata to each returned record
    df$departement <- queries$dep[i]

    df
  })

  # --- Step 5: Combine ----

  # Keep only successful (non-empty) results
  valid <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, results)

  # If all requests failed or returned empty results, exit safely
  if (length(valid) == 0L) {
    warning("No heritage identifiers were retrieved.", call. = FALSE)
    return(data.frame())
  }

  # Merge all partial results into a single data.frame
  combined <- do.call(rbind, valid)

  # Final safety check on merged output
  if (nrow(combined) == 0L) {
    warning("No heritage identifiers were retrieved.", call. = FALSE)
    return(data.frame())
  }

  # Ensure required columns exist before post-processing
  required_keys <- c("id", "departement")

  missing <- setdiff(required_keys, names(combined))
  if (length(missing) > 0L) {
    stop("Missing required columns in result: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  # Remove duplicated records based on ID and department combination
  combined <- combined[!duplicated(combined[, required_keys, drop = FALSE]), ]

  # Derive internal classification code from title field
  combined$code <- ids_to_codes(combined$title)

  # Optional final summary message
  if (verbose) {
    message(sprintf("%d heritage ID(s) retrieved", nrow(combined)))
  }

  return(combined)
}

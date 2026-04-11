#' Retrieve and download heritage spatial data for a given sf object
#'
#' This function retrieves and downloads spatial heritage datasets from the
#' French Ministry of Culture’s "Atlas du Patrimoine" service.
#'
#' It uses the spatial extent and inferred department(s) of the input `sf`
#' object to query an internal registry of heritage datasets, then downloads
#' and merges the corresponding spatial layers.
#'
#' @param x An `sf` object defining the area of interest.
#' @param data_code A single `character` heritage dataset codes to retrieve.
#'   Valid codes can be obtained with [get_heritage_layernames()].
#' @param buffer A `numeric` value (default = 2500). Buffer distance in meters
#'   used to slightly expand geometries before querying.
#' @param crs An `integer` or `sf::st_crs` object (default = 2154). Coordinate
#'   reference system used for spatial processing.
#' @param verbose Logical. If `TRUE` (default), prints progress and diagnostic messages.
#'
#' @details
#' This function currently supports metropolitan France departments (96 units).
#'
#' Internally, the function:
#' \enumerate{
#'   \item Validates the input `data_code` against an internal registry.
#'   \item Prepares and optionally buffers the input geometry.
#'   \item Computes spatial extents (bounding boxes) for each feature.
#'   \item Assigns each feature to one or more administrative departments.
#'   \item Filters available heritage records using an internal dataset registry.
#'   \item Builds query URLs for the Atlas du Patrimoine service.
#'   \item Downloads ZIP archives containing shapefiles.
#'   \item Reads, validates, and merges the resulting `sf` objects.
#' }
#'
#' Partial failures are tolerated: invalid or empty responses for individual
#' requests are skipped.
#'
#' @return
#' A single `sf` object containing all retrieved heritage features.
#' Returns an empty `sf` object if no data is found or all requests fail.
#'
#' @examples
#' \donttest{
#' if (interactive()) {
#'
#'   # Define a minimal spatial object (WGS84 point)
#'   my_sf_layer <- sf::st_sf(
#'     geometry = sf::st_sfc(
#'       sf::st_point(c(2.21, 48.82)),
#'       crs = 4326
#'     )
#'   )
#'
#'   # Retrieve heritage spatial data
#'   res <- get_heritage(
#'     x = my_sf_layer,
#'     data_code = "IMMH",
#'     buffer = 2000,
#'     crs = 2154,
#'     verbose = TRUE
#'   )
#'
#' }
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
                         verbose = TRUE) {

  # --- Step 0: Check service availability ----

  # Check whether the external Atlas service is available before any processing.
  # This avoids running expensive spatial operations if the backend is unreachable.
  if (!isTRUE(atlas_ok())) {
    stop("Atlas service is not available.", call. = FALSE)
  }

  # --- Step 1: Validate inputs ----

  # Validate that the requested heritage dataset code exists in the internal registry.
  data_check(data_code)

  # Enforce single dataset selection to avoid ambiguous multi-source queries.
  if (length(data_code) != 1L) {
    stop("`data_code` must be a single heritage code.", call. = FALSE)
  }

  # --- Step 2: Prepare + validate geometry ----

  # Prepare input geometry:
  # - reproject to target CRS
  # - apply buffer to extend spatial search area
  # - perform internal cleaning/normalisation of geometry
  y <- geo_prepare(x, crs = crs, buffer = buffer)
  geo_object_check(y)

  # Defensive check: ensure preprocessing did not remove all features
  if (is.null(y) || nrow(y) == 0L) {
    stop("Empty geometry after preprocessing.", call. = FALSE)
  }

  # --- Step 3.1: Compute extents ----

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

  # --- Step 3.2: Compute departments ----

  # Extract individual geometries to compute administrative department membership
  # Each geometry is mapped to one or more department codes
  y_geoms <- sf::st_geometry(y)

  y_deps <- lapply(seq_along(y_geoms), function(i) {

    # Wrap single geometry into an sf object for spatial lookup
    quiet(get_deps(
      sf::st_sf(geometry = sf::st_sfc(y_geoms[[i]], crs = sf::st_crs(y)))
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

  # --- Step 3.3: Build query groups (extent × dep × id) ----

  # Pre-split extents into indexed list for stable positional mapping.
  ext_list <- lapply(seq_len(nrow(y_extents)), function(i) {
    y_extents[i, , drop = FALSE]
  })

  z <- Map(function(i, deps_y) {

    # Extract spatial extent corresponding to current geometry index.
    ext_y <- ext_list[[i]]

    # Retrieve dataset records filtered by department and requested data code.
    df_ids <- data_filter(department = deps_y, data_code = data_code)

    # Skip group if no matching records exist in the dataset registry
    if (!is.data.frame(df_ids) || nrow(df_ids) == 0L) return(NULL)

    # Filter records strictly to requested departments (safety enforcement).
    df_ids <- df_ids[df_ids$departement %in% deps_y, , drop = FALSE]

    # Abort group if filtering removes all candidate records.
    if (nrow(df_ids) == 0L) {
      if (verbose) {
        message("No valid IDs after filtering for departments.")
      }
      return(NULL)
    }

    # Remove duplicate department entries to avoid redundant requests.
    df_ids <- df_ids[!duplicated(df_ids$departement), , drop = FALSE]

    if (nrow(df_ids) == 0L) return(NULL)

    # Construct final query table combining spatial extent and dataset metadata.
    data.frame(
      dep = df_ids$departement,
      id = df_ids$id,
      title = df_ids$title,
      guid = if ("guid" %in% names(df_ids)) df_ids$guid else NA_character_,
      left = rep(ext_y$left, nrow(df_ids)),
      bottom = rep(ext_y$bottom, nrow(df_ids)),
      right = rep(ext_y$right, nrow(df_ids)),
      top = rep(ext_y$top, nrow(df_ids)),
      stringsAsFactors = FALSE
    )

  }, seq_along(y_deps), y_deps)

  # Remove empty groups generated by filtering or invalid mappings.
  z <- Filter(Negate(is.null), z)
  names(z) <- NULL

  # --- Step 4: API requests ----

  # Internal helper function to validate that returned objects are usable sf objects before merging.
  is_valid_sf <- function(x) {

    if (is.null(x)) return(FALSE)
    if (!inherits(x, "sf")) return(FALSE)
    if (nrow(x) == 0L) return(FALSE)

    # Ensure geometry column exists and is not entirely empty.
    if (is.null(sf::st_geometry(x))) return(FALSE)
    if (all(sf::st_is_empty(x))) return(FALSE)

    TRUE
  }

  # Iterates over each query group (extent × department × dataset) and executes the full download and
  # spatial reconstruction workflow, returning a list of sf objects for each successfully processed group.
  results <- lapply(seq_along(z), function(k) {

    z_i <- z[[k]]

    if (is.null(z_i) || nrow(z_i) == 0L) return(NULL)

    if (verbose) {
      message("Processing group dep(s): ",
              paste(unique(z_i$dep), collapse = ", "))
    }

    # Internal helper function that processes a single query row by building the download request,
    # retrieving the corresponding spatial ZIP archive, and converting it into a validated sf object.
    process_row <- function(row) {

      # Build download request URL for zipped spatial dataset.
      url <- zip_query_build(
        id = row$id,
        title = row$title,
        guid = row$guid,
        extent_vals = c(row$left, row$bottom, row$right, row$top),
        crs = crs
      )

      # Download ZIP archive containing spatial data.
      zip_tmp <- zip_download(url, row$id)

      # Handle failed download attempts gracefully.
      if (is.null(zip_tmp)) {
        if (verbose) {
          message("Download failed for ID ", row$id,
                  " (dep ", row$dep, ")")
        }
        return(NULL)
      }

      # Extract and read shapefiles from downloaded archive.
      res <- geo_shapefile_read(zip_tmp, crs = crs)

      # Validate resulting spatial object integrity.
      if (!is_valid_sf(res)) {
        if (verbose) {
          message("Empty/invalid sf for ID ", row$id,
                  " (dep ", row$dep, ")")
        }
        return(NULL)
      }

      res
    }

    # Apply processing per dataset record in current group.
    res_i <- lapply(seq_len(nrow(z_i)), function(i) {
      process_row(z_i[i, ])
    })

    Filter(Negate(is.null), res_i)

  })

  # --- Step 5: Merge results ----

  flat <- unlist(results, recursive = FALSE)
  flat <- Filter(Negate(is.null), flat)

  # Return empty sf object if no valid data was retrieved.
  if (length(flat) == 0L) {
    return(
      sf::st_sf(
        geometry = sf::st_sfc(crs = crs)
      )[0, ]
    )
  }

  # Merge list of sf objects into a single spatial dataset.
  merged <- geo_sf_bind(flat)

  # Remove duplicated geometries to ensure dataset uniqueness.
  if (!is.null(merged) && nrow(merged) > 0L) {
    merged <- merged[!duplicated(sf::st_geometry(merged)), ]
  }

  # Optional summary of retrieved features.
  if (verbose) {
    message(sprintf("%d feature(s) retrieved", nrow(merged)))
  }

  return(merged)
}

# spatial filter section ----
#' Validate a spatial predicate string
#'
#' Checks that the provided spatial filter is one of the allowed OGC predicates.
#'
#' @param spatial_filter `character`. Spatial predicate to validate.
#'
#' @return `character`. The validated predicate in uppercase.
#'
#' @keywords internal
#'
geo_spatial_check <- function(spatial_filter) {
  valid_predicates <- c(
    "INTERSECTS", "DISJOINT", "CONTAINS", "WITHIN",
    "TOUCHES", "BBOX", "CROSSES", "OVERLAPS",
    "EQUALS", "RELATE", "DWITHIN", "BEYOND"
  )
  spatial_filter <- toupper(spatial_filter[1])
  if (!spatial_filter %in% valid_predicates) {
    stop("`spatial_filter` must be one of: ", paste(valid_predicates, collapse = ", "))
  }
  return(spatial_filter)
}

#' Filter an sf layer by spatial relationship
#'
#' This internal helper function filters features of an `sf` layer based on
#' a specified spatial relationship with another `sf` object.
#'
#' @param layer An `sf` object to be filtered.
#' @param x An `sf` object used as reference for the spatial relationship.
#' @param spatial_filter Character. The spatial predicate to apply. Must be one of:
#' `"INTERSECTS"`, `"DISJOINT"`, `"CONTAINS"`, `"WITHIN"`,
#' `"TOUCHES"`, `"BBOX"`, `"CROSSES"`, `"OVERLAPS"`, `"EQUALS"`.
#'
#' @return A subset of `layer` where features satisfy the specified spatial predicate
#' with respect to `x`.
#'
#' @details
#' - Uses [sf::st_intersects()], [sf::st_disjoint()], [sf::st_contains()], etc.
#'   depending on the value of `spatial_filter`.
#' - `"BBOX"` is treated as a simple intersection with bounding boxes.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' bbox <- st_as_sfc(st_bbox(nc))
#' geo_spatial_filter(nc, bbox, "INTERSECTS")
#' }
#'
#' @importFrom sf st_intersects st_disjoint st_contains st_within st_touches
#' @importFrom sf st_crosses st_overlaps st_equals
#'
#' @keywords internal
#'
geo_spatial_filter <- function(layer, x, spatial_filter) {
  idx <- switch(spatial_filter,
                "INTERSECTS" = sf::st_intersects(layer, x),
                "DISJOINT"   = sf::st_disjoint(layer, x),
                "CONTAINS"   = sf::st_contains(layer, x),
                "WITHIN"     = sf::st_within(layer, x),
                "TOUCHES"    = sf::st_touches(layer, x),
                "BBOX"       = sf::st_intersects(layer, x, sparse = TRUE),
                "CROSSES"    = sf::st_crosses(layer, x),
                "OVERLAPS"   = sf::st_overlaps(layer, x),
                "EQUALS"     = sf::st_equals(layer, x)
  )

  if (is.null(idx)) {
    stop("Invalid `spatial_filter`: must be one of INTERSECTS, DISJOINT, CONTAINS, WITHIN, TOUCHES, BBOX, CROSSES, OVERLAPS, or EQUALS.", call. = FALSE)
  }

  layer[lengths(idx) > 0, , drop = FALSE]
}

# geometry type section-
#' Determine the geometry type of an `sf` object
#'
#' This internal helper function identifies and normalizes the geometry type
#' of an `sf` object. It ensures that all geometries within the object share
#' a consistent type (e.g., `"POINT"`, `"LINESTRING"`, `"POLYGON"`).
#' If multiple geometry types are found, an error is raised.
#'
#' @param x An `sf` object whose geometry type should be determined.
#'
#' @details
#' The function retrieves the geometry type of the input using
#' [sf::st_geometry_type()] and standardizes it according to a simplified
#' classification:
#' \itemize{
#'   \item `"POINT"` and `"MULTIPOINT"` = `"POINT"`
#'   \item `"LINESTRING"` and `"MULTILINESTRING"` = `"LINE"`
#'   \item `"POLYGON"` and `"MULTIPOLYGON"` = `"POLYGON"`
#'   \item All other types = `"GEOMETRY"`
#' }
#'
#' Mixed or inconsistent geometry collections are not supported.
#'
#' @return
#' A single `character(1)` value representing the normalized geometry type.
#' Possible values are: `"POINT"`, `"LINE"`, `"POLYGON"`, or `"GEOMETRY"`.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' geo_object_type(nc)
#' }
#'
#' @importFrom sf st_geometry_type
#'
#' @keywords internal
#'
geo_object_type <- function(x) {
  a <- list(
    POINT = "POINT",
    POINT = "MULTIPOINT",
    LINE = "LINESTRING",
    LINE = "MULTILINESTRING",
    POLYGON = "POLYGON",
    POLYGON = "MULTIPOLYGON",
    other = "GEOMETRY",
    other = "GEOMETRYCOLLECTION",
    other = "CIRCULARSTRING",
    other = "COMPOUNDCURVE",
    other = "CURVEPOLYGON",
    other = "MULTICURVE",
    other = "MULTISURFACE",
    other = "CURVE",
    other = "SURFACE",
    other = "POLYHEDRALSURFACE",
    other = "TIN",
    other = "TRIANGLE"
  )
  type <- sf::st_geometry_type(x)
  levels(type) <- a
  type <- as.character(unique(type))
  if (length(type) > 1) {
    stop("GEOMETRYCOLLECTION objects should have consistent type",
         call. = FALSE
    )
  }
  return(type)
}

#' Check that spatial objects are valid `sf` layers with expected geometry type
#'
#' This internal helper verifies that all provided objects:
#' \itemize{
#'   \item are of class `sf` (optional, controlled by `check_class`);
#'   \item contain valid geometries (`sf::st_is_valid()`);
#'   \item contain no empty geometries (`!sf::st_is_empty()`);
#'   \item have a geometry type among the allowed ones (if specified).
#' }
#'
#' @param ... One or several spatial objects to test.
#' @param check_class Logical. Whether to check that each object inherits from
#' class `sf`. Default is `TRUE`.
#' @param check_valid Logical. Whether to check that geometries are valid.
#' Default is `TRUE`.
#' @param check_empty Logical. Whether to check that geometries are not empty.
#' Default is `TRUE`.
#' @param allowed_geom_classes Character vector of allowed geometry types
#' (e.g., `"POLYGON"`, `"LINE"`, `"POINT"`). If `NULL`, the geometry type
#' is not checked. Default is `"POLYGON"`.
#'
#' @details
#' The function raises an informative error message if any condition fails.
#' It is intended for internal validation of `sf` inputs before performing
#' geometric operations or API queries.
#'
#' Geometry type validation is performed via [geo_object_type()], which ensures
#' type consistency across geometries.
#'
#' @return
#' Invisibly returns `NULL`. Throws an error if any object fails validation.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Valid object
#' geo_object_check(nc)
#'
#' # Invalid: not an sf object
#' df <- data.frame(x = 1:3)
#' geo_object_check(nc, df)
#'
#' # Invalid: empty geometry
#' nc_invalid <- nc
#' nc_invalid$geometry[[1]] <- st_geometrycollection()
#' geo_object_check(nc_invalid)
#' }
#'
#' @seealso [geo_object_type()]
#'
#' @importFrom sf st_is_valid st_is_empty
#'
#' @keywords internal
#'
geo_object_check <- function(...,
                             check_class = TRUE,
                             check_valid = TRUE,
                             check_empty = TRUE,
                             allowed_geom_classes = "POLYGON") {

  dots <- list(...)
  arg_names <- as.list(substitute(list(...)))[-1L]

  for (i in seq_along(dots)) {
    obj <- dots[[i]]
    name <- deparse(arg_names[[i]])

    # Check class
    if (check_class && !inherits(obj, "sf")) {
      stop(sprintf("Object '%s' must be of class 'sf'.", name), call. = FALSE)
    }

    # Check geometry validity
    if (check_valid && any(!sf::st_is_valid(obj))) {
      stop(sprintf("Object '%s' contains invalid geometries.", name), call. = FALSE)
    }

    # Check for empty geometries
    if (check_empty && any(sf::st_is_empty(obj))) {
      stop(sprintf("Object '%s' contains empty geometries.", name), call. = FALSE)
    }

    # Check allowed geometry types
    if (!is.null(allowed_geom_classes)) {
      geom_type <- geo_object_type(obj)
      if (!geom_type %in% allowed_geom_classes) {
        stop(sprintf(
          "Object '%s' must have geometry type: %s (found: '%s').",
          name,
          paste(allowed_geom_classes, collapse = ", "),
          geom_type
        ), call. = FALSE)
      }
    }
  }

  invisible(NULL)
}

# sf section ----
#' Prepare and aggregate an sf object
#'
#' This helper function prepares and aggregates an `sf` object
#' to produce clean, valid, and unified geometries.
#'
#' @param x An `sf` object to process.
#' @param crs Integer. Code of the target CRS. Default is `2154`.
#' @param buffer Numeric. Buffer distance (in map units) used to merge nearby features. Default is `10`.
#'
#' @return An `sf` object with cleaned, projected, and aggregated geometries as `POLYGON`s.
#'
#' @details
#' The function performs a complete geometric preparation workflow:
#'
#' 1. Geometry validation: ensures that all geometries belong to allowed classes (`POINT`, `LINE`, or `POLYGON`).
#' 2. Geometry cleaning: invalid features are fixed with [sf::st_make_valid()], Z/M dimensions are dropped via [sf::st_zm()], and multipart geometries are cast to simple forms.
#' 3. Coordinate transformation: all geometries are transformed to the specified CRS using [sf::st_transform()].
#' 4. Aggregation: small buffer zones (controlled by `buffer`) are applied to merge adjacent or overlapping features.
#'    The buffered geometries are then dissolved with [sf::st_union()] and cast to `POLYGON` type.
#'
#' This process is useful for simplifying feature sets into larger contiguous study zones or analysis areas.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' zones <- geo_prepare(nc, buffer = 500)
#' }
#'
#' @importFrom sf st_make_valid st_zm st_transform st_buffer st_union st_cast st_sf
#'
#' @keywords internal
#'
geo_prepare <- function(x, crs = 2154, buffer = 10) {
  # Validate geometry classes
  geo_object_check(x, allowed_geom_classes = c("POINT", "LINE", "POLYGON"))

  # Clean geometries
  x <- silent_run(st_make_valid(x))
  x <- silent_run(st_zm(x))
  x <- silent_run(geo_cast(x))
  x <- st_transform(x, crs)

  # Aggregation
  x <- st_buffer(x, dist = buffer)
  x <- st_union(x)
  x <- st_sf(st_cast(x, "POLYGON"))
  x
}

#' Compute bounding boxes for each feature in an sf object
#'
#' This internal helper computes the bounding box (`left`, `bottom`, `right`, `top`)
#' for each feature in an `sf` object.
#'
#' @param x An `sf` object (POLYGON or MULTIPOLYGON) for which to compute bounding boxes.
#' @param crs Numeric, Code of the target CRS. Default is `4326`.
#'
#' @return A `data.frame` where each row corresponds to a feature's bounding box,
#' with columns `left`, `bottom`, `right`, `top`.
#'
#' @details
#' - The function first validates the input with [geo_object_check()].
#' - Transforms the object to the target CRS before computing bounding boxes.
#' - CRS and units are inherited from the input `sf` object.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' geo_extent(nc, crs = 2154)
#' }
#'
#' @importFrom sf st_transform st_geometry st_bbox
#'
#' @keywords internal
#'
geo_extent <- function(x, crs = 4326) {
  # Check that x is a valid sf object
  geo_object_check(x)

  # Transform to the target CRS
  x <- sf::st_transform(x, crs)

  # Compute bounding boxes for each feature
  bboxes <- do.call(rbind, lapply(sf::st_geometry(x), function(g) {
    bb <- sf::st_bbox(g)
    c(left = unname(bb["xmin"]),
      bottom = unname(bb["ymin"]),
      right = unname(bb["xmax"]),
      top = unname(bb["ymax"]))
  }))

  # Ensure proper data frame structure
  bboxes <- as.data.frame(bboxes)
  colnames(bboxes) <- c("left", "bottom", "right", "top")

  return(bboxes)
}

#' Retrieve INSEE department codes for each feature in an sf object
#'
#' This internal helper retrieves the administrative department (`code_insee`)
#' intersecting each feature of the input `sf` object. It uses the
#' *Admin Express COG* WFS service from IGN via the **happign** package.
#'
#' @param x An `sf` object defining the area(s) of interest.
#'
#' @return A character vector with the INSEE department code corresponding
#' to each feature of `x`. Returns `NA` if no intersection is found.
#'
#' @details
#' - The input is first transform to CRS:4326.
#' - Centroids of the features are computed before querying the WFS service.
#' - The function uses [silent_run()] to suppress warnings/messages during processing.
#' - If the WFS query fails or returns no features, a vector of `NA` values is returned.
#' - The spatial join is performed using [sf::st_join()] with `st_intersects`.
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' geo_dep(nc)
#' }
#'
#' @importFrom sf st_transform st_centroid st_geometry st_sf st_join st_intersects
#' @importFrom happign get_wfs
#'
#' @keywords internal
#'
geo_dep <- function(x) {
  # Coordinate transformation to CRS:4326
  x <- sf::st_transform(x, 4326)
  x <- silent_run(sf::st_centroid(x))

  # Try to get departments safely
  dep <- tryCatch({
    happign::get_wfs(
      x = x,
      layer = "ADMINEXPRESS-COG.LATEST:departement",
      predicate = intersects()
    )
  }, error = function(e) {
    message("happign::get_wfs() failed: Please try to reduce the spatial size")
    return(NULL)
  })

  # If no departments were retrieved, return NA vector
  if (is.null(dep) || nrow(dep) == 0) {
    message("No departments intersect x: Please try to reduce the spatial size")
    return(rep(NA_character_, nrow(x)))
  }

  # Join spatially to get INSEE codes
  joined <- sf::st_join(sf::st_sf(sf::st_geometry(x)),
                        dep[, "code_insee", drop = FALSE],
                        join = sf::st_intersects, left = TRUE)
  unique(joined$code_insee)
}

#' Cast geometries to simple types
#'
#' This internal helper converts multi-part geometries to their single-part
#' equivalents: MULTIPOLYGON became POLYGON, MULTILINESTRING became LINESTRING,
#' MULTIPOINT became POINT. If the input contains mixed types, each feature is
#' cast individually.
#'
#' @param x An `sf` object with geometries to cast.
#'
#' @return An `sf` object with geometries cast to simple types.
#'
#' @importFrom sf st_geometry_type st_cast
#'
#' @keywords internal
#'
geo_cast <- function(x) {
  geo_object_check(x, allowed_geom_classes = c("POINT", "LINE", "POLYGON"))

  geom_types <- as.character(sf::st_geometry_type(x))

  # Mapping multi-part geometries to simple equivalents
  single_map <- c(
    MULTIPOLYGON    = "POLYGON",
    MULTILINESTRING = "LINESTRING",
    MULTIPOINT      = "POINT"
  )

  # Helper to cast a single feature if needed
  cast_one <- function(obj, geom_type) {
    if (geom_type %in% names(single_map)) {
      sf::st_cast(obj, single_map[[geom_type]])
    } else {
      obj
    }
  }

  # If all geometries are of the same type, cast all at once
  unique_types <- unique(geom_types)
  if (length(unique_types) == 1) {
    type <- unique_types
    if (type %in% names(single_map)) {
      x <- sf::st_cast(x, single_map[[type]])
    }
    return(x)
  }

  # Otherwise, cast feature by feature
  x_list <- vector("list", nrow(x))
  for (i in seq_len(nrow(x))) {
    x_list[[i]] <- cast_one(x[i, ], geom_types[i])
  }

  do.call(rbind, x_list)
}

#' Check if an sf object exceeds size or extent thresholds
#'
#' This internal helper evaluates whether a spatial object is too large based
#' on area (for polygons) and bounding box dimensions.
#'
#' @param x An `sf` object to check.
#' @param area_threshold Numeric. Maximum allowed area in m2 (for polygon geometries). Default is 1e9.
#' @param extent_threshold Numeric. Maximum allowed bounding box width or height in meters. Default is 1.5e5.
#' @param verbose Logical. If `TRUE`, prints a message when the object is within limits.
#'
#' @return Invisibly returns `FALSE` if within limits. Throws an error if thresholds are exceeded.
#'
#' @importFrom sf st_geometry_type st_bbox st_area
#'
#' @keywords internal
#'
geo_too_large <- function(x, area_threshold = 1e9, extent_threshold = 1.5e5, verbose = TRUE) {
  if (!inherits(x, "sf")) stop("Input 'x' must be an sf object.")

  geom_type <- unique(sf::st_geometry_type(x))
  bbox <- sf::st_bbox(x)

  # Compute bounding box width and height
  width <- bbox["xmax"] - bbox["xmin"]
  height <- bbox["ymax"] - bbox["ymin"]

  # Compute approximate total area (for polygons only)
  area <- if (any(grepl("POLYGON", geom_type))) sum(sf::st_area(x)) else NA_real_

  # Initialize checks
  too_large <- FALSE
  reasons <- character(0)

  # Check thresholds
  if (!is.na(area) && as.numeric(area) > area_threshold) {
    too_large <- TRUE
    reasons <- c(reasons, sprintf("area exceeds %.2e m2 (limit %.2e)", as.numeric(area), area_threshold))
  }
  if (width > extent_threshold || height > extent_threshold) {
    too_large <- TRUE
    reasons <- c(reasons, sprintf("extent %.2e x %.2e m (limit %.2e)", width, height, extent_threshold))
  }

  # Handle output
  if (too_large) {
    stop(
      paste0("Spatial object too large: ", paste(reasons, collapse = "; ")),
      call. = FALSE
    )
  } else if (verbose) {
    message("Spatial object within acceptable size limits.")
  }

  invisible(FALSE)
}

#' Read and transform shapefiles from a ZIP archive
#'
#' Extracts shapefiles from a ZIP archive, reads them into `sf` objects,
#' and transform each to CRS:2154 (Lambert-93 projection for France),
#' unless already in the target CRS.
#'
#' @param zip_path `character`. Path to the ZIP archive to read.
#' @param crs `numeric` or `sf::st_crs` object. Source CRS to assume
#' if missing in the shapefiles. Default is 2154 (Lambert-93).
#'
#' @return A list of `sf` objects transform to CRS:2154.
#'
#' @importFrom sf st_read st_crs st_set_crs st_transform
#' @importFrom utils unzip
#'
#' @keywords internal
#'
geo_shapefiles_read <- function(zip_path, crs = 2154) {
  exdir <- tempfile()
  utils::unzip(zip_path, exdir = exdir)
  shp_files <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

  sf_list <- lapply(shp_files, function(shp_path) {
    tryCatch({
      sf_obj <- sf::st_read(shp_path, quiet = TRUE)

      # Coordinate transformation to CRS:2154 if needed
      if (is.na(sf::st_crs(sf_obj)$epsg)) {
        sf_obj <- sf::st_transform(sf_obj, crs)
      }

      sf_obj
    }, error = function(e) NULL)
  })

  # Remove failed reads
  Filter(Negate(is.null), sf_list)
}


#' Bind multiple sf objects into a single sf
#'
#' This internal helper takes a list of `sf` objects, aligns their columns,
#' and binds them together into a single `sf` object. It removes NULL or empty
#' objects and handles nested lists of `sf`.
#'
#' @param sf_list A list of `sf` objects (can include nested lists).
#'
#' @return A single `sf` object containing all input features, or `NULL`
#' if the input list is empty or contains no valid sf objects.
#'
#' @keywords internal
#'
geo_sf_bind <- function(sf_list) {
  # Handle empty input
  if (length(sf_list) == 0) return(NULL)

  # If the first element is itself a list of sf, flatten one level
  if (inherits(sf_list[[1]], "list") && all(vapply(sf_list[[1]], inherits, logical(1), "sf"))) {
    sf_list <- sf_list[[1]]
  }

  # Remove NULL or empty sf
  sf_list <- Filter(function(x) inherits(x, "sf") && nrow(x) > 0, sf_list)
  if (length(sf_list) == 0) return(NULL)

  # Align columns
  ref_cols <- unique(unlist(lapply(sf_list, colnames)))
  aligned <- lapply(sf_list, function(sf_obj) {
    missing <- setdiff(ref_cols, colnames(sf_obj))
    for (c in missing) sf_obj[[c]] <- NA
    sf_obj[, ref_cols, drop = FALSE]
  })

  # Bind all sf together
  if (length(aligned) == 1) aligned[[1]] else do.call(rbind, aligned)
}

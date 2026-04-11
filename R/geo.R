# geometry type section ----
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
#' @importFrom sf st_make_valid st_zm st_transform st_buffer st_union st_cast st_sf
#'
#' @keywords internal
#'
geo_prepare <- function(x, crs = 2154, buffer = 10) {
  # Validate geometry classes
  geo_object_check(x, allowed_geom_classes = c("POINT", "LINE", "POLYGON"))

  # Clean geometries
  x <- quiet(st_make_valid(x))
  x <- quiet(st_zm(x))
  x <- quiet(geo_cast(x))
  x <- st_transform(x, crs)

  # Aggregation
  buffer <- max(10, buffer)
  x <- st_buffer(x, dist = buffer)
  x <- st_union(x)
  x <- st_sf(st_cast(x, "POLYGON", warn = FALSE))
  x <- quiet(st_make_valid(x))
  x
}

#' Compute bounding boxes for each feature in an sf object
#'
#' Computes one bounding box per geometry in an `sf` object.
#'
#' @param x An `sf` object (POLYGON or MULTIPOLYGON) for which to compute bounding boxes.
#' @param crs Numeric or sf CRS. Target CRS used before computation (default 4326).
#'
#' @return A `data.frame` with one row per feature:
#' \describe{
#'   \item{left}{xmin}
#'   \item{bottom}{ymin}
#'   \item{right}{xmax}
#'   \item{top}{ymax}
#' }
#'
#' @details
#' - The function first validates the input with [geo_object_check()].
#' - Transforms the object to the target CRS before computing bounding boxes.
#' - CRS and units are inherited from the input `sf` object.
#'
#' @importFrom sf st_transform st_geometry st_bbox
#'
#' @keywords internal
#'
geo_extent <- function(x, crs = 4326) {

  geo_object_check(x)

  x <- sf::st_transform(x, crs)

  geoms <- sf::st_geometry(x)

  if (length(geoms) == 0L) {
    return(
      data.frame(
        left = numeric(0),
        bottom = numeric(0),
        right = numeric(0),
        top = numeric(0)
      )
    )
  }

  res <- lapply(geoms, function(g) {
    bb <- sf::st_bbox(g)

    data.frame(
      left   = unname(bb["xmin"]),
      bottom = unname(bb["ymin"]),
      right  = unname(bb["xmax"]),
      top    = unname(bb["ymax"])
    )
  })

  do.call(rbind, res)
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

#' Read and transform shapefile from a ZIP archive
#'
#' Extracts shapefiles from a ZIP archive, reads the shapefile found
#' in the "documents" folder into an `sf` object, and transforms it to
#' CRS:2154 (Lambert-93 projection for France), unless already in the target CRS.
#'
#' @param zip_path `character`. Path to the ZIP archive to read.
#' @param crs `numeric` or `sf::st_crs` object. Target CRS to transform to.
#' Default is 2154 (Lambert-93).
#'
#' @return An `sf` object transformed to CRS:2154.
#'
#' @importFrom sf st_read st_crs st_set_crs st_transform
#' @importFrom utils unzip
#'
#' @keywords internal
#'
geo_shapefile_read <- function(zip_path, crs = 2154) {
  # Extract ZIP to temporary directory
  exdir <- tempfile()
  utils::unzip(zip_path, exdir = exdir)

  # Find documents folder (could be at root or inside subdirectories)
  all_dirs <- list.dirs(exdir, recursive = TRUE, full.names = TRUE)
  documents_dir <- all_dirs[grepl("/documents$|\\\\documents$", all_dirs)]

  # Check if documents folder exists
  if (length(documents_dir) == 0) {
    stop("The 'documents' folder does not exist in the ZIP archive")
  }
  documents_dir <- documents_dir[1]

  if (!dir.exists(documents_dir)) {
    stop("The 'documents' folder does not exist in the ZIP archive")
  }

  # Find all shapefiles in documents folder
  shp_files <- list.files(documents_dir, pattern = "\\.shp$", full.names = TRUE, recursive = FALSE)

  # Check if there's exactly one shapefile
  if (length(shp_files) == 0) {
    stop("No shapefile (.shp) found in the 'documents' folder")
  }
  if (length(shp_files) > 1) {
    stop("Multiple shapefiles found in the 'documents' folder. Expected exactly one.")
  }

  # Read the shapefile
  sf_obj <- sf::st_read(shp_files[1], quiet = TRUE)

  # Transform to target CRS if needed
  if (is.na(sf::st_crs(sf_obj)$epsg) || sf::st_crs(sf_obj)$epsg != crs) {
    sf_obj <- sf::st_transform(sf_obj, crs)
  }

  return(sf_obj)
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

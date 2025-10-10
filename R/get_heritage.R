get_heritage <- function(x,
                         data_code,
                         buffer = 2500,
                         crs = 2154,
                         spatial_filter = "intersects",
                         verbose = TRUE) {

  # --- Validate data_code ---
  data_check(data_code)

  # --- Validate spatial filter ---
  spatial_filter <- geo_spatial_check(spatial_filter)

  # --- Prepare input geometry ---
  geo_too_large(x, verbose = verbose)  # Stop if geometry is too large
  x <- geo_prepare(x, crs)
  y <- geo_aggregate_light(x, buffer)  # Apply light aggregation with buffer

  # --- Compute extent and department ---
  extents <- geo_extent(y)
  deps <- silent_run(geo_dep(x))

  # --- Filter metadata by data_code ---
  ids <- data_filter(department = deps, data_code = data_code)

  # --- Basic checks ---
  if (is.null(extents) || length(extents) != 4) stop("Invalid extent")
  if (nrow(ids) == 0) stop("No matching IDs found")

  # --- Initialize result list ---
  final_result <- list()

  # --- Loop over unique codes ---
  for (code in unique(ids$code)) {
    if (verbose) message("\n--- Processing code ", code, " ---")

    code_rows <- ids[ids$code == code, , drop = FALSE]
    code_sf <- lapply(seq_len(nrow(code_rows)), function(i) {
      row <- code_rows[i, ]
      url <- zip_query_build(
        id = row$id,
        title = row$title,
        guid = if ("guid" %in% names(row)) row$guid else NULL,
        extent_vals = extents,
        crs = crs
      )
      if (verbose) message("Requesting ID ", row$id, " ...")
      zip_tmp <- zip_download(url, row$id, verbose)
      if (is.null(zip_tmp)) return(NULL)
      geo_shapefiles_read(zip_tmp)
    })

    # --- Merge downloaded sf objects ---
    merged <- geo_sf_bind(code_sf)
    if (!is.null(merged)) final_result[[code]] <- merged
  }

  # --- Return single sf if only one object, else list ---
  if (length(final_result) == 1) return(final_result[[1]])

  if (verbose) message("\nDone! Returned ", length(final_result), " sf object(s).")
  final_result
}

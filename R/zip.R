#' Build a URL query to download a ZIP
#'
#' Constructs a URL query to request a ZIP file containing spatial data from
#' the French Ministry of Culture's "Atlas du Patrimoine" service.
#'
#' @param id `character` or `numeric`. Identifier of the dataset.
#' @param title `character`. Title of the dataset.
#' @param guid `character`, optional. GUID of the dataset, if available.
#' @param extent_vals `numeric` vector of length 4. Bounding box coordinates in
#' order: left, bottom, right, top.
#' @param crs `numeric`, default `2154`. CRS code for the coordinate reference
#' system to use in the query.
#'
#' @return `character`. URL string for downloading the dataset in ZIP format.
#'
#' @details
#' - The function formats the bounding box and metadata into a JSON object,
#'   then encodes it into a URL suitable for the Atlas Patrimoines export service.
#' - CRS is converted to a string `"EPSG:<code>"`.
#'
#' @examples
#' \dontrun{
#' zip_query_build(
#'   id = 123,
#'   title = "Example Dataset",
#'   extent_vals = c(700000, 6600000, 701000, 6601000)
#' )
#' }
#'
#' @importFrom jsonlite toJSON
#' @importFrom sf st_crs
#'
#' @keywords internal
#'
zip_query_build <- function(id, title, guid = NULL, extent_vals, crs = 2154) {
  extent_vals <- unname(as.numeric(extent_vals))
  if (length(extent_vals) != 4) stop("extent_vals must have 4 numeric values: c(left, bottom, right, top)")

  # extent_vals = c(left, bottom, right, top)
  extent_list <- list(
    left   = extent_vals[1],
    bottom = extent_vals[2],
    right  = extent_vals[3],
    top    = extent_vals[4]
  )

  # Prepare metadata
  meta <- list(
    id = as.character(id),
    title = title,
    url = ""
  )
  if (!is.null(guid)) meta$guid <- guid

  # Build query
  query <- list(
    extent = extent_list,
    format = "SHP",
    srs = paste0("EPSG:", sf::st_crs(crs)$epsg),
    md = list(meta)
  )

  # Convert to JSON
  json_param <- jsonlite::toJSON(query, auto_unbox = TRUE)
  json_param <- gsub('\\"', "'", json_param, fixed = TRUE)

  # URL encode
  paste0(
    "http://atlas.patrimoines.culture.fr/services/export.php?data=",
    URLencode(json_param, reserved = TRUE)
  )
}

#' Download a ZIP file from a given URL
#'
#' Downloads a ZIP file from a specified URL and saves it to a temporary file.
#'
#' @param url `character`. URL to download the ZIP file from.
#' @param id `character` or `numeric`. Identifier of the dataset (used for messages).
#'
#' @return `character` or `NULL`.
#' Path to the temporary ZIP file, or `NULL` if download fails.
#'
#' @details
#' - Uses `httr2` to perform the HTTP request.
#' - If the HTTP response status is not 200, a warning is emitted and `NULL` is returned.
#' - Errors during download are caught and a warning is printed.
#'
#' @examples
#' \dontrun{
#' url <- zip_query_build(123, "Example Dataset", extent_vals = c(700000, 6600000, 701000, 6601000))
#' zip_file <- zip_download(url, 123)
#' }
#'
#' @importFrom httr2 request req_perform resp_status resp_body_raw
#'
#' @keywords internal
#'
zip_download <- function(url, id) {
  tryCatch({
    resp <- request(url) |> req_perform()

    if (resp_status(resp) != 200) {
      warning(
        sprintf("HTTP %s for ID %s", resp_status(resp), id),
        call. = FALSE
      )
      return(NULL)
    }

    tmpf <- tempfile(fileext = ".zip")
    writeBin(resp_body_raw(resp), tmpf)
    tmpf

  }, error = function(e) {
    warning(
      sprintf("Download error for ID %s: %s", id, e$message),
      call. = FALSE
    )
    invisible(NULL)
  })
}

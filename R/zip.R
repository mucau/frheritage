#' Build a URL query to download a ZIP
#'
#' Constructs a URL query to request a ZIP file containing geospatial data from
#' the French Ministry of Culture's "Atlas du Patrimoine" GeoSource service.
#'
#' @param id `character` or `numeric`. Identifier of the dataset.
#' @param title `character`. Title of the dataset.
#' @param guid `character`, optional. GUID of the dataset, if available.
#' @param extent_vals `numeric` vector of length 4. Bounding box coordinates in
#' order: left, bottom, right, top.
#' @param crs `numeric`, default `2154`. EPSG code for the coordinate reference
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
  extent_list <- setNames(as.list(extent_vals), c("left","bottom","right","top"))
  meta <- list(id = as.character(id), title = title)
  if (!is.null(guid)) meta$guid <- guid

  query <- list(
    extent = extent_list,
    format = "SHP",
    srs = paste0("EPSG:", st_crs(crs)$epsg),
    md = list(meta)
  )

  json_param <- toJSON(query, auto_unbox = TRUE)
  json_param <- gsub('\\"', "'", json_param, fixed = TRUE)
  paste0("http://atlas.patrimoines.culture.fr/services/export.php?data=",
         utils::URLencode(json_param, reserved = TRUE))
}

#' Download a ZIP file from a given URL
#'
#' Downloads a ZIP file from a specified URL and saves it to a temporary file.
#'
#' @param url `character`. URL to download the ZIP file from.
#' @param id `character` or `numeric`. Identifier of the dataset (used for messages).
#' @param verbose `logical`, default `TRUE`. If `TRUE`, prints warnings on failure.
#'
#' @return `character` or `NULL`. Path to the temporary ZIP file, or `NULL` if download fails.
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
zip_download <- function(url, id, verbose = TRUE) {
  tryCatch({
    resp <- request(url) |> req_perform()
    if (resp_status(resp) != 200) {
      warning("HTTP ", resp_status(resp), " for ID ", id)
      return(NULL)
    }
    tmpf <- tempfile(fileext = ".zip")
    writeBin(resp_body_raw(resp), tmpf)
    tmpf
  }, error = function(e) {
    warning("Download error for ID ", id, ": ", e$message)
    NULL
  })
}

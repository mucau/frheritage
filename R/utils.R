#' Available layer datasets
#'
#' A dataset containing available layer datasets from the French
#' Ministry of Culture's "Atlas du Patrimoine" GeoSource service.
#'
#' @format A `data.frame` with 5 columns:
#' \describe{
#'   \item{id}{(character) Layer id.}
#'   \item{title}{(character) Layer title.}
#'   \item{guid}{(integer) Layer guid.}
#'   \item{code}{(character) Internal code, determined from tile.}
#'   \item{departement}{(character) Layer department}
#' }
#'
#' @details
#' Contains only data available on the 96 departments of metropolitan France.
#'
"all_ids"

#' Execute an expression completely silently
#'
#' This utility executes any R expression or function call and returns its value,
#' while suppressing all printed output (cat, print), warnings, and messages.
#'
#' @param expr An R expression or function call to execute silently.
#'
#' @return The result of evaluating `expr`.
#'
#' @details
#' - Useful to suppress noisy outputs from functions during package or script execution.
#' - Evaluation occurs in the parent environment to access local variables.
#' - Works cross-platform by redirecting `capture.output` to `nul` on Windows or `/dev/null` on other OS.
#'
#' @examples
#' \dontrun{
#' # Run geo_dep silently
#' deps <- silent_run(geo_dep(your_sf_layer))
#' }
#'
#' @keywords internal
#'
silent_run <- function(expr) {
  # Determine null file for capture.output based on OS
  null_file <- if (.Platform$OS.type == "windows") "nul" else "/dev/null"

  # Evaluate in the parent environment to access local variables
  result <- capture.output(
    value <- suppressWarnings(suppressMessages(eval(substitute(expr), envir = parent.frame()))),
    file = null_file
  )

  # Return the actual result
  value
}


#' Build a URL request to download geospatial data ZIP
#'
#' Constructs a URL to request a ZIP file containing geospatial data from
#' the Atlas Patrimoines export service.
#'
#' @param id `character` or `numeric`. Dataset identifier.
#' @param title `character`. Title of the dataset.
#' @param guid `character`, optional. GUID of the dataset, if available.
#' @param extent `numeric` vector of length 4. Bounding box coordinates: left, bottom, right, top.
#' @param crs `numeric`, default `2154`. EPSG code for the coordinate reference system.
#' @param format `character`, default `"SHP"`. Desired output format (e.g., `"SHP"`, `"GPKG"`).
#'
#' @return `character`. URL string for downloading the dataset in the specified format.
#'
#' @details
#' - The function converts the bounding box and metadata into a JSON object,
#'   then encodes it for inclusion in the Atlas Patrimoines export URL.
#' - CRS is appended as `"EPSG:<code>"` string.
#'
#' @examples
#' \dontrun{
#' url <- build_request_url(
#'   id = 123,
#'   title = "Example Dataset",
#'   extent = c(700000, 6600000, 701000, 6601000)
#' )
#' }
#'
#' @importFrom jsonlite toJSON
#'
#' @keywords internal
#'
build_request_url <- function(id, title, guid = NULL, extent, crs = 2154, format = "SHP") {
  extent_list <- setNames(as.list(extent), c("left","bottom","right","top"))
  meta <- list(id = as.character(id), title = title)
  if (!is.null(guid)) meta$guid <- guid

  query <- list(
    extent = extent_list,
    format = format,
    srs = paste0("EPSG:", crs),
    md = list(meta)
  )

  json_param <- toJSON(query, auto_unbox = TRUE)
  json_param <- gsub('\\"', "'", json_param, fixed = TRUE)

  paste0("http://atlas.patrimoines.culture.fr/services/export.php?data=", utils::URLencode(json_param, reserved = TRUE))
}

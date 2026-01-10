#' Check that layer data codes are valid
#'
#' Validates that provided data codes exist in the heritage layers.
#'
#' @param data_code `character` vector. Data codes to check.
#'
#' @return `NULL`. Invisibly returns `NULL` if all codes are valid; otherwise throws an error.
#'
#' @details
#' - Uses `get_heritage_layernames()` to get the list of valid codes.
#' - Throws an error if any `data_code` is invalid.
#'
#' @seealso [get_heritage_layernames()]
#'
#' @examples
#' \dontrun{
#' data_check(c("monument", "site_archaeo"))
#' }
#'
#' @keywords internal
#'
data_check <- function(data_code) {
  valid_codes <- get_heritage_layernames()$code
  if (!all(data_code %in% valid_codes)) {
    stop("`data_code` must be one of: ", paste(valid_codes, collapse = ", "),
         call. = FALSE)
  }
  invisible(NULL)
}

#' Filter layer IDs by internal code and department
#'
#' Filters a dataset of layer IDs based on provided data codes and departments.
#'
#' @param ids `data.frame`. Input dataset of IDs. `NULL` as default.
#' If `NULL` function uses `frheritage::all_ids`.
#' @param department `character` vector. Department(s) to filter.
#' @param data_code `character` vector. Data codes to filter. Must be valid heritage layer codes.
#'
#' @return `data.frame`. Subset of `ids` filtered by `data_code` and `department`, with `departement` column removed.
#'
#' @details
#' - Uses `data_check()` to ensure that all `data_code` values are valid.
#' - The function currently ignores the `ids` argument and always uses `frheritage::all_ids`.
#'
#' @examples
#' \dontrun{
#' # Filter IDs for department "75" and code "monument"
#' data_filter(ids = NULL, department = "75", data_code = "monument")
#' }
#'
#' @keywords internal
#'
data_filter <- function(ids = NULL, department, data_code) {
  data_check(data_code)
  if (is.null(ids)){
    ids <- frheritage::all_ids
  }
  ids[ids[["code"]] %in% data_code & ids[["departement"]] %in% department,
      setdiff(names(ids), "departement"), drop = FALSE]
}

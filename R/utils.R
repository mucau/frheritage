#' Available layer datasets
#'
#' A dataset containing available layer datasets from the French
#' Ministry of Culture's "Atlas du Patrimoine" service.
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

#' Example files for testing
#'
#' These files are used internally for testing functions.
#'
#' @name extdata_examples
#' @docType data
#' @keywords internal
NULL

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
#' @importFrom utils capture.output
#'
#' @keywords internal
#'
quiet <- function(expr) {
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

#' Check if the Atlas service is reachable
#'
#' Performs a lightweight HTTP request to test whether a remote service
#' (typically the French cultural heritage Atlas) is reachable within a
#' specified timeout. The function uses a `HEAD` request to avoid downloading
#' content and returns a logical indicating availability.
#'
#' This function is designed to be robust and silent: any network error
#' (timeout, DNS failure, connection refusal, etc.) results in `FALSE`.
#'
#' @param url A `character` string of length 1. The URL of the service to test.
#'   Default is `"http://atlas.patrimoines.culture.fr/atlas/trunk/"`.
#' @param timeout Numeric. Maximum time in seconds allowed for the request
#'   (including connection and response). Default is `4`.
#'
#' @return A `logical` scalar:
#' \itemize{
#'   \item `TRUE` if the service responds with an HTTP status code < 400
#'   \item `FALSE` otherwise (error, timeout, or HTTP status >= 400)
#' }
#'
#' @details
#' The function relies on \pkg{httr2} and internally performs a `HEAD` request
#' using `httr2::req_perform()`. It does not modify global options and
#' does not raise errors.
#'
#' Note that this function checks **service reachability**, not full
#' functional correctness of the endpoint.
#'
#' @importFrom httr2 request req_method req_timeout req_perform resp_status
#'
#' @keywords internal
#'
atlas_ok <- function(url = "http://atlas.patrimoines.culture.fr/atlas/trunk/",
                     timeout = 4) {
  tryCatch({

    resp <- httr2::request(url) |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(timeout) |>
      httr2::req_perform()

    httr2::resp_status(resp) < 400

  }, error = function(e) FALSE)
}

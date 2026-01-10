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
#' @examples
#' \dontrun{
#' # Run geo_dep silently
#' deps <- quiet(geo_dep(your_sf_layer))
#' }
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

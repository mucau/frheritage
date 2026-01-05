#' Get heritage layer codes and labels
#'
#' This function provides a reference table mapping internal codes to
#' explore heritage layer available datasets from French Ministry
#' of Culture's "Atlas du Patrimoine".
#' Optionally, it can filter the table to return only selected codes.
#'
#' @param code Optional `character` vector of layer codes to filter.
#' If `NULL`, the full reference table is returned.
#'
#' @details
#' The available codes and their meanings are:
#' \itemize{
#'   \item `IMDN` — Domaines nationaux
#'   \item `IMMH` — Immeubles classes ou inscrits
#'   \item `IMUN` — Patrimoine Mondial UNESCO – Emprise surfacique des biens
#'   \item `LACR` — Architecture Contemporaine Remarquable
#'   \item `PADN` — Protection au titre des abords de domaines nationaux
#'   \item `PAMH` — Protection au titre des abords de monuments historiques
#'   \item `PAUN` — Patrimoine Mondial UNESCO – Zone tampon des biens
#'   \item `SICI` — Sites classes ou inscrits
#'   \item `SIPR` — Sites patrimoniaux remarquables
#'   \item `ZPPA` — Zones de presomption de prescriptions archeologiques
#' }
#'
#' @return A `data.frame` with two columns:
#' \describe{
#'   \item{code}{Official heritage layer code (character).}
#'   \item{label}{Descriptive label (character).}
#' }
#'
#' @examples
#' \dontrun{
#'   # Get the full table of heritage layer codes
#'   get_heritage_layernames()
#'
#'   # Filter for specific codes
#'   get_heritage_layernames(c("IMMH", "SICI"))
#' }
#'
#' @export
#' @importFrom utils data
get_heritage_layernames <- function(code = NULL) {
  # Reference table of heritage layer codes and labels
  datas <- data.frame(
    code = c("IMDN", "IMMH", "IMUN", "LACR", "PADN", "PAMH", "PAUN", "SICI", "SIPR", "ZPPA"),
    label = c(
      "Domaines nationaux",
      "Immeubles classes ou inscrits",
      "Patrimoine Mondial UNESCO - Emprise surfacique des biens",
      "Architecture Contemporaine Remarquable",
      "Protection au titre des abords de domaines nationaux",
      "Protection au titre des abords de monuments historiques",
      "Patrimoine Mondial UNESCO - Zone tampon des biens",
      "Sites classes ou inscrits",
      "Sites patrimoniaux remarquables",
      "Zones de presomption de prescriptions archeologiques"
    ),
    stringsAsFactors = FALSE
  )

  # Return the full table if no filtering is requested
  if (is.null(code)) {
    return(datas)
  }

  # Validate provided codes
  invalid <- setdiff(code, datas$code)
  if (length(invalid) > 0) {
    stop(sprintf("Invalid code(s): %s", paste(invalid, collapse = ", ")), call. = FALSE)
  }

  # Return only the requested rows
  datas[datas$code %in% code, , drop = FALSE]
}

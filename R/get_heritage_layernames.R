get_heritage_layernames <- function(code = NULL) {
  # Reference table of heritage layer codes and labels
  datas <- data.frame(
    code = c("IMDN", "IMMH", "IMUN", "LACR", "PADN", "PAMH", "PAUN", "SICI", "SIPR", "ZPPA"),
    label = c(
      "Domaines nationaux",
      "Immeubles classés ou inscrits",
      "Patrimoine Mondial UNESCO - Emprise surfacique des biens",
      "Architecture Contemporaine Remarquable",
      "Protection au titre des abords de domaines nationaux",
      "Protection au titre des abords de monuments historiques",
      "Patrimoine Mondial UNESCO - Emprise surfacique de la zone tampon des biens",
      "Sites classés ou inscrits",
      "Sites patrimoniaux remarquables",
      "Zones de présomption de prescriptions archéologiques"
    ),
    stringsAsFactors = FALSE
  )

  # If no code is provided, return the full reference table
  if (is.null(code)) {
    return(datas)
  }

  # Check if all provided codes exist in the reference list
  invalid <- setdiff(code, datas$code)
  if (length(invalid) > 0) {
    stop(sprintf("Invalid code(s): %s", paste(invalid, collapse = ", ")))
  }

  # Return filtered data frame (matching codes only)
  datas[datas$code %in% code, , drop = FALSE]
}

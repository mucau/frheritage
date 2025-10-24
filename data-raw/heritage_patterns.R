heritage_patterns <- list(
  IMMH = c("Immeubles classés ou inscrits"),
  PAMH = c("Protection au titre des abords de monuments historiques",
           "Abord d'un monument historique"),
  IMDN = c("Domaines nationaux"),
  PADN = c("Protection au titre des abords de domaines nationaux"),
  IMUN = c("Emprise surfacique des biens",
           "Périmètre des biens inscrits",
           "Patrimoine Mondial UNESCO - Bourgogne-Franche-Comté"),
  PAUN = c("Emprise surfacique de la zone tampon des biens"),
  SICI = c("Sites classés ou inscrits",
           "Site classé",
           "Site inscrit",
           "Sites inscrits",
           "Sites classés",
           "Sites classé ou inscrit"),
  SIPR = c("Sites Patrimoniaux Remarquables",
           "Sites patrimoniaux remarquables",
           "Site Patrimonial Remarquable"),
  ZPPA = c("Zones de présomption de prescription archéologique",
           "zones de présomption de prescription archéologique",
           "Zones de présomption de prescriptions archéologiques"),
  LACR = c("Architecture Contemporaine Remarquable",
           "Architecture contemporaine remarquable")
)

usethis::use_data(heritage_patterns, internal = TRUE, overwrite = TRUE)

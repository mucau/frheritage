#' Télécharger un export depuis l'API Atlas Patrimoines
#'
#' Cette fonction effectue une requête vers le service d'export de l'atlas des patrimoines culturels français
#' (http://atlas.patrimoines.culture.fr) en spécifiant une emprise géographique (extent) et une liste d'identifiants
#' de métadonnées (ids). Le résultat est téléchargé sous forme d'un fichier compressé (zip).
#'
#' @param extent Liste nommée contenant les coordonnées de l'emprise spatiale avec les éléments \code{left}, \code{bottom}, \code{right} et \code{top}, par exemple issus d'une fonction \code{geo_extent()}.
#' @param ids Vecteur de caractères ou nombres, contenant les identifiants des métadonnées à télécharger.
#' @param srs Chaîne de caractères indiquant le système de coordonnées de référence (projection). Par défaut \code{"EPSG:2154"}.
#' @param format Format de sortie demandé. Par défaut \code{"SHP"} (shapefile).
#' @param out_file Chemin du fichier local dans lequel sauvegarder le fichier téléchargé. Par défaut \code{"export.zip"}.
#'
#' @return Chemin du fichier local dans lequel sauvegarder le fichier téléchargé. La fonction télécharge et sauvegarde directement le fichier localement. En cas d'erreur HTTP, la fonction stoppe avec un message.
#' @details
#' Identifiants disponibles pour \code{ids} et leurs descriptions correspondantes :
#' \itemize{
#'   \item 1147 : Sites classés ou inscrits
#'   \item 3955 : Protection au titre des abords de monuments historiques
#'   \item 4525 : Patrimoine mondial de l'UNESCO - Emprise surfacique de la zone tampon des biens
#'   \item 1789 : Zones de présomption de prescriptions archéologiques
#'   \item 4602 : Immeubles classés ou inscrits
#' }
#' 
#' @importFrom httr GET write_disk
#' @importFrom jsonlite toJSON
#'
#' @examples
#' \dontrun{
#' extent <- list(left = 0.74, bottom = 47.92, right = 0.97, top = 48.01)
#' ids <- c("1147", "3955", "4525")
#' download_atlas(extent, ids, out_file = "mes_donnees.zip")
#' }
#'
#' @export
get_zip_from_atlas <- function(extent,
                               ids,
                               srs = "EPSG:2154",
                               format = "SHP",
                               out_file = tempfile(fileext = ".zip")) {
  
  # Construction du JSON de la requête
  md_list <- lapply(ids, function(id) list(id = id, title = "", url = "", guid = ""))
  query <- list(
    extent = extent,
    format = format,
    srs = srs,
    md = md_list
  )
  
  # Encodage de l'URL
  json_param <- URLencode(jsonlite::toJSON(query, auto_unbox = TRUE))
  base_url <- "http://atlas.patrimoines.culture.fr/services/export.php"
  full_url <- paste0(base_url, "?data=", json_param)
  
  # Téléchargement
  resp <- httr::GET(full_url, httr::write_disk(out_file, overwrite = TRUE))
  if (resp$status_code == 200) {
    message("Export téléchargé : ", out_file)
    return(out_file)
  } else {
    stop("Erreur : code HTTP ", resp$status_code)
  }
}

#' Extraire un shapefile depuis un fichier ZIP et le lire en objet sf
#'
#' Cette fonction extrait le contenu d'un fichier ZIP dans un répertoire temporaire (ou spécifié),
#' recherche le premier fichier shapefile (.shp) présent, le charge en tant qu'objet \code{sf}
#' et renvoie une liste contenant cet objet ainsi que le nom du shapefile sans son extension.
#'
#' @param zip_path Chemin vers le fichier ZIP contenant le shapefile.
#' @param exdir Répertoire dans lequel extraire les fichiers du ZIP. Par défaut un répertoire temporaire.
#'
#' @return Une liste contenant :
#' \itemize{
#'   \item \code{sf} : un objet \code{sf} correspondant au shapefile chargé.
#'   \item \code{name} : une chaîne de caractères avec le nom du shapefile (sans extension).
#' }
#'
#' @details
#' La fonction suppose qu'il y a au moins un fichier shapefile (.shp) dans le ZIP.
#' Si plusieurs sont présents, seul le premier sera lu.
#'
#' @importFrom sf st_read
#' @importFrom utils unzip
#'
#' @examples
#' \dontrun{
#' result <- extract_shapefile_from_zip("export.zip")
#' print(result$name)  # Affiche le nom du shapefile
#' plot(result$sf)     # Affiche la géométrie
#' }
#'
#' @export
extract_sf_from_zip <- function(zip_path, exdir = tempdir()) {
  # Vérifier que le fichier existe
  if (!file.exists(zip_path)) {
    stop("Le fichier ZIP spécifié n'existe pas : ", zip_path)
  }
  
  # Créer un sous-répertoire basé sur le nom du ZIP
  zip_name <- tolower(gsub("\\.zip$", "", basename(zip_path)))
  subdir <- file.path(exdir, zip_name)
  if (!dir.exists(subdir)) {
    dir.create(subdir, recursive = TRUE)
  }
  
  # Décompresser le ZIP dans le sous-répertoire
  utils::unzip(zip_path, exdir = subdir)
  
  # Chercher tous les shapefiles (.shp) dans le dossier
  shp_files <- list.files(subdir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
  
  if (length(shp_files) == 0) {
    stop("Aucun fichier .shp trouvé dans le ZIP : ", zip_path)
  }
  
  # Utiliser le premier shapefile trouvé
  shp_file <- shp_files[1]
  shp_name <- sub("\\.shp$", "", basename(shp_file))
  
  # Lire le shapefile (seule étape nécessitant 'sf')
  sf_obj <- sf::st_read(shp_file, quiet = TRUE)
  
  # Retourner un objet liste avec le sf et le nom du shapefile
  list(
    sf = sf_obj,
    name = shp_name
  )
}

#' Récupérer les identifiants et titres des éléments patrimoniaux dans une étendue géographique
#'
#' Cette fonction interroge le service RSS de l'atlas des patrimoines culturels pour récupérer
#' les identifiants (GUID) et les titres des éléments patrimoniaux situés dans une étendue géographique donnée,
#' limitée à un département français spécifié par son code INSEE.
#'
#' @param extent Liste contenant les coordonnées géographiques de l'étendue à interroger,
#'   avec les champs \code{left}, \code{bottom}, \code{right} et \code{top}
#'   correspondant respectivement aux longitudes et latitudes minimale et maximale.
#' @param insee_dep Chaîne de caractères ou nombre correspondant au code INSEE du département
#'   (exemple : \code{"41"} pour le Loir-et-Cher).
#'
#' @return Un \code{data.frame} avec deux colonnes :
#'   \item{guid}{Identifiant unique de l'élément patrimonial (GUID).}
#'   \item{title}{Titre ou description de l'élément patrimonial.}
#'   Si aucun élément n'est trouvé, la fonction retourne un \code{data.frame} vide.
#'
#' @details
#' La fonction construit une requête vers le service RSS de l'atlas des patrimoines en filtrant
#' sur une zone rectangulaire (BBOX) définie par l'étendue fournie et sur un département donné.
#' La requête passe par un proxy IGN pour contourner les restrictions CORS.
#'
#' @examples
#' extent <- list(left = 1.254076, bottom = 47.5416, right = 1.355672, top = 47.62094)
#' result <- get_ids_from_extent(extent, insee_dep = "41")
#' print(result)
#'
#' @export
#' 
get_ids_from_extent <- function(extent, insee_dep) {
  if (missing(insee_dep)) stop("Argument insee_dep manquant")
  if (missing(extent)) stop("Argument extent manquant")
  
  base_url <- "http://atlas.patrimoines.culture.fr/geosource/srv/fr/rss.search?"
  
  query_params <- list(
    "_dc" = as.character(as.numeric(Sys.time()) * 1000),
    from = 1,
    to = 30,
    sortby = "MCC",
    pertinentScaleLevel = 1,
    georss = "simple",
    westBL = extent$left,
    eastBL = extent$right,
    southBL = extent$bottom,
    northBL = extent$top,
    geoForm = "BBOX",
    geoID = paste0("DEP_", insee_dep),
    themekeywords = "Protection"
  )
  
  query_params <- lapply(query_params, as.character)
  
  query_string <- paste(
    names(query_params),
    sapply(query_params, URLencode, reserved = TRUE),
    sep = "=",
    collapse = "&"
  )
  
  proxy_url <- paste0(
    "http://atlas.patrimoines.culture.fr/atlas/trunk/proxy_ign.php?url=",
    URLencode(paste0(base_url, query_string), reserved = TRUE)
  )
  
  message("URL requête : ", proxy_url)
  
  # Gestion du timeout + retry
  res <- tryCatch(
    httr::GET(proxy_url, httr::timeout(60)),
    error = function(e) {
      message("Échec de la connexion, nouvel essai dans 5 secondes...")
      Sys.sleep(5)
      tryCatch(
        httr::GET(proxy_url, httr::timeout(60)),
        error = function(e) {
          warning("Échec définitif pour DEP_", insee_dep, " : ", e$message)
          return(NULL)
        }
      )
    }
  )
  
  if (is.null(res) || res$status_code != 200) {
    warning("Impossible de récupérer les données pour DEP_", insee_dep)
    return(data.frame(guid = character(), title = character(), stringsAsFactors = FALSE))
  }
  
  content_txt <- httr::content(res, as = "text", encoding = "UTF-8")
  doc <- xml2::read_xml(content_txt)
  
  items <- xml2::xml_find_all(doc, ".//item")
  if (length(items) == 0) {
    warning("Aucun item trouvé dans la réponse")
    return(data.frame())
  }
  
  titles <- xml2::xml_text(xml2::xml_find_all(items, "./title"))
  guids <- xml2::xml_text(xml2::xml_find_all(items, "./guid"))
  
  data.frame(
    guid = guids,
    title = titles,
    stringsAsFactors = FALSE
  )
}

#' Télécharge et agrège les données patrimoniales pour un objet spatial
#'
#' Cette fonction interroge l'Atlas du Patrimoine afin de récupérer les données
#' correspondant à un thème donné (immeubles classés, sites, etc.) pour
#' l'ensemble des départements intersectant un objet spatial donné.
#' Les résultats sont fusionnés en un seul objet `sf`.
#'
#' @param sf_obj Un objet `sf` représentant la zone d'intérêt (polygone ou autre).
#' @param theme Chaîne de caractères indiquant le thème à extraire.
#'   Doit être l'une des valeurs suivantes :
#'   - `"site"` : Sites classés ou inscrits,
#'   - `"ppmh"` : Abords ou monuments historiques,
#'   - `"imm"`  : Immeubles classés ou inscrits,
#'   - `"zppa"` : Zones de présomption de prescriptions archéologiques.
#'
#' @return Un objet `sf` contenant les entités correspondant au thème choisi,
#'   fusionnées pour tous les départements intersectant `sf_obj`.
#'   Retourne `NULL` si aucun résultat n'est trouvé.
#'
#' @details
#' Pour chaque département intersectant `sf_obj`, la fonction :
#' 1. Identifie l'étendue géographique (`extent`) à interroger.
#' 2. Télécharge la liste des identifiants disponibles via `get_ids_from_extent()`.
#' 3. Filtre les identifiants en fonction des mots-clés associés au thème choisi.
#' 4. Télécharge les données correspondantes (ZIP) avec `get_zip_from_atlas()`.
#' 5. Extrait le fichier `sf` via `extract_sf_from_zip()`.
#' 6. Fusionne l'ensemble des résultats via `do.call(rbind, ...)`.
#'
#' La fonction repose sur `happign::get_wfs()` pour récupérer les départements
#' et sur des fonctions utilitaires (`geo_extent`, `get_ids_from_extent`, etc.)
#' qui doivent être définies dans l'environnement utilisateur.
#'
#' @importFrom happign get_wfs
#'
#' @examples
#' \dontrun{
#' library(sf)
#' zone <- st_as_sf(st_sfc(st_buffer(st_point(c(2.35, 48.85)), 0.1), crs = 4326))
#' resultat <- get_patrimony(zone, theme = "imm")
#' plot(resultat["nom"])
#' }
#'
#' @seealso [happign::get_wfs()], [get_ids_from_extent()], [get_zip_from_atlas()], [extract_sf_from_zip()]
#' 
#' @export
#' 
get_patrimony <- function(sf_obj, theme = "imm") {
  # Vérifie que l'objet est bien un sf
  geo_check(sf_obj)
  
  # Thèmes et mots-clés associés
  themes <- list(
    site = c("Sites", "Sites classés ou inscrits"),
    ppmh = c("abords", "monuments historiques"),
    imm  = c("Immeubles", "Immeubles classés ou inscrits"),
    zppa = c("présomption", "prescriptions", 
             "Zones de présomption de prescriptions archéologiques")
  )
  
  if (!theme %in% names(themes)) {
    stop("Thème non valide. Choisir parmi : ", paste(names(themes), collapse = ", "))
  }
  keywords <- themes[[theme]]
  
  # Récupère les départements intersectant sf_obj
  deps <- happign::get_wfs(sf_obj, 
                           layer = "ADMINEXPRESS-COG-CARTO.LATEST:departement",
                           spatial_filter = "INTERSECTS")
  
  # Stockera les sf résultats de chaque département
  sf_list <- list()
  
  # Boucle sur chaque département
  for (i in seq_len(nrow(deps))) {
    dep <- deps[i, ]
    insee_dep <- dep$code_insee
    extent <- geo_extent(sf_obj)  # À adapter si besoin
    
    # Récupère les IDs pour le département
    ids <- get_ids_from_extent(extent, insee_dep)
    if (nrow(ids) == 0) next
    
    # Filtre les IDs par les mots-clés (sur le champ 'title')
    keep <- logical(nrow(ids))
    for (j in seq_len(nrow(ids))) {
      keep[j] <- any(grepl(paste(keywords, collapse = "|"), 
                           ids$title[j], ignore.case = TRUE))
    }
    
    ids <- ids[keep, , drop = FALSE]
    if (nrow(ids) == 0) next
    
    # Télécharge le zip (un seul id attendu normalement)
    zip_path <- get_zip_from_atlas(extent, ids$guid)
    
    # Extrait et lit le sf
    sf_result <- extract_sf_from_zip(zip_path)$sf
    
    # Stocke dans la liste
    sf_list[[length(sf_list) + 1]] <- sf_result
  }
  
  # Combine tous les résultats (s'il y en a plusieurs)
  if (length(sf_list) == 0) {
    warning("Aucun résultat trouvé pour le thème '", theme, "'.")
    return(NULL)
  }
  
  result <- do.call(rbind, sf_list)
  return(result)
}

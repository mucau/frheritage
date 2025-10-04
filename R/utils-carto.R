#' Use to get type of a geometry
#'
#' @param x `sf`
#' @importFrom sf st_geometry_type
#' 
#' @noRd
get_geom_type <- function(x) {
  a <- list(
    POINT = "POINT",
    POINT = "MULTIPOINT",
    LINE = "LINESTRING",
    LINE = "MULTILINESTRING",
    POLYGON = "POLYGON",
    POLYGON = "MULTIPOLYGON",
    other = "GEOMETRY",
    other = "GEOMETRYCOLLECTION",
    other = "CIRCULARSTRING",
    other = "COMPOUNDCURVE",
    other = "CURVEPOLYGON",
    other = "MULTICURVE",
    other = "MULTISURFACE",
    other = "CURVE",
    other = "SURFACE",
    other = "POLYHEDRALSURFACE",
    other = "TIN",
    other = "TRIANGLE"
  )
  type <- sf::st_geometry_type(x)
  levels(type) <- a
  type <- as.character(unique(type))
  if (length(type) > 1) {
    stop("GEOMETRYCOLLECTION objects should have consistent type",
         call. = FALSE
    )
  }
  return(type)
}

#' Get a border layer from polygons
#' 
#' This function convert polygon to line and remove shared lines
#' 
#' @param x `sf POLYGONS`, using a projected CRS
#' @note
#' If the polygon layer contains topology errors (such as contiguous
#' polygons not sharing exactly the same boundary)
#' 
#' @importFrom sf st_is_longlat
#' 
#' @return An `sf` object with two `MULTILINESTRING` geometry is returned
#' 
#' @export
poly_to_line <- function(x) {
  
  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }
  
  if (sf::st_is_longlat(x)) {
    stop("This feature does not work on unprojected (long/lat) layers.",
         call. = FALSE
    )
  }
  
  lines <- list(
    shared = extract_shared_lines(x),
    outer  = extract_outer_lines(x)
  )
  
  # remove empty result (if no shared line)
  lines <- Filter(\(x) !is.null(x), lines)
  
  lines_sf <- sf::st_sf(
      type = names(lines),
      geometry = do.call(c, lines)
    )
  
  return(lines_sf)
  
}

#' Extract shared lines between polygons
#' @param x `sf`
#' @importFrom sf st_is_longlat st_geometry st_intersection st_collection_extract st_union
#' @noRd
extract_shared_lines <- function(x){
  
  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }
  
  if (sf::st_is_longlat(x)) {
    stop("This feature does not work on unprojected (long/lat) layers.",
         call. = FALSE
    )
  }
    
  x <- sf::st_geometry(x)
  
  shared_lines <- tryCatch({
    sf::st_intersection(x, x) |> 
      sf::st_collection_extract("LINESTRING") |> 
      sf::st_union()
  },
  error = function(e) {
    NULL
  })

  return(shared_lines)
}

#' Extract outer of polygons
#' @param x `sf`
#' @importFrom sf st_is_longlat st_geometry st_union st_cast
#' @noRd
extract_outer_lines <- function(x){
  
  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }
  
  if (sf::st_is_longlat(x)) {
    stop("This feature does not work on unprojected (long/lat) layers.",
         call. = FALSE
    )
  }
  
  x <- sf::st_geometry(x)
  outer_lines <- sf::st_union(x) |> 
    sf::st_cast("MULTILINESTRING") |> 
    sf::st_union()
  
  return(outer_lines)
}

#' Creation d'un objet sf vide
#'
#' Cette fonction crée un objet de clas `sf` vide de la géométrie souhaitée (
#' point, line, polygon).
#' 
#' @param geom_type `character`; une des géométries autorisées par `sf` : `POLYGON`, 
#' `LINESTRING`, `POINT`, `MULTIPOLYGON`, `MULTILINESTRING`, `MULTIPOINT`
#' @param ... Attributs nommés et leurs classes. Par exemple, vous pouvez passer
#'  des arguments tels que : `PLACETTE = character(0)`. Chaque argument doit 
#'  être nommé selon l'attribut, avec une classe correspondante, 
#'  typiquement `character(0)`.
#'  
#' @importFrom sf st_sf st_sfc
#' 
#' @return `sf`
#' @export
#' @examples
#' \dontrun{
#' empty_sf <- create_empty_sf(
#'   "LINESTRING",
#'   PLACETTE = character(0),
#'   TSE_VOL = numeric(0))
#' }
create_empty_sf <- function(geom_type, ...){
  empty_sf_geom <- st_sfc()
  class(empty_sf_geom) <- c(paste0("sfc_", toupper(geom_type)), class(empty_sf_geom)[-1])
  fields <- list(..., geometry = empty_sf_geom)
  template <- st_sf(fields, crs = 2154)
  return(template)
}

#' Écriture d’un fichier spatial dans différents formats
#'
#' Cette fonction permet d’écrire un objet `sf` dans un fichier spatial au format
#' Shapefile, GeoPackage (GPKG) ou GeoJSON. Elle propose également une option de sauvegarde
#' sécurisée avec horodatage.
#'
#' @param sf_obj Un objet de classe `sf` à enregistrer.
#' @param repout Chemin de sortie (chemin complet du fichier avec ou sans extension).
#' @param crs Système de coordonnées en sortie (par défaut : 2154, Lambert 93).
#' @param format Format du fichier de sortie : `"SHP"` (par défaut), `"GPKG"` ou `"GEOJSON"`.
#' @param layer Nom de la couche à écrire (obligatoire pour GPKG si non déductible).
#' @param append Logique. Si `TRUE`, ajoute les données à une couche existante (utile pour GPKG).
#' @param verbose Logique. Si `TRUE`, affiche un message de confirmation après écriture.
#' @param secure Logique. Si `TRUE`, écrit également une copie sécurisée horodatée du fichier.
#'
#' @return Aucun objet retourné. La fonction écrit le fichier sur le disque.
#'
#' @details
#' - Le format `"SHP"` correspond à un fichier shapefile (ensemble de fichiers `.shp`, `.shx`, etc.).
#' - Le format `"GPKG"` permet d’enregistrer plusieurs couches dans un seul fichier.
#' - Le format `"GEOJSON"` supporte uniquement une couche par fichier.
#'
#' Le paramètre `layer` est obligatoire pour `GPKG`. Si non fourni, il est automatiquement déduit du nom du fichier.
#'
#' Si l’option `secure` est activée, une copie du fichier est créée avec un suffixe temporel au format `"_YYYYMMDD_HHMMSS"`.
#'
#' @examples
#' \dontrun{
#' geo_write_sf(my_sf, "data/export.shp")
#' geo_write_sf(my_sf, "data/foret.gpkg", format = "GPKG", layer = "ma_couche")
#' geo_write_sf(my_sf, "data/carte.geojson", format = "GEOJSON", secure = TRUE)
#' }
#'
#' @importFrom sf st_transform st_write
#' 
#' @export
#' 
geo_write_sf <- function(sf_obj,
                         repout,
                         crs = 2154,
                         format = "SHP",
                         layer = NULL,
                         append = FALSE,
                         verbose = FALSE,
                         secure = FALSE) {
  
  format <- toupper(format)
  supported_formats <- c("SHP", "GPKG", "GEOJSON")
  drivers <- c(SHP = "ESRI Shapefile", GPKG = "GPKG", GEOJSON = "GeoJSON")
  extensions <- c(SHP = ".shp", GPKG = ".gpkg", GEOJSON = ".geojson")
  
  if (!(format %in% supported_formats)) {
    stop(paste("Format non supporté:", format, 
               ". Choisissez parmi :", paste(supported_formats, collapse = ", ")))
  }
  
  # Forcer la bonne extension
  repout <- sub("\\.[^.]+$", extensions[[format]], repout)
  
  # Déterminer le nom de la couche si nécessaire
  if (is.null(layer)) {
    if (format == "GPKG") {
      filename <- basename(repout)
      layer <- sub("\\.[^.]+$", "", filename)
    } else {
      layer <- NULL
    }
  }
  
  # Projection
  x <- sf::st_transform(sf_obj, crs)
  
  # Écriture du fichier principal
  sf::st_write(
    obj = x,
    dsn = repout,
    layer = layer,
    append = append,
    driver = drivers[[format]],
    quiet = TRUE,
    layer_options = "ENCODING=UTF-8"
  ) |> suppressWarnings()
  
  if (verbose) {
    message("Le fichier a été enregistré dans : ", repout)
  }
  
  # Écriture d'une sauvegarde horodatée
  if (secure) {
    date <- gsub(":", "", format(Sys.time(), "%Y%m%d_%H%M%S"))
    repout_with_date <- sub(paste0(extensions[[format]], "$"), 
                            paste0("_", date, extensions[[format]]), 
                            repout)
    
    sf::st_write(
      obj = x,
      dsn = repout_with_date,
      layer = layer,
      append = append,
      driver = drivers[[format]],
      quiet = TRUE,
      layer_options = "ENCODING=UTF-8"
    ) |> suppressWarnings()
  }
}

#' Gère les géométries vides dans un objet `sf`
#'
#' Cette fonction permet de détecter, marquer et/ou supprimer les géométries vides
#' d'un objet `sf`.
#'
#' @param sf_obj Un objet de classe `sf`.
#' @param remove_empty Booléen. Si `TRUE`, les géométries vides seront supprimées. Par défaut `TRUE`.
#' @param flag_empty Booléen. Si `TRUE`, une colonne `geom_empty` est ajoutée pour indiquer les géométries vides. Par défaut `FALSE`.
#'
#' @return L'objet `sf` traité, avec ou sans les géométries vides, et/ou une colonne booléenne `geom_empty`.
#'
#' @importFrom sf st_is_empty
#' @export
#' 
geo_handle_empty <- function(sf_obj, remove_empty = TRUE, flag_empty = FALSE) {
  geo_check(sf_obj)
  
  # Vérifie les géométries vides
  empty_geom <- sf::st_is_empty(sf_obj)
  
  # Ajoute un flag si demandé
  if (flag_empty) {
    sf_obj$geom_empty <- empty_geom
  }
  
  # Supprime les géométries vides si demandé
  if (remove_empty) {
    sf_obj <- sf_obj[!empty_geom, ]
  }
  
  return(sf_obj)
}

#' Renomme la colonne de géométrie d’un objet `sf`
#'
#' Cette fonction renomme automatiquement la colonne de géométrie d’un objet `sf`
#' si elle ne porte pas déjà le nom souhaité.
#'
#' @param sf_obj Un objet `sf`.
#' @param new Nom souhaité pour la colonne de géométrie. Par défaut `"geometry"`.
#'
#' @return Un objet `sf` avec la colonne de géométrie renommée.
#'
#' @importFrom sf st_geometry
#' 
#' @export
#' 
geo_rename_column <- function(sf_obj, new = "geometry") {
  geo_check(sf_obj)
  
  # Essaye de détecter le champ géométrique via l'attribut
  geom_col <- attr(sf::st_geometry(sf_obj), "sf_column")
  
  # Si l'attribut est manquant ou vide, rechercher la colonne de classe sfc
  if (is.null(geom_col) || !(geom_col %in% names(sf_obj))) {
    sfc_cols <- names(sf_obj)[sapply(sf_obj, inherits, "sfc")]
    if (length(sfc_cols) == 0) {
      stop("Aucune colonne de type 'sfc' détectée dans l'objet `sf`.", call. = FALSE)
    }
    geom_col <- sfc_cols[1]  # Prend la première colonne géométrique
    sf::st_geometry(sf_obj) <- geom_col  # Réassocie la géométrie
  }
  
  # Si déjà au bon nom, rien à faire
  if (identical(geom_col, new)) return(sf_obj)
  
  # Vérifie si un champ du nom 'new' existe déjà
  if (new %in% names(sf_obj)) {
    stop(sprintf("Impossible de renommer : une colonne '%s' existe déjà.", new), call. = FALSE)
  }
  
  # Renomme et met à jour le champ géométrique
  names(sf_obj)[names(sf_obj) == geom_col] <- new
  sf::st_geometry(sf_obj) <- new
  
  return(sf_obj)
}

#' Extraire l'emprise (bbox) d'un objet sf
#'
#' Cette fonction prend en entrée un objet spatial \code{sf} et retourne une liste
#' représentant l'emprise (bounding box) de cet objet, avec les éléments nommés
#' \code{left}, \code{bottom}, \code{right} et \code{top} correspondant respectivement
#' aux coordonnées minimales et maximales de l'objet.
#'
#' @param sf_obj Un objet spatial de classe \code{sf} (simple features)
#' @param crs Système de projection. Par défaut `4326`.
#'
#' @return Une liste nommée de quatre éléments numériques : \code{left}, \code{bottom}, \code{right}
#' et \code{top}, représentant les coordonnées de l'emprise.
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' extent <- geo_extent(nc)
#' print(extent)
#'
#' @export
geo_extent <- function(sf_obj, crs = 4326) {
  geo_check(sf_obj)
  
  sf_obj <- sf_obj |> st_transform(crs)
  bbox <- sf::st_bbox(sf_obj)
  list(
    left = unname(bbox["xmin"]),
    bottom = unname(bbox["ymin"]),
    right = unname(bbox["xmax"]),
    top = unname(bbox["ymax"])
  )
}

#' Fusionner deux objets sf
#'
#' Cette fonction fusionne les géométries de deux objets `sf` en une seule géométrie. 
#' Elle fonctionne pour des objets de type homogène : `POLYGON`, `LINESTRING` ou `POINT`.
#'
#' @param x Un objet `sf`.
#' @param y Un objet `sf`.
#'
#' @return Un objet `sf` contenant une seule géométrie résultant de la fusion de `x` et `y`.
#'
#' @details
#' La fonction utilise `sf::st_union()` pour combiner les géométries. 
#' Le résultat est un objet `sf` avec une seule ligne, même si `x` et `y` contenaient plusieurs entités.
#' Si les types de géométrie de `x` et `y` sont différents, la fonction peut générer un `GEOMETRYCOLLECTION`.
#'
#' @examples
#' library(sf)
#' # Exemple avec des polygones
#' poly1 <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(0,0),c(1,0),c(1,1),c(0,1),c(0,0))))))
#' poly2 <- st_sf(geometry = st_sfc(st_polygon(list(rbind(c(1,1),c(2,1),c(2,2),c(1,2),c(1,1))))))
#' geo_union(poly1, poly2)
#'
#' @seealso \code{\link[sf]{st_union}}, \code{\link[sf]{st_geometry}}
#' 
#' @export
#' 
geo_union <- function(x, y){
  geo_check(x, y)
  merged_geom <- sf::st_union(sf::st_geometry(x), sf::st_geometry(y))
  sf::st_sf(geometry = merged_geom)
}

#' Correction topologique des géométries d'un objet `sf` avec `qgisprocess`
#' 
#' La fonction corrige la topologie d'un objet `sf` avec `qgisprocess::qgis_run_algorithm("grass:v.clean")`.
#' 
#' @param sf_obj Objet `sf` dont la topologie doit être corriger.
#' @param tool `character`, default `"snap"`. L'outil GRASS à utiliser (ex: "snap", "rmdupl", "break", etc.).
#' @param snap_tolerance `numeric`, default `0.05`. Tolérance pour le snap (en mètres).
#' @param min_area `numeric`, default `0.1`. Aire minimale à conserver (en m²).
#' @param verbose `logical`, default `"TRUE"`. Affiche des messages informatifs sur le déroulement du traitement.
#'
#' @return L'objet `sf_obj` corrigé
#'
#' @importFrom qgisprocess qgis_run_algorithm qgis_providers
#' @importFrom sf st_write st_read st_make_valid
#'
#' @export
#' 
geo_clean_sf_with_grass <- function(sf_obj,
                                    tool = "snap",
                                    snap_tolerance = 0.05,
                                    min_area = 0.1,
                                    verbose = TRUE) {
  
  if (verbose){color_cat("Corrections topologiques avec grass:v.clean :")}
  geo_check(sf_obj)
  
  # Vérifie que GRASS est bien dispo via QGIS
  providers <- qgisprocess::qgis_providers()
  if (!"GRASS" %in% providers$provider_title) {
    stop("Le fournisseur GRASS n'est pas disponible dans QGIS.")
  }
  
  # Fichier temporaire d'entrée/sortie
  input_path <- tempfile(fileext = ".gpkg")
  output_path <- tempfile(fileext = ".gpkg")
  
  # Écriture du fichier temporaire
  sf::st_write(sf_obj, input_path, quiet = TRUE)
  
  # Appel à v.clean via QGIS
  result <- suppressMessages(
    qgisprocess::qgis_run_algorithm("grass:v.clean",
                                    input = input_path,
                                    type = "area",
                                    tool = tool,
                                    output = output_path,
                                    GRASS_SNAP_TOLERANCE_PARAMETER = snap_tolerance,
                                    GRASS_MIN_AREA_PARAMETER = min_area
    )
  )
  
  # Lecture du résultat
  cleaned_sf <- sf::st_read(output_path, quiet = TRUE, stringsAsFactors = FALSE)[, -1] |>
    st_make_valid()
  if (verbose){color_cat(" > Corrections réalisées", "green")}
  
  return(cleaned_sf)
}

#' Vérifie que les objets sont de type `sf`, valides, non vides et de type géométrique attendu
#'
#' Cette fonction vérifie que tous les objets passés en argument sont :
#' - des objets `sf` (optionnel, `check_class = TRUE`),
#' - valides (`sf::st_is_valid()` ; optionnel),
#' - non vides (`!sf::st_is_empty()` ; optionnel),
#' - avec une géométrie de type autorisé (optionnel).
#'
#' @param ... Un ou plusieurs objets à tester.
#' @param check_class Vérifie que l'objet est bien de classe `sf`. Par défaut `TRUE`.
#' @param check_valid Vérifie que les géométries sont valides. Par défaut `TRUE`.
#' @param check_empty Vérifie que les géométries ne sont pas vides. Par défaut `TRUE`.
#' @param allowed_geom_classes Vecteur de classes géométriques autorisées. Par défaut `c("sfc_POLYGON", "sfc_MULTIPOLYGON")`.
#'        Si NULL, ne vérifie pas la classe géométrique.
#'
#' @return Rien. Lève une erreur descriptive si une condition n’est pas remplie.
#'
#' @importFrom sf st_is_valid st_is_empty 
#' 
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' geo_check(nc)  # Ok
#'
#' df <- data.frame(x = 1:3)
#' \dontrun{
#' geo_check(nc, df)  # Erreur : 'df' n'est pas un sf
#' }
#'
#' nc_invalid <- nc
#' nc_invalid$geometry[[1]] <- st_geometrycollection()  # vide
#' \dontrun{
#' geo_check(nc_invalid)  # Erreur : contient des géométries vides
#' }
#'
geo_check <- function(..., 
                      check_class = TRUE, 
                      check_valid = TRUE, 
                      check_empty = TRUE,
                      allowed_geom_classes = "POLYGON") {
  
  dots <- list(...)
  arg_names <- as.list(substitute(list(...)))[-1L]
  
  for (i in seq_along(dots)) {
    obj <- dots[[i]]
    name <- deparse(arg_names[[i]])
    
    # Vérifie la classe sf
    if (check_class && !inherits(obj, "sf")) {
      stop(sprintf("L'objet '%s' doit être un objet de type 'sf'.", name), call. = FALSE)
    }
    
    # Vérifie la validité géométrique
    if (check_valid && any(!sf::st_is_valid(obj))) {
      stop(sprintf("L'objet '%s' contient des géométries invalides.", name), call. = FALSE)
    }
    
    # Vérifie qu’il n’est pas vide
    if (check_empty && any(sf::st_is_empty(obj))) {
      stop(sprintf("L'objet '%s' contient des géométries vides.", name), call. = FALSE)
    }
    
    # Vérifie les types géométriques
    if (!is.null(allowed_geom_classes)) {
      if (get_geom_type(obj) != allowed_geom_classes) {
        stop(sprintf("L'objet '%s' doit avoir une géométrie de type : %s (trouvé : '%s').",
                     name,
                     paste(allowed_geom_classes, collapse = ", "),
                     get_geom_type(obj), call. = FALSE))
      }
    }
  }
}

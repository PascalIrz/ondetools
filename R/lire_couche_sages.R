#' Lecture de la couche des SAGEs
#'
#' Cette fonction permet d'importer dans R le fichier shapefile polygones des SAGEs.
#'     Par défaut, le système de coordonnées (SCR) est le Lambert 93 (EPSG:2154).
#'
#' @param fichier_shp Chaîne de caractères indiquant le chemin vers le fichier shapefile des SAGEs. Il peut être
#'     absolu ou relatif.
#' @param scr Numérique entier indiquant le Système de Coordonnées de Référence par son code EPSG.
#'     A titre d'exemples le code du Lambert II étendu est 27572 et celui dui WGS84 est 4326.
#'
#' @return La fonction retourne un objet de classe géographique sf comprenant les polygones correspondant
#'     aux périmètres des SAGEs.
#' @export
#' @importFrom sf st_read st_crs
#'
#' @examples \dontrun{
#' sages <- lire_couche_sages(fichier_shp = couche_sage, scr = 2154)}
#'
lire_couche_sages <- function(fichier_shp, scr = 2154) {

  sages <- st_read(fichier_shp)

  sf::st_crs(sages) = scr

  return(sages)


}


#' Créer la couche géographique des stations
#'
#' Cette fonction crée un objet géographique à partir du fichier ONDE au format "long" et de ses colonnes de coordonnées
#'     ("CoordXSiteHydro", "CoordYSiteHydro"). Le système de projection est Lambert 93.
#'
#' @param onde_df Le dataframe contenant les données ONDE. NB les colonnes ne doivent pas avoir été renommées.
#'
#' @return La sortie est l'objet géographique de classe sf constitué des points correspondant à chacune des stations. Il comprend
#'     autant de lignes qu'il y a de stations.
#' @export
#' @importFrom sf st_as_sf st_crs
#' @importFrom dplyr select mutate
#'
#' @examples \dontrun{
#' stations_onde_geo <- creer_couche_geo_stations(onde_df = onde)}
#'
#'
creer_couche_geo_stations <- function(onde_df) {

  stations_onde_geo <- onde_df %>%
    st_as_sf(coords = c("CoordXSiteHydro", "CoordYSiteHydro"))

  sf::st_crs(stations_onde_geo) = 2154

  stations_onde_geo <- stations_onde_geo %>%
    select(CdSiteHydro, LbRegion) %>%
    mutate(LbRegion = as.character(LbRegion)) %>%
    unique()

  return(stations_onde_geo)

}



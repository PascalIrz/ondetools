#' Ajout des données d'assecs aux stations
#'
#' Cette fonction permet d'ajouter à l'objet géographique (classe `sf` points) des stations les attributs
#'     caractérisant les assecs. #'     La correspondance se fait par une jointure sur les codes des
#'     stations CdSiteHydro.
#'
#' @param stations_geo L'objet géographique points des stations
#' @param assecs_df Le dataframe contenant les données d'assecs
#'
#' @return La fonction renvoit l'objet géographique points des stations auquel ont été ajoutées les données
#'     d'assecs.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate
#' @importFrom tidyr replace_na
#'
#' @examples
#'
ajouter_donnees_assecs_aux_stations <-  function(stations_geo, assecs_df) {

  stations_geo <- stations_geo %>%
    left_join(assecs_df) %>%
    mutate(pourcentage_assecs = replace_na(pourcentage_assecs, replace = 0))

  return(stations_geo)

}


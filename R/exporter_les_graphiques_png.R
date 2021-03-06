#' Export des graphiques en format png
#'
#' Cette fonction exporte chacun des graphiques au format png, le nomme selon les codes SAGEs et station +
#'     les intitulés des SAGEs. Ils sont stockés dans le répertoire fourni par l'utilisateur. Si le répertoire
#'     n'existe pas, la fonction le crée. Sinon, elle écrase son contenu antérieur. Au final, il y a un fichier
#'     par station. L'exécution de la fonction prend environ une seconde par station.
#'
#' @param stations_geo La couche géographique des stations (classe `sf`), contenant la variable "CdSiteHydro".
#'     du fichier ONDE.
#' @param liste_graphiques La liste contenant les graphiques, qui aura été auparavant produite par la
#'     fonction produire_graph_pour_toutes_les_stations().
#' @param repertoire Chaîne de caractère indiquant le chemin vers le répertoire de stockage des fichiers png.
#'
#' @return La fonction produit autant de fichiers png qu'il y a de stations.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate pull
#' @importFrom stringr str_replace_all str_squish
#' @importFrom ggplot2 ggsave
#' @importFrom purrr pwalk
#'
#' @examples \dontrun{
#' exporter_les_graphiques_png(stations_geo = stations_onde_geo,
#' liste_graphiques = graphiques,
#' repertoire = 'processed_data/graphiques')
#' }
exporter_les_graphiques_png <- function(stations_geo, liste_graphiques, repertoire)  {


  # nommage des fichiers graphiques
  if(exists("NOM", where = stations_geo)) {

    noms_graphiques <- stations_geo %>%
      select(CdSiteHydro, NOM) %>%
      mutate(NOM = iconv(NOM, from = "UTF-8", to = "ASCII//TRANSLIT"),
             NOM = str_replace_all(NOM, pattern = "[-,]", replacement = "_"),
             nom = paste0(CdSiteHydro, "_", NOM, ".png"),
             nom = str_squish(nom)) %>%
      pull(nom)
  } else {
    noms_graphiques <- stations_geo %>%
      pull(CdSiteHydro)%>%
      paste0(".png")
  }


  dir.create(repertoire, showWarnings = FALSE)

  # enregistrement d'un fichier png par graphique avec le nom qui va bien
  # (adapté de https://r4ds.had.co.nz/iteration.html#walk)
  pwalk(list(noms_graphiques, liste_graphiques), ggsave, path = repertoire)


}




#' Produire les graphiques pour toutes les stations
#'
#' Cette fonction est une "moulinette" qui permet d'appliquer la fonction de production du graphique non pas à une
#'     station, mais à un ensemble.
#'
#' @param stations Le vecteur (caractère) des identifiants des stations au sens du champ "CdSiteHydro" de la base ONDE.
#' @param fonction_graphique La fonction graphique à appliquer sur chacune des stations. Ici il s'agit de la fonction
#'     produire_graph_pour_toutes_les_stations().
#' @param onde_df_ts_mois Le dataframe des données ONDE qui a été complété pour les mois où il n'y a pas eu d'observations.
#'     Produit par le fonction completer_observations_mois_manquants().
#' @param couleurs Un vecteur nommé qui associe à chacune des modalités d'observation (ex : écoulement visible)
#'     une couleur.
#'
#' @return La fonction produit un objet de classe "list" qui contient autant de graphiques qu'il y a de stations.
#' @export
#'
#' @importFrom purrr map
#'
#' @examples
#'
produire_graph_pour_toutes_les_stations <- function(stations, fonction_graphique, onde_df_ts_mois, couleurs) {

graphiques <- map(.x = stations,
                  .f = fonction_graphique,
                  onde_df = onde_df_ts_mois,
                  couleurs = mes_couleurs)

return(graphiques)

}



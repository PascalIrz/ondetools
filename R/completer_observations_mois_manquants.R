#' Ajout des mois sans observations
#'
#' Afin de permettre une représentation graphique de l'ensemble de la période de collecte des données
#'     d'étiage, cette fonction ajoute à un tableau de données ONDE, qui ne comprend que des données
#'     d'observations, tous les mois pour lesquels il n'y a pas eu d'observations. Le tableau de sortie
#'     comprend donc un nombre de lignes qui est le produit de nombre de stations par le nombre
#'     d'années d'observations par 12 mois : 12 x nb_années x nb_stations.
#'
#' @param onde_df Le dataframe contenant les données ONDE en format "long".
#' @param stations Le vecteur (caractère) des identifiants des stations au sens du champ "CdSiteHydro" de la base ONDE.
#'
#' @return Le dataframe contenant les données ONDE en format "long" complété avec les mois sans données pour lesquels
#'     l'observation est notée NA.
#' @export
#' @importFrom magrittr %>%
#' @importFrom stringr str_wrap
#' @importFrom dplyr filter select mutate
#' @importFrom tidyr complete replace_na
#' @importFrom forcats fct_recode
#'
#' @examples \dontrun{
#' onde_ts_mois <- completer_observations_mois_manquants(onde_df = onde,
#' stations = codes_stations)}
#'
#'
completer_observations_mois_manquants <- function(onde_df, stations) {

  onde_ts_mois <- onde_df %>%
    filter(CdSiteHydro %in% stations) %>%
    select(CdSiteHydro, LbSiteHydro, Annee, Mois, RsObservationNat) %>%
    complete(CdSiteHydro, Annee, Mois, fill = list(RsObservationNat = NA)) %>%
<<<<<<< HEAD
    mutate(#RsObservationNat = replace_na(RsObservationNat, replace = "NA"),
=======
    mutate(RsObservationNat = replace_na(as.character(RsObservationNat), replace = "NA"),
>>>>>>> master
           RsObservationNat = as.factor(RsObservationNat),
           RsObservationNat = fct_recode(RsObservationNat, "Ecoulement visible" = "1",
                                         "Ecoulement non visible" = "2", "Assec" = "3",
                                         "Observation impossible" = "4"),
           RsObservationNat = str_wrap(RsObservationNat, 12))

  return(onde_ts_mois)

}


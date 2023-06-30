#' Fonction pour calculer la récurrence des assecs consécutifs sur les stations
#'
#' @param onde_df un tableau de données onde téléchargé avec la
#'  fonction \code{telecharger_donnees_onde_api}
#'
#' @return un tableau de synthèse avec le nombre de stations (et pourcentage) par durée
#'  d'assecs (de 1 à 5 mois consécutifs) sur la période de campagne usuelle (mai à septembre)
#' @export
#'
#' @importFrom data.table rleid
#' @importFrom dplyr filter mutate distinct arrange group_by row_number summarise n ungroup
#'
#' @examples
#' \dontrun{
#' onde_14 <- telecharger_donnees_onde_api(dpt = c('14'))
#' calculer_recurrence_assecs(onde_df = onde_14)
#' }
#'
calculer_recurrence_assecs <- function(onde_df){

  onde_df %>%
    dplyr::filter(libelle_type_campagne == 'usuelle') %>%
    dplyr::mutate(Mois = format(as.Date(date_campagne), '%m')) %>%
    dplyr::distinct(code_station, libelle_station, Annee, Mois, libelle_type_campagne, libelle_ecoulement) %>%
    dplyr::arrange(code_station, Annee, Mois) %>%
    dplyr::group_by(
      code_station,
      Annee,
      ID = data.table::rleid(code_station, libelle_ecoulement == 'Assec' )
    ) %>%
    dplyr::mutate(
      mois_assec_consec = ifelse(
        libelle_ecoulement == 'Assec',
        dplyr::row_number(), 0L
      )
    ) %>%
    dplyr::mutate(
      mois_assec_consec = ifelse(
        libelle_ecoulement == 'Assec',
        dplyr::row_number(), 0L
      )
    ) %>%
    dplyr::group_by(
      Annee, code_station) %>%
    dplyr::summarise(.,
                     max_nb_mois_assec  = max(mois_assec_consec),
                     .groups = "drop"
    ) %>%
    dplyr::group_by(. ,
                    Annee, max_nb_mois_assec) %>%
    dplyr::summarise(., nb_sta = dplyr::n(), .groups = "drop_last") %>%
    dplyr::mutate(frq = prop.table(nb_sta)) %>%
    dplyr::mutate(Label = ifelse(max_nb_mois_assec == '1' | max_nb_mois_assec == '0',
                                 paste0(max_nb_mois_assec, " mois"),
                                 paste0(max_nb_mois_assec, " mois cons\u00e9cutifs"))) %>%
    dplyr::ungroup()
}

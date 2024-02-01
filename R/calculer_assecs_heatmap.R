#' Fonction pour calculer les pourcentages de stations en assec

#' @description
#' Calcule les pourcentages de stations en assec (sur les mois de campagnes usuelles)
#'  et met en forme du tableau pour le graphique de type \code{heatmap}
#'
#' @param onde_df un tableau de données onde téléchargé avec la
#'  fonction \code{telecharger_donnees_onde_api}
#'
#' @return un tableau résumant au format pour le graphique avec la
#'  fonction \code{produire_graph_heatmap_assecs}
#' @export
#'
#' @importFrom dplyr filter summarise n mutate arrange
#' @importFrom glue glue
#' @importFrom tidyr complete
#'
#' @examples
#' \dontrun{
#' onde_14 <- telecharger_donnees_onde_api(dpt = c('14'))
#' calculer_assecs_heatmap(onde_df = onde_14)
#' }

calculer_assecs_heatmap <- function(onde_df) {
  onde_df %>%
    dplyr::filter(libelle_type_campagne == 'usuelle') %>%
    dplyr::mutate(Mois = format(as.Date(date_campagne), '%m')) %>%
    dplyr::group_by(Mois, Annee, libelle_type_campagne, code_campagne, code_departement) %>%
    dplyr::summarise(n_donnees = dplyr::n(),
                     n_assecs = length(libelle_ecoulement[libelle_ecoulement == 'Assec']),
                     .groups = "drop") %>%
    dplyr::mutate(pourcentage_assecs = round(n_assecs / n_donnees * 100, digits = 2),
                  taille_point = sqrt(pourcentage_assecs+1)) %>%
    dplyr::arrange(Annee, Mois) %>%
    tidyr::complete(Annee, Mois, libelle_type_campagne) %>%
    dplyr::mutate(Mois = factor(Mois)) %>%
    # label pourcentage
    dplyr::mutate(Label = ifelse(is.na(n_assecs), "", glue::glue("{n_assecs}/{n_donnees}"))) %>%
    # label (nb stations / nb total)
    dplyr::mutate(Label_p = ifelse(is.na(n_assecs), "", glue::glue("{round(pourcentage_assecs,0)}%")))
}

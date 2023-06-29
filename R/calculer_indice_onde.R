#' Fonction pour calculer les indices ONDE sur les campagnes usuelles
#'
#' La fonction permet de calculer les indices ONDE départementaux à partir des campagnes
#'  usuelles d'un tableau. L'indice est une note variant de 1 à 10, en prenant en compte
#'  les nombres de stations avec écoulements interrompus et écoulements continus.
#'
#' @param onde_df un tableau de données onde téléchargé avec la fonction \code{telecharger_donnees_onde_api}
#' @param force_complementaire argument booléen. Si \code{TRUE}, force le calcul des indices sur toutes les campagnes
#'  (usuelles et complémentaires avec un minimum de 25 observations par campagne). Par défaut, égal à \code{FALSE}
#'
#' @return retourne un tableau avec une colonne 'indice'
#' @export
#'
#' @importFrom dplyr filter group_by summarise
#'
#' @examples
#' \dontrun{
#' calculer_indice_onde(onde_df = onde)
#' }

calculer_indice_onde <- function(onde_df = NULL,
                                 force_complementaire = F) {
  if (force_complementaire == FALSE) {
    {
      {
        onde_df
      }
    } %>%
      dplyr::filter(libelle_type_campagne == 'usuelle') %>%
      dplyr::group_by(code_campagne,
                      code_departement,
                      date_campagne,
                      libelle_type_campagne,
                      Annee) %>%
      dplyr::summarise(
        nb_sta = length(unique(code_station)),
        nb_ecoul_cont = sum(grepl(
          "Ecoulement visib.", libelle_ecoulement
        )),
        nb_ecoul_interr = sum(grepl("Ecoulement non", libelle_ecoulement)),
        nb_NA = sum(
          grepl(
            "Observation impossible|Donn\u00e9e manquante",
            libelle_ecoulement
          )
        ),
        indice = if_else(nb_NA == 0, round(((5 * nb_ecoul_interr + 10 * nb_ecoul_cont) / nb_sta
        ), 1), NA)
      )

  } else {
    {
      {
        onde_df
      }
    } %>%
      dplyr::group_by(code_campagne,
                      code_departement,
                      date_campagne,
                      libelle_type_campagne,
                      Annee) %>%
      dplyr::summarise(
        nb_sta = length(unique(code_station)),
        nb_ecoul_cont = sum(grepl(
          "Ecoulement visib.", libelle_ecoulement
        )),
        nb_ecoul_interr = sum(grepl("Ecoulement non", libelle_ecoulement)),
        nb_NA = sum(
          grepl(
            "Observation impossible|Donn\u00e9e manquante",
            libelle_ecoulement
          )
        ),
        indice = if_else(nb_NA == 0 & nb_sta >= 25, round(((5 * nb_ecoul_interr + 10 * nb_ecoul_cont) / nb_sta
        ), 1), NA)
      )

  }

}

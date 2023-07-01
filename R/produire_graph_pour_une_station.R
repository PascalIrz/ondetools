#' Produire le graphique des assecs pour une station
#'
#' Cette fonction permet de produire le graphique des assecs pour une station donnée sur l'ensemble des années
#'     de la série.
#'
#' @param code_station L'identifiant de la station ONDE (Variable CdSiteHydro).
#' @param onde_df Le dataframe en format "long" qui contient les données ONDE.
#' @param couleurs Un vecteur nommé qui associe à chacune des modalités d'observation (ex : écoulement visible)
#'     une couleur.
#'
#' @return Le graphique obtenu par ggplot.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter pull
#' @importFrom ggplot2 ggplot aes geom_point coord_flip scale_color_manual scale_y_continuous scale_x_continuous
#' @importFrom ggplot2 labs guides guide_legend
#' @importFrom stringr str_wrap
#'
#' @examples \dontrun{
#' mes_couleurs <- c("Ecoulement\nvisible" = "blue",
#' "Ecoulement\nnon visible" = "brown",
#' "Assec" = "red",
#' "Observation\nimpossible" = "grey",
#' "NA" = "grey")
#'
#' produire_graph_pour_une_station(code_station = "J0014011",
#' onde_df = onde_ts_mois,
#' couleurs = mes_couleurs)
#' }
produire_graph_pour_une_station <- function(code_station, onde_df, couleurs){

  prov <- onde_df %>%
    filter(CdSiteHydro == code_station)

  nom_station <- prov %>%
    filter(!is.na(LbSiteHydro)) %>%
    pull(LbSiteHydro)

  nom_station <- nom_station[1]

  prov %>%
    ggplot(aes(x = Annee,
               y = Mois %>% as.numeric,
               color = stringr::str_wrap(RsObservationNat, 12))) +
    geom_point(shape = 15, size = 12) +
    coord_flip() +
    scale_color_manual(values = couleurs, na.value = "grey") +
    scale_y_continuous(breaks = 1:12, labels = 1:12) +
    scale_x_continuous(breaks = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T),
                       labels = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T)) +
    labs(x = "", y = "Mois", title = nom_station) +
    guides(color = guide_legend(title = NULL))

}

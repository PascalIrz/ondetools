#' Passage du tableau de données en format large
#'
#' Cette fonction "pivote" le tableau ONDE produit par la fonction assembler_fichiers_onde_annuels_csv(),
#'     qui est en format "long" , pour le mettre en format "large", avec une colonne par combinaison
#'     annee_mois.
#'
#' @param onde_df_long dataframe en format long issu de la fonction gerer_les_campagnes()
#'
#' @return le dataframe en format large (une colonne par annee_mois)
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select
#' @importFrom tidyr pivot_wider
#'
#' @examples \dontrun{
#' onde_lb_large <- passer_en_format_large(onde_df_long = onde)}
#'
passer_en_format_large <- function(onde_df_long) {

  onde_df_large <- onde_df_long %>%
    mutate(annee_mois = paste(Annee, Mois, sep = '_')) %>%
    select(-Annee, -Mois) %>%
    pivot_wider(names_from = annee_mois, values_from = RsObservationNat) %>%
    select(sort(names(.))) %>%
    select(CdSiteHydro, LbSiteHydro, NomEntiteHydrographique, CdTronconHydrographique, LbCommune, CdCommune, CdDepartement, LbRegion,
           NomCircAdminBassin, CoordXSiteHydro, CoordYSiteHydro, ProjCoordSiteHydro, everything())

  return(onde_df_large)

}

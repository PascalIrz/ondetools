#' Gestion des campagnes d'observation de terrain
#'
#' Avec cette fonction, les données sont filtrées pour ne conserver qu'une observation
#'     par mois. De mai à septembre inclus, c'est l'observation de la campagne
#'     "usuelle" qui est retenue. D'octobre à avril, c'est la plus sèche des observations
#'     "complémentaires".
#'
#' @param onde_df Le dataframe contenant les données ONDE mis en forme par la fonction
#'     assembler_fichiers_onde_annuels_csv().
#'
#' @return Le dataframe avec une donnée par mois après application de règles concernant les campagnes
#'     usuelles et complémentaires.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by summarise ungroup bind_rows
#'
#' @examples\dontrun{
#' onde <- gerer_les_campagnes(onde_df = onde)}
#'
gerer_les_campagnes <- function (onde_df){

  # été
  onde_ete <- onde_df %>%
    filter(TypeCampObservations == "usuelle", Mois %in% c('05', '06', '07', '08', '09')) %>%
    select(-(TypeCampObservations:LbRsObservationNat), -FLG) %>%
    group_by(CdSiteHydro, LbSiteHydro, NomEntiteHydrographique, CdTronconHydrographique, LbCommune, CdCommune,
             CdDepartement, LbRegion, NomCircAdminBassin, CoordXSiteHydro, CoordYSiteHydro, ProjCoordSiteHydro,
             Annee, Mois) %>%
    summarise(RsObservationNat = max(RsObservationNat, na.rm = TRUE))  %>%
    ungroup()

  # hiver
  onde_hiver <- onde_df %>%
    filter(!(Mois %in% c('05', '06', '07', '08', '09'))) %>%
    select(-(TypeCampObservations:LbRsObservationNat), -FLG) %>%
    group_by(CdSiteHydro, LbSiteHydro, NomEntiteHydrographique, CdTronconHydrographique, LbCommune, CdCommune,
             CdDepartement, LbRegion, NomCircAdminBassin, CoordXSiteHydro, CoordYSiteHydro, ProjCoordSiteHydro,
             Annee, Mois) %>%
    summarise(RsObservationNat = max(RsObservationNat, na.rm = TRUE))  %>%
    ungroup()

  # empilement des valeurs été et hiver
  onde_lb <- bind_rows(onde_ete, onde_hiver)

  return(onde_lb)
}

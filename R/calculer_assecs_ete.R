#' Caractérisation des assecs
#'
#' Caractérisation des assecs. Comme le recours à des campagnes dites "complémentaires" varie largement
#'     selon les régions et départements, pour que la caractérisation ait un sens, elle est restreinte
#'     aux observations des mois d'été, donc des données des campagnes "usuelles".
#'
#' @param onde_df Le dataframe contenant les données ONDE en format long.
#'
#' @return Un dataframe donnant pour chaque station (ligne) le nombre d'observations, le nombre d'assecs
#'     et le pourcentage des observations où la station était en assec.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter pull group_by summarise ungroup mutate
#' @importFrom magrittr %>%
#' @importFrom magrittr %>%
#'
#' @examples
#'
calculer_assecs_ete <- function(onde_df) {

  assecs <- onde_df %>%
    select(CdSiteHydro, LbSiteHydro, Annee, Mois, RsObservationNat) %>%
    filter(CdSiteHydro %in% (stations_onde_geo %>% pull(CdSiteHydro))) %>%
    filter(Mois %in% c('05', '06', '07', '08', '09')) %>%
    group_by(CdSiteHydro, LbSiteHydro) %>%
    summarise(n_donnees = n(),
              n_assecs = length(RsObservationNat[RsObservationNat==3])) %>%
    ungroup() %>%
    mutate(pourcentage_assecs = round(n_assecs / n_donnees * 100, digits = 2),
           taille_point = sqrt(pourcentage_assecs))

  return(assecs)

}




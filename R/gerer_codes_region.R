#' Gestion des codes régions
#'
#' Gestion des codes régions, en particulier des problèmes issus des fusions qui ont changé les intitulés.
#'
#' @param onde_df Un dataframe contenant des données ONDE mis en forme par la fonction
#'     assembler_fichiers_onde_annuels_csv().
#'
#' @return La sortie de la fonction est un dataframe contenant des données ONDE où les codes région
#'     ont été recodés pour être conformes au découpage régional en 2020.
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom forcats fct_recode
#'
#' @examples \dontrun{
#' onde_bretagne_2019 <- gerer_codes_region(onde_bretagne_2019)}
#'
gerer_codes_region <- function (onde_df) {

  onde_df <- onde_df %>%
    mutate(LbRegion = as.factor(LbRegion),
           LbRegion = fct_recode(LbRegion, Normandie = "BASSE-NORMANDIE",
                                 Normandie = "NORMANDIE",
                                 Nouvelle_aquitaine = "LIMOUSIN",
                                 Nouvelle_aquitaine = "NOUVELLE-AQUITAINE",
                                 ARA = "AUVERGNE-RHÔNE-ALPES",
                                 Nouvelle_aquitaine = "POITOU-CHARENTES",
                                 ARA = "AUVERGNE",
                                 ARA = "RHONE-ALPES",
                                 CVdL = "CENTRE",
                                 CVdL = "CENTRE-VAL-DE-LOIRE",
                                 Occitanie = "LANGUEDOC-ROUSSILLON",
                                 Occitanie = "OCCITANIE",
                                 BFC = "BOURGOGNE",
                                 BFC = "BOURGOGNE-FRANCHE-COMTÉ",
                                 PdL = "PAYS-DE-LA-LOIRE",
                                 Bretagne = "BRETAGNE"))

  return(onde_df)

}

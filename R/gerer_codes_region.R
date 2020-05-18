#' Gestion des codes régions
#'
#' Gestion des codes régions, en particulier des problèmes issus des fusions qui ont changé les intitulés.
#' La mise à jour des régions sur les fichiers de données a été opérée début 2020 donc cette fonction
#      n'est utile que pour les fichiers qui ont été téléchargés antérieurement. 
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
                                 Normandie = "HAUTE-NORMANDIE",
                                 Nouvelle_aquitaine = "LIMOUSIN",
                                 Nouvelle_aquitaine = "NOUVELLE-AQUITAINE",
                                 Nouvelle_aquitaine = "POITOU-CHARENTES",
                                 Nouvelle_aquitaine = "AQUITAINE",
                                 ARA = "AUVERGNE",
                                 ARA = "AUVERGNE-RHÔNE-ALPES",
                                 ARA = "RHONE-ALPES",
                                 CVdL = "CENTRE",
                                 CVdL = "CENTRE-VAL-DE-LOIRE",
                                 Occitanie = "LANGUEDOC-ROUSSILLON",
                                 Occitanie = "OCCITANIE",
                                 Occitanie = "MIDI-PYRENEES",
                                 BFC = "BOURGOGNE",
                                 BFC = "BOURGOGNE-FRANCHE-COMTÉ",
                                 BFC = "FRANCHE-COMTE",
                                 PdL = "PAYS-DE-LA-LOIRE",
                                 Bretagne = "BRETAGNE",
                                 Grand_Est = "LORRAINE",
                                 Grand_Est = "CHAMPAGNE-ARDENNE",
                                 Grand_Est = "ALSACE",
                                 Grand_Est = "GRAND-EST",
                                 HdF = "PICARDIE",
                                 HdF = "NORD-PAS-DE-CALAIS",
                                 HdF = "HAUTS-DE-FRANCE",
                                 IdF = "ILE-DE-FRANCE",
                                 PACA = "PROVENCE-ALPES-COTE-D'AZUR",
                                 PACA = "PROVENCE-ALPES-CÔTE-D-AZUR",
                                 Corse = "CORSE"))

  return(onde_df)

}

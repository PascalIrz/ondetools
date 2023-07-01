#' Calculer un bilan des types d'écoulement pour les campagnes ONDE usuelles selon
#'  la typologie nationale ou départementale
#'
#' @param onde_df un tableau de données onde téléchargé avec la fonction \code{telecharger_donnees_onde_api}
#' @param referentiel_onde le nom de la typologie à utiliser,
#'  soit \code{Typologie nationale} ou \code{Typologie départementale}
#' @param mod Le nom pour la colonne créée pour la typologie. Par exemple, soit
#'  \code{lib_ecoul3mod} pour la typologie nationale ou \code{lib_ecoul4mod} pour la typologie départementale,
#' @param mod_levels les niveaux de facteurs souhaités en fonction des typologies.
#' @param force_complementaire booléen, si TRUE les complémentaires sont prises en compte dans le calcul. Par défaut, FALSE.
#' @param ... paramètre optionnel
#'
#' @return un dataframe avec les pourcentages par types d’écoulement selon la typologie choisie
#' @export
#'
#' @importFrom dplyr filter mutate case_when group_by summarise n arrange desc across
#' @importFrom glue glue
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' ## bilan écoulement sur typologie nationale sur les usuelles uniquement
#' calculer_bilan_ecoulement(onde_df = onde_df,
#'                         mod = lib_ecoul,
#'                         mod_levels = c("Ecoulement visible",
#'                                        "Ecoulement non visible",
#'                                        "Assec",
#'                                        "Observation impossible",
#'                                        "Donnée manquante"),
#'                         referentiel_onde = "Typologie nationale",
#'                         force_complementaire = F)
#'
#' ## bilan écoulement sur typologie départementale avec les complémentaires
#' calculer_bilan_ecoulement(onde_df = onde_df,
#'                           mod = lib_ecoul,
#'                           mod_levels = c("Ecoulement visible acceptable",
#'                                          "Ecoulement visible faible",
#'                                          "Ecoulement non visible",
#'                                          "Assec",
#'                                          "Observation impossible",
#'                                          "Donnée manquante"),
#'                           referentiel_onde = "Typologie départementale",
#'                           force_complementaire = F)
#'
#' ## bilan écoulement sur typologie nationale avec les complémentaires
#' calculer_bilan_ecoulement(onde_df = onde_df,
#'                         mod = lib_ecoul,
#'                         mod_levels = c("Ecoulement visible",
#'                                        "Ecoulement non visible",
#'                                        "Assec",
#'                                        "Observation impossible",
#'                                        "Donnée manquante"),
#'                         referentiel_onde = "Typologie nationale",
#'                         force_complementaire = T)
#'
#'}
calculer_bilan_ecoulement <-
  function(onde_df,
           referentiel_onde = 'Typologie nationale',
           mod,
           mod_levels,
           force_complementaire = F,
           ...) {

    onde_df <-
      if(force_complementaire == FALSE){
        onde_df %>%
          dplyr::filter(libelle_type_campagne == 'usuelle')
      } else {
        onde_df
      }

    onde_df %>%
      {
        if ({{referentiel_onde}} == 'Typologie nationale'){

          dplyr::mutate(.,
                        {{mod}} := dplyr::case_when(
                          libelle_ecoulement == 'Ecoulement visible faible' ~ 'Ecoulement visible',
                          libelle_ecoulement == 'Ecoulement visible acceptable' ~ 'Ecoulement visible',
                          TRUE ~ libelle_ecoulement
                        )) %>%
            dplyr::group_by(.,
                            date_campagne,
                            code_campagne,
                            code_departement,
                            libelle_type_campagne,
                            {{mod}})
        } else {


          dplyr::mutate(.,
                        {{mod}} := dplyr::case_when(
                          libelle_ecoulement == 'Ecoulement visible' ~ 'Ecoulement visible acceptable',
                          TRUE ~ libelle_ecoulement
                        )) %>%
            dplyr::group_by(.,
                            date_campagne,
                            code_campagne,
                            code_departement,
                            libelle_type_campagne,
                            {{mod}})
        } } %>%
      dplyr::summarise(NB = dplyr::n(), .groups = "drop_last") %>%
      dplyr::mutate(frq = NB / sum(NB) *100) %>%
      dplyr::arrange(code_departement, dplyr::desc(date_campagne), ...) %>%
      dplyr::mutate(Label = ifelse(is.na(NB),"",glue::glue("{NB}"))) %>%
      dplyr::mutate(Label_p = ifelse(is.na(frq),"",glue::glue("{round(frq,0)}%"))) %>%
      dplyr::mutate(Mois = format(date_campagne, "%m"),
                    Annee = format(date_campagne, "%Y")) %>%
      dplyr::mutate(Typologie = rep({{referentiel_onde}}, dplyr::n())) %>%
      dplyr::mutate(
        dplyr::across(
          {{mod}},
          function(x) {
            factor(x, levels = mod_levels, ordered = TRUE)
          }
        )
      )
  }

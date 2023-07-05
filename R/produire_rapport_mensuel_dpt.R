#' Fonction pour produire un rapport mensuel départemental ONDE
#'
#' @param onde_df tableau contenant les données ONDE
#' @param code_departement un seul code (insee) de département (ex: '27') ou
#'  une liste de départements pour plusieurs rapports (ex: \code{c('14', '27', '76')})
#' @param annee_mois l'année et le mois du(des) rapport(s)
#' @param region_dr le nom de la DR qui sera affichée sur la page de garde du
#'  rapport
#' @param complementaire booléen, si TRUE génère un rapport pour une campagne complémentaire.
#'  par défaut FALSE.
#' @param dossier_sortie le dossier de sortie où le rapport sera stocké. Par défaut,
#'  le dossier de travail \code{getwd()}
#'
#' @importFrom cli cli_h1 cli_alert_success
#' @importFrom furrr future_walk
#' @importFrom glue glue
#' @importFrom lubridate ym
#' @importFrom progressr with_progress progressor
#' @importFrom rmarkdown render
#' @importFrom stringr str_extract
#'
#' @return Renvoie un rapport mensuel au format Word pour les données du réseau ONDE
#'  pour le département le mois et l'année spécifié
#' @export
#'
#' @examples
#' ## Un rapport pour le département 02 pour le mois de mai 2022 ('2022_05')
#' \dontrun{
#' produire_rapport_mensuel_dpt(onde_df = donnees_onde,
#'                              code_departement = '02',
#'                              annee_mois = "2022_05",
#'                              region_dr = 'Hauts-de-France',
#'                              complementaire = F,
#'                              dossier_sortie = "./OUTPUT")
#'
#' ## Rapports pour plusieurs départements (ex: 14, 27, 76) pour des complémentaires
#' ##  du mois de juin 2023 ('2023_06')
#'
#' produire_rapport_mensuel_dpt(onde_df = donnees_onde,
#'                              code_departement = c('14', '27', '76'),
#'                              annee_mois = "2022_06",
#'                              region_dr = 'Normandie',
#'                              complementaire = T,
#'                              dossier_sortie = "./OUTPUT")
#' }

produire_rapport_mensuel_dpt <- function(onde_df,
                                         code_departement,
                                         annee_mois,
                                         region_dr,
                                         complementaire = FALSE,
                                         dossier_sortie = getwd()) {

  options(future.rng.onMisuse = "ignore")

  if(length(code_departement)>1){
    cli::cli_h1("ONDE - cr\u00e9ation de rapports mensuels d\u00e9partementaux")
  } else {
    cli::cli_h1("ONDE - cr\u00e9ation d\'un rapport mensuel d\u00e9partemental")
  }

  annee_rapport <- stringr::str_extract(annee_mois, '[0-9]{1,4}')
  mois_rapport <- format(lubridate::ym(annee_mois), "%B %Y")

  if(complementaire == TRUE) {type_rapport <- "compl\u00e9mentaire"} else {type_rapport <- "usuelle"}

  progressr::with_progress({
    p <- progressr::progressor(steps = length(code_departement))

    furrr::future_walk(code_departement,
                       ~ {
                         p()
                         rmarkdown::render(
                           input = system.file(
                             "rmarkdown/templates/rapport_mensuel_onde_dpt/skeleton/skeleton.Rmd",
                             package = "ondetools"
                           ),
                           output_file = glue::glue("Bilan_ONDE_mensuel_{annee_mois}_{type_rapport}_SD{.x}.docx"),
                           output_dir = dossier_sortie,
                           quiet = TRUE,
                           params = list(
                             onde_df = onde_df,
                             code_departement = .x,
                             region_dr = region_dr,
                             annee_rapport = annee_rapport,
                             mois_rapport = mois_rapport,
                             type_rapport = type_rapport
                           )
                         )
                         cli::cli_alert_success(glue::glue("rapport {type_rapport} dpt. {.x} - p\u00e9riode {annee_mois} - OK !!! (cf. dossier {dossier_sortie})"))

                       })
  })
}


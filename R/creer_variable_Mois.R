#' Création de la variable Mois
#'
#' Création de la variable Mois en 2 caractères alphanumériques (+pratique pour les graphiques ultérieurs)

#'
#' @param onde_df Le dataframe contenant les données ONDE, mis en forme en format "long".
#'
#' @return Le dataframe contenant les données ONDE, mis en forme en format "long", auquel a été ajoutée
#'     une variable caractère "Mois" codée de "01" à "12".
#' @export
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd
#' @importFrom stringr str_pad
#'
#' @examples
#'
creer_variable_Mois <- function(onde_df) {

  onde_df <- onde_df %>%
    mutate(Mois = lubridate::ymd(DtRealObservation) %>%
             lubridate::month() %>%
             str_pad(width = 2, side = "left", pad = "0")) %>%
    filter(TRUE)

}

#' Assemblage des fichiers annuels
#'
#' Cette fonction permet d'assembler - en les empilant - les fichiers csv issus de la fonction
#'     telecharger_fichiers_onde_annuels() en un unique dataframe. La sortie est le dataframe national
#'     pour l'ensemble des années où la donnée est disponible.
#'
#' @param annual_onde_files_dir Chaîne de caractères indiquant le chemin vers le répertoire où se trouvent
#'     les fichiers annuels
#'
#' @return Dataframe national pour l'ensemble des années de données disponibles
#' @export
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
#' @importFrom data.table fread
#' @importFrom stringr str_replace_all
#'
#' @examples
#'
assembler_fichiers_onde_annuels_csv <- function(annual_onde_files_dir) {

  # liste des fichiers .csv
  csv_files <- list.files(path = annual_onde_files_dir, pattern = "*.csv")

  # ces fichiers sont lus puis "empilés"
  # utilisation de rbind qui est plus tout-terrain que map() %>% bind_rows() ou mp_df()
  onde <- purrr::map(paste(annual_onde_files_dir, csv_files, sep = '/'),
                     data.table::fread, encoding = "UTF-8", colClasses = c(CdCommune = "character")) %>% 
    reduce(rbind)

  # renommage des variables pour enlever les caractères < et >
  names(onde) <- names(onde) %>%
    str_replace_all(pattern = '[<>]', replacement = '')



  return(onde)

}

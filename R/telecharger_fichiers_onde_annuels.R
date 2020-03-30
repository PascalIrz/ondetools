#' Télécharger les fichiers
#'
#' Cette fonction permet de télécharger les fichiers compressés mis à disposition sur le portail des données ONDE,
#'     de les stocker dans un répertoire local et de les décompresser. Il sont nommés de façon à
#'     indiquer l'année à laquelle chacun correspond. Les métadonnées (en pdf) sont stockées dans le
#'     même répertoire.
#'
#' @param url La chaîne de caractères indiquant l'url où se trouvent les fichiers annuels à télécharger.
#' @param raw_data_dir La chaîne de caractère indiquant le chemin vers le répertoire où stocker les fichiers
#'     bruts compressés + décompressés.
#'
#' @return La fonction crée un répertoire local et y dépose les fichiers annuels compressés (extension .zip)
#'     et décompressés (extension .csv).
#' @export
#' @importFrom magrittr %>%
#' @importFrom xml2 read_html
#' @importFrom rvest html_attr html_nodes
#' @importFrom stringr str_subset str_split
#' @importFrom utils download.file unzip
#'
#' @examples \dontrun{
#' telecharger_fichiers_onde_annuels() }
#'
telecharger_fichiers_onde_annuels <- function(url = "https://onde.eaufrance.fr/content/t%C3%A9l%C3%A9charger-les-donn%C3%A9es-des-campagnes-par-ann%C3%A9e",
                                              raw_data_dir = 'raw_data') {

  # liste des URL des fichiers zippés
  zip_urls <- read_html(url) %>%
    html_nodes("*") %>%
    html_attr("href") %>%
    stringr::str_subset(".zip")

  # création du répertoire de stockage s'il n'existe pas encore
   annual_onde_files_dir <- paste(raw_data_dir, "fichiers_onde_annuels_zippes", sep = '/')
  # fs::dir_create(annual_onde_files_dir)
  # annual_onde_files_dir <- "fichiers_onde_annuels_zippes"
   dir.create(raw_data_dir)
   dir.create(annual_onde_files_dir, showWarnings = FALSE)

   skip_with_message = simpleError('Did not work out')

  # boucle pour télécharger et dézipper les fichiers annuels
  for(url in zip_urls) {

    zipped_year_data_file_name <- str_split(url, pattern = "/") %>%
      unlist()

    zipped_year_data_file_name <- zipped_year_data_file_name[8]

    year_data_file_path <- paste(annual_onde_files_dir, zipped_year_data_file_name, sep = '/')
    tryCatch(download.file(url, destfile = year_data_file_path),
             error = function(e) skip_with_message)

    unzip(zipfile = year_data_file_path, exdir = annual_onde_files_dir)

  }

}

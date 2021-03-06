% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/telecharger_fichiers_onde_annuels.R
\name{telecharger_fichiers_onde_annuels}
\alias{telecharger_fichiers_onde_annuels}
\title{Télécharger les fichiers}
\usage{
telecharger_fichiers_onde_annuels(url, raw_data_dir = "raw_data")
}
\arguments{
\item{url}{La chaîne de caractères indiquant l'url où se trouvent les fichiers annuels à télécharger.}

\item{raw_data_dir}{La chaîne de caractère indiquant le chemin vers le répertoire où stocker les fichiers
bruts compressés + décompressés.}
}
\value{
La fonction crée un répertoire local et y dépose les fichiers annuels compressés (extension .zip)
    et décompressés (extension .csv).
}
\description{
Cette fonction permet de télécharger les fichiers compressés mis à disposition sur le portail des données ONDE,
    de les stocker dans un répertoire local et de les décompresser. Il sont nommés de façon à
    indiquer l'année à laquelle chacun correspond. Les métadonnées (en pdf) sont stockées dans le
    même répertoire.
}
\examples{
\dontrun{
url_onde <- paste0("https://onde.eaufrance.fr/content/",
"t\%C3\%A9l\%C3\%A9charger-les-donn\%C3\%A9es-des-campagnes-par-ann\%C3\%A9e")

telecharger_fichiers_onde_annuels(url = url_onde, raw_data_dir = 'raw_data')
}
}

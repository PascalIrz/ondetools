% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/creer_variable_Mois.R
\name{creer_variable_Mois}
\alias{creer_variable_Mois}
\title{Création de la variable Mois}
\usage{
creer_variable_Mois(onde_df)
}
\arguments{
\item{onde_df}{Le dataframe contenant les données ONDE, mis en forme en format "long".}
}
\value{
Le dataframe contenant les données ONDE, mis en forme en format "long", auquel a été ajoutée
    une variable caractère "Mois" codée de "01" à "12".
}
\description{
Création de la variable Mois en 2 caractères alphanumériques (01 à 12). Ce format est plus pratique pour les
    graphiques ultérieurs qui nécessitent une variable catégorielle mais ordonnée.
}
\examples{
\dontrun{
stations_onde_geo <- stations_onde_geo \%>\%
    creer_variable_Mois()
}
}

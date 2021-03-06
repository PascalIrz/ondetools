% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/produire_carte_dynamique.R
\name{produire_carte_dynamique}
\alias{produire_carte_dynamique}
\title{Produire la carte dynamique}
\usage{
produire_carte_dynamique(couche_stations, popups_stations, couche_sages = NA)
}
\arguments{
\item{couche_stations}{Couche_stations (objet R de classe sf) qui a été prétraitée pour comporter un champ "pourcentage_assecs".}

\item{popups_stations}{Liste comprenant autant d'éléments (graphiques) qu'il y a de stations représentées. Cette liste est
produite par la fonction produire_graph_pour_toutes_les_stations().}

\item{couche_sages}{Paramètre facultatif. Couche des SAGEs (objet R de classe sf) qui doit comporter un champ "NOM".}
}
\value{
La fonction retourne la carte dynamique qui s'ouvre dans le navigateur web.
}
\description{
Fonction permattant de produire la carte dynamique des stations avec des symboles ronds proportionnels au pourcentage
    des observations où la station est en assec avec une sous-couche des SAGE. Plusieurs fonds de cartes sont proposés.
}
\examples{
\dontrun{
produire_carte_dynamique (couche_sages = sages,
couche_stations = stations_onde_geo,
popups_stations = graphiques)
}
}

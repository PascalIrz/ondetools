% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{propluvia_zone}
\alias{propluvia_zone}
\title{Zones Propluvia}
\format{
un dataframe au  format \code{sf} (multilignes - crs RGF93)
\describe{
  \item{CdEntiteHy}{code de référence du cours d'eau} (ex: 14 pour le Calvados)
  \item{NomEntiteH}{nom du cours d'eau}
  \item{Classe}{rang du cours d'eau de 1 à 4}
  \item{geometry}{la géométrie du cours d'eau}
}
}
\usage{
propluvia_zone
}
\description{
Un tableau contenant les informations géographiques (multipolygones) pour les
 zones d'arrêtés.
}
\keyword{datasets}

#' Cours d'eau BD Carthage 1 à 4
#'
#' Un tableau contenant les informations géographiques (multilignes) pour les
#'  cours d'eau de rangs 1 à 4 issus de la BD Carthage.
#'
#' @format un dataframe au  format \code{sf} (multilignes - crs RGF93)
#' \describe{
#'   \item{CdEntiteHy}{code de référence du cours d'eau} (ex: 14 pour le Calvados)
#'   \item{NomEntiteH}{nom du cours d'eau}
#'   \item{Classe}{rang du cours d'eau de 1 à 4}
#'   \item{geometry}{la géométrie du cours d'eau}
#' }
"coursdeau_carthage_classe1a4"


#' Zones Propluvia
#'
#' Un tableau contenant les informations géographiques (multipolygones) pour les
#'  zones d'arrêtés.
#'
#' @format un dataframe au  format \code{sf} (multilignes - crs RGF93)
#' \describe{
#'   \item{CdEntiteHy}{code de référence du cours d'eau} (ex: 14 pour le Calvados)
#'   \item{NomEntiteH}{nom du cours d'eau}
#'   \item{Classe}{rang du cours d'eau de 1 à 4}
#'   \item{geometry}{la géométrie du cours d'eau}
#' }
"propluvia_zone"

#' Couleurs pour modalités ONDE - typologie 4 modalités
#'
#' Un vecteur pour les 4 modalités d'observations du protocole ONDE.
#'
#' @format un vecteur de couleurs
"onde_4mod"


#' Couleurs pour modalités ONDE - typologie 5 modalités
#'
#' Un vecteur pour les 5 modalités d'observations du protocole ONDE.
#'
#' @format un vecteur de couleurs
"onde_5mod"

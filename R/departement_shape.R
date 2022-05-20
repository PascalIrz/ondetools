#' Contours des départements français
#' @encoding UTF-8
#' Donnees spatiales au format 'sf', sytteme de projection RGF93 / Lambert-93
#'
#' @format Dataframe au format sf avec 101 lignes et 3 variables:
#' \describe{
#'   \item{code insee}{numero de departement, par exemple '14' pour le Calvados}
#'   \item{nom}{nom du departement}
#'   \item{geometry}{geometrie des departements}
#' }
#' @source Shapefile contours departements sur le portail data.gouv.fr \url{https://www.data.gouv.fr/fr/datasets/}
"departement_shape"

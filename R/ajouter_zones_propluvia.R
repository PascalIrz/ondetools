#' Ajouter les données de zones d'alerte Propluvia au tableau de données onde
#'
#' @param onde_df un tableau de données onde téléchargé avec la
#'  fonction \code{telecharger_donnees_onde_api}
#' @param propluvia_shape la couche spatiale pour les zones d'alerte Propluvia.
#'  Une version existe en tant que jeux de données dans le package : (\code{propluvia_zone}).
#'
#' @return un dataframe comprenant les données onde et les données Propluvia jointées spatialement.
#' @export
#'
#' @importFrom dplyr filter
#' @importFrom sf st_as_sf st_transform st_buffer st_join
#'
#' @examples
#' \dontrun{
#' ajouter_zones_propluvia(onde_df = onde_14, propluvia_shape = propluvia_zone)
#' }

ajouter_zones_propluvia <- function(onde_df,
                                    propluvia_shape){
  onde_df <-
    onde_df %>%
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4326
    ) %>%
    sf::st_transform(crs = 2154)

  propluvia_shape <-
    propluvia_shape %>%
    dplyr::filter(dpt %in% unique(onde_df$code_departement)) %>%
    sf::st_transform(crs = st_crs(onde_df)) %>%
    sf::st_buffer(dist = 50)

  sf::st_join(onde_df, propluvia_shape)
}

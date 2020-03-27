#' Produire la carte dynamique
#'
#' Fonction permattant de produire la carte dynamique des stations avec des symboles ronds proportionnels au pourcentage
#'     des observations où la station est en assec avec une sous-couche des SAGE. Plusieurs fonds de cartes sont proposés.
#'
#' @param couche_stations Couche_stations (objet R de classe sf) qui a été prétraitée pour comporter un champ "pourcentage_assecs".
#' @param popups_stations Liste comprenant autant d'éléments (graphiques) qu'il y a de stations représentées. Cette liste est
#'     produite par la fonction produire_graph_pour_toutes_les_stations().
#' @param couche_sages Paramètre facultatif. Couche des SAGEs (objet R de classe sf) qui doit comporter un champ "NOM".
#'
#' @return La fonction retourne la carte dynamique qui s'ouvre dans le navigateur web.
#' @export
#' @importFrom mapview mapview
#' @importFrom leafpop popupGraph
#'
#' @examples
#'
produire_carte_dynamique <- function(couche_stations, popups_stations, couche_sages = NA) {

  carte1 <- if(is.na(couche_sages)) {NA

    } else {

          mapview(couche_sages, zcol = "NOM", alpha.regions = 0.3, viewer.suppress = TRUE, legend = FALSE)

          }


  carte2 <- if(is.na(couche_sages)) {

    mapview(couche_stations, cex = "pourcentage_assecs", layer.name = "Assecs", legend = FALSE,
            popup = leafpop::popupGraph(popups_stations), viewer.suppress = TRUE)
    } else {

    mapview(couche_stations, cex = "pourcentage_assecs", layer.name = "Assecs", legend = FALSE,
                         popup = leafpop::popupGraph(popups_stations))
            }

  ma_carte <- if(is.na(carte1)) {carte2} else {carte1 + carte2}


  return(ma_carte)


}


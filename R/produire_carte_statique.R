#' Produire une carte départementale de la situation mensuelle des observations ONDE
#'
#' @param onde_df_mois tableau contenant les données ONDE pour le mois souhaité
#' @param code_departement le code insee du département, par exemple '14' pour l'Eure
#' @param referentiel_onde la typologie à utiliser,
#'  soit \code{Typologie nationale} ou \code{Typologie départementale}
#' @param couleurs un vecteur de couleur correspond aux référentiels
#'  national ou départemental
#' @param dptFR_shp une couche shape pour les départements FR
#'
#' @return un graphique \code{ggplot2}
#' @export
#'
#' @importFrom dplyr filter recode
#' @importFrom ggplot2 ggplot geom_sf scale_fill_manual coord_sf unit ggtitle theme_void theme element_text element_blank element_rect element_line
#' @importFrom ggspatial annotation_scale annotation_north_arrow
#' @importFrom glue glue
#' @importFrom lubridate month year
#' @importFrom sf st_as_sf st_intersection st_geometry st_as_sfc st_bbox st_buffer
#'
#' @examples
#' \dontrun{
#'
#' dptFR <-
#'   COGiter::departements_geo %>%
#'   dplyr::left_join(COGiter::departements %>%
#'                      dplyr::select(DEP, REG, NOM_DEP))
#'
#' produire_carte_statique(code_departement = '76',
#'                         onde_df_mois = onde_df_sel,
#'                         referentiel_onde = 'Typologie nationale',
#'                         couleurs = onde_4mod,
#'                         dptFR_shp = dptFR)
#'
#' produire_carte_statique(code_departement = '76',
#'                         onde_df_mois = onde_df_sel,
#'                         referentiel_onde = 'Typologie départementale',
#'                         couleurs = onde_5mod,
#'                         dptFR_shp = dptFR)
#' }

produire_carte_statique <- function(onde_df_mois = NULL,
                                    code_departement = NULL,
                                    referentiel_onde = 'Typologie nationale',
                                    couleurs = onde_4mod,
                                    dptFR_shp = NULL) {

  if (is.null(code_departement)) {
    stop(
      "code dpt manquant"
    )
  }

  if (is.null(onde_df_mois)) {
    stop(
      "dataframe ONDE manquant"
    )
  }

  if (is.null(dptFR_shp)) {
    stop(
      "shape FR dpt manquant"
    )
  }

  if (is.null(referentiel_onde) ||
      !referentiel_onde %in% c("Typologie nationale", "Typologie d\u00e9partementale")) {
    stop(
      "Le nom du r\u00e9f\u00e9rentiel est manquant ou mal \u00e9crit. Veuillez sp\u00e9cifier \'Typologie nationale\' ou \'Typologie d\u00e9partementale\'.",
      call. = FALSE
    )
  }

  dpt_shp <-
    {{dptFR_shp}} %>%
    dplyr::filter(DEP == {
      {
        code_departement
      }
    })

  onde_df_mois <-
    onde_df_mois %>%
    dplyr::filter(CdDepartement == {
      {
        code_departement
      }
    }) %>%
    sf::st_as_sf(
      coords = c('CoordXSiteHydro', 'CoordYSiteHydro'),
      crs = unique(onde_df_mois$ProjCoordSiteHydro)
    ) %>%
    mutate(
      LbRsObservationNat = sub(' ', '\n', LbRsObservationNat),
      LbRsObservationDpt = ifelse(
        LbRsObservationDpt == 'Ecoulement visible',
        'Ecoulement visible acceptable',
        LbRsObservationDpt
      ),
      LbRsObservationDpt = ifelse(
        LbRsObservationDpt == "Ecoulement non visible",
        "Ecoulement\nnon visible",
        gsub(" ", "\n", LbRsObservationDpt)
      )
    ) %>%
    {
      if ({
        {
          referentiel_onde
        }
      } == 'Typologie nationale')
        mutate(
          .,
          observations = LbRsObservationNat,
          couleurs = dplyr::recode(LbRsObservationNat,!!!onde_4mod)
        )
      else
        mutate(
          .,
          observations = LbRsObservationDpt,
          couleurs = dplyr::recode(LbRsObservationDpt,!!!onde_5mod)
        )
    }

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = dptFR_shp , fill = "grey95") +
    ggplot2::geom_sf(data = dpt_shp,
                     fill = "grey70",
                     lwd = 1.2) +
    ggplot2::geom_sf(
      data = sf::st_intersection(
        sf::st_geometry(coursdeau_carthage_classe1a4),
        sf::st_geometry(sf::st_as_sfc(sf::st_bbox(dpt_shp)) %>%
                          sf::st_buffer(dist =
                                          20000))
      ),
      lwd = 0.2,
      col = "#225ea8"
    ) +
    ggplot2::geom_sf(
      data = onde_df_mois,
      aes(fill = observations),
      pch = 21,
      size = 3.5,
      alpha = 0.8
    ) +
    ggplot2::scale_fill_manual(
      name = glue::glue("Situation stations"),
      values = {
        {
          couleurs
        }
      }
    ) +
    ggplot2::coord_sf(
      xlim = sf::st_bbox(dpt_shp)[c(1, 3)],
      ylim = sf::st_bbox(dpt_shp)[c(2, 4)],
      default_crs = NULL,
      expand = T
    ) +
    ggspatial::annotation_scale(
      location = "br",
      text_cex = 0.8,
      height = ggplot2::unit(0.3, "cm")
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      height = ggplot2::unit(0.9, "cm"),
      width = ggplot2::unit(0.9, "cm")
    ) +
    ggplot2::ggtitle(
      label = glue::glue(
        'R\u00e9seau ONDE - {unique(dpt_shp$NOM_DEP)} {unique(lubridate::month(onde_df_mois$DtRealObservation,label = T, locale = \"fr_FR\"))} {unique(lubridate::year(onde_df_mois$DtRealObservation))} - {referentiel_onde}'
      )
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.text = ggplot2::element_text(size = 8),
      legend.title =  ggplot2::element_text(face = "bold",
                                            size = 8),
      plot.title = ggplot2::element_text(face = "bold",
                                         size = 8),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "lightblue", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = NA),
      panel.grid.minor = ggplot2::element_line(colour = NA),
      legend.position = "right",
      plot.background = ggplot2::element_blank()
    )
}

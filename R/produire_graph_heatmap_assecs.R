#' Produire un graphique type heatmap pour les assecs des campagnes usuelles onde
#'
#' @param heatmap_df un dataframe produit avec la fonction \code{calculer_assecs_heatmap}
#' @param autoscale booléen, si TRUE l'échelle de couleurs est automatique pour les pourcentages.
#'  Par défaut, autoscale = FALSE (échelle de 0 à 100).
#' @param rotate booléen, si TRUE les années sont représentées sur l'axe des ordonnées et
#'  les mois sur l'axe des abscisses. Par défaut, FALSE.
#'
#' @return un graphique \code{ggplot2} type heatmap
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_size scale_x_continuous ylab xlab labs theme_bw theme element_text element_blank scale_fill_gradientn coord_flip
#' @importFrom glue glue
#' @importFrom grDevices adjustcolor hcl.colors
#' @importFrom scales breaks_width
#'
#' @examples
#' \dontrun{
#' onde_14 <- telecharger_donnees_onde_api(dpt = c('14'))
#'
#' heatmap_14 <- calculer_assecs_heatmap(onde_df = onde_14)
#'
#' produire_graph_heatmap_assecs(heatmap_df = heatmap_14, autoscale = T)
#'
#' }

produire_graph_heatmap_assecs <- function(heatmap_df,
                                          autoscale = F,
                                          rotate = F) {
  heatplot <-
    heatmap_df %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = Annee,
        y = factor(Mois),
        fill = pourcentage_assecs
      )
    ) +
    ggplot2::geom_tile(col = 'white', linewidth = 0.5) +
    ggplot2::geom_text(
      mapping = ggplot2::aes(label = Label_p),
      size = 2.5,
      color = "black",
      fontface = 'bold.italic'
    ) +
    ggplot2::scale_size(guide = 'none') +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(1),
      expand = c(0,0)
    ) +
    ggplot2::ylab("Mois") +
    ggplot2::xlab(NULL) +
    ggplot2::labs(title = glue::glue("Proportion de stations en assec dans le d\u00e9partement {unique(heatmap_df$code_departement)}"),
                  subtitle = 'Campagnes usuelles') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(size = 10, face = 'bold'),
      plot.subtitle = ggplot2::element_text(size = 9, face = 'italic'),
      axis.text.x = ggplot2::element_text(size=9),
      axis.text.y = ggplot2::element_text(size=9),
      legend.position = 'right',
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )

  if(autoscale == T){
    heatplot <-
      heatplot +
      ggplot2::scale_fill_gradientn(
        "% d\'assecs",
        colors = adjustcolor(hcl.colors(10, "RdYlBu", rev = T),
                             alpha.f = 0.8),
        #limits = c(0,100),
        na.value = adjustcolor("grey90", alpha.f = 0.7)
      )
  } else {
    heatplot <-
      heatplot +
      ggplot2::scale_fill_gradientn(
        "% d\'assecs",
        colors = adjustcolor(hcl.colors(10, "RdYlBu", rev = T),
                             alpha.f = 0.8),
        limits = c(0,100),
        na.value = adjustcolor("grey90", alpha.f = 0.7)
      )
  }

  if(rotate == T){
    heatplot +
      ggplot2::coord_flip()
  } else {
    heatplot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(size=9,angle = 45, hjust = 1),
      )
  }
}

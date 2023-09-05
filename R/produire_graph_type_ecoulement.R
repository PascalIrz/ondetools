#' Produire le graphique bilan des écoulements
#'
#' @param data_bilan un dataframe comprenant les différents types d'écoulement observés.
#'  Peut être calculé avec la fonction \code{calculer_bilan_ecoulement}
#' @param lib_ecoulement la colonne contenant la variable des types écoulement
#' @param historique booléen. Si FALSE, représente le graphique pour la dernière année en cours
#'  (année maximum des données).
#'  Si TRUE, représente le graphique résumant l'historique pour toutes les années.
#'
#' @return un graphique avec \code{ggplot2}
#' @export
#'
#' @importFrom dplyr mutate filter ungroup
#' @importFrom data.table rleid
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes geom_bar facet_grid position_stack coord_flip ylab xlab labs scale_fill_manual theme_bw theme element_text element_rect element_line element_blank guides guide_legend
#' @importFrom ggrepel geom_text_repel
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Exemple bilan typologie départementale campagnes usuelles pour le 27
#' data_bilan_ecoulement <-
#'   calculer_bilan_ecoulement(onde_df = onde_df,
#'                             mod = lib_ecoul,
#'                             mod_levels = c("Ecoulement visible acceptable",
#'                                            "Ecoulement visible faible",
#'                                            "Ecoulement non visible",
#'                                            "Assec",
#'                                            "Observation impossible",
#'                                            "Donnée manquante"),
#'                             referentiel_onde = "Typologie départementale",
#'                             force_complementaire = F) %>%
#'   dplyr::filter(code_departement == 27)
#'
#' ## graph bilan de l'année en cours
#' produire_graph_type_ecoulement(data_bilan = data_bilan_ecoulement,
#'                                lib_ecoulement = lib_ecoul,
#'                                historique = F)
#'
#' ## graph bilan de toutes les années
#' produire_graph_type_ecoulement(data_bilan = data_bilan_ecoulement,
#'                                lib_ecoulement = lib_ecoul,
#'                                historique = T)
#'
#' # Exemple bilan typologie nationale TOUTES campagnes pour le 27
#' data_bilan_ecoulement <-
#'   calculer_bilan_ecoulement(onde_df = onde_df,
#'                             mod = lib_ecoul,
#'                             mod_levels = c("Ecoulement visible",
#'                                            "Ecoulement non visible",
#'                                            "Assec",
#'                                            "Observation impossible",
#'                                            "Donnée manquante"),
#'                             referentiel_onde = "Typologie nationale",
#'                             force_complementaire = T) %>%
#'   dplyr::filter(code_departement == 27)
#'
#' produire_graph_type_ecoulement(data_bilan = data_bilan_ecoulement,
#'                                lib_ecoulement = lib_ecoul,
#'                                historique = F)
#'   }

produire_graph_type_ecoulement <- function(data_bilan,
                                           lib_ecoulement,
                                           historique = FALSE) {

  if (historique == T) {

    data_bilan %>%
      dplyr::group_by(Mois, Annee, libelle_type_campagne) %>%
      dplyr::filter(date_campagne == max(date_campagne)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Mois = paste(Mois, substr(libelle_type_campagne,1,4),sep = "_")) %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = frq,
          x = forcats::fct_rev(factor(Annee)),
          fill = forcats::fct_rev({{lib_ecoulement}}),
          label = Label_p
        )
      ) +
      ggplot2::geom_bar(
        position = "stack",
        stat = "identity",
        alpha = 0.7,
        colour = 'black',
        width = 0.7,
        linewidth = 0.01
      ) +
      ggplot2::facet_grid(~Mois) +
      ggrepel::geom_text_repel(
        size = 2.7,
        color = "black",
        fontface = 'bold.italic',
        hjust = 1,
        position = ggplot2::position_stack(vjust = 0.5)
      ) +
      ggplot2::coord_flip() +
      ggplot2::ylab("Pourcentage (%)") +
      ggplot2::xlab(NULL) +
      ggplot2::labs(title = glue::glue("Historique des types d\'\u00e9coulement - d\u00e9partement {unique(data_bilan$code_departement)}"),
                    subtitle = glue::glue('{unique(data_bilan$Typologie)}')) +
      ggplot2::scale_fill_manual(
        name = "Situation stations",
        values = c("Ecoulement visible" = "#4575b4",
                   "Ecoulement visible acceptable" = "#4575b4",
                   "Ecoulement visible faible" = "#bdd7e7",
                   "Assec" = "#d73027",
                   "Ecoulement non visible" = "#fe9929",
                   "Observation impossible" = "grey50",
                   "Donn\\u00e9e manquante" = 'grey')
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(size = 10, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 9, face = 'italic'),
        legend.text = ggplot2::element_text(size = 8.5),
        legend.title = ggplot2::element_text(size = 9, face = 'bold'),
        axis.text.y = ggplot2::element_text(size = 9, colour = 'black'),
        axis.text.x = ggplot2::element_text(size = 9, colour = 'black'),
        strip.text.x = ggplot2::element_text(size = 9, color = "black", face = "bold"),
        strip.background = ggplot2::element_rect(
          color="black", fill="grey80", linewidth = 1, linetype="solid"
        ),
        panel.grid.major = ggplot2::element_line(colour = NA),
        panel.grid.minor = ggplot2::element_line(colour = NA),
        legend.position = "bottom",
        plot.background = ggplot2::element_blank(),
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
      )

  } else {

    data_bilan <-
      data_bilan %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Mois_c = paste(Mois, substr(libelle_type_campagne,1,4),sep = "_")) %>%
      dplyr::group_by(Mois_c) %>%
      dplyr::mutate(campagne_factor = data.table::rleid(code_campagne),
                    Mois_c = ifelse(campagne_factor == "1", Mois_c, paste0(Mois_c, campagne_factor))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(Annee == max(as.numeric(Annee)))

    data_bilan %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          y = frq,
          x = forcats::fct_rev(factor(Mois_c)),
          fill= forcats::fct_rev({{lib_ecoulement}}),
          label = Label_p
        )
      ) +
      ggplot2::geom_bar(
        position = "stack",
        stat = "identity",
        alpha = 0.7,
        colour = 'black',
        width = 0.7,
        linewidth = 0.01
      ) +
      ggplot2::facet_grid(~Annee ) +
      ggrepel::geom_text_repel(
        size = 3,
        color = "black",
        fontface = 'bold.italic',
        hjust = 1,
        position = ggplot2::position_stack(vjust = 0.5),

      ) +
      ggplot2::coord_flip() +
      ggplot2::ylab("Pourcentage (%)") +
      ggplot2::xlab(NULL) +
      ggplot2::labs(title = glue::glue("Historique des types d\'\u00e9coulement - d\u00e9partement {unique(data_bilan$code_departement)}"),
                    subtitle = glue::glue('{unique(data_bilan$Typologie)}')) +
      ggplot2::scale_fill_manual(
        name = "Situation stations",
        values = c("Ecoulement visible" = "#4575b4",
                   "Ecoulement visible acceptable" = "#4575b4",
                   "Ecoulement visible faible" = "#bdd7e7",
                   "Assec" = "#d73027",
                   "Ecoulement non visible" = "#fe9929",
                   "Observation impossible" = "grey50",
                   "Donn\\u00e9e manquante" = 'grey')
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(size = 9, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 9, face = 'italic'),
        legend.text = ggplot2::element_text(size = 8.5),
        legend.title = ggplot2::element_text(size = 9, face = 'bold'),
        axis.text.y = ggplot2::element_text(size = 9, colour = 'black'),
        axis.text.x = ggplot2::element_text(size = 9, colour = 'black'),
        strip.text.x = ggplot2::element_text(size = 9, color = "black", face = "bold"),
        strip.background = ggplot2::element_rect(
          color="black", fill="grey80", linewidth = 1, linetype="solid"
        ),
        panel.grid.major = ggplot2::element_line(colour = NA),
        panel.grid.minor = ggplot2::element_line(colour = NA),
        legend.position = "bottom",
        plot.background = ggplot2::element_blank(),
      )+
      ggplot2::guides(
        fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)
      )
  }
}

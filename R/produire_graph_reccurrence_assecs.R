#' Produire un graphique du nombre de stations réparties selon les durées en
#'  assecs consécutifs par années de suivis.
#'
#' @description
#' La durée des assecs, en nombre de mois consécutifs, est calculée pour chaque station sur
#'  la période des campagnes usuelles (de mai à septembre). Les campagnes complémentaires ne sont
#'  pas utilisées.
#'
#' @param df_assecs un tableau résumé de la durée des assecs produit avec
#' @param round_prec nombre de chiffres pour arrondir la valeur du pourcentage calculé
#'
#' @return un graphique \code{ggplot2}
#' @export
#'
#' @importFrom dplyr ungroup filter mutate
#' @importFrom ggplot2 ggplot aes geom_bar geom_text position_stack scale_fill_manual scale_y_continuous labs theme_bw theme element_text element_blank ylab xlab
#' @importFrom glue glue
#' @importFrom scales percent_format
#' @importFrom tidyr complete
#'
#' @examples
#' \dontrun{
#' onde_14 <- telecharger_donnees_onde_api(dpt = c('14'))
#'
#' onde_14_recurrence <- calculer_recurrence_assecs(onde_df = onde_14)
#'
#' produire_graph_reccurrence_assecs(df_assecs = onde_14_recurrence)
#' }
produire_graph_reccurrence_assecs <- function(df_assecs,
                                              round_prec = 1) {
  df_assecs %>%
    dplyr::ungroup() %>%
    tidyr::complete(Annee,
                    Label,
                    fill = list(max_nb_mois_assec = 0,
                                #nb_station = 0,
                                frq = 0)) %>%
    dplyr::filter(Label != '0 mois') %>%

    dplyr::mutate(Label = factor(
      Label,
      levels = c(
        paste0(5:2, " mois cons\u00e9cutifs"), "1 mois"
      )
    )
    ) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = as.factor(Annee), y = frq, fill = Label)
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(
      mapping = ggplot2::aes(y = frq, label = nb_sta),
      fontface ="italic",
      size = 2.8,
      position = ggplot2::position_stack(vjust = 0.5),
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      "Dur\u00e9e",
      values = c(
        "1 mois" = "#FFFFB2FF",
        "2 mois cons\\u00e9cutifs" = "#FECC5CFF",
        "3 mois cons\\u00e9cutifs" = "#FD8D3CFF",
        "4 mois cons\\u00e9cutifs" = "#F03B20FF",
        "5 mois cons\\u00e9cutifs" = "#BD0026FF"
      ),
      drop = FALSE
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(round_prec)) +
    ggplot2::labs(title = glue::glue("Proportions et nombre de stations concern\u00e9es")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      title = ggplot2::element_text(size = 10,face = 'bold'),
      axis.text.x = ggplot2::element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size=9),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::ylab(NULL) +
    ggplot2::xlab(NULL)
}

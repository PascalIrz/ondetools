#' Produire graphique historique pour station (nouvelle version!)
#'
#' @description
#' Nouvelle version de la fonction pour produire un graphique à la station de l'historique des écoulements.
#' Celle-ci permet d'utiliser les données téléchargées issues de l'API
#' et de pouvoir représenter, à la fois, les campagnes usuelles et complémentaires
#' sur le même graphique. Il est également possible d'utiliser la typologie départementale.
#'
#'
#' @param station_vec identifiant de la station à représenter graphiquement (colonne \code{code_station})
#' @param onde_df un tableau de données onde téléchargé avec la
#'  fonction \code{telecharger_donnees_onde_api}
#' @param type_mod nom de colonne choisie définissant la variable écoulement
#' @param mod_levels vecteur définissant les noms de modalités d'écoulement.
#'  Doit correspondre aux valeurs retrouvées dans la colonne \code{type_mod}
#' @param mod_colors vecteur de couleur nommé qui associe à chacune des modalités
#'  d'observation une couleur.
#'
#' @return un graphique \code{ggplot2} pour l'historique des observations à la station
#'  avec les différentes campagnes (usuelles et complémentaires)
#' @export
#'
#' @examples
#' \dontrun{
#' onde_14 <- telecharger_donnees_onde_api(dpt = c('14'))
#'
#' onde_periode <-
#'   onde_14 %>%
#'   dplyr::mutate(
#'     Annee = as.numeric(Annee),
#'     Mois = format(as.Date(date_campagne), "%m"),
#'     Mois_campagne = lubridate::ym(paste0(Annee,Mois,sep="-"))
#'   ) %>%
#'   dplyr::mutate(
#'     libelle_ecoulement = dplyr::case_when(
#'       libelle_ecoulement == 'Ecoulement visible' ~ 'Ecoulement visible acceptable',
#'       TRUE ~ libelle_ecoulement
#'     )
#'   )
#'
#' produire_graph_pour_une_station_v2(station_vec = onde_periode$code_station[1],
#'                                 type_mod = libelle_ecoulement,
#'                                 onde_df = onde_periode,
#'                                 mod_levels = c("Assec",
#'                                                "Ecoulement\nnon visible",
#'                                                "Ecoulement\nvisible\nfaible",
#'                                                "Ecoulement\nvisible\nacceptable",
#'                                                "Observation\nimpossible",
#'                                                "Donnée\nmanquante"),
#'                                 mod_colors = onde_5mod
#'
#' )
#' }


produire_graph_pour_une_station_v2 <-
  function(station_vec,
           onde_df,
           type_mod,
           mod_levels,
           mod_colors) {
    prov <- onde_df %>%
      dplyr::mutate(
        Annee = factor(Annee, levels = min(Annee):max(Annee))
      ) %>%
      dplyr::filter(code_station == station_vec) %>%
      dplyr::mutate(label_p = paste0(libelle_type_campagne,'\n',{{type_mod}},'\n',date_campagne),
                    label_sta = paste0(libelle_station,' (',code_station,')'),
                    label_png = paste0("ONDE_dpt",code_departement,"_",label_sta)) %>%
      dplyr::rename(modalite = {{type_mod}}) %>%
      (function(df_temp) {
        dplyr::bind_rows(
          df_temp %>%
            dplyr::filter(libelle_type_campagne == "compl\u00e9mentaire"),
          df_temp %>%
            dplyr::filter(
              libelle_type_campagne == "usuelle"
            ) %>%
            tidyr::complete(
              code_station,
              libelle_station,
              Annee,
              Mois,
              fill = list(
                libelle_type_campagne = "usuelle",
                modalite = "Donn\u00e9e manquante"
              )
            ) %>%
            dplyr::mutate(
              date_campagne = dplyr::if_else(
                is.na(date_campagne),
                lubridate::as_date(paste0(Annee, "-", as.numeric(Mois), "-25")),
                date_campagne
              )
            ) %>%
            dplyr::filter(
              date_campagne <= Sys.Date()
            )
        ) %>%
          dplyr::arrange(
            Annee, Mois, dplyr::desc(libelle_type_campagne)
          )

      }) %>%
      dplyr::mutate(
        modalite = stringr::str_wrap(modalite, 12) %>%
          factor(levels = mod_levels)
      ) %>%
      dplyr::mutate(Annee = Annee %>%
                      as.character() %>%
                      as.numeric())

    nom_station <- unique(prov$label_sta)
    nom_station_graph <- unique(prov$label_png)

    graph1 <- prov %>%
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = Annee,
          y = as.numeric(Mois)
        )
      ) +
      ggplot2::geom_point(
        mapping = ggplot2::aes(
          fill = stringr::str_wrap(modalite, 12),
          shape = libelle_type_campagne,
          size = libelle_type_campagne,
        ),
        col='black'
      ) +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_manual(
        values = mod_colors,
        breaks = levels(prov$modalite),
        name = 'Modalit\u00e9s'
      ) +
      ggplot2::scale_shape_manual(
        values = c(21,22),
        name = 'Type campagne'
      ) +
      ggplot2::scale_size_manual(
        values = c(4.5, 8.5),
        name = 'Type campagne'
      ) +
      ggplot2::scale_y_continuous(
        breaks = 1:12,
        labels = 1:12,
        limits = c(1, 12)
      ) +
      ggplot2::scale_x_continuous(
        breaks = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T),
        labels = min(prov$Annee, na.rm = T):max(prov$Annee, na.rm = T)
      ) +
      ggplot2::labs(
        x = "", y = "Mois",
        title = unique(prov$libelle_station),
        subtitle = unique(prov$code_station)
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        title = ggplot2::element_text(size = 9, face = 'bold'),
        plot.subtitle = ggplot2::element_text(size = 8.5, face = 'bold'),
        legend.text = ggplot2::element_text(size = 8.5),
        legend.title = ggplot2::element_text(size = 9, face = 'bold'),
        axis.text.x = ggplot2::element_text(size=9),
        axis.text.y = ggplot2::element_text(size=9),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::guides(
        fill = ggplot2::guide_legend(override.aes=list(shape = 22, size = 5))
      )

    graph1
  }

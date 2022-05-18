#' Produire un graphique synthétique sur l'historique des observations pour un département
#'
#' @param onde_df Le dataframe contenant les données ONDE en format long.
#' @param departement Le département, correspondant à son numéro (ex: '14' pour le Calvados)
#'
#' @return un graphique à 3 panels avec :
#' (i) un panel 'heatmap' représentant la proportion mensuelle des stations en assecs sur la chronique d'observations estivales usuelles (mai à septembre),
#' (ii) un panel correspondant à la moyenne annuelle des stations en assecs pour les variations interannuelles;
#' (iii) un panel représentant la durée d'assecs pour les stations du département sur la période d'observation estivales usuelles (mai à septembre).
#' La durée des assecs pour chaque station est calculée selon le nombre de mois consécutifs où des observations avec la modalité assec est observée.
#' La plus grande séquence d’observation en assec est gardée pour le calcul de la durée (rq. entre observations successives, l’écoulement peut revenir visible).
#' Ainsi, les durées en assec peuvent aller de 0 mois (c.à.d. des écoulements toujours visibles et consécutivement observés sur la période estivale) à 5 mois (c.à.d. des assecs toujours observés sur la période estivale).
#' Le pourcentage représenté pour chaque classe de durée est calculé pour les années d’observations.
#'
#' @export
#'
#' @examples
#' produire_graph_historique_dpt(onde_df, departement =  '76')
#'
#' @importFrom data.table rleid
#' @importFrom dplyr filter select group_by summarise n ungroup mutate arrange
#' @importFrom forcats fct_rev
#' @importFrom ggplot2 ggplot aes geom_text scale_size scale_x_continuous geom_line geom_point labs scale_y_continuous ggtitle theme_bw theme geom_bar position_stack scale_fill_brewer ylab xlab element_text
#' @importFrom glue glue
#' @importFrom grDevices adjustcolor hcl.colors
#' @importFrom lubridate month
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom scales breaks_width percent_format percent
#' @importFrom sf st_drop_geometry
#' @importFrom tidyr complete as_tibble
produire_graph_historique_dpt <- function(onde_df,departement) {

  num_dpt <- departement

  onde_df <-
    onde_df %>%
    dplyr::filter(CdDepartement == num_dpt) %>%
    dplyr::mutate(Mois = format(as.Date(DtRealObservation),"%m"),
                  DtRealObservation = lubridate::ymd(DtRealObservation),
                  LbRsObservationNat = factor(LbRsObservationNat,
                                              levels = c("Ecoulement visible",
                                                         "Assec",
                                                         "Ecoulement non visible",
                                                         "Observation impossible") )) %>%
    sf::st_as_sf(coords = c("CoordXSiteHydro","CoordYSiteHydro"), crs = 2154)

  ## 1) heatmap pourcentage assecs mensuels - saison estivale

  heatmap_df <-
    onde_df %>%
    dplyr::filter(TypeCampObservations  == 'usuelle') %>%
    dplyr::select(CdDepartement,CdSiteHydro, LbSiteHydro, Annee,Mois, RsObservationNat) %>%
    dplyr::filter(Mois %in% c("05","06", "07", "08", "09")) %>%
    dplyr::group_by(CdDepartement,Mois,Annee) %>%
    dplyr::summarise(n_donnees = dplyr::n(),
                     n_assecs = length(RsObservationNat[RsObservationNat ==3])) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(pourcentage_assecs = round(n_assecs / n_donnees * 100, digits = 2),
                  taille_point = sqrt(pourcentage_assecs+1)) %>%
    dplyr::arrange(Annee,Mois) %>%
    tidyr::complete(Annee,Mois) %>%
    dplyr::mutate(Mois = lubridate::month(as.numeric(Mois),label=T, abbr = T, locale = 'French')) %>%
    # label pourcentage
    dplyr::mutate(Label = ifelse(is.na(n_assecs),"",glue::glue("{n_assecs}/{n_donnees}"))) %>%
    # label (nb stations / nb total)
    dplyr::mutate(Label_p = ifelse(is.na(n_assecs),"",glue::glue("{round(pourcentage_assecs,0)}%")))


  plot_heatmap_assecs <-
    heatmap_df %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = Annee, y = Mois, fill=pourcentage_assecs) +
    geom_tile(col='white',size=0.5) +
    scale_fill_gradientn("% d\'assecs",
                         colors = grDevices::adjustcolor(grDevices::hcl.colors(10, "RdYlBu",rev = T),alpha.f = 0.8),
                         limits=c(0,100),na.value = grDevices::adjustcolor("grey90",alpha.f = 0.7)) +
    ggplot2::geom_text(ggplot2::aes(label=Label_p),size=2.5,color="black",fontface='italic') +
    #geom_text(aes(label = Label),size=3,color="black",fontface='italic') +
    ggplot2::scale_size(guide='none') +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0,0)) +
    ylab("Mois") + xlab("Ann\u00e9es") +
    ggtitle("Proportion mensuelle des stations en assecs") +
    theme_bw() +
    theme(title = element_text(size = 9,face = 'bold'),
          axis.text.x = element_text(size=8,angle = 45,hjust = 1),
          axis.text.y = element_text(size=8),
          legend.position = 'right',
          axis.ticks = element_blank(),
          panel.grid=element_blank())

  ## 2) plot moyenne annuelle assecs

  moy_ann_assecs_df <-
    onde_df %>%
    dplyr::filter(TypeCampObservations  == 'usuelle') %>%
    dplyr::group_by(CdSiteHydro, Annee) %>%
    dplyr::summarise(nb_assec = sum(LbRsObservationNat == "Assec" & TypeCampObservations == "usuelle"),
              nb_obs_usuelles = sum(TypeCampObservations == "usuelle"),
              pc_assec_usuelles = nb_assec / nb_obs_usuelles) %>%
    dplyr::group_by(Annee) %>%
    dplyr::summarise(pc_assec_usuelles = mean(pc_assec_usuelles))

  plot_moy_ann_assecs <-
    moy_ann_assecs_df %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = Annee, y = pc_assec_usuelles) +
    ggplot2::geom_line(col = "#c20000") +
    ggplot2::geom_point(fill = "#c20000", pch= 21, size=3) +
    ggplot2::labs(x = "Ann\u00e9es",
         y = "Pourcentage") +
    ggplot2::scale_y_continuous(limits = c(0, max(moy_ann_assecs_df$pc_assec_usuelles) + 0.03), labels= scales::percent_format(1)) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
    ggplot2::ggtitle("Proportion moyenne annuelle des stations en assecs") +
    ggplot2::theme_bw() +
    ggplot2::theme(title = element_text(size = 9,face = 'bold'),
          axis.text.x = element_text(size=8,angle = 45,hjust = 1),
          axis.text.y = element_text(size=8))


  ## 3) durees assecs

  duree_assecs_df <-
    onde_df %>%
    dplyr::filter(TypeCampObservations  == 'usuelle') %>%
    dplyr::select(CdDepartement,CdSiteHydro, LbSiteHydro, Annee,Mois, DtRealObservation, RsObservationNat, LbRsObservationNat) %>%
    dplyr::filter(Mois %in% c("05","06", "07", "08", "09")) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(mois_num = lubridate::month(DtRealObservation)) %>%
    tidyr::as_tibble() %>%
    dplyr::arrange(CdSiteHydro,DtRealObservation) %>%
    dplyr::group_by(CdSiteHydro,Annee, ID = data.table::rleid(CdSiteHydro,RsObservationNat == 3 )) %>%
    dplyr::mutate(mois_assec_consec = ifelse(RsObservationNat == 3, row_number(), 0L)) %>%
    dplyr::group_by(Annee,CdSiteHydro) %>%
    dplyr::summarise(max_nb_mois_assec  = max(mois_assec_consec),.groups = "drop") %>%
    dplyr::mutate(max_nb_mois_assec = factor(max_nb_mois_assec,ordered = T)) %>%
    dplyr::group_by(Annee,max_nb_mois_assec) %>%
    dplyr::summarise(nb_station = dplyr::n()) %>%
    dplyr::mutate(pct=prop.table(nb_station)) %>%
    dplyr::mutate(max_nb_mois_assec = forcats::fct_rev(max_nb_mois_assec),
           label = ifelse(max_nb_mois_assec == '1' | max_nb_mois_assec == '0',
                          paste0(max_nb_mois_assec, " mois"),
                          paste0(max_nb_mois_assec, " mois cons\u00e9cutifs")))

  plot_duree_assecs <-
    duree_assecs_df %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = as.factor(Annee), y = pct, fill = max_nb_mois_assec) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(y = pct, label = scales::percent(pct,accuracy = 1)),
              fontface="italic",size=2.2,
              position = ggplot2::position_stack(vjust = 0.5),
              show.legend = FALSE) +
    ggplot2::scale_fill_brewer("Dur\u00e9e d\'assecs",
                      palette = "YlOrRd",
                      direction = -1,
                      labels=sort(unique(duree_assecs_df$label),decreasing = T)) +
    ggplot2::scale_y_continuous(labels= scales::percent_format(1)) +
    ggplot2::ylab("Pourcentage") +
    ggplot2::xlab("Ann\u00e9es") +
    ggplot2::ggtitle("Proportion de stations selon la dur\u00e9e en assecs") +
    ggplot2::theme_bw() +
    ggplot2::theme(title = ggplot2::element_text(size = 9,face = 'bold'),
          axis.text.x = ggplot2::element_text(size = 8,angle = 45,hjust = 1),
          axis.text.y = ggplot2::element_text(size=8))



  ## 4) plot final combinés
  #require(patchwork)

  plot_historique <-
    (plot_heatmap_assecs/plot_moy_ann_assecs/plot_duree_assecs) +
    patchwork::plot_layout(heights = c(1.7, 1.1, 2.3)) +
    patchwork::plot_annotation(tag_levels = "a",tag_suffix = ')',
                               title = c(paste0("Historique des observations pour les stations du d\u00e9partement ",num_dpt, " - R\u00e9seau ONDE")),
                               caption = paste("Source: r\u00e9seau ONDE (OFB)\n \u00a9OFB",
                                               format(Sys.time(), '%Y'), "- Date d\'\u00e9dition:",format(Sys.time(), '%d/%m/%Y'))) &
    theme(plot.title = element_text(size = 8, face='bold'),
          plot.tag = element_text(face = 'bold'))

  plot_historique
}

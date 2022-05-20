produire_carte_statique <-
  function(depts_sel, onde_df_mois){

    base::load(here::here("./processed_data/departement_shape.Rdata"))
    base::load(here::here("processed_data","coursdeau_carthage_classe1a4.Rdata"))

    nom_dpt <- depts %>% dplyr::filter(code_insee %in% depts_sel) %>% dplyr::pull(nom)

    depts_i <- depts_sel %>% sf::st_simplify(dTolerance=100)

    ggplot() +
      geom_sf(data= depts %>% sf::st_simplify(dTolerance=100) , fill="grey95") +
      geom_sf(data= depts_i, fill="grey60",lwd=1.2) +

      geom_sf(data= st_intersection(sf::st_geometry(coursdeau_carthage_classe1a4),
                                    sf::st_geometry(sf::st_as_sfc(st_bbox(depts_i)) %>% sf::st_buffer(dist=20000))),
              lwd=0.2, col="#225ea8") +

      geom_sf(data= (onde_df_mois %>% dplyr::filter(CdDepartement %in% depts_sel)),aes(fill=LbRsObservationNat),pch=21,size=3.5,alpha=0.8) +

      ggplot2::coord_sf(xlim = sf::st_bbox(depts_sel)[c(1,3)],
                        ylim = sf::st_bbox(depts_sel)[c(2,4)],default_crs = NULL,expand = T) +

      ggspatial::annotation_scale(location = "br", text_cex = 0.9, height = unit(0.3, "cm")) +
      ggspatial::annotation_north_arrow(location = "tr",height = unit(0.9, "cm"), width = unit(0.9, "cm")) +
      ggplot2::ggtitle(paste0("Réseau ONDE - ","Situation du département (",nom_dpt,")"," au ",
                              paste0(unique(onde_df_mois$Mois),"/",unique(onde_df_mois$Annee)))) +
      ggplot2::labs(caption = paste("Source: ONDE (OFB)\n ©OFB",
                                    format(Sys.time(), '%Y'), "- Date d'édition:",format(Sys.time(), '%d/%m/%Y'))) +

      ggplot2::scale_fill_manual(name = "Situation stations",
                                 values = c("Ecoulement visible" = "#4575b4",
                                            "Assec" = "#d73027",
                                            "Ecoulement non visible" = "#fe9929",
                                            "Observation impossible" = "grey90")) +
      ggplot2::theme_void() +
      ggplot2::theme(title = ggplot2::element_text(size = 8),
                     legend.text = ggplot2::element_text(size = 7),
                     plot.title = ggplot2::element_text(face = "bold"),
                     axis.text = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     panel.background=ggplot2::element_rect(fill = "lightblue",colour = NA),
                     panel.grid.major = ggplot2::element_line(colour = NA),
                     panel.grid.minor = ggplot2::element_line(colour = NA),
                     legend.position = "right",
                     plot.background = ggplot2::element_blank(),
                     # Delete margin: top, right, bottom, and left
                     #plot.margin=margin(0.5,0,0,0,"cm")
      )

  }

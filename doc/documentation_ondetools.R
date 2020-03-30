## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----packages, message = FALSE------------------------------------------------
library(ondetools)

# autres packages nécessaire pour exécuter les lignes de code ci-dessous
library(tidyverse)
library(sf) 
library(mapview)

## ----exemple1, eval = FALSE---------------------------------------------------
#  ?calculer_assecs_ete()

## ----telechargemement, eval = TRUE--------------------------------------------
url_onde <- "https://onde.eaufrance.fr/content/t%C3%A9l%C3%A9charger-les-donn%C3%A9es-des-campagnes-par-ann%C3%A9e"
telecharger_fichiers_onde_annuels(url = url_onde, raw_data_dir = 'raw_data')

## ---- eval = TRUE-------------------------------------------------------------
list.files("raw_data/fichiers_onde_annuels_zippes")

## ----assemblage, eval = TRUE--------------------------------------------------
onde <- assembler_fichiers_onde_annuels_csv(annual_onde_files_dir = "raw_data/fichiers_onde_annuels_zippes")

## ----eval = FALSE, echo = FALSE-----------------------------------------------
#  load(file = 'onde_bretagne_2019.RData')
#  onde <- onde_bretagne_2019

## -----------------------------------------------------------------------------
dim(onde)

## -----------------------------------------------------------------------------
str(onde)

## ----gestionMois--------------------------------------------------------------
onde <- onde %>%
  gerer_codes_region() %>%
  mutate(Mois = lubridate::ymd(DtRealObservation) %>%
           lubridate::month() %>%
           str_pad(width = 2, side = "left", pad = "0")) %>%
  filter(TRUE)

## -----------------------------------------------------------------------------
onde <- onde %>%
  filter(LbRegion %in% c("Bretagne", "PdL"))

## ----gererCampagnes-----------------------------------------------------------
onde <- gerer_les_campagnes(onde_df = onde)

## -----------------------------------------------------------------------------
onde_lb_large <- passer_en_format_large(onde_df_long = onde)

## ----eval = FALSE-------------------------------------------------------------
#  dir.create(file.path(subDir = 'processed_data'), showWarnings = FALSE)
#  write_excel_csv2(x = onde, path = 'processed_data/onde_long.csv')
#  write_excel_csv2(x = onde_lb_large, path = 'processed_data/onde_large.csv')

## -----------------------------------------------------------------------------
gdata::keep(onde, sure = TRUE)

## -----------------------------------------------------------------------------
stations_onde_geo <- creer_couche_geo_stations(onde_df = onde)

## -----------------------------------------------------------------------------
ggplot(data = stations_onde_geo) + geom_sf()

## ----eval = FALSE-------------------------------------------------------------
#  chemin <- "//svfcvin2/DFS/COMMUNS/REGIONS/BRE/DR/dr35_IG_metier/CATALOGUE/EAU/GESTION_TERRITOIRE/SAGE/sage_2016_DIR2_shp.shp"
#  sages <- lire_couche_sages(fichier_shp = couche_sage, scr = 2154)

## ----echo = FALSE-------------------------------------------------------------
couche_sage <- 'D:/Pascal/boulot/onde/donnees_geographiques_reference/sage_2016_DIR2_shp.shp'
sages <- lire_couche_sages(fichier_shp = couche_sage, scr = 2154)

## -----------------------------------------------------------------------------
ggplot(data = sages) + geom_sf()

## -----------------------------------------------------------------------------
stations_onde_geo <- st_join(x = stations_onde_geo, y = sages)

## ----doublons, eval = FALSE, fig.height = 6, fig.width = 10, align = "center"----
#  doublons <- stations_onde_geo$CdSiteHydro %>%
#    table %>%
#    as.data.frame() %>%
#    filter(Freq > 1) %>%
#    .[,1] %>%
#    as.character()
#  
#  doublons_geo <- stations_onde_geo %>%
#    filter(CdSiteHydro %in% doublons)
#  
#  mapview(stations_onde_geo, viewer.suppress = FALSE, legend = FALSE) +
#    mapview(sages, zcol = "NOM", alpha = 0.1, legend = FALSE) +
#    mapview(doublons_geo, color = "red", legend = FALSE)

## -----------------------------------------------------------------------------
stations_onde_geo <- stations_onde_geo %>%
  filter(!(CdSiteHydro == "J3104013" & NOM == "Bas Léon")) %>%
  filter(!(is.na(NOM) & (LbRegion %in% c("Bretagne", "PdL") == FALSE)))

## -----------------------------------------------------------------------------
assecs <- calculer_assecs_ete(onde_df = onde)

## -----------------------------------------------------------------------------
stations_onde_geo <- ajouter_donnees_assecs_aux_stations(stations_geo = stations_onde_geo,
                                                         assecs_df = assecs)

## ----eval = FALSE-------------------------------------------------------------
#  stations_onde_geo <- stations_onde_geo %>%
#    st_transform(crs = 27572)

## ----eval = FALSE-------------------------------------------------------------
#  mapview(sages, zcol = "NOM", alpha.regions = 0.3,
#          viewer.suppress = TRUE, legend = FALSE) +
#    mapview(stations_onde_geo, cex = "taille_point",
#            layer.name = "Assecs", label = "Station ONDE",
#            col.regions = "red")
#  

## -----------------------------------------------------------------------------
mes_couleurs <- c("Ecoulement\nvisible" = "blue",
          "Ecoulement\nnon visible" = "brown",
          "Assec" = "red",
          "Observation\nimpossible" = "grey",
          "NA" = "grey")

## -----------------------------------------------------------------------------
codes_stations <- stations_onde_geo %>%
  pull(CdSiteHydro) %>%
  unique()

onde_ts_mois <- completer_observations_mois_manquants(onde_df = onde,
                                                      stations = codes_stations)

## ----graphUneStation, fig.height = 5, fig.width = 6, align = "center"---------
ma_station <- onde %>% pull(CdSiteHydro) %>% .[3689] #Q choix d'une station au pif

produire_graph_pour_une_station(code_station = ma_station, onde_df = onde_ts_mois, couleurs = mes_couleurs)

## -----------------------------------------------------------------------------
graphiques <- produire_graph_pour_toutes_les_stations(stations = codes_stations,
                                                      fonction_graphique = produire_graph_pour_une_station,
                                                      onde_df = onde_ts_mois,
                                                      couleurs = mes_couleurs)

## ---- fig.height = 5, fig.width = 6, align = "center"-------------------------
graphiques[10:12]

## ----eval = FALSE-------------------------------------------------------------
#  exporter_les_graphiques_png(stations_geo = stations_onde_geo,
#                              liste_graphiques = graphiques,
#                              repertoire = 'processed_data/graphiques')

## ----cartoDynamique, fig.height = 7, fig.width = 11, align = "center", warning=FALSE----

mapviewOptions(basemaps = c("OpenStreetMap", "OpenTopoMap", 
                            "Esri.WorldShadedRelief", "Esri.WorldImagery")) 
# mapviewOptions(default = TRUE) permettrait de revenir au paramétrage par défaut


produire_carte_dynamique (couche_sages = sages,
                          couche_stations = stations_onde_geo,
                          popups_stations = graphiques)


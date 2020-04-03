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

## ----telechargemement, eval = TRUE, warning = FALSE---------------------------

url_onde <- paste0("https://onde.eaufrance.fr/content/",
"t%C3%A9l%C3%A9charger-les-donn%C3%A9es-des-campagnes-par-ann%C3%A9e")

telecharger_fichiers_onde_annuels(url = url_onde, raw_data_dir = 'raw_data')

## ---- eval = TRUE-------------------------------------------------------------
list.files("raw_data/fichiers_onde_annuels_zippes")

## ----assemblage, eval = TRUE--------------------------------------------------
onde <- assembler_fichiers_onde_annuels_csv(annual_onde_files_dir = "raw_data/fichiers_onde_annuels_zippes")

## ----suppressionRawData, eval = TRUE, echo = TRUE-----------------------------
unlink("raw_data/fichiers_onde_annuels_zippes", recursive = TRUE)

## -----------------------------------------------------------------------------
dim(onde)

## -----------------------------------------------------------------------------
str(onde)

## ----gestionMois--------------------------------------------------------------

onde <- gerer_codes_region(onde) %>%
  mutate(Mois = lubridate::ymd(DtRealObservation) %>%
           lubridate::month() %>%
           str_pad(width = 2, side = "left", pad = "0")) %>%
  filter(TRUE)


## -----------------------------------------------------------------------------

onde <- filter(onde, LbRegion %in% c("Bretagne", "PdL"))


## ----gererCampagnes-----------------------------------------------------------
onde <- gerer_les_campagnes(onde_df = onde)

## -----------------------------------------------------------------------------
onde_lb_large <- passer_en_format_large(onde_df_long = onde)

## ----eval = FALSE-------------------------------------------------------------
#  dir.create(file.path(subDir = 'processed_data'), showWarnings = FALSE)
#  write_excel_csv2(x = onde, path = 'processed_data/onde_long.csv')
#  write_excel_csv2(x = onde_lb_large, path = 'processed_data/onde_large.csv')

## -----------------------------------------------------------------------------
stations_onde_geo <- creer_couche_geo_stations(onde_df = onde)

## -----------------------------------------------------------------------------
ggplot(data = stations_onde_geo) + geom_sf()

## -----------------------------------------------------------------------------
assecs <- calculer_assecs_ete(onde_df = onde)

## -----------------------------------------------------------------------------
stations_onde_geo <- ajouter_donnees_assecs_aux_stations(stations_geo = stations_onde_geo,
                                                         assecs_df = assecs)

## ----eval = TRUE--------------------------------------------------------------
stations_onde_geo <- st_transform(stations_onde_geo, crs = 27572)

## ----eval = FALSE-------------------------------------------------------------
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
codes_stations <- pull(stations_onde_geo, CdSiteHydro) %>%
  unique()

onde_ts_mois <- completer_observations_mois_manquants(onde_df = onde,
                                                      stations = codes_stations)

## ----graphUneStation, fig.height = 5, fig.width = 6, align = "center"---------
ma_station <- pull(onde, CdSiteHydro)
ma_station <- ma_station[3689]

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

## ----cartoDynamique, fig.height = 7, fig.width = 8, align = "center", warning=FALSE----

mapviewOptions(basemaps = c("OpenStreetMap", "OpenTopoMap", 
                            "Esri.WorldShadedRelief", "Esri.WorldImagery")) 
# mapviewOptions(default = TRUE) permettrait de revenir au paramétrage par défaut


produire_carte_dynamique (couche_stations = stations_onde_geo,
                          popups_stations = graphiques)


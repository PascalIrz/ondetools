<!-- badges: start -->
[![R-CMD-check](https://github.com/PascalIrz/ondetools/workflows/R-CMD-check/badge.svg)](https://github.com/PascalIrz/ondetools/actions)
<!-- badges: end -->


<div align="right"><img src="http://kamoke.fr/wp-content/uploads/2020/04/hex_ondetools_logo.png"></div align="right">

# {ondetools}


Le package {ondetools} vise à faciliter le chargement et l'exploitation des données de [l'observatoire national des étiages - ONDE](https://www.data.gouv.fr/fr/datasets/observatoire-national-des-etiages/). Ces données sont [mises à disposition en ligne](https://onde.eaufrance.fr/content/t%C3%A9l%C3%A9charger-les-donn%C3%A9es-des-campagnes-par-ann%C3%A9e). Elles couvrent la France métropolitaine depuis 2012.

This package proposes a set of functions for easier processing of the [ONDE monitoring data on the intermittent streams of mainland France](https://www.data.gouv.fr/fr/datasets/observatoire-national-des-etiages/). It allows streamlining a workflow for downloading the annual data files, tidying them and displaying the results on an interactive map. The data collection started in 2012.

La logique suivie est celle d’une chaîne de traitements depuis le téléchargement des données brutes sur le web jusqu’à la visualisation cartographique.

Il propose un ensemble de fonctions qui permettent de :

- télécharger les fichiers bruts annuels
- les décompresser
- les assembler
- pivoter le tableau en format “large”
- compléter les données des stations par des données sur les SAGEs
- calculer les statistiques d’assecs estivaux
- visualiser graphiquement le “tableau de bord” de chacune des stations
- exporter ces graphiques au format .bmp
- visualiser les données en cartographie dynamique

# Documentation

Chaque fonction est accompagnée d'une aide.

Une documentation donne une vue d'ensemble de la chaîne de traitements "type".

# ATTENTION

La version 2.9.0 du package {mapview} sur le dépôt CRAN contient un bug. Si vous n'arrivez pas à afficher la carte interactive, désinstallez {mapview} puis réinstallez-la version de développement depuis Github avec :
`remotes::install_github("r-spatial/mapview")`.


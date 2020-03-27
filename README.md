# ondetools

Le package `ondetools` vise à faciliter le chargement et l'exploitation des données de [l'observatoire des étiages](https://www.data.gouv.fr/fr/datasets/observatoire-national-des-etiages/). Ces données sont téléchargeables depuis [l'url suivante](https://onde.eaufrance.fr/content/t%C3%A9l%C3%A9charger-les-donn%C3%A9es-des-campagnes-par-ann%C3%A9e). Elles couvrent la France métropolitaine depuis 2012.

La logique suivie est celle d’une chaîne de traitements depuis le téléchargement des données brutes sur le web jusqu’à la visualisation cartographique.

Il propose un ensemble de fonctions qui permettent de :
- télécharger les fichiers bruts annuels annuels
- les décompresser
- les assembler
- recoder les régions (fusions)
- pivoter le tableau en format “large”
- compléter les données des stations par des données sur les SAGEs
- calculer les statistiques d’assecs estivaux
- visualiser graphiquement le “tableau de bord” de chacune des stations
- exporter ces graphiques au format .bmp
- visualiser les données en cartographie dynamique

# Lancement complet du pipeline de préparation des données

source("R/00_config.R")

source("R/01_prep_contrat.R")

source("R/02_prep_sinistre.R")

source("R/03_jointure_contrat_sinistre.R")

source("R/04_recodage_et_NA.R")

source("R/05_Analyse et visualisation.R")

cat("Pipeline terminé avec succès !\n")

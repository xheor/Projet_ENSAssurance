# R/00_config.R
packages <- c("readxl","dplyr","tidyr","stringr","lubridate","janitor",
              "ggplot2","forcats","skimr","arrow")
invisible(lapply(packages, require, character.only = TRUE))

chemin_contrat  <- "data/raw/Contrat.xlsx"
chemin_sinistre <- "data/raw/Sinistre.xlsx"

chemin_sortie <- "data/processed"
if (!dir.exists(chemin_sortie)) dir.create(chemin_sortie, recursive = TRUE)

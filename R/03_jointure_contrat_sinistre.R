source("R/00_config.R")

contrat <- read.csv(file.path(chemin_sortie, "contrat_propre.csv"))
sinistre <- read.csv(file.path(chemin_sortie, "sinistre_sans_doublons.csv"))

# Conversion type
contrat$id3_ass_vhr <- as.character(contrat$id3_ass_vhr)
# Arrondi âge conducteur
contrat$drv1age <- round(as.numeric(contrat$drv1age))

assurances <- c("base", "0km", "vhr")

col_contrat <- function(i, ass) {
  if (ass %in% c("base", "vhr")) paste0("id", i, "_ass_", ass) else paste0("id", i, "_ass", ass)
}

jointure <- bind_rows(
  unlist(lapply(1:3, function(i) {
    lapply(assurances, function(ass) {
      col_y <- col_contrat(i, ass)
      
      tmp <- contrat
      tmp$id_join <- tmp[[col_y]]   # on garde la colonne originale + on crée une clé commune
      tmp$source_id <- col_y        # optionnel: pour savoir quel id a servi
      
      merge(
        sinistre,
        tmp,
        by.x = "idx_sin",
        by.y = "id_join"
      )
    })
  }), recursive = FALSE)
)
write.csv(jointure, file.path(chemin_sortie, "jointure.csv"), row.names=FALSE)

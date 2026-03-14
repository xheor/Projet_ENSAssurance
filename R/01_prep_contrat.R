source("R/00_config.R")

#Chargement fichier contrat
contrat <- readxl::read_excel(chemin_contrat) %>% janitor::clean_names()

# Nettoyage des colonnes texte (suppression des espaces et des chaînes vides),
# conversion des dates au format Date et transformation de l’année en entier.
contrat <- contrat %>%
  mutate(across(where(is.character), ~na_if(str_trim(.x), ""))) %>%
  mutate(
    sit_start_date = ymd(sit_start_date),
    sit_end_date   = ymd(sit_end_date),
    idx_year       = as.integer(idx_year)
  )

# Vérification des doublons sur la combinaison (contrat, année) 
# et si il en existe suppression des lignes dupliquées en conservant une seule occurrence par situation.
dupliquer <- contrat %>% count(idx_ct, idx_year) %>% filter(n > 1)
if (nrow(dupliquer) > 0) contrat <- contrat %>% distinct(idx_ct, idx_year, .keep_all = TRUE)


# Conversion des colonnes en numériques.
contrat <- contrat %>%
  mutate(
    drv1age        = as.numeric(drv1age),
    vh_age         = as.numeric(vh_age),
    vh_value       = as.numeric(vh_value),
    sit_expo       = as.numeric(sit_expo),
    cot_ass_base   = as.numeric(cot_ass_base),
    cot_ass0km     = as.numeric(cot_ass0km),
    cot_ass_vhr    = as.numeric(cot_ass_vhr)
  )

#Nettoyage des valeurs incohérentes + création de variables 
contrat <- contrat %>%
  mutate(
    drv1age = if_else(!is.na(drv1age) & (drv1age < 16 | drv1age > 100), NA_real_, drv1age),
    vh_age  = if_else(!is.na(vh_age) & (vh_age < 0 | vh_age > 60), NA_real_, vh_age),
    vh_value = if_else(!is.na(vh_value) & vh_value <= 0, NA_real_, vh_value),
    sit_expo = if_else(!is.na(sit_expo) & sit_expo < 0, NA_real_, sit_expo)
  ) %>%
  mutate(
    duree_contrat_jours = as.numeric(sit_end_date - sit_start_date),
    jeune_conducteur = if_else(!is.na(drv1age) & drv1age < 25, 1L, 0L),
    cotisation_total = coalesce(cot_ass_base, 0) +
      coalesce(cot_ass0km, 0) +
      coalesce(cot_ass_vhr, 0)
  )

write.csv(contrat, file.path(chemin_sortie, "contrat_propre.csv"), row.names=FALSE)

source("R/00_config.R")

data <- read.csv(file.path(chemin_sortie, "jointure.csv"))



# Recodage variables + Analyse des valeurs manquantes et valeurs aberrantes

#RECODAGE

unique(data$vh_marque)
unique(data$drv1drive_licence_type)
unique(data$vh_energy)
unique(data$vh_segment)


data <- data %>%
  mutate(
    vh_marque_recode = case_when(
      vh_marque == "Peugeot" ~ "PEU",
      vh_marque == "Renault" ~ "REN",
      vh_marque == "Citroen" ~ "CIT",
      vh_marque == "Nissan" ~ "NIS",
      vh_marque == "Toyota" ~ "TOY",
      vh_marque == "Volkswagen" ~ "VOL",
      vh_marque == "BMW" ~ "BMW",
      vh_marque == "Audi" ~ "AUD",
      vh_marque == "Mercedes Benz" ~ "MER",
      vh_marque == "Ford" ~ "FOR",
      vh_marque == "Fiat" ~ "FIA",
      vh_marque == "Seat" ~ "SEA",
      vh_marque == "Dacia" ~ "DAC",
      vh_marque == "Autre" ~ "AUT",
      TRUE ~ "AUT"
    )
  )

data <- data %>%
  mutate(
    drv1drive_licence_type_recode = case_when(
      drv1drive_licence_type == "Traditionnel" ~ "TRADI",
      drv1drive_licence_type == "Cond Accompagnée" ~ "COND_ACC",
      TRUE ~ "AUTRE"
    )
  )




#ANALYSE DES VALEURS MANQUANTES


# Nombre de NA par variable
na_summary <- data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "nb_na") %>%
  arrange(desc(nb_na))

#Les cas de NA sont normaux étant donné que les colonnes id permettent de retrouver la colonne "idx_sin" et les NA de "clos_sin" indiquent seulement que le dossier n'est pas encore cloturé.

#ANALYSE DES VALEURS ABERRANTES

vars_num <- names(data)[sapply(data, is.numeric)]
vars_num

# Variables numériques continues pertinentes

vars_num <- c("mt_eval","mt_regl","delai_declaration_jours",
              "drv1age","drv1drive_licence_age",
              "vh_age","vh_weight","vh_din","vh_value","ct_deduc","claims_ant",
              "cot_ass_base","cot_ass0km","cot_ass_vhr",
              "duree_contrat_jours",
              "cotisation_total")

vars_num <- vars_num[vars_num %in% names(data)]


data_long <- data %>%
  select(all_of(vars_num)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value")

# Boxplots
ggplot(data_long, aes(x = variable, y = value)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Boxplots des variables numériques",
       x = "Variable",
       y = "Valeur")
#Les valeurs sont cohérentes avec les activités assurantielles et ne nécessitent pas de correction

#on supprime des colonnes "inutiles" pour l'analyse future
data <- data %>%
  select(
    -incoh_decl_before_surv,
    -incoh_gest_before_decl,
    -incoh_clo_before_surv,
    -incoh_neg_eval,
    -incoh_neg_regl,
    -ct_ass_base,
    -ct_ass0km,
    -ct_ass_vhr,
    -id1_ass_base,
    -id1_ass0km,
    -id1_ass_vhr,
    -id2_ass_base,
    -id2_ass0km,
    -id2_ass_vhr,
    -id3_ass_base,
    -id3_ass0km,
    -id3_ass_vhr,

  )



data %>%
  count(vh_segment) %>%
  mutate(pct = round(100*n/sum(n),2)) %>%
  ggplot(aes(x=reorder(vh_segment,pct), y=pct)) +
  geom_col(fill="steelblue") +
  coord_flip() +
  labs(title="Répartition des segments véhicules", y="%")

data %>%
  count(drv1sex) %>%
  mutate(pct = round(100*n/sum(n),2)) %>%
  ggplot(aes(x=drv1sex, y=pct)) +
  geom_col(fill="darkgreen") +
  labs(title="Répartition par sexe", y="%")

data %>%
  count(ct_km) %>%
  mutate(pct = round(100*n/sum(n),2)) %>%
  ggplot(aes(x=ct_km, y=pct)) +
  geom_col(fill="orange") +
  labs(title="Option Petit Rouleur", y="%")

write.csv(data,
          file.path(chemin_sortie, "dataset_final.csv"),
          row.names = FALSE)

write.csv(na_summary,
          file.path(chemin_sortie, "resume_na.csv"),
          row.names = FALSE)
source("R/00_config.R")

df <- read.csv(file.path(chemin_sortie, "dataset_final.csv"))


#Statistiques descriptives générales

summary(df)



# Nombre de sinistres selon l'âge du conducteur
if ("drv1age" %in% names(df)) {
  ggplot(df, aes(x = drv1age)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    labs(title = "Distribution de l'âge du conducteur principal",
         x = "Âge", y = "Effectif") +
    theme_minimal()
}

# Répartition par sexe
if ("drv1sex" %in% names(df)) {
  df %>%
    count(drv1sex) %>%
    ggplot(aes(x = drv1sex, y = n)) +
    geom_col(fill = "purple") +
    labs(title = "Répartition des conducteurs par sexe",
         x = "Sexe", y = "Effectif") +
    theme_minimal() %>%
    print()
}

# Répartition des types de véhicules
if ("vh_segment" %in% names(df)) {
  df %>%
    count(vh_segment, sort = TRUE) %>%
    ggplot(aes(x = reorder(vh_segment, n), y = n)) +
    geom_col(fill = "forestgreen") +
    coord_flip() +
    labs(title = "Répartition des véhicules par segment",
         x = "Segment", y = "Effectif") +
    theme_minimal() %>%
    print()
}

# Répartition selon l’énergie
if ("vh_energy" %in% names(df)) {
  df %>%
    count(vh_energy, sort = TRUE) %>%
    ggplot(aes(x = reorder(vh_energy, n), y = n)) +
    geom_col(fill = "goldenrod") +
    coord_flip() +
    labs(title = "Répartition des véhicules selon l'énergie",
         x = "Énergie", y = "Effectif") +
    theme_minimal() %>%
    print()
}

# Contrats jeune conducteur
if ("jeune_conducteur" %in% names(df)) {
  df %>%
    count(jeune_conducteur) %>%
    ggplot(aes(x = as.factor(jeune_conducteur), y = n)) +
    geom_col(fill = "red3") +
    labs(title = "Répartition des jeunes conducteurs",
         x = "Jeune conducteur", y = "Effectif") +
    theme_minimal() %>%
    print()
}

# Nombre de sinistres antérieurs
if ("claims_ant" %in% names(df)) {
  ggplot(df, aes(x = claims_ant)) +
    geom_histogram(binwidth = 1, fill = "darkcyan", color = "white") +
    labs(title = "Distribution du nombre de sinistres antérieurs",
         x = "Nombre de sinistres antérieurs", y = "Effectif") +
    theme_minimal() %>%
    print()
}

# Délai de déclaration
if ("delai_declaration_jours" %in% names(df)) {
  ggplot(df, aes(x = delai_declaration_jours)) +
    geom_histogram(bins = 30, fill = "brown", color = "white") +
    labs(title = "Distribution du délai de déclaration",
         x = "Délai de déclaration (jours)", y = "Effectif") +
    theme_minimal() %>%
    print()
}


# Analyse temporelle

if ("surv_sin" %in% names(df)) {
  df %>%
    mutate(annee_sinistre = year(surv_sin)) %>%
    count(annee_sinistre) %>%
    ggplot(aes(x = annee_sinistre, y = n)) +
    geom_col(fill = "steelblue") +
    labs(title = "Nombre de sinistres par année",
         x = "Année", y = "Nombre de sinistres") +
    theme_minimal() %>%
    print()
}

if ("idx_year" %in% names(df)) {
  df %>%
    count(idx_year) %>%
    ggplot(aes(x = idx_year, y = n)) +
    geom_col(fill = "darkgreen") +
    labs(title = "Répartition des contrats par année",
         x = "Année", y = "Nombre de contrats") +
    theme_minimal() %>%
    print()
}

# Cotisation totale
if ("cotisation_total" %in% names(df)) {
  ggplot(df, aes(x = cotisation_total)) +
    geom_histogram(bins = 30, fill = "darkblue", color = "white") +
    labs(title = "Distribution de la cotisation totale",
         x = "Cotisation totale", y = "Effectif") +
    theme_minimal() %>%
    print()
}


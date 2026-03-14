source("R/00_config.R")

sinistre <- readxl::read_excel(chemin_sinistre) %>% janitor::clean_names()
# Nettoyage et conversion des types, suppression des chaînes vides,
# transformation des dates au format Date, des montants en numérique,
# et de la garantie en variable catégorielle.
sinistre <- sinistre %>%
  mutate(across(where(is.character), ~na_if(str_trim(.x), ""))) %>%
  mutate(
    surv_sin = ymd(surv_sin),
    decl_sin = ymd(decl_sin),
    clo_sin  = ymd(clo_sin),
    gest_sin = ymd(gest_sin),
    mt_eval  = as.numeric(mt_eval),
    mt_regl  = as.numeric(mt_regl),
    gar_sin  = as.factor(gar_sin)
  )

# Gestion des incohérences de dates et montants négatifs).
sinistre <- sinistre %>%
  mutate(
    incoh_decl_before_surv = !is.na(surv_sin) & !is.na(decl_sin) & decl_sin < surv_sin,
    incoh_gest_before_decl = !is.na(decl_sin) & !is.na(gest_sin) & gest_sin < decl_sin,
    incoh_clo_before_surv  = !is.na(surv_sin) & !is.na(clo_sin)  & clo_sin  < surv_sin,
    incoh_neg_eval = !is.na(mt_eval) & mt_eval < 0,
    incoh_neg_regl = !is.na(mt_regl) & mt_regl < 0
  )
#On recupere les "incoherences" de montant négatifs qui pourra nous permettre de faire des vérification au cas par cas.
# Cependant, un montant négatif n'est pas à supprimer en effet cela peut correspondre à des remboursements !
sinistre_incoherent_montant <- sinistre %>%
  filter(incoh_neg_eval | incoh_neg_regl)
#On recupere les "incoherences" de date qui pourra nous permettre de faire des vérification au cas par cas.
sinistre_incoherent_date <- sinistre %>%
  filter(incoh_decl_before_surv | incoh_gest_before_decl | incoh_clo_before_surv)

# Copie de sauvegarde
sinistre_base <- sinistre

#doublons idx_sin (si table de "mouvements" à différentes dates de gestion)
dup_idx <- sinistre %>% count(idx_sin) %>% filter(n > 1)
sin_dups <- sinistre %>% semi_join(dup_idx, by="idx_sin") %>% arrange(idx_sin, desc(gest_sin))


# Suppression des doublons en conservant la version la plus récente de chaque sinistre,
# puis création de variables indicatrices (délai de déclaration, sinistre clôturé, sinistre payé).
sinistre <- sinistre %>%
  arrange(idx_sin, desc(gest_sin)) %>%
  group_by(idx_sin) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    delai_declaration_jours = as.numeric(decl_sin - surv_sin),
    sinistre_cloturé = if_else(!is.na(clo_sin), 1L, 0L),
    sinistre_payé   = if_else(!is.na(mt_regl) & mt_regl > 0, 1L, 0L)
  )

# Calcul d’indicateurs par type de garantie :
# nombre de sinistres, taux de sinistres payés,
# coût total réglé et coût moyen,
# puis tri par volume décroissant.
resume_garantie <- sinistre %>%
  group_by(gar_sin) %>%
  summarise(
    n_sin = n(),
    paid_rate = mean(sinistre_payé, na.rm=TRUE),
    mt_regl_somme = sum(mt_regl, na.rm=TRUE),
    mt_regl_moyenne = mean(mt_regl, na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(desc(n_sin))

# exports
write.csv(sinistre, file.path(chemin_sortie, "sinistre_sans_doublons.csv"), row.names=FALSE)
write.csv(sin_dups,  file.path(chemin_sortie, "sinistre_base.csv"), row.names=FALSE)
write.csv(resume_garantie,file.path(chemin_sortie, "sinistre_resume_par_garantie.csv"), row.names=FALSE)

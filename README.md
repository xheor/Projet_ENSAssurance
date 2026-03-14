# Projet_ENSAssurance_Theo_Roussel

Vous trouverez ici la réalisation d’un projet de première année à l’ENSAR parcours **Science de la Donnée**, portant sur l’analyse de **sinistres automobiles dans le secteur assurantiel**.

Ce projet a été réalisé dans le cadre d’un travail académique visant à appliquer des méthodes de **data science, d’analyse statistique et de visualisation de données** sur un jeu de données issu d’une compagnie d’assurance fictive : **ENSAssuRances**.


# Objectif du projet

L’objectif de ce projet est d’analyser une base de données contenant des **contrats d’assurance automobile et les sinistres associés**, afin de mieux comprendre les **facteurs influençant la sinistralité et les coûts des sinistres**.

Plus précisément, le projet vise à :

- **nettoyer, structurer et préparer les données** issues des tables contrats et sinistres ;
- **réaliser une analyse exploratoire des données** pour comprendre la structure du portefeuille ;
- **visualiser les informations clés** afin d’identifier des tendances et faciliter l’interprétation ;
- **appliquer des méthodes statistiques et d’analyse multivariée** (ACP, K-means, CAH, tests statistiques) ;
- **identifier différents profils de sinistres et de véhicules** ;
- produire des **insights utiles pour la compréhension des risques en assurance automobile**.

L’ensemble du travail s’inscrit dans une démarche complète de **data science appliquée à l’assurance**, allant de l’ingénierie des données jusqu’à l’analyse statistique et la restitution des résultats.

# Organisation du dossier
    • 📁 data/ - données
        •	📁 source/ - données brutes
        •	📁 processed/ - données nettoyées et préparées pour l'analyse
    • 📁 R/ - scripts R utilisés pour la préparation des données
    • 📓 ACP_Kmeans_CAH_TestStat.rmd - Rapport complet (exploration, ACP, clustering, tests statistiques)
    • 📓 Lancement.R - Code R permettant de lancer tous les dossiers de dataprep en une seule fois
    • 📊 Dataviz_ENSAssurance.R – Application R shiny avec synthèse et aide a la décision

Fonctionnement du projet :

    -  Les données brutes sont stockées dans le dossier **`data/source/`**.
    - Le script **`R/lancement.R`** permet d’exécuter l’ensemble des scripts de préparation des données en une seule fois. Cette étape réalise les différentes         opérations de **nettoyage, transformation et structuration des données**, afin de produire une base prête à être utilisée pour l’analyse.
    - Le fichier **R Markdown** contient l’ensemble de l’analyse statistique du projet. Il présente notamment les différentes méthodes utilisées, telles que            **l’analyse en composantes principales (ACP)**, le **clustering K-means**, la **classification ascendante hiérarchique (CAH)** ainsi que les **tests              statistiques**, et propose une interprétation des résultats obtenus.
    - L’application **R Shiny** s’appuie sur le fichier **`data/processed/dataset_final.csv`** pour proposer un **tableau de bord interactif**. Ce dashboard           permet d’explorer les données, de visualiser les principaux indicateurs et fournit une **synthèse des résultats accompagnée d’éléments d’aide à la                 décision** pour l’analyse du portefeuille d’assurance.


Contenu du fichier markdown :

    Le fichier **R Markdown** correspond au **rapport d’analyse statistique du projet**. Il présente les différentes méthodes d’analyse de données appliquées sur      la base de sinistres.

    Le document est structuré autour des étapes suivantes :
    - **Analyse en composantes principales (ACP)** : réduction de dimension et étude des relations entre les variables quantitatives.
    - **Clustering K-means** : identification de groupes d’observations présentant des caractéristiques similaires.
    - **Classification ascendante hiérarchique (CAH)** : visualisation de la structure des groupes à l’aide d’un dendrogramme.
    - **Tests statistiques** : application de tests (corrélation de Pearson et test du Chi-deux) afin d’étudier les relations entre certaines variables.

    Ce rapport permet ainsi de présenter de manière structurée les principales analyses statistiques réalisées sur la base de données et d’en interpréter les          résultats.

Pour refaire l’analyse :

    Lancer "Lancement.R" pour effectuer la datapreparation
    Vérifier que le fichier "dataset_final.csv" a bien été créé
    Lancer ensuite l'application "Dataviz_ENSAssurance.R" et "ACP_Kmeans_CAH_TestStat.rmd"
    Vous pouvez ensuite explorer le rapport statistique ainsi que la synthese des informations et l'aide à la décision



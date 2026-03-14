# ENSAssuRances - Dashboard R Shiny Sinistres & Contrats


library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(readr)
library(scales)
library(bslib)
library(plotly)
library(leaflet)
library(sf)
library(htmltools)
library(purrr)

#Chargement des données
DATA_PATH <- "data/processed/dataset_final.csv"

data <- readr::read_csv(
  DATA_PATH,
  show_col_types = FALSE,
  guess_max = 1000000
)


#Fonctions utilitaires

normalize_dep <- function(x) {
  x <- trimws(as.character(x))
  x <- toupper(x)
  x <- ifelse(grepl("^[0-9]$", x), paste0("0", x), x)
  x
}


#Normalisation des données
if ("surv_sin" %in% names(data)) {
  if (!inherits(data$surv_sin, "Date")) {
    data <- data %>%
      mutate(surv_sin = suppressWarnings(as.Date(surv_sin)))
  }
  
  data <- data %>%
    mutate(annee_surv = lubridate::year(surv_sin))
}

insee_col <- intersect(c("ct_insee", "ctINSEE"), names(data))

if (length(insee_col) == 1) {
  data <- data %>%
    mutate(
      ct_insee_std = as.character(.data[[insee_col]]),
      departement = str_sub(ct_insee_std, 1, 2)
    )
} else if (!("departement" %in% names(data))) {
  data$departement <- NA_character_
}

if ("departement" %in% names(data)) {
  data <- data %>%
    mutate(departement = normalize_dep(departement))
}

#Carte départements
STADIA_API_KEY <- "882d8ad2-6fa9-4afa-870a-3411bab66fd9"

URL_DEP <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements-version-simplifiee.geojson"

tmp_geojson <- tempfile(fileext = ".geojson")

dept_sf <- tryCatch({
  utils::download.file(URL_DEP, tmp_geojson, mode = "wb", quiet = TRUE)
  sf::read_sf(tmp_geojson, quiet = TRUE)
}, error = function(e) {
  NULL
})

if (!is.null(dept_sf)) {
  dept_sf <- dept_sf %>%
    rename(departement = code) %>%
    mutate(
      departement = normalize_dep(departement),
      nom_departement = nom
    ) %>%
    st_transform(4326)
}


#UI
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#f5f7fb",
    fg = "#111827",
    primary = "#0f172a",
    secondary = "#334155",
    success = "#0f766e",
    info = "#2563eb",
    warning = "#d97706",
    danger = "#dc2626",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(180deg, #eef3f9 0%, #f8fafc 100%);
        color: #0f172a;
      }

      .container-fluid {
        max-width: 1680px;
        padding-left: 22px;
        padding-right: 22px;
      }

      .app-hero {
        background:
          linear-gradient(135deg, rgba(15,23,42,0.98) 0%, rgba(30,41,59,0.97) 55%, rgba(37,99,235,0.93) 100%);
        border-radius: 24px;
        padding: 30px 34px;
        margin: 8px 0 22px 0;
        box-shadow: 0 20px 48px rgba(15, 23, 42, 0.18);
        border: 1px solid rgba(255,255,255,0.08);
      }

      .app-badge {
        display: inline-block;
        padding: 6px 12px;
        border-radius: 999px;
        background: rgba(255,255,255,0.10);
        color: #dbeafe;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.08em;
        text-transform: uppercase;
        margin-bottom: 12px;
      }

      .app-title {
        color: #ffffff;
        font-weight: 900;
        font-size: 36px;
        line-height: 1.12;
        margin: 0 0 8px 0;
        letter-spacing: -0.03em;
      }

      .app-subtitle {
        color: rgba(255,255,255,0.84);
        font-size: 15px;
        margin: 0;
        max-width: 920px;
      }

      .sidebarPanel, .well {
        background: rgba(255,255,255,0.92);
        border: 1px solid #dbe4f0;
        border-radius: 20px;
        box-shadow: 0 12px 28px rgba(15, 23, 42, 0.06);
        padding: 22px;
      }

      .filters-title {
        font-size: 17px;
        font-weight: 800;
        color: #0f172a;
        margin-bottom: 14px;
      }

      .filters-subtitle {
        font-size: 12px;
        color: #64748b;
        margin-bottom: 18px;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        font-weight: 700;
      }

      .tab-content {
        background: linear-gradient(180deg, rgba(255,255,255,0.72) 0%, rgba(255,255,255,0.95) 100%);
        border: 1px solid #dbe4f0;
        border-radius: 22px;
        padding: 22px;
        box-shadow: 0 16px 40px rgba(15, 23, 42, 0.06);
      }

      .page-banner {
        background: linear-gradient(135deg, #eff6ff 0%, #f8fafc 100%);
        border: 1px solid #dbeafe;
        border-radius: 18px;
        padding: 16px 18px;
        margin-bottom: 18px;
      }

      .page-banner-title {
        font-size: 18px;
        font-weight: 800;
        color: #0f172a;
        margin: 0 0 4px 0;
      }

      .page-banner-subtitle {
        font-size: 13px;
        color: #64748b;
        margin: 0;
      }

      .card {
        background: rgba(255,255,255,0.96);
        border: 1px solid #e2e8f0;
        border-radius: 22px;
        box-shadow: 0 14px 36px rgba(15, 23, 42, 0.06);
        padding: 22px;
        margin-bottom: 18px;
      }

      .card h4 {
        font-weight: 800;
        color: #0f172a;
        margin-top: 0;
        margin-bottom: 16px;
        letter-spacing: -0.01em;
        line-height: 1.25;
      }

      .nav-tabs {
        display: flex;
        flex-wrap: nowrap !important;
        overflow-x: auto;
        overflow-y: hidden;
        white-space: nowrap;
        border-bottom: none;
        gap: 10px;
        margin-bottom: 14px;
        padding-bottom: 6px;
      }

      .nav-tabs > li {
        float: none;
        display: inline-block;
      }

      .nav-tabs > li > a {
        border: 1px solid #dbe4f0 !important;
        background: linear-gradient(180deg, #f8fbff 0%, #e8eff8 100%) !important;
        color: #334155 !important;
        border-radius: 14px !important;
        padding: 12px 18px !important;
        font-weight: 800;
        margin-right: 0 !important;
        transition: all 0.2s ease-in-out;
      }

      .nav-tabs > li > a:hover {
        background: linear-gradient(180deg, #eef4fd 0%, #dce9fb 100%) !important;
        color: #0f172a !important;
        transform: translateY(-1px);
      }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background: linear-gradient(135deg, #0f172a 0%, #1d4ed8 100%) !important;
        color: #ffffff !important;
        border: 1px solid transparent !important;
        box-shadow: 0 10px 22px rgba(29, 78, 216, 0.24);
      }

      .nav-tabs::-webkit-scrollbar {
        height: 8px;
      }

      .nav-tabs::-webkit-scrollbar-thumb {
        background: #c7d2e3;
        border-radius: 999px;
      }

      .form-group label {
        font-weight: 700;
        color: #1e293b;
        margin-bottom: 6px;
      }

      .form-control, .selectize-input, .irs, .irs-line {
        border-radius: 12px !important;
      }

      .selectize-input {
        border: 1px solid #dbe4f0 !important;
        box-shadow: none !important;
        min-height: 44px;
        padding-top: 10px !important;
        padding-bottom: 10px !important;
      }

      .kpi-box {
        border-radius: 20px;
        padding: 22px;
        color: white;
        box-shadow: 0 16px 34px rgba(15, 23, 42, 0.12);
        min-height: 132px;
      }

      .kpi-dark {
        background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%);
      }

      .kpi-blue {
        background: linear-gradient(135deg, #1d4ed8 0%, #2563eb 100%);
      }

      .kpi-teal {
        background: linear-gradient(135deg, #0f766e 0%, #14b8a6 100%);
      }

      .kpi-label {
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        opacity: 0.82;
        font-weight: 700;
        margin-bottom: 10px;
      }

      .kpi-value {
        font-size: 32px;
        font-weight: 800;
        line-height: 1.1;
      }

      .kpi-note {
        margin-top: 8px;
        font-size: 13px;
        opacity: 0.82;
      }

      .help-block, .text-muted, .shiny-output-error-validation {
        color: #64748b;
      }

      .js-plotly-plot .plotly .modebar {
        background: rgba(255,255,255,0.7) !important;
        border-radius: 10px;
      }

      .leaflet-container {
        border-radius: 18px;
        overflow: hidden;
        background: #f8fafc;
      }

      .leaflet-control {
        box-shadow: 0 8px 20px rgba(15, 23, 42, 0.10) !important;
        border-radius: 12px !important;
      }

      .leaflet-popup-content-wrapper {
        border-radius: 14px !important;
      }
    "))
  ),
  
  div(
    class = "app-hero",
    div(class = "app-badge", "ENSAssuRances"),
    h1(class = "app-title", "Dashboard Sinistres & Exposition au risque"),
    p(
      class = "app-subtitle",
      "Vision synthétique du portefeuille sinistré : volumes, profils, caractéristiques véhicule, signaux d'exposition et disparités territoriales."
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(class = "filters-subtitle", "Pilotage"),
      div(class = "filters-title", "Filtres"),
      uiOutput("year_filter_ui"),
      uiOutput("segment_filter_ui"),
      uiOutput("energy_filter_ui"),
      uiOutput("km_filter_ui"),
      uiOutput("sex_filter_ui"),
      uiOutput("dept_filter_ui")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        
        tabPanel(
          "Vue d’ensemble",
          div(
            class = "page-banner",
            div(class = "page-banner-title", "Vue d’ensemble"),
            div(class = "page-banner-subtitle", "Indicateurs globaux, volumes annuels et niveau général d'exposition observée.")
          ),
          fluidRow(
            column(
              4,
              div(
                class = "kpi-box kpi-dark",
                div(class = "kpi-label", "Sinistres distincts"),
                div(class = "kpi-value", textOutput("kpi_sin")),
                div(class = "kpi-note", "Volume total observé")
              )
            ),
            column(
              4,
              div(
                class = "kpi-box kpi-blue",
                div(class = "kpi-label", "Contrats distincts"),
                div(class = "kpi-value", textOutput("kpi_ct")),
                div(class = "kpi-note", "Contrats présents dans la base")
              )
            ),
            column(
              4,
              div(
                class = "kpi-box kpi-teal",
                div(class = "kpi-label", "Sinistres / contrats"),
                div(class = "kpi-value", textOutput("kpi_rate")),
                div(class = "kpi-note", "Indicateur synthétique")
              )
            )
          ),
          br(),
          fluidRow(
            column(
              7,
              div(
                class = "card",
                h4("Évolution du nombre de sinistres par année de survenance"),
                plotlyOutput("plot_ex1", height = "330px")
              )
            ),
            column(
              5,
              div(
                class = "card",
                h4("Répartition des contrats sinistrés par année d'exercice"),
                plotlyOutput("plot_ex2", height = "330px")
              )
            )
          )
        ),
        
        tabPanel(
          "Véhicules",
          div(
            class = "page-banner",
            div(class = "page-banner-title", "Analyse véhicule"),
            div(class = "page-banner-subtitle", "Marques, segments, énergie et groupes les plus représentés dans les sinistres.")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                h4("Top 12 des marques de véhicules les plus représentées dans les sinistres"),
                plotlyOutput("plot_ex3a", height = "500px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                h4("Répartition des véhicules sinistrés par segment commercial"),
                plotlyOutput("plot_ex3b", height = "500px")
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                h4("Répartition des véhicules sinistrés selon le type d'énergie"),
                plotlyOutput("plot_ex4", height = "420px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                h4("Répartition des véhicules sinistrés selon leur groupe"),
                plotlyOutput("plot_ex5", height = "420px")
              )
            )
          )
        ),
        
        tabPanel(
          "Profils",
          div(
            class = "page-banner",
            div(class = "page-banner-title", "Analyse des profils conducteurs"),
            div(class = "page-banner-subtitle", "Lecture des sinistres selon le sexe, l'âge, les antécédents et l'option Petit Rouleur.")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                h4("Nombre de sinistres observés selon l'option Petit Rouleur"),
                plotlyOutput("plot_ex6", height = "420px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                h4("Nombre et part des sinistres selon le sexe du conducteur"),
                plotlyOutput("plot_ex11", height = "420px")
              )
            )
          ),
          br(),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                h4("Évolution du nombre de sinistres selon l'âge du conducteur"),
                plotlyOutput("plot_ex7", height = "420px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                h4("Nombre de sinistres selon les antécédents déclarés"),
                plotlyOutput("plot_ex8", height = "420px")
              )
            )
          )
        ),
        
        tabPanel(
          "Risque",
          div(
            class = "page-banner",
            div(class = "page-banner-title", "Lecture du risque observé"),
            div(class = "page-banner-subtitle", "Focus sur les segments et caractéristiques les plus présentes dans les sinistres observés.")
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                h4("Nombre de sinistres observés selon le segment commercial du véhicule"),
                plotlyOutput("plot_ex9", height = "420px")
              )
            ),
            column(
              6,
              div(
                class = "card",
                h4("Top 10 des catégories présentant l'indice sinistres / contrats le plus élevé"),
                helpText("Lecture indicative calculée à partir des variables segment, marque et énergie."),
                plotlyOutput("plot_ex10", height = "420px")
              )
            )
          )
        ),
        
        tabPanel(
          "Zones",
          div(
            class = "page-banner",
            div(class = "page-banner-title", "Analyse territoriale"),
            div(class = "page-banner-subtitle", "Carte des départements français selon le ratio sinistres / contrats observé.")
          ),
          div(
            class = "card",
            h4("Carte des départements selon le ratio sinistres / contrats"),
            leafletOutput("map_zones", height = "620px")
          )
        ),
        
        tabPanel(
          "Synthèse",
          div(
            class = "page-banner",
            div(class = "page-banner-title", "Synthèse & Aide à la décision"),
            div(class = "page-banner-subtitle", "Résumé des principaux constats et pistes d'action.")
          ),
          fluidRow(
            column(
              12,
              div(
                class = "card",
                h4("Synthèse des principaux enseignements"),
                
                div(
                  style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:16px; padding:18px; margin-bottom:18px;",
                  tags$p(
                    style = "margin-bottom:14px;",
                    "L’analyse met en évidence un portefeuille sinistré composé de ",
                    tags$b("28 824 sinistres distincts"),
                    " répartis sur ",
                    tags$b("22 757 contrats"),
                    ", soit un ratio moyen de ",
                    tags$b("1,27 sinistre par contrat"),
                    ". Le volume observé progresse nettement en ",
                    tags$b("2023"),
                    ", aussi bien sur le nombre de sinistres que sur le nombre de contrats concernés."
                  ),
                  tags$p(
                    style = "margin-bottom:14px;",
                    "Les sinistres concernent principalement les marques ",
                    tags$b("Citroën, Renault et Peugeot"),
                    ". En termes de segment, la ",
                    tags$b("citadine"),
                    " ressort comme la catégorie la plus représentée, devant les segments ",
                    tags$b("familial, compact et SUV"),
                    "."
                  ),
                  tags$p(
                    style = "margin-bottom:14px;",
                    "Concernant l’énergie, les véhicules ",
                    tags$b("essence"),
                    " et ",
                    tags$b("diesel"),
                    " dominent en volume, tandis que les motorisations ",
                    tags$b("électriques/hybrides"),
                    " apparaissent parmi les catégories à surveiller en termes d’indice sinistres / contrats."
                  ),
                  tags$p(
                    style = "margin-bottom:14px;",
                    "Les sinistres concernent davantage les ",
                    tags$b("hommes"),
                    " que les femmes, et l’âge du conducteur fait apparaître un pic de fréquence autour de la ",
                    tags$b("quarantaine"),
                    "."
                  ),
                  tags$p(
                    style = "margin-bottom:14px;",
                    "L’option ",
                    tags$b("Petit Rouleur"),
                    " distingue fortement les sinistres : la majorité des sinistres est observée sur les contrats ",
                    tags$b("sans option Petit Rouleur"),
                    "."
                  ),
                  tags$p(
                    style = "margin-bottom:0;",
                    "Enfin, la lecture territoriale met en évidence des ",
                    tags$b("écarts départementaux marqués"),
                    " dans le ratio sinistres / contrats, ce qui confirme l’intérêt d’un pilotage géographique du risque."
                  )
                ),
                
                h4("Aide à la décision"),
                
                fluidRow(
                  column(
                    6,
                    div(
                      style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:16px; padding:18px; margin-bottom:16px;",
                      tags$h5(style = "margin-top:0; font-weight:800;", "Segmentation tarifaire"),
                      tags$p(
                        style = "margin-bottom:0;",
                        "Affiner la segmentation sur les critères les plus structurants : ",
                        tags$b("segment, marque, énergie et département"),
                        "."
                      )
                    )
                  ),
                  column(
                    6,
                    div(
                      style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:16px; padding:18px; margin-bottom:16px;",
                      tags$h5(style = "margin-top:0; font-weight:800;", "Prévention ciblée"),
                      tags$p(
                        style = "margin-bottom:0;",
                        "Renforcer les actions sur les profils les plus exposés, notamment les ",
                        tags$b("conducteurs masculins"),
                        ", les tranches d’âge intermédiaires et les usages non Petit Rouleur."
                      )
                    )
                  )
                ),
                
                fluidRow(
                  column(
                    6,
                    div(
                      style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:16px; padding:18px; margin-bottom:16px;",
                      tags$h5(style = "margin-top:0; font-weight:800;", "Suivi renforcé"),
                      tags$p(
                        style = "margin-bottom:0;",
                        "Mettre en place un suivi plus régulier des catégories sensibles : certaines ",
                        tags$b("marques, segments et motorisations"),
                        "."
                      )
                    )
                  ),
                  column(
                    6,
                    div(
                      style = "background:#f8fafc; border:1px solid #e2e8f0; border-radius:16px; padding:18px; margin-bottom:16px;",
                      tags$h5(style = "margin-top:0; font-weight:800;", "Pilotage territorial"),
                      tags$p(
                        style = "margin-bottom:0;",
                        "Exploiter la dimension géographique pour ajuster les règles de souscription et concentrer l’analyse sur les ",
                        tags$b("zones les plus exposées"),
                        "."
                      )
                    )
                  )
                ),
                
                div(
                  style = "background:linear-gradient(135deg, #eff6ff 0%, #f8fafc 100%); border:1px solid #dbeafe; border-radius:16px; padding:18px; margin-top:8px;",
                  tags$h4(style = "margin-top:0; margin-bottom:10px;", "Conclusion"),
                  tags$p(
                    style = "margin-bottom:0;",
                    "Dans l’ensemble, la sinistralité observée semble portée principalement par des véhicules de diffusion large, en particulier les ",
                    tags$b("citadines"),
                    ", par certaines ",
                    tags$b("marques généralistes"),
                    ", par les contrats ",
                    tags$b("sans option Petit Rouleur"),
                    ", ainsi que par des ",
                    tags$b("écarts géographiques notables"),
                    ". Les résultats plaident en faveur d’une stratégie combinant ",
                    tags$b("segmentation tarifaire plus fine"),
                    ", ",
                    tags$b("prévention ciblée"),
                    " et ",
                    tags$b("pilotage territorial renforcé"),
                    "."
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
        


#SERVER

server <- function(input, output, session) {
  
  ggplotly_clean <- function(p, left_margin = 60) {
    ggplotly(p, tooltip = "text") %>%
      config(displaylogo = FALSE) %>%
      layout(
        margin = list(l = left_margin, r = 20, b = 50, t = 20),
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "#0f172a", size = 13)
        )
      )
  }
  
  output$year_filter_ui <- renderUI({
    if ("annee_surv" %in% names(data) && any(!is.na(data$annee_surv))) {
      yrs <- sort(unique(na.omit(data$annee_surv)))
      sliderInput(
        "year_range", "Année sinistre",
        min = min(yrs), max = max(yrs),
        value = c(min(yrs), max(yrs)),
        sep = ""
      )
    } else {
      helpText("Filtre année indisponible.")
    }
  })
  
  output$segment_filter_ui <- renderUI({
    if ("vh_segment" %in% names(data)) {
      choices <- sort(unique(na.omit(data$vh_segment)))
      selectInput("seg", "Segment", choices = c("Tous", choices), selected = "Tous")
    } else NULL
  })
  
  output$energy_filter_ui <- renderUI({
    if ("vh_energy" %in% names(data)) {
      choices <- sort(unique(na.omit(data$vh_energy)))
      selectInput("energy", "Énergie", choices = c("Tous", choices), selected = "Tous")
    } else NULL
  })
  
  output$km_filter_ui <- renderUI({
    if ("ct_km" %in% names(data)) {
      choices <- sort(unique(na.omit(data$ct_km)))
      selectInput("km", "Petit rouleur", choices = c("Tous", choices), selected = "Tous")
    } else NULL
  })
  
  output$sex_filter_ui <- renderUI({
    if ("drv1sex" %in% names(data)) {
      choices <- sort(unique(na.omit(data$drv1sex)))
      selectInput("sex", "Sexe", choices = c("Tous", choices), selected = "Tous")
    } else NULL
  })
  
  output$dept_filter_ui <- renderUI({
    if ("departement" %in% names(data)) {
      choices <- sort(unique(na.omit(data$departement)))
      selectInput("dept", "Département", choices = c("Tous", choices), selected = "Tous")
    } else NULL
  })
  
  data_f <- reactive({
    df <- data
    
    if (!is.null(input$seg) && input$seg != "Tous" && "vh_segment" %in% names(df)) {
      df <- df %>% filter(vh_segment == input$seg)
    }
    if (!is.null(input$energy) && input$energy != "Tous" && "vh_energy" %in% names(df)) {
      df <- df %>% filter(vh_energy == input$energy)
    }
    if (!is.null(input$km) && input$km != "Tous" && "ct_km" %in% names(df)) {
      df <- df %>% filter(ct_km == input$km)
    }
    if (!is.null(input$sex) && input$sex != "Tous" && "drv1sex" %in% names(df)) {
      df <- df %>% filter(drv1sex == input$sex)
    }
    if (!is.null(input$dept) && input$dept != "Tous" && "departement" %in% names(df)) {
      df <- df %>% filter(departement == input$dept)
    }
    if (!is.null(input$year_range) && "annee_surv" %in% names(df)) {
      df <- df %>%
        filter(
          !is.na(annee_surv),
          annee_surv >= input$year_range[1],
          annee_surv <= input$year_range[2]
        )
    }
    
    df
  })
  
  contrats_f <- reactive({
    data_f() %>%
      distinct(across(any_of(c(
        "idx_ct", "idx_year", "vh_marque", "vh_segment", "vh_energy",
        "vh_group", "vh_class", "ct_km", "drv1age", "drv1sex", "departement"
      ))))
  })
  
  sinistres_f <- reactive({
    data_f() %>%
      distinct(across(any_of(c(
        "idx_sin", "surv_sin", "annee_surv", "mt_regl", "mt_eval",
        "drv1age", "claims_ant", "vh_segment", "vh_marque",
        "vh_energy", "drv1sex", "departement", "idx_ct"
      ))))
  })
  
  zones_sf <- reactive({
    validate(
      need(!is.null(dept_sf), "Impossible de charger les contours des départements."),
      need(inherits(dept_sf, "sf"), "Les contours chargés ne sont pas valides."),
      need(nrow(dept_sf) > 0, "Aucun contour de département disponible.")
    )
    
    stats_dep <- data_f() %>%
      filter(!is.na(departement)) %>%
      mutate(departement = normalize_dep(departement)) %>%
      group_by(departement) %>%
      summarise(
        nb_sin = n_distinct(idx_sin),
        nb_ct  = n_distinct(idx_ct),
        .groups = "drop"
      ) %>%
      mutate(taux = ifelse(nb_ct > 0, 100 * nb_sin / nb_ct, NA_real_))
    
    dept_sf %>%
      left_join(stats_dep, by = "departement")
  })
  
  output$kpi_sin <- renderText({
    format(n_distinct(sinistres_f()$idx_sin), big.mark = " ")
  })
  
  output$kpi_ct <- renderText({
    format(n_distinct(contrats_f()$idx_ct), big.mark = " ")
  })
  
  output$kpi_rate <- renderText({
    sin_n <- n_distinct(sinistres_f()$idx_sin)
    ct_n  <- n_distinct(contrats_f()$idx_ct)
    if (ct_n > 0) paste0(round(sin_n / ct_n, 2)) else "NA"
  })
  
  output$plot_ex1 <- renderPlotly({
    df <- sinistres_f() %>%
      filter(!is.na(annee_surv)) %>%
      count(annee_surv)
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = annee_surv,
        y = n,
        text = paste0(
          "Année : ", annee_surv,
          "<br>Nombre de sinistres : ", format(n, big.mark = " ")
        )
      )
    ) +
      geom_area(fill = "#93c5fd", alpha = 0.55) +
      geom_line(color = "#1d4ed8", linewidth = 1.2) +
      geom_point(color = "#0f172a", size = 2.2) +
      scale_x_continuous(breaks = unique(df$annee_surv)) +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(x = "Année", y = "Nombre de sinistres") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly_clean(p)
  })
  
  output$plot_ex2 <- renderPlotly({
    df <- contrats_f() %>%
      filter(!is.na(idx_year)) %>%
      count(idx_year)
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = factor(idx_year),
        y = n,
        text = paste0(
          "Année d'exercice : ", idx_year,
          "<br>Nombre de contrats : ", format(n, big.mark = " ")
        )
      )
    ) +
      geom_col(fill = "#0f766e", alpha = 0.9, width = 0.72) +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(x = "Année d'exercice", y = "Nombre de contrats") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly_clean(p)
  })
  
  output$plot_ex3a <- renderPlotly({
    df <- contrats_f() %>%
      filter(!is.na(vh_marque)) %>%
      count(vh_marque, sort = TRUE) %>%
      slice_head(n = 12)
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = reorder(vh_marque, n),
        y = n,
        text = paste0(
          "Marque : ", vh_marque,
          "<br>Contrats sinistrés : ", format(n, big.mark = " ")
        )
      )
    ) +
      geom_col(fill = "#1e293b", alpha = 0.95, width = 0.72) +
      coord_flip() +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(x = NULL, y = "Nombre de contrats sinistrés") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly_clean(p, left_margin = 110)
  })
  
  output$plot_ex3b <- renderPlotly({
    df <- contrats_f() %>%
      filter(!is.na(vh_segment)) %>%
      count(vh_segment, sort = TRUE) %>%
      mutate(part = n / sum(n)) %>%
      slice_head(n = 12)
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = reorder(vh_segment, n),
        y = n,
        text = paste0(
          "Segment : ", vh_segment,
          "<br>Contrats sinistrés : ", format(n, big.mark = " "),
          "<br>Part : ", percent(part, accuracy = 0.1)
        )
      )
    ) +
      geom_col(fill = "#2563eb", alpha = 0.92, width = 0.72) +
      coord_flip() +
      expand_limits(y = max(df$n, na.rm = TRUE) * 1.22) +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(x = NULL, y = "Nombre de contrats sinistrés") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly_clean(p, left_margin = 120)
  })
  
  output$plot_ex4 <- renderPlotly({
    df <- contrats_f() %>%
      filter(!is.na(vh_energy)) %>%
      count(vh_energy, sort = TRUE)
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = reorder(vh_energy, n),
        y = n,
        text = paste0(
          "Énergie : ", vh_energy,
          "<br>Véhicules sinistrés : ", format(n, big.mark = " ")
        )
      )
    ) +
      geom_segment(
        aes(xend = reorder(vh_energy, n), y = 0, yend = n),
        color = "#94a3b8", linewidth = 1.2
      ) +
      geom_point(size = 5, color = "#2563eb") +
      coord_flip() +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(x = NULL, y = "Nombre de véhicules sinistrés") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly_clean(p, left_margin = 120)
  })
  
  output$plot_ex5 <- renderPlotly({
    df <- contrats_f() %>%
      filter(!is.na(vh_group)) %>%
      count(vh_group, sort = TRUE) %>%
      mutate(vh_group = as.factor(vh_group))
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = reorder(vh_group, n),
        y = n,
        text = paste0(
          "Groupe véhicule : ", vh_group,
          "<br>Véhicules sinistrés : ", format(n, big.mark = " ")
        )
      )
    ) +
      geom_col(fill = "#475569", alpha = 0.88) +
      coord_flip() +
      labs(x = "Groupe véhicule", y = "Nombre de véhicules sinistrés") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly_clean(p, left_margin = 120)
  })
  
  output$plot_ex6 <- renderPlotly({
    df <- data_f() %>%
      filter(!is.na(ct_km), !is.na(idx_sin)) %>%
      group_by(ct_km) %>%
      summarise(nb_sin = n_distinct(idx_sin), .groups = "drop") %>%
      mutate(pct = nb_sin / sum(nb_sin))
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = ct_km,
        y = nb_sin,
        fill = ct_km,
        text = paste0(
          "Option Petit Rouleur : ", ct_km,
          "<br>Nombre de sinistres : ", format(nb_sin, big.mark = " "),
          "<br>Part des sinistres : ", percent(pct, accuracy = 0.1)
        )
      )
    ) +
      geom_col(alpha = 0.92, width = 0.62, show.legend = FALSE) +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(
        x = "Option Petit Rouleur",
        y = "Nombre de sinistres",
        caption = "O = Oui, N = Non"
      ) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly_clean(p)
  })
  
  output$plot_ex7 <- renderPlotly({
    df <- data_f() %>%
      filter(!is.na(drv1age)) %>%
      group_by(drv1age) %>%
      summarise(nb_sin = n_distinct(idx_sin), .groups = "drop")
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = drv1age,
        y = nb_sin,
        text = paste0(
          "Âge du conducteur : ", drv1age,
          "<br>Nombre de sinistres : ", format(nb_sin, big.mark = " ")
        )
      )
    ) +
      geom_line(color = "#0f766e", linewidth = 1.1) +
      geom_point(color = "#0f172a", size = 2.2) +
      labs(x = "Âge du conducteur", y = "Nombre de sinistres distincts") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly_clean(p)
  })
  
  output$plot_ex8 <- renderPlotly({
    df <- data_f() %>%
      filter(!is.na(claims_ant)) %>%
      group_by(claims_ant) %>%
      summarise(nb_sin = n_distinct(idx_sin), .groups = "drop")
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = as.factor(claims_ant),
        y = nb_sin,
        text = paste0(
          "Antécédents déclarés : ", claims_ant,
          "<br>Nombre de sinistres : ", format(nb_sin, big.mark = " ")
        )
      )
    ) +
      geom_col(fill = "#f59e0b", alpha = 0.9, width = 0.7) +
      labs(x = "Nombre d'antécédents déclarés", y = "Nombre de sinistres distincts") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly_clean(p)
  })
  
  output$plot_ex9 <- renderPlotly({
    df <- data_f() %>%
      filter(!is.na(vh_segment)) %>%
      group_by(vh_segment) %>%
      summarise(nb_sin = n_distinct(idx_sin), .groups = "drop") %>%
      arrange(desc(nb_sin))
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = reorder(vh_segment, nb_sin),
        y = nb_sin,
        text = paste0(
          "Segment : ", vh_segment,
          "<br>Nombre de sinistres : ", format(nb_sin, big.mark = " ")
        )
      )
    ) +
      geom_segment(
        aes(xend = reorder(vh_segment, nb_sin), y = 0, yend = nb_sin),
        color = "#cbd5e1", linewidth = 1.2
      ) +
      geom_point(size = 5, color = "#dc2626") +
      coord_flip() +
      labs(x = NULL, y = "Nombre de sinistres distincts") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.y = element_blank())
    
    ggplotly_clean(p, left_margin = 120)
  })
  
  output$plot_ex10 <- renderPlotly({
    dfu <- data_f() %>%
      filter(!is.na(idx_ct), !is.na(idx_sin)) %>%
      distinct(idx_ct, idx_sin, .keep_all = TRUE)
    
    taux_one <- function(var) {
      if (!(var %in% names(dfu))) return(NULL)
      
      sin <- dfu %>%
        filter(!is.na(.data[[var]])) %>%
        group_by(.data[[var]]) %>%
        summarise(nb_sin = n_distinct(idx_sin), .groups = "drop")
      
      ct <- dfu %>%
        filter(!is.na(.data[[var]])) %>%
        group_by(.data[[var]]) %>%
        summarise(nb_ct = n_distinct(idx_ct), .groups = "drop")
      
      sin %>%
        inner_join(ct, by = var) %>%
        mutate(
          indice = 100 * nb_sin / nb_ct,
          variable = case_when(
            var == "vh_segment" ~ "Segment",
            var == "vh_marque"  ~ "Marque",
            var == "vh_energy"  ~ "Énergie",
            TRUE ~ var
          ),
          categorie = as.character(.data[[var]])
        ) %>%
        select(variable, categorie, nb_sin, nb_ct, indice)
    }
    
    top <- bind_rows(lapply(c("vh_segment", "vh_marque", "vh_energy"), taux_one)) %>%
      arrange(desc(indice)) %>%
      slice_head(n = 10)
    
    validate(need(nrow(top) > 0, "Pas assez de données pour ce graphique."))
    
    cols <- c(
      "Énergie" = "#ef4444",
      "Marque"  = "#16a34a",
      "Segment" = "#3b82f6"
    )
    
    p <- ggplot(
      top,
      aes(
        x = indice,
        y = reorder(categorie, indice),
        fill = variable,
        text = paste0(
          "Catégorie : ", categorie,
          "<br>Type : ", variable,
          "<br>Sinistres : ", format(nb_sin, big.mark = " "),
          "<br>Contrats : ", format(nb_ct, big.mark = " "),
          "<br>Indice : ", round(indice, 1), "%"
        )
      )
    ) +
      geom_col(width = 0.72, alpha = 0.95) +
      scale_fill_manual(values = cols, name = "Type") +
      scale_x_continuous(
        labels = function(x) paste0(round(x, 0), "%"),
        expand = expansion(mult = c(0, 0.05))
      ) +
      labs(
        x = "Indice sinistres / contrats (%)",
        y = NULL,
        fill = "Type"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        plot.margin = margin(10, 20, 10, 20)
      )
    
    ggplotly_clean(p, left_margin = 170)
  })
  
  output$plot_ex11 <- renderPlotly({
    df <- data_f() %>%
      filter(!is.na(drv1sex)) %>%
      group_by(drv1sex) %>%
      summarise(nb = n_distinct(idx_sin), .groups = "drop") %>%
      mutate(pct = 100 * nb / sum(nb))
    
    validate(need(nrow(df) > 0, "Pas de données disponibles."))
    
    p <- ggplot(
      df,
      aes(
        x = drv1sex,
        y = nb,
        fill = drv1sex,
        text = paste0(
          "Sexe du conducteur : ", drv1sex,
          "<br>Nombre de sinistres : ", format(nb, big.mark = " "),
          "<br>Part : ", round(pct, 1), "%"
        )
      )
    ) +
      geom_col(alpha = 0.9, width = 0.65, show.legend = FALSE) +
      labs(x = "Sexe du conducteur", y = "Nombre de sinistres distincts") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
    
    ggplotly_clean(p)
  })
  
  output$map_zones <- renderLeaflet({
    validate(
      need(!is.null(dept_sf), "Impossible de charger les contours des départements."),
      need(inherits(dept_sf, "sf"), "Les contours chargés ne sont pas valides."),
      need(nrow(dept_sf) > 0, "Aucun contour de département disponible."),
      need(nzchar(STADIA_API_KEY), "Clé Stadia Maps manquante.")
    )
    
    map_data <- zones_sf()
    
    validate(
      need(!is.null(map_data), "Aucune donnée cartographique disponible."),
      need(inherits(map_data, "sf"), "Les données cartographiques ne sont pas valides."),
      need(nrow(map_data) > 0, "Aucune ligne cartographique à afficher.")
    )
    
    pal <- colorNumeric(
      palette = "Reds",
      domain = map_data$taux,
      na.color = "#e5e7eb"
    )
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Département : %s<br/>Sinistres : %s<br/>Contrats : %s<br/>Ratio : %s",
      ifelse(is.na(map_data$nom_departement), "Département", map_data$nom_departement),
      ifelse(is.na(map_data$departement), "-", map_data$departement),
      ifelse(is.na(map_data$nb_sin), "0", format(map_data$nb_sin, big.mark = " ")),
      ifelse(is.na(map_data$nb_ct), "0", format(map_data$nb_ct, big.mark = " ")),
      ifelse(is.na(map_data$taux), "NA", paste0(round(map_data$taux, 1), "%"))
    ) %>%
      lapply(htmltools::HTML)
    
    leaflet(options = leafletOptions(zoomControl = TRUE, minZoom = 5)) %>%
      addTiles(
        urlTemplate = paste0(
          "https://tiles.stadiamaps.com/tiles/stamen_toner_lite/{z}/{x}/{y}{r}.png?api_key=",
          STADIA_API_KEY
        ),
        attribution = '&copy; Stadia Maps, &copy; OpenMapTiles &copy; OpenStreetMap contributors',
        options = tileOptions(maxZoom = 20)
      ) %>%
      addPolygons(
        data = map_data,
        fillColor = ~pal(taux),
        fillOpacity = 0.80,
        color = "black",
        weight = 0.8,
        opacity = 1,
        smoothFactor = 0.2,
        label = labels,
        labelOptions = labelOptions(
          direction = "auto",
          style = list(
            "font-weight" = "500",
            "padding" = "8px 10px"
          )
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#111827",
          fillOpacity = 0.92,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = map_data$taux,
        title = "Taux de sinistralité (%)",
        opacity = 0.9
      ) %>%
      fitBounds(lng1 = -5.7, lat1 = 41.0, lng2 = 10.0, lat2 = 51.5)
  })
}

shinyApp(ui, server)
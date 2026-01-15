library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

# =======================================================
# 1. GLOBAL SETUP
# =======================================================

# --- LOAD DATA ---
cleaned_abs    <- read.csv("../cleaned_abs.csv")
metadata_cours <- read.csv("../metadata_cours.csv")
dates_examens  <- read.csv("../dates_examens.csv")

# Load daily schedule stats for volume horaire analysis
stats_dams3 <- read.csv2("../Stats_Quotidiennes_DaMS3.csv") %>% mutate(Groupe = "DaMS3")
stats_dams4 <- read.csv2("../Stats_Quotidiennes_DaMS4.csv") %>% mutate(Groupe = "DaMS4")
stats_dams5 <- read.csv2("../Stats_Quotidiennes_DaMS5.csv") %>% mutate(Groupe = "DaMS5")

stats_quotidiennes <- bind_rows(stats_dams3, stats_dams4, stats_dams5) %>%
  mutate(
    Date = as.Date(Date),
    Total_Heures_Cours = as.numeric(gsub(",", ".", Total_Heures_Cours)),
    nombre_absents = as.numeric(nombre_absents),
    # Categorize hours for Chi-squared test
    Cat_Heures = case_when(
      Total_Heures_Cours <= 3 ~ "Faible (<=3h)",
      Total_Heures_Cours <= 6 ~ "Moyen (3-6h)",
      Total_Heures_Cours > 6 ~ "Élevé (>6h)"
    ),
    Cat_Heures = factor(Cat_Heures, levels = c("Faible (<=3h)", "Moyen (3-6h)", "Élevé (>6h)")),
    # Categorize absences
    Cat_Absences = case_when(
      nombre_absents == 0 ~ "Aucune",
      nombre_absents <= 3 ~ "Faible (1-3)",
      nombre_absents <= 7 ~ "Moyen (4-7)",
      nombre_absents > 7 ~ "Élevé (>7)"
    ),
    Cat_Absences = factor(Cat_Absences, levels = c("Aucune", "Faible (1-3)", "Moyen (4-7)", "Élevé (>7)"))
  ) %>%
  filter(!is.na(Total_Heures_Cours) & !is.na(nombre_absents))

# Exam Schedule
exam_schedule <- dates_examens %>%
  left_join(metadata_cours, by = "code_cours") %>%
  filter(!is.na(niveau)) %>%
  select(exam_date = date, niveau, intitule_cours = intitule_cours.x) %>%
  distinct() %>%
  mutate(exam_date = as.Date(exam_date))

# Time Slots
slots_text <- c("08:00:00", "09:45:00", "11:30:00", "14:00:00", "15:45:00", "17:30:00", "19:15:00", "21:00:00")
dummy_date <- "2000-01-01"
slots_time <- as.POSIXct(paste(dummy_date, slots_text), format = "%Y-%m-%d %H:%M:%S")
slot_labels <- c("08:00", "09:45", "11:30", "14:00", "15:45", "17:30", "19:15")

# Day order
ordre_jours <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi")

# Date Range
date_debut <- min(as.Date(cleaned_abs$date_only), na.rm = TRUE)
date_fin <- max(as.Date(cleaned_abs$date_only), na.rm = TRUE)

# =======================================================
# 2. USER INTERFACE (UI)
# =======================================================
ui <- fluidPage(
  titlePanel("Dashboard Absentéisme - Analyse Comportementale"),
  
  sidebarLayout(
    sidebarPanel(
      # --- GLOBAL FILTERS ---
      h4("Filtres Globaux"),
      checkboxGroupInput("selected_civ", "Sélectionner les Cohortes:", 
                         choices = unique(cleaned_abs$code_civilité), 
                         selected = unique(cleaned_abs$code_civilité)),
      dateRangeInput("date_range", "Période:",
                     start = date_debut,
                     end = date_fin,
                     min = date_debut,
                     max = date_fin,
                     format = "dd/mm/yyyy",
                     language = "fr"),
      hr(),
      
      # --- CONDITIONAL PANELS ---
      
      # Controls for Student Inspector
      conditionalPanel(
        condition = "input.main_tabs == 'Analyse Individuelle'",
        h4("Sélection Étudiant"),
        selectInput("student_selector", "Choisir un Étudiant:", choices = NULL)
      ),
      
      # Controls for Population Comparison
      conditionalPanel(
        condition = "input.main_tabs == 'Comparaison Populations'",
        h4("Définir les Groupes"),
        helpText("Les étudiants au-dessus de ce seuil sont 'Irréguliers'."),
        sliderInput("split_threshold", "Seuil (nombre d'absences):", min = 1, max = 100, value = 10)
      ),
      
      # Controls for Strategic Analysis
      conditionalPanel(
        condition = "input.main_tabs == 'Absences Stratégiques'",
        h4("Paramètres Examen"),
        helpText("Nombre de jours avant l'examen considérés comme 'stratégiques'."),
        sliderInput("days_before_exam", "Jours avant examen:", min = 1, max = 7, value = 3)
      ),
      
      # Controls for Chi-squared Test
      conditionalPanel(
        condition = "input.main_tabs == 'Test Khi-2'",
        h4("Configuration du Test"),
        radioButtons("chi2_mode", "Mode d'analyse:",
                     choices = c("Absentéisme vs Variable" = "student_var",
                                 "Volume horaire vs Absences" = "hours_absences"),
                     selected = "student_var"),
        hr(),
        # Options for student variable mode
        conditionalPanel(
          condition = "input.chi2_mode == 'student_var'",
          helpText("Variable 1 : Niveau d'absentéisme (peu/beaucoup)"),
          sliderInput("chi2_seuil", "Seuil (percentile):", min = 20, max = 80, value = 50),
          hr(),
          selectInput("chi2_var2", "Variable 2 (catégorielle):",
                      choices = c("Jour de la semaine" = "jour_semaine",
                                  "Créneau horaire" = "heure_slot",
                                  "Type de cours" = "type_cours",
                                  "Cohorte" = "code_civilité",
                                  "Groupe" = "groupe_eleve",
                                  "Catégorie de cours" = "categorie_cours"),
                      selected = "jour_semaine")
        ),
        # Options for hours vs absences mode
        conditionalPanel(
          condition = "input.chi2_mode == 'hours_absences'",
          helpText("Analyse de la corrélation entre le volume horaire journalier et le nombre d'absences."),
          selectInput("chi2_groupe", "Groupe à analyser:",
                      choices = c("Tous les groupes" = "all", "DaMS3" = "DaMS3", "DaMS4" = "DaMS4", "DaMS5" = "DaMS5"),
                      selected = "all")
        )
      ),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(id = "main_tabs",
                  
                  # --- TAB 0: GENERAL ANALYSIS ---
                  tabPanel("Analyse Générale",
                           h4(textOutput("gen_pop_summary"), style = "color: #7f8c8d; font-style: italic;"),
                           h3("Vue d'Ensemble"),
                           hr(),
                           
                           fluidRow(
                             column(6,
                                    h4("Absences par Créneau Horaire"),
                                    plotOutput("gen_plot_hour", height = "300px")
                             ),
                             column(6,
                                    h4("Absences par Jour de la Semaine"),
                                    plotOutput("gen_plot_day", height = "300px")
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(6,
                                    h4("Absences par Catégorie de Cours"),
                                    plotOutput("gen_plot_category", height = "350px")
                             ),
                             column(6,
                                    h4("Absences par Matière (Top 15)"),
                                    plotOutput("gen_plot_subject", height = "350px")
                             )
                           ),
                           hr(),
                           h4("Évolution de l'Absentéisme au Fil du Semestre"),
                           plotOutput("gen_plot_weekly", height = "250px")
                  ),
                  
                  # --- TAB 1: INDIVIDUAL STUDENT ANALYSIS ---
                  tabPanel("Analyse Individuelle",
                           h4(textOutput("stu_pop_summary"), style = "color: #7f8c8d; font-style: italic;"),
                           h3(textOutput("stu_header")),
                           hr(),
                           
                           fluidRow(
                             column(6,
                                    h4("Absences par Créneau Horaire"),
                                    plotOutput("stu_plot_hour", height = "300px")
                             ),
                             column(6,
                                    h4("Absences par Jour de la Semaine"),
                                    plotOutput("stu_plot_day", height = "300px")
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(6,
                                    h4("Absences par Catégorie de Cours"),
                                    plotOutput("stu_plot_category", height = "350px")
                             ),
                             column(6,
                                    h4("Absences par Matière"),
                                    plotOutput("stu_plot_subject", height = "350px")
                             )
                           ),
                           hr(),
                           h4("Évolution de l'Absentéisme au Fil du Semestre"),
                           plotOutput("stu_plot_weekly", height = "250px")
                  ),
                  
                  # --- TAB 2: POPULATION COMPARISON ---
                  tabPanel("Comparaison Populations",
                           h4(textOutput("comp_pop_summary"), style = "color: #7f8c8d; font-style: italic;"),
                           h3("Absents réguliers vs irréguliers"),
                           
                           # Population Distribution (narrower)
                           fluidRow(
                             column(6,
                                    h4("Répartition de la Population"),
                                    plotOutput("comp_plot_dist", height = "200px")
                             ),
                             column(6,
                                    h4("Évolution de l'Absentéisme par Semaine"),
                                    plotOutput("comp_plot_weekly", height = "200px")
                             )
                           ),
                           hr(),
                           
                           fluidRow(
                             column(6,
                                    h4("Absences par Créneau Horaire"),
                                    plotOutput("comp_plot_hour", height = "300px")
                             ),
                             column(6,
                                    h4("Absences par Jour de la Semaine"),
                                    plotOutput("comp_plot_day", height = "300px")
                             )
                           ),
                           hr(),
                           fluidRow(
                             column(6,
                                    h4("Absences par Catégorie de Cours"),
                                    plotOutput("comp_plot_category", height = "400px")
                             ),
                             column(6,
                                    h4("Absences par Matière (Top 15)"),
                                    plotOutput("comp_plot_subject", height = "400px")
                             )
                           )
                  ),
                  
                  # --- TAB 3: STRATEGIC ABSENCES ---
                  tabPanel("Absences Stratégiques",
                           h4(textOutput("strat_pop_summary"), style = "color: #7f8c8d; font-style: italic;"),
                           h3("Absences liées aux Examens"),
                           hr(),
                           
                           fluidRow(
                             column(6,
                                    h4("Volume: Stratégique vs Normal"),
                                    plotOutput("strat_plot_volume", height = "300px")
                             ),
                             column(6,
                                    h4("Proportion des Absences Stratégiques"),
                                    plotOutput("strat_plot_ratio", height = "300px")
                             )
                           ),
                           hr(),
                           h4("Examens Provoquant le plus d'Absences"),
                           plotOutput("strat_plot_top_exams", height = "500px")
                  ),
                  
                  # --- TAB 4: CHI-SQUARED TEST ---
                  tabPanel("Test Khi-2",
                           h4(textOutput("chi2_pop_summary"), style = "color: #7f8c8d; font-style: italic;"),
                           h3("Test d'Indépendance du Khi-2"),
                           helpText("Ce test vérifie s'il existe une relation statistiquement significative entre le niveau d'absentéisme et une variable catégorielle."),
                           hr(),
                           
                           # Ligne 1: Tableau de Contingence
                           h4("Tableau de Contingence"),
                           tableOutput("chi2_table"),
                           hr(),
                           
                           # Ligne 2: Résidus Standardisés
                           h4("Résidus Standardisés"),
                           helpText("Positif (rouge) = sur-représentation, Négatif (bleu) = sous-représentation."),
                           plotOutput("chi2_plot", height = "300px"),
                           hr(),
                           
                           # Ligne 3: Résultats du Test
                           h4("Résultats du Test"),
                           verbatimTextOutput("chi2_result")
                  )
      )
    )
  )
)

# =======================================================
# 3. SERVER LOGIC
# =======================================================
server <- function(input, output, session) {
  
  # --- GLOBAL FILTERED DATA ---
  filtered_abs <- reactive({
    cleaned_abs %>% 
      filter(code_civilité %in% input$selected_civ) %>%
      mutate(date_only = as.Date(date_only)) %>%
      filter(date_only >= input$date_range[1] & date_only <= input$date_range[2])
  })
  
  # Reactive date limits for plots
  selected_date_debut <- reactive({ input$date_range[1] })
  selected_date_fin <- reactive({ input$date_range[2] })
  
  # =======================================================
  # INDIVIDUAL STUDENT ANALYSIS
  # =======================================================
  
  # Update student selector
  observe({
    students <- filtered_abs() %>%
      count(code_eleve) %>%
      arrange(desc(n))
    choices <- setNames(students$code_eleve, paste0(students$code_eleve, " (", students$n, " abs)"))
    updateSelectInput(session, "student_selector", choices = choices)
  })
  
  # Get selected student data
  selected_stu_data <- reactive({
    req(input$student_selector)
    cleaned_abs %>%
      filter(code_eleve == input$student_selector) %>%
      left_join(metadata_cours, by = "code_cours")
  })
  
  # Population Summary for Individual Tab
  output$stu_pop_summary <- renderText({
    data <- selected_stu_data()
    req(nrow(data) > 0)
    paste("Population: 1 étudiant |", nrow(data), "absences")
  })
  
  # Header
  output$stu_header <- renderText({
    data <- selected_stu_data()
    req(nrow(data) > 0)
    paste("Étudiant:", input$student_selector, 
          "| Cohorte:", unique(data$code_civilité)[1], 
          "| Groupe:", unique(data$groupe_eleve)[1])
  })
  
  # Plot: Absences by Hour
  output$stu_plot_hour <- renderPlot({
    data <- selected_stu_data()
    req(nrow(data) > 0)
    
    data %>%
      mutate(raw_time = as.POSIXct(paste(dummy_date, heure), format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(slot_idx = cut(raw_time, breaks = slots_time, labels = FALSE, right = FALSE)) %>%
      filter(!is.na(slot_idx)) %>%
      mutate(slot_label = slot_labels[slot_idx]) %>%
      count(slot_label) %>%
      mutate(slot_label = factor(slot_label, levels = slot_labels)) %>%
      ggplot(aes(x = slot_label, y = n)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
      labs(x = "Heure de début", y = "Nombre d'absences") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot: Absences by Day of Week
  output$stu_plot_day <- renderPlot({
    data <- selected_stu_data()
    req(nrow(data) > 0)
    
    data %>%
      filter(jour_semaine %in% ordre_jours) %>%
      count(jour_semaine) %>%
      mutate(jour_semaine = factor(jour_semaine, levels = ordre_jours)) %>%
      ggplot(aes(x = jour_semaine, y = n)) +
      geom_col(fill = "#2ecc71", alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
      labs(x = "Jour", y = "Nombre d'absences") +
      theme_minimal(base_size = 12)
  })
  
  # Plot: Absences by Category
  output$stu_plot_category <- renderPlot({
    data <- selected_stu_data()
    req(nrow(data) > 0)
    
    data %>%
      filter(!is.na(categorie_cours) & categorie_cours != "") %>%
      mutate(categorie_cours = case_when(
        categorie_cours == "Pj" ~ "Projet",
        categorie_cours == "Pr" ~ "Prog",
        TRUE ~ categorie_cours
      )) %>%
      count(categorie_cours) %>%
      ggplot(aes(x = reorder(categorie_cours, n), y = n)) +
      geom_col(fill = "#9b59b6", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold") +
      coord_flip() +
      labs(x = "", y = "Nombre d'absences") +
      theme_minimal(base_size = 12)
  })
  
  # Plot: Absences by Subject
  output$stu_plot_subject <- renderPlot({
    data <- selected_stu_data()
    req(nrow(data) > 0)
    
    data %>%
      filter(!is.na(intitule_cours) & intitule_cours != "") %>%
      count(intitule_cours) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = reorder(intitule_cours, n), y = n)) +
      geom_col(fill = "#e67e22", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold") +
      coord_flip() +
      labs(x = "", y = "Nombre d'absences") +
      theme_minimal(base_size = 12)
  })
  
  # Plot: Weekly Evolution (Individual Student)
  output$stu_plot_weekly <- renderPlot({
    data <- selected_stu_data()
    req(nrow(data) > 0)
    
    data %>%
      mutate(date_only = as.Date(date_only),
             week = floor_date(date_only, "week")) %>%
      count(week) %>%
      ggplot(aes(x = week, y = n)) +
      geom_line(color = "#3498db", size = 1) +
      geom_point(color = "#3498db", size = 3) +
      geom_text(aes(label = n), vjust = -0.8, size = 3) +
      scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks", limits = c(selected_date_debut(), selected_date_fin())) +
      labs(x = "Semaine", y = "Nombre d'absences") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # =======================================================
  # POPULATION COMPARISON
  # =======================================================
  
  # Update threshold slider based on data
  observe({
    req(filtered_abs())
    stu_counts <- filtered_abs() %>% count(code_eleve)
    max_abs <- max(stu_counts$n, na.rm = TRUE)
    med_abs <- median(stu_counts$n, na.rm = TRUE)
    updateSliderInput(session, "split_threshold", min = 1, max = max_abs, value = med_abs)
  })
  
  # Prepare comparison data
  comparison_data <- reactive({
    req(filtered_abs())
    
    student_totals <- filtered_abs() %>%
      group_by(code_eleve) %>%
      summarise(total_abs = n()) %>%
      mutate(group_label = ifelse(total_abs > input$split_threshold, "Irréguliers", "Réguliers"))
    
    filtered_abs() %>%
      left_join(student_totals, by = "code_eleve") %>%
      left_join(metadata_cours, by = "code_cours")
  })
  
  # Color palette for groups
  group_colors <- c("Irréguliers" = "#e74c3c", "Réguliers" = "#3498db")
  
  # Population Summary for Comparison Tab
  output$comp_pop_summary <- renderText({
    req(comparison_data())
    
    stats <- comparison_data() %>%
      select(code_eleve, group_label) %>%
      distinct() %>%
      count(group_label)
    
    n_reg <- stats %>% filter(group_label == "Réguliers") %>% pull(n)
    n_irreg <- stats %>% filter(group_label == "Irréguliers") %>% pull(n)
    n_reg <- ifelse(length(n_reg) == 0, 0, n_reg)
    n_irreg <- ifelse(length(n_irreg) == 0, 0, n_irreg)
    total_students <- n_reg + n_irreg
    total_abs <- nrow(comparison_data())
    
    paste("Population:", total_students, "étudiants (", n_reg, "réguliers,", n_irreg, "irréguliers) |", total_abs, "absences au total")
  })
  
  # Plot: Population Distribution
  output$comp_plot_dist <- renderPlot({
    req(comparison_data())
    
    distinct_students <- comparison_data() %>%
      select(code_eleve, total_abs, group_label) %>%
      distinct()
    
    ggplot(distinct_students, aes(x = total_abs, fill = group_label)) +
      geom_histogram(binwidth = 2, color = "white", alpha = 0.8) +
      geom_vline(xintercept = input$split_threshold, linetype = "dashed", size = 1, color = "#2c3e50") +
      scale_fill_manual(values = group_colors) +
      labs(x = "Absences par étudiant", y = "Étudiants", fill = "Groupe") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "top")
  })
  
  # Plot: Weekly Evolution (Comparison)
  output$comp_plot_weekly <- renderPlot({
    req(comparison_data())
    
    comparison_data() %>%
      mutate(date_only = as.Date(date_only),
             week = floor_date(date_only, "week")) %>%
      count(group_label, week) %>%
      group_by(group_label) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      ggplot(aes(x = week, y = pct, color = group_label)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = group_colors) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(date_labels = "%d %b", date_breaks = "3 weeks", limits = c(selected_date_debut(), selected_date_fin())) +
      labs(x = "Semaine", y = "Part des absences (%)", color = "Groupe") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
  })
  
  # Plot: Absences by Hour (Comparison)
  output$comp_plot_hour <- renderPlot({
    req(comparison_data())
    
    comparison_data() %>%
      mutate(raw_time = as.POSIXct(paste(dummy_date, heure), format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(slot_idx = cut(raw_time, breaks = slots_time, labels = FALSE, right = FALSE)) %>%
      filter(!is.na(slot_idx)) %>%
      mutate(slot_label = slot_labels[slot_idx]) %>%
      count(group_label, slot_label) %>%
      group_by(group_label) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      mutate(slot_label = factor(slot_label, levels = slot_labels)) %>%
      ggplot(aes(x = slot_label, y = pct, fill = group_label)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = group_colors) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Heure de début", y = "Part des absences (%)", fill = "Groupe") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
  })
  
  # Plot: Absences by Day of Week (Comparison)
  output$comp_plot_day <- renderPlot({
    req(comparison_data())
    
    comparison_data() %>%
      filter(jour_semaine %in% ordre_jours) %>%
      count(group_label, jour_semaine) %>%
      group_by(group_label) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      mutate(jour_semaine = factor(jour_semaine, levels = ordre_jours)) %>%
      ggplot(aes(x = jour_semaine, y = pct, fill = group_label)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = group_colors) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Jour", y = "Part des absences (%)", fill = "Groupe") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  # Plot: Absences by Category (Comparison)
  output$comp_plot_category <- renderPlot({
    req(comparison_data())
    
    comparison_data() %>%
      filter(!is.na(categorie_cours) & categorie_cours != "") %>%
      mutate(categorie_cours = case_when(
        categorie_cours == "Pj" ~ "Projet",
        categorie_cours == "Pr" ~ "Prog",
        TRUE ~ categorie_cours
      )) %>%
      count(group_label, categorie_cours) %>%
      group_by(group_label) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(categorie_cours, pct), y = pct, fill = group_label)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = group_colors) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(x = "", y = "Part des absences (%)", fill = "Groupe") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  # Plot: Absences by Subject (Comparison - Top 15)
  output$comp_plot_subject <- renderPlot({
    req(comparison_data())
    
    # Get top 15 subjects overall
    top_subjects <- comparison_data() %>%
      filter(!is.na(intitule_cours) & intitule_cours != "") %>%
      count(intitule_cours) %>%
      arrange(desc(n)) %>%
      slice_head(n = 15) %>%
      pull(intitule_cours)
    
    comparison_data() %>%
      filter(intitule_cours %in% top_subjects) %>%
      count(group_label, intitule_cours) %>%
      group_by(group_label) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup() %>%
      ggplot(aes(x = reorder(intitule_cours, pct), y = pct, fill = group_label)) +
      geom_col(position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = group_colors) +
      scale_y_continuous(labels = scales::percent) +
      coord_flip() +
      labs(x = "", y = "Part des absences (%)", fill = "Groupe") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  # =======================================================
  # GENERAL ANALYSIS
  # =======================================================
  
  # Prepare general data with metadata
  general_data <- reactive({
    req(filtered_abs())
    filtered_abs() %>%
      left_join(metadata_cours, by = "code_cours")
  })
  
  # Population Summary for General Tab
  output$gen_pop_summary <- renderText({
    req(general_data())
    n_students <- n_distinct(general_data()$code_eleve)
    n_abs <- nrow(general_data())
    paste("Population:", n_students, "étudiants |", n_abs, "absences")
  })
  
  # Plot: Absences by Hour (General)
  output$gen_plot_hour <- renderPlot({
    req(general_data())
    
    general_data() %>%
      mutate(raw_time = as.POSIXct(paste(dummy_date, heure), format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(slot_idx = cut(raw_time, breaks = slots_time, labels = FALSE, right = FALSE)) %>%
      filter(!is.na(slot_idx)) %>%
      mutate(slot_label = slot_labels[slot_idx]) %>%
      count(slot_label) %>%
      mutate(slot_label = factor(slot_label, levels = slot_labels)) %>%
      ggplot(aes(x = slot_label, y = n)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
      labs(x = "Heure de début", y = "Nombre d'absences") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot: Absences by Day of Week (General)
  output$gen_plot_day <- renderPlot({
    req(general_data())
    
    general_data() %>%
      filter(jour_semaine %in% ordre_jours) %>%
      count(jour_semaine) %>%
      mutate(jour_semaine = factor(jour_semaine, levels = ordre_jours)) %>%
      ggplot(aes(x = jour_semaine, y = n)) +
      geom_col(fill = "#2ecc71", alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, fontface = "bold") +
      labs(x = "Jour", y = "Nombre d'absences") +
      theme_minimal(base_size = 12)
  })
  
  # Plot: Absences by Category (General)
  output$gen_plot_category <- renderPlot({
    req(general_data())
    
    general_data() %>%
      filter(!is.na(categorie_cours) & categorie_cours != "") %>%
      mutate(categorie_cours = case_when(
        categorie_cours == "Pj" ~ "Projet",
        categorie_cours == "Pr" ~ "Prog",
        TRUE ~ categorie_cours
      )) %>%
      count(categorie_cours) %>%
      ggplot(aes(x = reorder(categorie_cours, n), y = n)) +
      geom_col(fill = "#9b59b6", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold") +
      coord_flip() +
      labs(x = "", y = "Nombre d'absences") +
      theme_minimal(base_size = 12)
  })
  
  # Plot: Absences by Subject (General - Top 15)
  output$gen_plot_subject <- renderPlot({
    req(general_data())
    
    general_data() %>%
      filter(!is.na(intitule_cours) & intitule_cours != "") %>%
      count(intitule_cours) %>%
      arrange(desc(n)) %>%
      slice_head(n = 15) %>%
      ggplot(aes(x = reorder(intitule_cours, n), y = n)) +
      geom_col(fill = "#e67e22", alpha = 0.8) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold") +
      coord_flip() +
      labs(x = "", y = "Nombre d'absences") +
      theme_minimal(base_size = 12)
  })
  
  # Plot: Weekly Evolution (General)
  output$gen_plot_weekly <- renderPlot({
    req(general_data())
    
    general_data() %>%
      mutate(date_only = as.Date(date_only),
             week = floor_date(date_only, "week")) %>%
      count(week) %>%
      ggplot(aes(x = week, y = n)) +
      geom_line(color = "#3498db", size = 1) +
      geom_point(color = "#3498db", size = 3) +
      geom_text(aes(label = n), vjust = -0.8, size = 3) +
      scale_x_date(date_labels = "%d %b", date_breaks = "2 weeks", limits = c(selected_date_debut(), selected_date_fin())) +
      labs(x = "Semaine", y = "Nombre d'absences") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # =======================================================
  # STRATEGIC ABSENCES
  # =======================================================
  
  # Prepare strategic analysis data
  strategic_data <- reactive({
    req(filtered_abs())
    
    df <- filtered_abs() %>% 
      mutate(absence_id = row_number(), date_only = as.Date(date_only))
    
    # Find absences that occurred within N days before an exam
    impact <- df %>%
      inner_join(exam_schedule, by = c("code_civilité" = "niveau"), relationship = "many-to-many") %>%
      mutate(days_until = as.numeric(exam_date - date_only)) %>%
      filter(days_until >= 0 & days_until <= input$days_before_exam)
    
    # Top exams causing absences
    top_exams <- impact %>%
      group_by(intitule_cours, exam_date, code_civilité) %>%
      summarise(total = n(), distinct_students = n_distinct(code_eleve), .groups = "drop") %>%
      arrange(desc(total))
    
    # Flag strategic absences
    pre_exam_flags <- impact %>% select(absence_id) %>% distinct() %>% mutate(is_strategic = TRUE)
    
    # Counts by strategic vs normal
    counts <- df %>%
      left_join(pre_exam_flags, by = "absence_id") %>%
      mutate(is_strategic = !is.na(is_strategic)) %>%
      group_by(is_strategic, code_civilité) %>%
      summarise(count = n(), .groups = "drop")
    
    list(top_exams = top_exams, counts = counts, total_strategic = sum(pre_exam_flags$is_strategic, na.rm = TRUE))
  })
  
  # Population Summary for Strategic Tab
  output$strat_pop_summary <- renderText({
    req(strategic_data())
    data <- strategic_data()
    total_abs <- sum(data$counts$count)
    strategic_abs <- data$total_strategic
    paste("Population:", n_distinct(filtered_abs()$code_eleve), "étudiants |", 
          total_abs, "absences |", strategic_abs, "stratégiques (", 
          round(100 * strategic_abs / total_abs, 1), "%)")
  })
  
  # Plot: Strategic Volume
  output$strat_plot_volume <- renderPlot({
    req(strategic_data())
    
    cols <- c("FALSE" = "#3498db", "TRUE" = "#e74c3c")
    plot_data <- strategic_data()$counts
    
    ggplot(plot_data, aes(x = code_civilité, y = count, fill = is_strategic)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7) +
      geom_text(
        aes(label = count), 
        position = position_dodge(width = 0.8), 
        vjust = -0.5, 
        fontface = "bold"
      ) +
      scale_fill_manual(
        values = cols,
        name = "Type:",
        labels = c("TRUE" = "Stratégique", "FALSE" = "Normal")
      ) +
      labs(x = "Cohorte", y = "Nombre d'absences") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "top")
  })
  
  # Plot: Strategic Ratio
  output$strat_plot_ratio <- renderPlot({
    req(strategic_data())
    
    cols <- c("FALSE" = "#3498db", "TRUE" = "#e74c3c")
    plot_data <- strategic_data()$counts %>%
      group_by(code_civilité) %>%
      mutate(pct = count / sum(count)) %>%
      ungroup()
    
    ggplot(plot_data, aes(x = code_civilité, y = pct, fill = is_strategic)) +
      geom_col(position = "fill", width = 0.7) +
      geom_text(
        aes(label = scales::percent(pct, accuracy = 1)), 
        position = position_fill(vjust = 0.5), 
        fontface = "bold", 
        color = "white"
      ) +
      scale_fill_manual(values = cols) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Cohorte", y = "Proportion") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })
  
  # Plot: Top Exams
  output$strat_plot_top_exams <- renderPlot({
    req(strategic_data())
    
    data <- head(strategic_data()$top_exams, 20)
    
    ggplot(data, aes(x = reorder(intitule_cours, total), y = total)) +
      geom_segment(aes(xend = intitule_cours, yend = 0), color = "grey50") +
      geom_point(aes(color = code_civilité, size = distinct_students)) +
      coord_flip() +
      labs(
        x = "",
        y = "Absences générées",
        color = "Cohorte",
        size = "Étudiants"
      ) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "right")
  })
  
  # =======================================================
  # CHI-SQUARED TEST
  # =======================================================
  
  # Prepare data for chi-squared test (student variable mode)
  chi2_data <- reactive({
    req(filtered_abs())
    req(input$chi2_mode == "student_var")
    
    # Calculate absences per student
    student_totals <- filtered_abs() %>%
      count(code_eleve, name = "total_abs")
    
    # Calculate percentile threshold
    seuil <- quantile(student_totals$total_abs, input$chi2_seuil / 100)
    
    # Assign absence category (2 groups only)
    student_totals <- student_totals %>%
      mutate(niveau_absenteisme = ifelse(total_abs <= seuil, "Peu", "Beaucoup"))
    
    # Join with absence data
    df <- filtered_abs() %>%
      left_join(student_totals, by = "code_eleve") %>%
      left_join(metadata_cours, by = "code_cours") %>%
      mutate(
        raw_time = as.POSIXct(paste(dummy_date, heure), format = "%Y-%m-%d %H:%M:%S"),
        slot_idx = cut(raw_time, breaks = slots_time, labels = FALSE, right = FALSE),
        heure_slot = ifelse(!is.na(slot_idx), slot_labels[slot_idx], NA),
        type_cours = str_to_title(str_trim(type_cours)),
        categorie_cours = case_when(
          categorie_cours == "Pj" ~ "Projet",
          categorie_cours == "Pr" ~ "Prog",
          TRUE ~ categorie_cours
        )
      ) %>%
      filter(!is.na(niveau_absenteisme) & !is.na(!!sym(input$chi2_var2)))
    
    df
  })
  
  # Prepare data for chi-squared test (hours vs absences mode)
  chi2_hours_data <- reactive({
    req(input$chi2_mode == "hours_absences")
    
    df <- stats_quotidiennes
    
    # Filter by group if selected
    if (input$chi2_groupe != "all") {
      df <- df %>% filter(Groupe == input$chi2_groupe)
    }
    
    df %>% filter(!is.na(Cat_Heures) & !is.na(Cat_Absences))
  })
  
  # Population Summary
  output$chi2_pop_summary <- renderText({
    if (input$chi2_mode == "student_var") {
      req(chi2_data())
      n_obs <- nrow(chi2_data())
      n_students <- n_distinct(chi2_data()$code_eleve)
      paste("Population:", n_students, "étudiants |", n_obs, "observations")
    } else {
      req(chi2_hours_data())
      n_obs <- nrow(chi2_hours_data())
      n_groups <- n_distinct(chi2_hours_data()$Groupe)
      paste("Population:", n_obs, "journées analysées |", n_groups, "groupe(s)")
    }
  })
  
  # Contingency Table
  output$chi2_table <- renderTable({
    if (input$chi2_mode == "student_var") {
      req(chi2_data())
      tbl <- table(chi2_data()[["niveau_absenteisme"]], chi2_data()[[input$chi2_var2]])
    } else {
      req(chi2_hours_data())
      tbl <- table(chi2_hours_data()[["Cat_Heures"]], chi2_hours_data()[["Cat_Absences"]])
    }
    as.data.frame.matrix(tbl)
  }, rownames = TRUE)
  
  # Chi-squared Test Result
  output$chi2_result <- renderPrint({
    if (input$chi2_mode == "student_var") {
      req(chi2_data())
      tbl <- table(chi2_data()[["niveau_absenteisme"]], chi2_data()[[input$chi2_var2]])
      var1_name <- "Niveau d'absentéisme"
      var2_name <- input$chi2_var2
    } else {
      req(chi2_hours_data())
      tbl <- table(chi2_hours_data()[["Cat_Heures"]], chi2_hours_data()[["Cat_Absences"]])
      var1_name <- "Volume horaire"
      var2_name <- "Nombre d'absences"
    }
    
    if (min(dim(tbl)) < 2) {
      cat("Erreur: Il faut au moins 2 catégories dans chaque variable.")
      return()
    }
    
    # Check expected frequencies before test
    test <- chisq.test(tbl)
    expected <- test$expected
    low_expected <- sum(expected < 5)
    
    cat("=== Résultats du Test Khi-2 ===\n\n")
    cat("Variables analysées:\n")
    cat("  - Variable 1:", var1_name, "\n")
    cat("  - Variable 2:", var2_name, "\n\n")
    cat("Statistique X²:", round(test$statistic, 2), "\n")
    cat("Degrés de liberté:", test$parameter, "\n")
    cat("P-value:", format.pval(test$p.value, digits = 4), "\n\n")
    
    # Show warning if approximation may be incorrect
    if (low_expected > 0) {
      cat("⚠️  ATTENTION: Approximation possiblement incorrecte!\n")
      cat("   ", low_expected, "cellule(s) avec effectif attendu < 5.\n")
      cat("   Interprétez les résultats avec prudence.\n\n")
    }
    
    if (test$p.value < 0.001) {
      cat("Conclusion: Relation TRÈS SIGNIFICATIVE (p < 0.001)\n")
      cat("Les variables sont fortement dépendantes.")
    } else if (test$p.value < 0.01) {
      cat("Conclusion: Relation SIGNIFICATIVE (p < 0.01)\n")
      cat("Les variables sont dépendantes.")
    } else if (test$p.value < 0.05) {
      cat("Conclusion: Relation significative (p < 0.05)\n")
      cat("Il existe une relation entre les variables.")
    } else {
      cat("Conclusion: PAS de relation significative (p >= 0.05)\n")
      cat("Les variables semblent indépendantes.")
    }
  })
  
  # Residuals Plot
  output$chi2_plot <- renderPlot({
    if (input$chi2_mode == "student_var") {
      req(chi2_data())
      tbl <- table(chi2_data()[["niveau_absenteisme"]], chi2_data()[[input$chi2_var2]])
      x_label <- "Niveau d'absentéisme"
      y_label <- input$chi2_var2
    } else {
      req(chi2_hours_data())
      tbl <- table(chi2_hours_data()[["Cat_Heures"]], chi2_hours_data()[["Cat_Absences"]])
      x_label <- "Volume horaire"
      y_label <- "Nombre d'absences"
    }
    
    if (min(dim(tbl)) < 2) return(NULL)
    
    test <- chisq.test(tbl)
    residuals_df <- as.data.frame(as.table(test$stdres))
    names(residuals_df) <- c("Var1", "Var2", "Residual")
    
    ggplot(residuals_df, aes(x = Var1, y = Var2, fill = Residual)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(Residual, 1)), color = "black", size = 4) +
      scale_fill_gradient2(low = "#3498db", mid = "white", high = "#e74c3c", midpoint = 0) +
      labs(x = x_label, y = y_label, fill = "Résidu") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui = ui, server = server)

shinyApp(ui = ui, server = server)
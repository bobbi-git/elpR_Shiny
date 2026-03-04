#Purpose of script: To make a shiny app for the elpR package.
#Author: Jidapa Janpathompong
#Date created: April 2025
#Date last updated: February 10, 2026

#Instructions: To run the app, call elpRApp() in the console.

#Notes: This script is organized by sections in the UI:
#ABOUT
#1. Basic Info
#2. Sound Check
#3. Exclude Files
#4. Restructure
#5. Data Summaries
#6. Results
#HELP

elpRApp <- function(){
  # #load libraries
  # library(shiny)
  # library(bslib) #shiny layout functions
  # library(shinyFiles) #shinyDirChoose(), parseDirPath()
  # library(gbRd) #Rd_fun()
  # library(tools) #Rd2HTML()
  # library(readr) #read_files()
  # library(shinyjs) #useShinyjs()
  # library(ggplot2)
  # library(leaflet)
  # library(dplyr) #summarize()
  # library(mapview) #mapshot()
  # library(webshot) #mapshot()
  # if(webshot::is_phantomjs_installed() == FALSE){
  #   webshot::install_phantomjs()
  # } #related to downloading leaflet maps
  # library(DT) #datatable()

  #_______________________________________________________________________________
  #### DEFINE UI ####
  ui <- page_fillable(
    #HTML styling
    tags$head(
      tags$style(HTML("
    .title {
    font-size: 40px;
    margin-top: -20px;
    margin-bottom: -20px;
    }
    h2{
    margin-top: 0px;
    margin-bottom: 0px;
    }
    h3{
    margin-top: 10px;
    margin-bottom: 10px;
    }
    p {
    margin-top: -10px;
    margin-bottom; 0px;
    }
    .image {
    position: absolute;
    top: 10px;
    right: 20px;
    }
    #eng_button {
    position: absolute;
    top: 38px;
    right: 230px;
    padding: 10px;
    padding-left: 15px;
    padding-right: 15px;
    border-radius: 0%;
    }
    #french_button {
    position: absolute;
    top: 38px;
    right: 150px;
    padding: 10px;
    border-radius: 0%;
    }
    #subtitle {
    font-size: 18px;
    margin-top: -50px;
    #margin-bottom: -50px;
    }
    .line {
    position: absolute;
    top: 75px;
    left: 0px;
    width: 100%
    }
    "))
    ),

    #______________________________________
    ##### header #####
    div(class = "title", "ElpR App"),
    div(uiOutput("eng_ui")),
    div(uiOutput("french_ui")),
    div(class = "image", imageOutput("logo")),
    div(uiOutput("subtitle")),
    div(class = "line", hr()),

    #______________________________________
    ##### body #####
    navlistPanel(
      well = TRUE,

      ###### ABOUT ######
      tabPanel(
        textOutput("tab_about"),
        layout_columns(
          uiOutput("header_welcome"),
          uiOutput("text_welcome"),
          uiOutput("header_contributers"),
          uiOutput("text_contributers"),
          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12), md = c(10), lg = c(12))
        )
      ),

      ###### 1. Basic Info ######
      tabPanel(
        textOutput("tab_1"),
        layout_columns(
          card(
            card_header(textOutput("card_1_1")),

            textInput("deployment_name_in", label = textOutput("card_1_1_dep_name"), value = "kk_202405_may"),
            textInput("deployment_num_in", label = "Deployment number:", value = "02"),
            textInput("disk_ID_in", label = "Disk ID:", value = "00"),
            fileInput("sites_in", label = "Sites txt file:"),
            shinyDirButton(id = "parent_dir_in",
                           label = "Choose the \"files_for_elpR\" folder",
                           title = "Choose a folder"),
            textOutput("parent_dir_recieved")
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12,12), md = c(9,12), lg = c(6,12))
        )
      ),

      ###### 2. Sound Check ######
      tabPanel(
        textOutput("tab_2"),
        layout_columns(
          card(
            card_header("Input Sound Check Information"),

            shinyDirButton(id = "sound_path_in",
                           label = "Choose folder containing sound files",
                           title = "Choose a folder"),
            textOutput("sound_path_recieved"),
            numericInput("fileDurationMin_in", label = "File duration (minutes):", value = 14000),
            numericInput("sample_rate_in", label = "Sample rate (Hz):", value = 8000),
            selectInput("sound_file_ext_in", "Sound file extention:", choices = list(".wav", ".flac", ".aiff"))
          ),

          card(
            card_header("Run Sound Check"),

            actionButton("check_sound_check_info_in", "Check Your Info"),
            tableOutput("check_sound_check_info_output"),
            input_task_button("run_sound_check_in", "Run Sound Check"),
            "Results preview:",
            verbatimTextOutput("run_sound_check_output")
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12,12,12), md = c(9,9,12), lg = c(6,6,12))
        )
      ),

      ###### 3. Exclude Files ######
      tabPanel(
        textOutput("tab_3"),
        layout_columns(
          card(
            card_header("Input Exclude Files Information"),

            shinyDirButton(id = "extra_sounds_in",
                           label = "Choose folder to output extra sounds",
                           title = "Choose a folder"),
            textOutput("extra_sounds_recieved"),
            radioButtons("have_swift_files_in", label = "Do you have Swift files in the sounds folder?",
                         choices = list("Yes", "No"),
                         selected = "No"),
            uiOutput("merge_swift_files_output")
          ),

          card(
            card_header("Run Exclude Files"),

            actionButton("check_exclude_files_info_in", "Check Your Info"),
            tableOutput("check_exclude_files_info_output"),
            input_task_button("run_exclude_files_in", "Run Exclude Files"),
            "Results summary:",
            verbatimTextOutput("run_exclude_files_output")
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12,12,12), md = c(9,9,12), lg = c(6,6,12))
        )
      ),

      ###### 4. Restructure ######
      tabPanel(
        textOutput("tab_4"),
        layout_columns(
          "Choose type of data:",

          navset_pill(
            ###### > rumbles ######
            tabPanel(
              "Rumbles",
              layout_columns(
                "\n",

                card(
                  card_header("Input Rumble Detector Information"),

                  textInput("sample_rate_chr_in", label = "Sample Rate:", value = "8kHz"),
                  radioButtons("three_rand_days_in", label = "Do you want 3 random days per week? (y/n):",
                               choices = list("Yes", "No"),
                               selected = "Yes"),
                  radioButtons("min_23hours_in", label = "Does your project require a minimum of 23 hours per day? (y/n):",
                               choices = list("Yes", "No"),
                               selected = "Yes"),
                  textInput("score_column_name_in", label = "Score column name:", value = "Score"),
                  selectInput("detector_in", "Detector used:", choices = list("HoriHarm", "FruitPunchAI", "Stanford Detector")),
                  numericInput("detector_score_in", label = "Detector score:", value = 0.2),
                  numericInput("filter_score_in", label = "Filter score:", value = 0.4)
                ),

                card(
                  card_header("Run Rumble Restructure"),

                  actionButton("check_restructure_info_in", "Check Your Info"),
                  tableOutput("check_restructure_info_output"),
                  input_task_button("run_restructure_in", "Run Rumble Restructure"),
                  "Results summary:",
                  tableOutput("run_restructure_output")
                ),

                col_widths = breakpoints(sm = c(12,12,12), md = c(12,9,9), lg = c(12,6,6))
              )
            ),

            ###### > gunshots ######
            tabPanel(
              "Gunshots",
              layout_columns(
                "\n",

                card(
                  card_header("Input Gunshot Detector Information"),

                  textInput("sample_rate_chr_gun_in", label = "Sample Rate:", value = "8kHz"),
                  selectInput("detector_gun_in", "Detector used:", choices = list("DTDguns8")),
                  numericInput("detector_score_gun_in", label = "Detector score:", value = 0.53),
                  numericInput("filter_score_gun_in", label = "Filter score:", value = 0.53)
                ),

                card(
                  card_header("Run Gunshot Restructure"),

                  actionButton("check_gun_restructure_info_in", "Check Your Info"),
                  tableOutput("check_gun_restructure_info_output"),
                  input_task_button("run_gun_restructure_in", "Run Gunshot Restructure"),
                  "Results summary:",
                  tableOutput("run_gun_restructure_output")
                ),

                col_widths = breakpoints(sm = c(12,12,12), md = c(12,9,9), lg = c(12,6,6))
              )
            ),

            ###### > general ######
            tabPanel(
              "General",
              layout_columns(
                "\n",

                card(
                  card_header("Input General Information"),

                  shinyDirButton(id = "general_merge_in",
                                 label = "Choose folder containing selection tables to merge",
                                 title = "Choose a folder"),
                  textOutput("general_merge_recieved"),
                  radioButtons("recursive_in", label = "How are the selection tables organized?",
                               choices = list("In one folder",
                                              "In multiple subfolders"))
                ),

                card(
                  card_header("Run General Restructure"),

                  actionButton("check_general_restructure_info_in", "Check Your Info"),
                  tableOutput("check_general_restructure_info_output"),
                  input_task_button("run_general_restructure_in", "Run General Restructure"),
                  "Results summary:",
                  verbatimTextOutput("run_general_restructure_output")
                ),

                col_widths = breakpoints(sm = c(12,12,12), md = c(12,9,9), lg = c(12,6,6))
              )
            )
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12), md = c(12), lg = c(12))
        )
      ),

      ###### 5. Data Summaries ######
      tabPanel(
        textOutput("tab_5"),
        layout_columns(
          card(
            card_header("Input Data Summaries Information"),

            textInput("project_name_in", label = "Project name:", value = "PNNN"),
            textInput("deployment_nums_in", label = "Deployment number(s):", value = "01-20"),
            selectInput("summary_detector_in", "Detector used:", choices = list("HoriHarm", "FruitPunchAI", "Stanford Detector")),
            shinyDirButton(id = "summary_folder_in",
                           label = "Choose folder to output data summaries",
                           title = "Choose a folder"),
            textOutput("summary_folder_recieved"),
            shinyDirButton(id = "selection_tables_folder_in",
                           label = "Choose folder containing selection tables",
                           title = "Choose a folder"),
            textOutput("selection_tables_folder_recieved"),
            shinyDirButton(id = "zero_selection_tables_folder_in",
                           label = "Choose folder containing zero-day selection tables",
                           title = "Choose a folder"),
            textOutput("zero_selection_tables_folder_recieved")
          ),

          card(
            card_header("Input OPTIONAL Data Summaries Information"),

            radioButtons("sound_check_include_in", label = "Do you want to include a sound check file?",
                         choices = list("Yes", "No"),
                         selected = "No"),
            uiOutput("sound_check_include_output"),
            textOutput("sound_check_folder_recieved"),
            radioButtons("use_only_sites_provided_in", label = "Do you only want to include sites listed in a sites file?",
                         choices = list("Yes", "No"),
                         selected = "No"),
            uiOutput("use_only_sites_provided_output"),
            radioButtons("rand_dates_needed_in", label = "Do you need random dates?",
                         choices = list("Yes", "No"),
                         selected = "No"),
            radioButtons("ele_bad_sound_remove_in", label = "Do you want to exclude sounds <23 hours?",
                         choices = list("Yes", "No"),
                         selected = "No")
          ),

          card(
            card_header("Run Data Summaries"),

            actionButton("check_summary_info_in", "Check Your Info"),
            tableOutput("check_summary_info_output"),
            input_task_button("run_summary_in", "Run Data Summaries"),
            "Results summary:",
            verbatimTextOutput("run_summary_output")
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12,12,12,12), md = c(9,9,12,12), lg = c(6,6,10,12))
        )
      ),

      ###### 6. Results! ######
      tabPanel(
        textOutput("tab_6"),
        layout_columns(
          navset_pill(
            ###### > plots ######
            tabPanel(
              "Plots",
              layout_columns(
                "\n",

                card(
                  card_header("Input File Information"),

                  shinyDirButton(id = "saved_plots_folder_in",
                                 label = "Choose folder containing .Rds file with saved plots",
                                 title = "Choose a folder"),
                  textOutput("saved_plots_folder_recieved"),
                  textInput("saved_plots_file_in", label = "Name of .Rds file:", value = "saved_plots.Rds"),

                  useShinyjs(),
                  input_task_button("load_plots", "Load Plots"),
                  p(id = "load_plots_message", "Loading plots...")
                ),

                card(
                  min_height = "500px",
                  layout_sidebar(
                    sidebar = sidebar(
                      p(tags$b("Plot Options")),
                      p(id = "placeholder"),
                      p(tags$b("Download Options")),
                      radioButtons("download_ext_in", label = "File format to download:",
                                   choices = list(".png", ".eps"),
                                   selected = ".png"),
                    ),
                    plotOutput("plots_output"),
                    downloadButton("download_plots", "Download Plot")
                  )
                ),

                col_widths = breakpoints(sm = c(12,12,12), md = c(12,9,12), lg = c(12,6,12))
              )
            ),

            ###### > maps ######
            tabPanel(
              "Map",
              layout_columns(
                "\n",

                card(
                  card_header("Input File Information"),

                  shinyDirButton(id = "saved_tables_folder_in",
                                 label = "Choose folder containing data summary tables",
                                 title = "Choose a folder"),
                  textOutput("saved_tables_folder_recieved"),
                  fileInput("site_lat_long_map",
                            label = "Sites txt file:"),

                  useShinyjs(),
                  input_task_button("load_tables", "Load Tables"),
                  p(id = "load_tables_message", "Loading tables...")
                ),

                card(
                  min_height = "600px",
                  layout_sidebar(
                    sidebar = sidebar(
                      p(tags$b("Map Options")),
                      p(id = "placeholder2")
                    ),
                    leafletOutput("map_output"),
                    downloadButton("download_maps", "Download Map"),
                  )
                ),

                card(
                  min_height = "600px",
                  dataTableOutput(outputId = "map_df")
                ),

                col_widths = breakpoints(sm = c(12,12,12), md = c(12,9,12), lg = c(12,6,12))
              )
            )
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12), md = c(12), lg = c(12))
        )
      ),

      ###### HELP ######
      tabPanel(
        textOutput("tab_help"),
        layout_columns(
          accordion(
            open = FALSE,
            multiple = FALSE,

            accordion_panel(
              "What does \"Sound Check\" do?",
              htmlOutput("sound_check_documentation")
            ),
            accordion_panel(
              "What does \"Exclude Files\" do?",
              htmlOutput("exclude_files_documentation")
            ),
            accordion_panel(
              "What does \"Restructure\" for Rumbles do?",
              htmlOutput("restructure_documentation")
            ),
            accordion_panel(
              "What does \"Restructure\" for Gunshots do?",
              htmlOutput("gun_restructure_documentation")
            ),
            accordion_panel(
              "What does \"Restructure\" for General do?",
              htmlOutput("general_restructure_documentation")
            ),
            accordion_panel(
              "What does \"Data Summaries\" do?",
              htmlOutput("summary_documentation")
            ),
            accordion_panel(
              "What does \"Results!\" for Plots do?",
              h2("Plot Results"),
              h3("Description"),
              p("This page displays the plots generated by the Data Summaries function.
                  First, select the folder containing your Data Summaries output, and load in the data.
                  Next, use the options on the sidebar to view and download the plots."),
              h3("Inputs"),
              tags$ul(
                tags$li("Folder containing the .rds file generated by the Data Summaries function.")
              ),
              h3("Outputs"),
              tags$ul(
                tags$li("Plots in the .rds file generated by the Data Summaries function.
                          (See \"What does \'Data Summaries\' do?\" for more details.)")
              ),
              h3("Author(s)"),
              p("Jidapa Janpathompong")
            ),
            accordion_panel(
              "What does \"Results!\" for Map do?",
              h2("Map Results"),
              h3("Description"),
              p("This page displays maps using data generated by the Data Summaries function.
                  First, select the folder containing your Data Summaries output, and load in the data.
                  Next, use the options on the sidebar to view and download the maps."),
              h3("Inputs"),
              tags$ul(
                tags$li("Folder containing the data tables generated by the Data Summaries function."),
                tags$li("A .txt file listing sites in data.")
              ),
              h3("Outputs"),
              tags$ul(
                tags$li(
                  "Maps using data generated by the Data Summaries function.
                    Specifically uses:",
                  tags$ul(
                    tags$li("*Rumbles_Site_Weekly_Summaries.txt"),
                    tags$li("*Rumbles_ZeroDays_soundExcluded_3randDaysOnly_MonthlyMean_Site.txt")
                  )
                )
              ),
              h3("Author(s)"),
              p("Jidapa Janpathompong")
            )
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12,12), md = c(10,12), lg = c(10,12))
        )
      )
    )
  )

  #_______________________________________________________________________________
  #### DEFINE SERVER LOGIC ####

  server <- function(input, output) {
    #______________________________________
    ##### header #####
    #render logo
    output$logo <- renderImage({
      list(src = "inst/elp_logo.png",
           contentType = "image/png",
           width = 100,
           height = 80)
    }, deleteFile = FALSE)

    #initialize/set variables for translation buttons
    output$subtitle <- renderText({subtitle_eng})
    output$tab_about <- renderText({tab_about_eng})
    output$tab_1 <- renderText({tab_1_eng})
    output$tab_2 <- renderText({tab_2_eng})
    output$tab_3 <- renderText({tab_3_eng})
    output$tab_4 <- renderText({tab_4_eng})
    output$tab_5 <- renderText({tab_5_eng})
    output$tab_6 <- renderText({tab_6_eng})
    output$tab_help <- renderText({tab_help_eng})

    output$header_welcome <- renderUI({header_welcome_eng})
    output$text_welcome <- renderUI({text_welcome_eng})
    output$header_contributers <- renderUI({header_contributers_eng})
    output$text_contributers <- renderUI({text_contributers_eng})

    output$card_1_1 <- renderText({card_1_1_eng})
    output$card_1_1_dep_name <- renderText({card_1_1_dep_name_eng})

    output$parent_dir_recieved <- renderText({card_1_1_dir_recieved_eng()})

    translate_val <- reactiveVal(0)
    unclicked_color <- "background-color: white; color: #404040"
    clicked_color <- "background-color: #007bc2; color: white"

    #BUTTON: translate to English
    #render button
    output$eng_ui <- renderUI({
      if(translate_val() == 1){
        actionButton("eng_button", label = "English", style = unclicked_color)
      } else{
        actionButton("eng_button", label = "English", style = clicked_color)
      }
    })
    #make button function
    observeEvent(input$eng_button, {
      translate_val(0)
      output$subtitle <- renderText({subtitle_eng})
      output$tab_about <- renderText({tab_about_eng})
      output$tab_1 <- renderText({tab_1_eng})
      output$tab_2 <- renderText({tab_2_eng})
      output$tab_3 <- renderText({tab_3_eng})
      output$tab_4 <- renderText({tab_4_eng})
      output$tab_5 <- renderText({tab_5_eng})
      output$tab_6 <- renderText({tab_6_eng})
      output$tab_help <- renderText({tab_help_eng})

      output$header_welcome <- renderUI({header_welcome_eng})
      output$text_welcome <- renderUI({text_welcome_eng})
      output$header_contributers <- renderUI({header_contributers_eng})
      output$text_contributers <- renderUI({text_contributers_eng})

      output$card_1_1 <- renderText({card_1_1_eng})
      output$card_1_1_dep_name <- renderText({card_1_1_dep_name_eng})

      output$parent_dir_recieved <- renderText({card_1_1_dir_recieved_eng()})
    })

    #BUTTON: translate to French
    #render button
    output$french_ui <- renderUI({
      if(translate_val() == 0){
        actionButton("french_button", label = "Français", style = unclicked_color)
      } else{
        actionButton("french_button", label = "Français", style = clicked_color)
      }
    })
    #make button function
    observeEvent(input$french_button, {
      translate_val(1)
      output$subtitle <- renderText({subtitle_french})
      output$tab_about <- renderText({tab_about_french})
      output$tab_1 <- renderText({tab_1_french})
      output$tab_2 <- renderText({tab_2_french})
      output$tab_3 <- renderText({tab_3_french})
      output$tab_4 <- renderText({tab_4_french})
      output$tab_5 <- renderText({tab_5_french})
      output$tab_6 <- renderText({tab_6_french})
      output$tab_help <- renderText({tab_help_french})

      output$header_welcome <- renderUI({header_welcome_french})
      output$text_welcome <- renderUI({text_welcome_french})
      output$header_contributers <- renderUI({header_contributers_french})
      output$text_contributers <- renderUI({text_contributers_french})

      output$card_1_1 <- renderText({card_1_1_french})
      output$card_1_1_dep_name <- renderText({card_1_1_dep_name_french})

      output$parent_dir_recieved <- renderText({card_1_1_dir_recieved_french()})
    })

    #______________________________________
    ##### ABOUT #####
    #subtitle
    subtitle_eng <- "To facilitate the use of the ElpR App"
    subtitle_french <- "Pour faciliter l'utilisation du package ElpR"

    #tab labels
    tab_about_eng <- "About"
    tab_1_eng <- "1. Basic Info"
    tab_2_eng <- "2. Sound Check"
    tab_3_eng <- "3. Exclude Files"
    tab_4_eng <- "4. Restructure"
    tab_5_eng <- "5. Data Summaries"
    tab_6_eng <- "6. Results!"
    tab_help_eng <- "HELP"

    tab_about_french <- "À Propos"
    tab_1_french <- "1. Informations de Base"
    tab_2_french <- "2. Vérification du Son"
    tab_3_french <- "3. Exclure des Fichiers"
    tab_4_french <- "4. Restructurer"
    tab_5_french <- "5. Résumés de Données"
    tab_6_french <- "6. Résultats!"
    tab_help_french <- "AIDE"

    #English body
    header_welcome_eng <- HTML("<h2>Welcome!</h2>")
    text_welcome_eng <- HTML(
      "<p>This app was designed to help facilitate the use of the ElpR package.
      The app was created using <a href = 'https://shiny.posit.co/'>Shiny</a>, a package used for building interactive web interfaces in R.
      <br/>
      <br/>The ElpR R package contains tools for processing and analyzing elephant rumble detector output.
      This package was originally made at the <a href = 'https://www.elephantlisteningproject.org/'>Elephant Listening Project</a> for the team and its collaborators.
      <br/>
      <br/>The pages listed on the sidebar reflect the chronological order that the elpR functions should be used in.
      Visit the HELP page to learn more about each function.</p>")
    header_contributers_eng <- HTML("<h2>Contributers</h2>")
    text_contributers_eng <- HTML(
      "<p><b>Bobbi Estabrook</b>: Author and Creator of the ElpR R package.
      <br/><b>Jidapa Janpathompong</b>: Creator of the ElpR app.</p>"
    )

    #French body
    header_welcome_french <- HTML("<h2>Bienvenue!</h2>")
    text_welcome_french <- HTML(
      "<p>Cette application a été conçue pour faciliter l'utilisation du package ElpR.
      Elle a été créée avec <a href = 'https://shiny.posit.co/'>Shiny</a>, un package permettant de développer des interfaces web interactives en R.
      <br/>
      <br/>Le package R ElpR contient des outils pour le traitement et l'analyse des données issues du détecteur de grondements d'éléphants.
      Ce package a été initialement développé par l'équipe du projet <a href = 'https://www.elephantlisteningproject.org/'>Elephant Listening Project</a> pour ses collaborateurs.
      <br/>
      <br/>Les pages listées dans la barre latérale correspondent à l'ordre chronologique d'utilisation des fonctions d'elpR.
      Consultez la page d'aide pour en savoir plus sur chaque fonction.</p>")
    header_contributers_french <- HTML("<h2>Contributeurs</h2>")
    text_contributers_french <- HTML(
      "<p><b>Bobbi Estabrook</b>: Auteure et créatrice du package R ElpR.
      <br/><b>Jidapa Janpathompong</b>: Créatrice de l’application ElpR.</p>"
    )

    #______________________________________
    ##### definitions #####

    #needed for retrieving folder input paths
    volumes = getVolumes()()

    #______________________________________
    ##### 1. Basic Info #####

    #English & French translations
    card_1_1_eng <- "Input Basic Information"
    card_1_1_dep_name_eng <- "Deployment name: "

    card_1_1_french <- "Saisir des informations de base"
    card_1_1_dep_name_french <- "Nom du déploiement: "

    #retrieve folder input path
    shinyDirChoose(input, "parent_dir_in", roots = volumes)
    card_1_1_dir_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$parent_dir_in))})
    card_1_1_dir_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$parent_dir_in))})

    #create list of user inputs
    inputs_basic_info <- reactive({
      list(
        sites = input$sites_in$datapath,
        parent_dir = parseDirPath(volumes, input$parent_dir_in)
      )
    })

    #______________________________________
    ##### 2. Sound Check #####

    #retrieve folder input path
    shinyDirChoose(input, "sound_path_in", roots = volumes)
    output$sound_path_recieved <- renderText({
      paste("Sound path recieved: ", parseDirPath(volumes, input$sound_path_in))
    })

    #create list of user inputs
    inputs_sound_check <- reactive({
      list(
        deployment_name = input$deployment_name_in,
        deployment_num = input$deployment_num_in,
        disk_ID = input$disk_ID_in,
        sound_path = parseDirPath(volumes, input$sound_path_in),
        fileDurationMin = input$fileDurationMin_in,
        sample_rate = input$sample_rate_in,
        sound_file_ext = input$sound_file_ext_in
      )
    })

    #BUTTON: check sound check info
    output$check_sound_check_info_output <- renderTable({
      #validate if all fields have inputs
      validate(
        need(input$sites_in, "Please input a Sites txt file!"),
        need(input$parent_dir_in, "Please input the \"files_for_elpR\" folder!"),
        need(input$sound_path_in, "Please input a folder with sound files!")
      )

      #create table
      x = unlist(inputs_basic_info())
      y = unlist(inputs_sound_check())
      z = c(input$sites_in$name, x[2], y)
      data.frame(
        Fields = c(
          "Sites file",
          "files_for_elpR",
          "Deployment name",
          "Deployment number",
          "Disk ID",
          "Sound path",
          "File duration",
          "Sample rate",
          "File extension"
        ),
        Inputs = z
      )
    }, hover = TRUE) |>
      bindEvent(input$check_sound_check_info_in)

    #BUTTON: run sound check
    observeEvent(input$run_sound_check_in, {
      result <- sound_check_function(
        x = parseDirPath(volumes, input$sound_path_in),
        parent_dir = parseDirPath(volumes, input$parent_dir_in),
        deployment_name = input$deployment_name_in,
        deployment_num = input$deployment_num_in,
        disk_ID = input$disk_ID_in,
        sites = input$sites_in$datapath,
        fileDurationMin = input$fileDurationMin_in,
        sample_rate = input$sample_rate_in,
        sound_file_ext = input$sound_file_ext_in
      )
      output$run_sound_check_output <- renderPrint({
        result
      })
    })

    #______________________________________
    ##### 3. Exclude Files #####

    #retrieve folder input path
    shinyDirChoose(input, "extra_sounds_in", roots = volumes)
    output$extra_sounds_recieved <- renderText({
      paste("Extra sounds path recieved: ", parseDirPath(volumes, input$extra_sounds_in))
    })

    #render more inputs based on radio button selections
    ##RADIO BUTTON: have_swift_files
    observeEvent(input$have_swift_files_in, {
      if(input$have_swift_files_in == "Yes"){
        output$merge_swift_files_output <- renderUI({
          radioButtons("merge_swift_files_in", label = "Do you want to merge the Swift files?",
                       choices = list("Yes", "No"),
                       selected = "No")
        })
      }
      else if(input$have_swift_files_in == "No"){
        output$merge_swift_files_output <- renderUI({
          NULL
        })
      }
    })

    #create list of user inputs
    inputs_exclude_files <- reactive({
      if(input$have_swift_files_in == "Yes"){
        merge_swift_files_display = input$merge_swift_files_in
      } else if(input$have_swift_files_in == "No"){
        merge_swift_files_display = "N/A"
      }
      list(
        extra_sounds_folder = parseDirPath(volumes, input$extra_sounds_in),
        have_SwiftFiles = input$have_swift_files_in,
        merge_swift_files = merge_swift_files_display
      )
    })

    #BUTTON: check exclude files info
    output$check_exclude_files_info_output <- renderTable({
      #validate if all fields have inputs
      validate(
        need(input$parent_dir_in, "Please input the \"files_for_elpR\" folder!"),
        need(input$sound_path_in, "Please input a folder with sound files!"),
        need(input$extra_sounds_in, "Please input a folder to output extra sounds!")
      )

      #create table
      x = unlist(inputs_sound_check())
      y = unlist(inputs_exclude_files())
      z = c(parseDirPath(volumes, input$parent_dir_in), x, y)
      data.frame(
        Fields = c(
          "files_for_elpR",
          "Deployment name",
          "Deployment number",
          "Disk ID",
          "Sound path",
          "File duration",
          "Sample rate",
          "File extension",
          "Extra sounds path",
          "Have Swift files",
          "Merge Swift files"
        ),
        Inputs = z
      )
    }, hover = TRUE) |>
      bindEvent(input$check_exclude_files_info_in)

    #BUTTON: run exclude files
    observeEvent(input$run_exclude_files_in, {
      if(input$have_swift_files_in == "Yes"){
        have_SwiftFiles_value = "y"
        if(input$merge_swift_files_in == "Yes"){
          merge_swift_files_value = "y"
        } else if(input$merge_swift_files_in == "No"){
          merge_swift_files_value = "n"
        }
      } else if(input$have_swift_files_in == "No"){
        have_SwiftFiles_value = "n"
        merge_swift_files_value = "n"
      }
      result <- sound_exclude_function(
        sound_path = parseDirPath(volumes, input$sound_path_in),
        parent_dir = parseDirPath(volumes, input$parent_dir_in),
        extra_sounds_folder = parseDirPath(volumes, input$extra_sounds_in),
        deployment_name = input$deployment_name_in,
        deployment_num = input$deployment_num_in,
        disk_ID = input$disk_ID_in,
        sound_file_ext = input$sound_file_ext_in,
        have_SwiftFiles = have_SwiftFiles_value,
        merge_swift_files = merge_swift_files_value
      )
      output$run_exclude_files_output <- renderPrint({
        result
      })
    })

    #______________________________________
    ##### 4. Restructure #####

    ###### > rumbles ######

    #create list of user inputs
    inputs_restructure <- reactive({
      list(
        sample_rate = input$sample_rate_chr_in,
        three_rand_days = input$three_rand_days_in,
        min_23hrs = input$min_23hours_in,
        score_column_name = input$score_column_name_in,
        Detector = input$detector_in,
        Detector_ScoreThreshold = input$detector_score_in,
        Filter_ScoreThreshold = input$filter_score_in
      )
    })

    #BUTTON: check rumble restructure info
    output$check_restructure_info_output <- renderTable({
      #validate if all fields have inputs
      validate(
        need(input$parent_dir_in, "Please input the \"files_for_elpR\" folder!"),
        need(input$sites_in, "Please input a Sites txt file!")
      )

      #create table
      x = unlist(inputs_sound_check())
      y = unlist(inputs_restructure())
      z = c(parseDirPath(volumes, input$parent_dir_in), x[1:3], input$sites_in$name, y)
      data.frame(
        Fields = c("files_for_elpR",
                   "Deployment name",
                   "Deployment number",
                   "Disk ID",
                   "Sites file",
                   "Sample rate",
                   "Three random days per week",
                   "Minimum 23 hours per day",
                   "Score column name",
                   "Detector",
                   "Detector score",
                   "Filter score"),
        Inputs = z
      )
    }, hover = TRUE) |>
      bindEvent(input$check_restructure_info_in)

    #BUTTON: run rumble restructure
    observeEvent(input$run_restructure_in, {
      if(input$three_rand_days_in == "Yes"){
        three_rand_days_value = "y"
      } else if(input$three_rand_days_in == "No"){
        three_rand_days_value = "n"
      }
      if(input$min_23hours_in == "Yes"){
        min_23hours_value = "y"
      } else if(input$min_23hours_in == "No"){
        min_23hours_value = "n"
      }
      if(input$detector_in == "HoriHarm"){
        detector_in_value = "HHv6"
      } else if(input$detector_in == "FruitPunchAI"){
        detector_in_value = "FPv1"
      } else if(input$detector_in == "Stanford Detector"){
        detector_in_value = "SDv1"
      }

      result <- restructure_rumble_function(
        parent_dir = parseDirPath(volumes, input$parent_dir_in),
        deployment_name = input$deployment_name_in,
        deployment_num = input$deployment_num_in,
        disk_ID = input$disk_ID_in,
        sites = input$sites_in$datapath,
        sample_rate = input$sample_rate_chr_in,
        three_rand_days = three_rand_days_value,
        min_23hrs = min_23hours_value,
        score_column_name = input$score_column_name_in,
        Detector = detector_in_value,
        Detector_ScoreThreshold = input$detector_score_in,
        Filter_ScoreThreshold = input$filter_score_in
      )
      rumble_count <- c(sum(as.integer(result[,3]), na.rm = TRUE),
                        sum(as.integer(result[,4]), na.rm = TRUE),
                        sum(as.integer(result[,5]), na.rm = TRUE))
      rumble_type <- c("Rumbles > 0.2",
                       "Rumbles > 0.4",
                       "Random Rumbles > 0.4")
      output$run_restructure_output <- renderTable({
        data.frame(
          Detection = rumble_type,
          Count = rumble_count
        )
      }, hover = TRUE)
    })

    ###### > gunshots ######

    #create list of user inputs
    inputs_gun_restructure <- reactive({
      list(
        sample_rate = input$sample_rate_chr_gun_in,
        Detector = input$detector_gun_in,
        Detector_ScoreThreshold = input$detector_score_gun_in,
        Filter_ScoreThreshold = input$filter_score_gun_in
      )
    })

    #BUTTON: check gun restructure info
    output$check_gun_restructure_info_output <- renderTable({
      #validate if all fields have inputs
      validate(
        need(input$parent_dir_in, "Please input the \"files_for_elpR\" folder!"),
        need(input$sites_in, "Please input a Sites txt file!")
      )

      #create table
      x = unlist(inputs_sound_check())
      y = unlist(inputs_gun_restructure())
      z = c(parseDirPath(volumes, input$parent_dir_in), x[1:3], input$sites_in$name, y)
      data.frame(
        Fields = c("files_for_elpR",
                   "Deployment name",
                   "Deployment number",
                   "Disk ID",
                   "Sites file",
                   "Sample rate",
                   "Detector",
                   "Detector score",
                   "Filter score"),
        Inputs = z
      )
    }, hover = TRUE) |>
      bindEvent(input$check_gun_restructure_info_in)

    #BUTTON: run gun restructure
    observeEvent(input$run_gun_restructure_in, {
      result <- restructure_gunshot_function(
        parent_dir = parseDirPath(volumes, input$parent_dir_in),
        deployment_name = input$deployment_name_in,
        deployment_num = input$deployment_num_in,
        disk_ID = input$disk_ID_in,
        sites = input$sites_in$datapath,
        sample_rate = input$sample_rate_chr_gun_in,
        Detector = input$detector_gun_in,
        Detector_ScoreThreshold = input$detector_score_gun_in,
        Filter_ScoreThreshold = input$filter_score_gun_in
      )
      gunshot_count <- c(sum(as.integer(result[,3]), na.rm = TRUE))
      gunshot_type <- c("Gunshots")
      output$run_gun_restructure_output <- renderTable({
        data.frame(
          Detection = gunshot_type,
          Count = gunshot_count
        )
      }, hover = TRUE)
    })

    ###### > general ######

    #retrieve folder input path
    shinyDirChoose(input, "general_merge_in", roots = volumes)
    output$general_merge_recieved <- renderText({
      paste("Folder path recieved: ", parseDirPath(volumes, input$general_merge_in))
    })

    #create list of user inputs
    inputs_general_restructure <- reactive({
      list(
        path = parseDirPath(volumes, input$general_merge_in),
        recursive = input$recursive_in
      )
    })

    #BUTTON: check general restructure info
    output$check_general_restructure_info_output <- renderTable({
      #validate if all fields have inputs
      validate(
        need(input$general_merge_in, "Please input a path to selection tables!")
      )

      #create table
      x = unlist(inputs_general_restructure())
      data.frame(
        Fields = c("Folder path",
                   "Table organization"),
        Inputs = x
      )
    }, hover = TRUE) |>
      bindEvent(input$check_general_restructure_info_in)

    #BUTTON: run general restructure
    observeEvent(input$run_general_restructure_in, {
      if(input$recursive_in == "In one folder"){
        recursive_in_value = FALSE
      } else if(input$recursive_in == "In multiple subfolders"){
        recursive_in_value = TRUE
      }

      result <- restructure_general_function(
        path = parseDirPath(volumes, input$general_merge_in),
        recursive = recursive_in_value
      )
      output$run_general_restructure_output <- renderPrint({
        result
      })
    })

    #______________________________________
    ##### 5. Data Summaries #####

    #retrieve folder input paths
    shinyDirChoose(input, "summary_folder_in", roots = volumes)
    output$summary_folder_recieved <- renderText({
      paste("Summary folder recieved: ", parseDirPath(volumes, input$summary_folder_in))
    })
    shinyDirChoose(input, "selection_tables_folder_in", roots = volumes)
    output$selection_tables_folder_recieved <- renderText({
      paste("Selection tables folder recieved: ", parseDirPath(volumes, input$selection_tables_folder_in))
    })
    shinyDirChoose(input, "zero_selection_tables_folder_in", roots = volumes)
    output$zero_selection_tables_folder_recieved <- renderText({
      paste("Zero-day selection tables folder recieved: ", parseDirPath(volumes, input$zero_selection_tables_folder_in))
    })

    #render more inputs based on radio button selections
    ##RADIO BUTTON: sound_check_include
    output$sound_check_include_output <- renderUI({
      if(input$sound_check_include_in == "Yes"){
        shinyDirButton(id = "sound_check_folder_in",
                       label = "Choose folder containing sound check files",
                       title = "Choose a folder")
      }
      else if(input$sound_check_include_in == "No"){
        output$sound_check_folder_recieved <- renderText({
          NULL
        })
      }
    })
    observeEvent(input$sound_check_folder_in, {
      shinyDirChoose(input, "sound_check_folder_in", roots = volumes)
      output$sound_check_folder_recieved <- renderText({
        paste("Sound check folder recieved: ", parseDirPath(volumes, input$sound_check_folder_in))
      })
    })

    ##RADIO BUTTON: use_only_sites_provided
    output$use_only_sites_provided_output <- renderUI({
      if(input$use_only_sites_provided_in == "Yes"){
        fileInput("sites_lat_long_in", label = "Sites txt file with latitude and longitude:")
      }
    })

    #create list of user inputs
    inputs_summary <- reactive({
      if(input$sound_check_include_in == "Yes"){
        sound_check_folder_value = parseDirPath(volumes, input$sound_check_folder_in)
      } else if(input$sound_check_include_in == "No"){
        sound_check_folder_value = "N/A"
      }
      if(input$use_only_sites_provided_in == "Yes"){
        sites_lat_long_value = input$sites_lat_long_in$name
      } else if(input$use_only_sites_provided_in == "No"){
        sites_lat_long_value = "N/A"
      }

      list(
        project_name = input$project_name_in,
        deployment_num = input$deployment_nums_in,
        detector_name = input$summary_detector_in,
        output = parseDirPath(volumes, input$summary_folder_in),
        ele_tables = parseDirPath(volumes, input$selection_tables_folder_in),
        zero_txt = parseDirPath(volumes, input$zero_selection_tables_folder_in),
        sound_check_include = input$sound_check_include_in,
        sound_checks = sound_check_folder_value,
        use_only_sites_provided = input$use_only_sites_provided_in,
        site_lat_long = sites_lat_long_value,
        rand_dates_needed = input$rand_dates_needed_in,
        ele_bad_sound_remove = input$ele_bad_sound_remove_in
      )
    })

    #BUTTON: check summary info
    output$check_summary_info_output <- renderTable({
      #validate if all fields have inputs
      validate(
        need(input$parent_dir_in, "Please input the \"files_for_elpR\" folder!"),
        need(input$summary_folder_in, "Please input a folder to output data summaries!"),
        need(input$selection_tables_folder_in, "Please input a folder containing selection tables!"),
        need(input$zero_selection_tables_folder_in, "Please input a folder containing zero-day selection tables!"),
        if(input$sound_check_include_in == "Yes"){
          need(input$sound_check_folder_in, "Do you want to include a sound check file?")
        },
        if(input$use_only_sites_provided_in == "Yes"){
          need(input$sites_lat_long_in, "Do you want to include a sites file?")
        }
      )

      #create table
      x = unlist(inputs_summary())
      y = c(parseDirPath(volumes, input$parent_dir_in), x)
      data.frame(
        Fields = c("files_for_elpR",
                   "Project name",
                   "Deployment number(s)",
                   "Detector used",
                   "Folder to output summaries",
                   "Folder containing selection tables",
                   "Folder containing zero-day selection tables",
                   "Use sound check files",
                   "Folder containing sound check files",
                   "Use sites file",
                   "Sites file with latitude and longitude",
                   "Random dates needed",
                   "Exclude sounds <23 hours"),
        Inputs = y
      )
    }, hover = TRUE) |>
      bindEvent(input$check_summary_info_in)

    #BUTTON: run summary
    observeEvent(input$run_summary_in, {
      if(input$sound_check_include_in == "Yes"){
        sound_check_include_value = "y"
        sound_check_folder_value = parseDirPath(volumes, input$sound_check_folder_in)
      } else if(input$sound_check_include_in == "No"){
        sound_check_include_value = "n"
        sound_check_folder_value = NULL
      }
      if(input$use_only_sites_provided_in == "Yes"){
        use_only_sites_provided_value = "y"
        sites_lat_long_value = read.table(input$sites_lat_long_in$datapath, header = T, sep ="\t", check.names=FALSE,quote = "\"")
      } else if(input$use_only_sites_provided_in == "No"){
        use_only_sites_provided_value = "n"
        sites_lat_long_value = NULL
      }
      if(input$rand_dates_needed_in == "Yes"){
        rand_dates_needed_value = "y"
      } else if(input$rand_dates_needed_in == "No"){
        rand_dates_needed_value = "n"
      }
      if(input$ele_bad_sound_remove_in == "Yes"){
        ele_bad_sound_remove_value = "y"
      } else if(input$ele_bad_sound_remove_in == "No"){
        ele_bad_sound_remove_value = "n"
      }

      result <- data_summaries_function(
        parent_dir = parseDirPath(volumes, input$parent_dir_in),
        project_name = input$project_name_in,
        deployment_num = input$deployment_nums_in,
        detector_name = input$summary_detector_in,
        output = parseDirPath(volumes, input$summary_folder_in),
        ele_tables = parseDirPath(volumes, input$selection_tables_folder_in),
        zero_txt = parseDirPath(volumes, input$zero_selection_tables_folder_in),
        sound_check_include = sound_check_include_value,
        sound_checks = sound_check_folder_value,
        use_only_sites_provided = use_only_sites_provided_value,
        site_lat_long = sites_lat_long_value,
        rand_dates_needed = rand_dates_needed_value,
        ele_bad_sound_remove = ele_bad_sound_remove_value
      )
      output$run_summary_output <- renderPrint({
        result
      })
    })

    #______________________________________
    ##### 6. Results! #####

    ###### > plots ######

    #retrieve folder input path
    shinyDirChoose(input, "saved_plots_folder_in", roots = volumes)
    output$saved_plots_folder_recieved <- renderText({
      paste("Saved plots folder recieved: ", parseDirPath(volumes, input$saved_plots_folder_in))
    })

    #load plots from data summaries
    saved_plots <- reactiveVal({})
    shinyjs::hide("load_plots_message")
    observeEvent(input$load_plots, {
      #validate if plots have been loaded
      #although nothing actually shows in the UI :(
      validate(
        need(input$saved_plots_folder_in, "Please input a path to your .Rds file!")
      )

      #trigger loading message while plots are loading
      shinyjs::show("load_plots_message")

      #load plots into environment from user inputted file path
      saved_plots(
        readRDS(paste(parseDirPath(volumes, input$saved_plots_folder_in), "/", input$saved_plots_file_in, sep = ""))
      )

      #display drop-down menu for choosing plots
      removeUI(selector = "#plot_input_div", multiple = TRUE, immediate = TRUE)
      insertUI(
        selector = "#placeholder",
        where = "beforeBegin",
        ui = div(
          id = "plot_input_div",
          selectInput(
            "plot_input",
            label = "Select plot to view:",
            choices = list("Total Rumbles",
                           "Night vs Day Rumbles",
                           "Rumbles per Hour",
                           "Rumbles per Day",
                           "Rumbles per Week",
                           "Rumbles per Month"),
            selected = NULL)
        )
      )

      #remove loading message when plots finish loading
      shinyjs::hide("load_plots_message")
    })

    #customize facet wrap options
    observeEvent(input$plot_input, {
      #remove existing drop-down menus before adding more
      removeUI(selector = "#facet_wrap_input_div", multiple = TRUE, immediate = TRUE)
      removeUI(selector = "#night_annot_input_div", multiple = TRUE, immediate = TRUE)
      removeUI(selector = "#overlay_input_div", multiple = TRUE, immediate = TRUE)

      #display drop-down menu
      if(input$plot_input == "Rumbles per Hour" |
         input$plot_input == "Rumbles per Day" |
         input$plot_input == "Rumbles per Week" |
         input$plot_input == "Rumbles per Month"){
        insertUI(
          selector = "#placeholder",
          where = "beforeBegin",
          ui = div(
            id = "facet_wrap_input_div",
            selectInput(
              "facet_wrap_input",
              label = "Categorize by...",
              choices = list("Default", "Site", "Strata", "Vegetation Class"),
              selected = "Default"
            )
          )
        )
      }
    })

    #customize night annotation options (for weekly rumbles by strata)
    observeEvent(input$facet_wrap_input, {
      #remove existing drop-down menus before adding more
      removeUI(selector = "#night_annot_input_div", multiple = TRUE, immediate = TRUE)
      if (input$facet_wrap_input != "Strata") {
        shinyjs::runjs("Shiny.setInputValue('night_annot_input', null);")
      }

      #display drop-down menu
      if(input$plot_input == "Rumbles per Week" &
         !is.null(input$facet_wrap_input) &
         input$facet_wrap_input == "Strata"){
        insertUI(
          selector = "#placeholder",
          where = "beforeBegin",
          ui = div(
            id = "night_annot_input_div",
            selectInput(
              "night_annot_input",
              label = "Annotate for...",
              choices = list("Default", "Night Rumbles"),
              selected = "Default"
            )
          )
        )
      }
    })

    #customize regression overlay options (for monthly rumbles by strata)
    observeEvent(input$facet_wrap_input, {
      #remove existing drop-down menus before adding more
      removeUI(selector = "#overlay_input_div", multiple = TRUE, immediate = TRUE)
      if (input$facet_wrap_input != "Strata") {
        shinyjs::runjs("Shiny.setInputValue('overlay_input', null);")
      }

      #display drop-down menu
      if(input$plot_input == "Rumbles per Month" &
         !is.null(input$facet_wrap_input) &
         input$facet_wrap_input == "Strata"){
        insertUI(
          selector = "#placeholder",
          where = "beforeBegin",
          ui = div(
            id = "overlay_input_div",
            selectInput(
              "overlay_input",
              label = "Display...",
              choices = list("Default", "Overlay"),
              selected = "Default"
            )
          )
        )
      }
    })

    #select plots based on drop-down menu selections
    #helper function to check if plot exists in user's .Rds file
    check_plot_existence <- function(plot_var, message = "No plot available for this option."){
      if(!is.null(plot_var)){
        plot_var
      } else{
        validate(
          need(!is.null(plot_var), message)
        )
      }
    }
    display_this_plot <- reactive({
      #choose plot to display based on user's drop-down selections
      if(input$plot_input == "Total Rumbles"){
        check_plot_existence(saved_plots()$total_plot)
      } else if(input$plot_input == "Night vs Day Rumbles"){
        check_plot_existence(saved_plots()$night_day_plot)
      } else if(input$plot_input == "Rumbles per Hour"){
        if(input$facet_wrap_input == "Default"){
          check_plot_existence(saved_plots()$hourly_plot)
        } else if(input$facet_wrap_input == "Site"){
          check_plot_existence(saved_plots()$hourly_plot)
          saved_plots()$hourly_plot + facet_wrap(~Site)
        } else if(input$facet_wrap_input == "Strata"){
          check_plot_existence(saved_plots()$hourly_plot)
          temp <- saved_plots()$hourly_plot@data$Strata
          if(!is.null(temp)){
            saved_plots()$hourly_plot + facet_wrap(~Strata)
          } else{
            NULL
          }
        } else if(input$facet_wrap_input == "Vegetation Class"){
          check_plot_existence(saved_plots()$hourly_plot)
          temp <- saved_plots()$hourly_plot@data$`Vegetation Class`
          if(!is.null(temp)){
            saved_plots()$hourly_plot + facet_wrap(~`Vegetation Class`)
          } else{
            NULL
          }
        }
      } else if(input$plot_input == "Rumbles per Day"){
        if(input$facet_wrap_input == "Default"){
          check_plot_existence(saved_plots()$daily_plot_site)
        } else if(input$facet_wrap_input == "Site"){
          check_plot_existence(saved_plots()$daily_plot_site)
        } else if(input$facet_wrap_input == "Strata"){
          check_plot_existence(saved_plots()$daily_plot_strata)
        } else if(input$facet_wrap_input == "Vegetation Class"){
          check_plot_existence(saved_plots()$daily_plot_veg)
        }
      } else if(input$plot_input == "Rumbles per Week"){
        if(input$facet_wrap_input == "Default"){
          check_plot_existence(saved_plots()$weekly_plot_site)
        } else if(input$facet_wrap_input == "Site"){
          check_plot_existence(saved_plots()$weekly_plot_site)
        } else if(input$facet_wrap_input == "Strata"){
          if(input$night_annot_input == "Default"){
            check_plot_existence(saved_plots()$weekly_plot_strata)
          } else if(input$night_annot_input == "Night Rumbles"){
            check_plot_existence(saved_plots()$weekly_plot_strata)

            saved_plots()$weekly_plot_strata +
              geom_line(aes(x = WeekDate, y = meanRumblesNight), color = "red", linetype = "dotted") +
              geom_point(aes(x = WeekDate, y = meanRumblesNight), color = "red") +
              scale_y_continuous(
                breaks = seq(0, max(saved_plots()$weekly_plot_strata$data$MeanDailyRumbles + saved_plots()$weekly_plot_strata$data$seDailyRumbles, na.rm = TRUE)+5,  by = 10),
                sec.axis = sec_axis(
                  ~.,
                  name = "Mean Night Rumbles per Week (±SE)",
                  breaks = seq(0, max(saved_plots()$weekly_plot_strata$data$MeanDailyRumbles + saved_plots()$weekly_plot_strata$data$seDailyRumbles, na.rm = TRUE)+5,  by = 10)
                )) +
              theme(panel.grid.major = element_line(colour = "gray90"),
                    panel.grid.minor = element_blank(),
                    axis.title.y.right = element_text(color = "red"),
                    axis.text.y.right = element_text(color = "red"))
          }
        } else if(input$facet_wrap_input == "Vegetation Class"){
          check_plot_existence(saved_plots()$weekly_plot_veg)
        }
      } else if(input$plot_input == "Rumbles per Month"){
        if(input$facet_wrap_input == "Default"){
          check_plot_existence(saved_plots()$monthly_plot)
        } else if(input$facet_wrap_input == "Site"){
          check_plot_existence(saved_plots()$monthly_plot_site)
        } else if(input$facet_wrap_input == "Strata"){
          if(input$overlay_input == "Default"){
            check_plot_existence(saved_plots()$monthly_plot_strata)
          } else if(input$overlay_input == "Overlay"){
            check_plot_existence(saved_plots()$monthly_plot_strata_overlay)
          }
        } else if(input$facet_wrap_input == "Vegetation Class"){
          NULL
        }
      }
    })

    #display plots!
    output$plots_output <- renderPlot({
      #validate if plots have been loaded
      validate(
        need(input$plot_input, "Load your plots to view them!"),
      )

      #validate if plot is not NULL
      validate(
        need(!is.null(display_this_plot()), "No plot available for this option.")
      )

      #display plot
      display_this_plot()
    })

    #name plots for downloading
    plot_file_name <- reactive({
      if(!is.null(input$night_annot_input)){
        paste(
          gsub(" ", "_", tolower(input$plot_input)), "_",
          gsub(" ", "_", tolower(input$facet_wrap_input)), "_",
          gsub(" ", "_", tolower(input$night_annot_input)), "_plot",
          sep = "")
      } else if(!is.null(input$overlay_input)){
        paste(
          gsub(" ", "_", tolower(input$plot_input)), "_",
          gsub(" ", "_", tolower(input$facet_wrap_input)), "_",
          gsub(" ", "_", tolower(input$overlay_input)), "_plot",
          sep = "")
      } else if(!is.null(input$facet_wrap_input)){
        paste(
          gsub(" ", "_", tolower(input$plot_input)), "_",
          gsub(" ", "_", tolower(input$facet_wrap_input)), "_plot",
          sep = "")
      } else{
        paste(
          gsub(" ", "_", tolower(input$plot_input)), "_plot",
          sep = "")
      }
    })

    #download plots!
    output$download_plots <- downloadHandler(
      filename = function(){
        paste(plot_file_name(), input$download_ext_in, sep = "")
      },
      content = function(file){
        ggsave(file, plot = display_this_plot(), width = 10, height = 8)
      }
    )

    ###### > maps ######

    #retrieve folder input path
    shinyDirChoose(input, "saved_tables_folder_in", roots = volumes)
    output$saved_tables_folder_recieved <- renderText({
      paste("Saved tables folder recieved: ", parseDirPath(volumes, input$saved_tables_folder_in))
    })

    #load plots from data summaries
    tables_for_mapping <- reactiveVal(list())
    shinyjs::hide("load_tables_message")
    observeEvent(input$load_tables, {
      #validate if tables have been loaded
      #although nothing actually shows in the UI :(
      validate(
        need(input$saved_tables_folder_in, "Please input a path to your summary tables!")
      )

      #trigger loading message while tables are loading
      shinyjs::show("load_tables_message")

      #load tables into environment from user inputted file path
      ele_weekly <- read_tsv(
        paste(
          parseDirPath(volumes, input$saved_tables_folder_in),
          "/",
          list.files(path = parseDirPath(volumes, input$saved_tables_folder_in), pattern = "Rumbles_Site_Weekly_Summaries.txt"),
          sep = ""),
        col_names = TRUE)
      ele_monthly_site <- read_tsv(
        paste(
          parseDirPath(volumes, input$saved_tables_folder_in),
          "/",
          list.files(path = parseDirPath(volumes, input$saved_tables_folder_in), pattern = "Rumbles_ZeroDays_soundExcluded_3randDaysOnly_MonthlyMean_Site.txt"),
          sep = ""),
        col_names = TRUE)
      site_lat_long_map <- read_tsv(
        input$site_lat_long_map$datapath,
        col_names = TRUE)

      #configure tables for mapping
      tables_for_mapping(NULL)
      temp_list <- tables_for_mapping()
      if("Vegetation Class" %in% names(ele_weekly) &&
         "Strata" %in% names(ele_weekly)){
        #avg daily rumbles for "Strata" & "Vegetation Class"
        temp_list <- c(
          temp_list,
          list(
            ele_site_avg =
              ele_weekly %>%
              group_by(Site) %>%
              summarize(
                "Latitude" = round(first(Latitude),5),
                "Longitude" = round(first(Longitude),5),
                "Avg Rumbles" = sum(sumRumbles)/sum(n),
                "Strata" = first(Strata),
                "Vegetation Class" = first(`Vegetation Class`)
              )
          )
        )
      } else if("Strata" %in% names(ele_weekly)){
        #avg daily rumbles for "Strata"
        temp_list <- c(
          temp_list,
          list(
            ele_site_avg =
              ele_weekly %>%
              group_by(Site) %>%
              summarize(
                "Latitude" = round(first(Latitude),5),
                "Longitude" = round(first(Longitude),5),
                "Avg Rumbles" = sum(sumRumbles)/sum(n),
                "Strata" = first(Strata)
              )
          )
        )
      } else if("Vegetation Class" %in% names(ele_weekly)){
        #avg daily rumbles for "Vegetation Class"
        temp_list <- c(
          temp_list,
          list(
            ele_site_avg =
              ele_weekly %>%
              group_by(Site) %>%
              summarize(
                "Latitude" = round(first(Latitude),5),
                "Longitude" = round(first(Longitude),5),
                "Avg Rumbles" = sum(sumRumbles)/sum(n),
                "Vegetation Class" = first(`Vegetation Class`)
              )
          )
        )
      } else{
        #avg daily rumbles with NO "Strata" NOR "Vegetation Class"
        temp_list <- c(
          temp_list,
          list(
            ele_site_avg =
              ele_weekly %>%
              group_by(Site) %>%
              summarize(
                "Latitude" = round(first(Latitude),5),
                "Longitude" = round(first(Longitude),5),
                "Avg Rumbles" = sum(sumRumbles)/sum(n)
              )
          )
        )
      }

      temp_list <- c(
        temp_list,
        list(
          #avg daily rumbles for "Year"
          ele_year_avg =
            ele_monthly_site %>%
            group_by(Site, Year) %>%
            summarize(
              "Avg Rumbles" = sum(sumRumbles)/sum(n)
            ) %>%
            merge(site_lat_long_map, by = "Site"),

          #avg daily rumbles for "Month"
          ele_month_avg =
            ele_monthly_site %>%
            group_by(Site, Month) %>%
            summarize(
              "Avg Rumbles" = sum(sumRumbles)/sum(n)
            ) %>%
            merge(site_lat_long_map, by = "Site")
        )
      )
      tables_for_mapping(temp_list)

      #display drop-down menu for choosing maps
      removeUI(selector = "#map_input_div", multiple = TRUE, immediate = TRUE)
      insertUI(
        selector = "#placeholder2",
        where = "beforeBegin",
        ui = div(
          id = "map_input_div",
          selectInput(
            "map_input",
            label = "Map average daily rumbles by:",
            choices = list(
              "Default",
              "Strata",
              "Vegetation Class",
              "Year",
              "Month"
            ),
            selected = NULL)
        )
      )

      #remove loading message when tables finish loading
      shinyjs::hide("load_tables_message")
    })

    #customize year & month slider options
    observeEvent(input$map_input, {
      #remove existing drop-down menus before adding more
      removeUI(selector = "#year_slider_input_div", multiple = TRUE, immediate = TRUE)
      if (input$map_input != "Year") {
        shinyjs::runjs("Shiny.setInputValue('year_slider_input', null);")
      }
      removeUI(selector = "#month_slider_input_div", multiple = TRUE, immediate = TRUE)
      if (input$map_input != "Month") {
        shinyjs::runjs("Shiny.setInputValue('month_slider_input', null);")
      }

      #display drop-down menus
      if(input$map_input == "Year"){

        req(tables_for_mapping())
        req(tables_for_mapping()$ele_year_avg)

        ele_year_avg <- tables_for_mapping()$ele_year_avg
        years_vector <- sort(unique(ele_year_avg$Year))

        insertUI(
          selector = "#placeholder2",
          where = "beforeBegin",
          ui = div(
            id = "year_slider_input_div",
            sliderInput(
              "year_slider_input",
              label = "View year:",
              min = min(years_vector),
              max = max(years_vector),
              value = min(years_vector),
              step = 1
            )
          )
        )
      }
      if(input$map_input == "Month"){

        req(tables_for_mapping())
        req(tables_for_mapping()$ele_month_avg)

        ele_month_avg <- tables_for_mapping()$ele_month_avg
        months_vector <- sort(unique(ele_month_avg$Month))

        insertUI(
          selector = "#placeholder2",
          where = "beforeBegin",
          ui = div(
            id = "month_slider_input_div",
            sliderInput(
              "month_slider_input",
              label = "View month:",
              min = min(months_vector),
              max = max(months_vector),
              value = min(months_vector),
              step = 1
            )
          )
        )
      }
    })

    #select maps based on drop-down menu selections
    display_this_map <- reactive({
      #create default map with ele_site_avg
      ele_site_avg <- tables_for_mapping()$ele_site_avg
      m <- leaflet(ele_site_avg) %>%

        #Add default base map
        addProviderTiles(provider = providers$Esri.WorldTopoMap,
                         group = "map") %>%

        #Set center point and zoom level
        setView(lng = mean(ele_site_avg$Longitude),
                lat = mean(ele_site_avg$Latitude),
                zoom = 10)

      #choose map to display based on user's drop-down selections
      if(input$map_input == "Default"){
        ele_site_avg <- tables_for_mapping()$ele_site_avg
        m <- m %>%

          #Add markers
          addCircleMarkers(
            lng = ~Longitude,
            lat = ~Latitude,
            radius = ~sqrt(`Avg Rumbles`) * 10,  # Adjust the multiplier to change circle sizes
            popup = ~paste("Site:", Site, "<br>Avg Rumbles:", `Avg Rumbles`),
            fillOpacity = 0.7
          )

        #View map
        m
      } else if(input$map_input == "Strata"){
        ele_site_avg <- tables_for_mapping()$ele_site_avg
        if(!is.null(ele_site_avg$Strata)){
          color_by_strata <- colorFactor(palette = "viridis",
                                         domain = unique(ele_site_avg$Strata))
          m <- m %>%

            #Add legend
            addLegend(
              position = "bottomright",
              pal = color_by_strata,
              values = ~Strata,
              title = "Strata"
            ) %>%

            #Add layers control
            addLayersControl(overlayGroups = unique(ele_site_avg$Strata),
                             position = "topleft")

          #Add markers
          for(strata in unique(ele_site_avg$Strata)){
            subset_data <- ele_site_avg %>%
              filter(Strata == strata)

            m <- m %>%
              addCircleMarkers(
                data = subset_data,
                lng = ~Longitude,
                lat = ~Latitude,
                radius = ~sqrt(`Avg Rumbles`) * 10,  # Adjust the multiplier to change circle sizes
                popup = ~paste("Site:", Site, "<br>Avg Rumbles:", `Avg Rumbles`),
                color = ~color_by_strata(Strata),
                fillOpacity = 0.7,
                group = paste(strata)
              )
          }

          #View map
          m
        } else{
          NULL
        }
      } else if(input$map_input == "Vegetation Class"){
        ele_site_avg <- tables_for_mapping()$ele_site_avg
        if(!is.null(ele_site_avg$`Vegetation Class`)){
          color_by_veg <- colorFactor(palette = "plasma",
                                      domain = unique(ele_site_avg$`Vegetation Class`))
          m <- m %>%

            #Add legend
            addLegend(
              position = "bottomright",
              pal = color_by_veg,
              values = ~`Vegetation Class`,
              title = "Vegetation Class"
            ) %>%

            #Add layers control
            addLayersControl(overlayGroups = unique(ele_site_avg$`Vegetation Class`),
                             position = "topleft")

          #Add markers
          for(veg in unique(ele_site_avg$`Vegetation Class`)){
            subset_data <- ele_site_avg %>%
              filter(`Vegetation Class` == veg)

            m <- m %>%
              addCircleMarkers(
                data = subset_data,
                lng = ~Longitude,
                lat = ~Latitude,
                radius = ~sqrt(`Avg Rumbles`) * 10,  # Adjust the multiplier to change circle sizes
                popup = ~paste("Site:", Site, "<br>Avg Rumbles:", `Avg Rumbles`),
                color = ~color_by_veg(`Vegetation Class`),
                fillOpacity = 0.7,
                group = paste(veg)
              )
          }

          #View map
          m
        } else{
          NULL
        }
      } else if(input$map_input == "Year"){
        ele_year_avg <- tables_for_mapping()$ele_year_avg
        color_by_rumbles <- colorNumeric(palette = "viridis",
                                         domain = range(ele_year_avg$`Avg Rumbles`))
        m <- leaflet(ele_year_avg) %>%

          #Add default base map
          addProviderTiles(provider = providers$Esri.WorldTopoMap,
                           group = "map") %>%

          #Set center point and zoom level
          setView(lng = mean(ele_year_avg$Longitude),
                  lat = mean(ele_year_avg$Latitude),
                  zoom = 10) %>%

          #Add legend
          addLegend(
            position = "bottomright",
            pal = color_by_rumbles,
            values = ~`Avg Rumbles`,
            title = "Avg Daily Rumbles"
          ) %>%

          #Add markers
          addCircleMarkers(
            data = ele_year_avg %>%
              filter(Year == input$year_slider_input),
            lng = ~Longitude,
            lat = ~Latitude,
            radius = ~sqrt(`Avg Rumbles`) * 8,  # Adjust the multiplier to change circle sizes
            popup = ~paste("Site:", Site, "<br>Avg Rumbles:", `Avg Rumbles`),
            color = ~color_by_rumbles(`Avg Rumbles`),
            fillOpacity = 0.7,
            group = paste(input$year_slider_input)
          )

        #View map
        m
      } else if(input$map_input == "Month"){
        ele_month_avg <- tables_for_mapping()$ele_month_avg
        color_by_rumbles <- colorNumeric(palette = "viridis",
                                         domain = range(ele_month_avg$`Avg Rumbles`))
        m <- leaflet(ele_month_avg) %>%

          #Add default base map
          addProviderTiles(provider = providers$Esri.WorldTopoMap,
                           group = "map") %>%

          #Set center point and zoom level
          setView(lng = mean(ele_month_avg$Longitude),
                  lat = mean(ele_month_avg$Latitude),
                  zoom = 10) %>%

          #Add legend
          addLegend(
            position = "bottomright",
            pal = color_by_rumbles,
            values = ~`Avg Rumbles`,
            title = "Avg Daily Rumbles"
          ) %>%

          #Add markers
          addCircleMarkers(
            data = ele_month_avg %>%
              filter(Month == input$month_slider_input),
            lng = ~Longitude,
            lat = ~Latitude,
            radius = ~sqrt(`Avg Rumbles`) * 8,  # Adjust the multiplier to change circle sizes
            popup = ~paste("Site:", Site,
                           "<br>Avg Rumbles:", `Avg Rumbles`),
            color = ~color_by_rumbles(`Avg Rumbles`),
            fillOpacity = 0.7,
            group = paste(input$month_slider_input)
          )

        #View map
        m
      }
    })

    #display maps!
    output$map_output <- renderLeaflet({
      #validate if tables have been loaded
      validate(
        need(input$map_input, "Load your tables to map them!"),
      )

      #validate if map is not NULL
      validate(
        need(!is.null(display_this_map()), "No map available for this option.")
      )

      #display map
      display_this_map()
    })

    #name maps for downloading
    map_file_name <- reactive({
      if(!is.null(input$year_slider_input)){
        paste(
          gsub(" ", "_", tolower(input$map_input)), "_",
          gsub(" ", "_", as.character(input$year_slider_input)), "_map",
          sep = "")
      } else if(!is.null(input$month_slider_input)){
        paste(
          gsub(" ", "_", tolower(input$map_input)), "_",
          gsub(" ", "_", as.character(input$month_slider_input)), "_map",
          sep = "")
      } else{
        paste(
          gsub(" ", "_", tolower(input$map_input)), "_map",
          sep = "")
      }
    })

    #download maps!
    output$download_maps <- downloadHandler(
      filename = function(){
        paste(map_file_name(), ".png", sep = "")
      },
      content = function(file){
        mapshot(display_this_map(), file = file, remove_controls = c("layersControl"), vwidth = 700, vheight = 500)
      }
    )

    #select data tables to display based on drop-down menu selections
    display_this_df <- reactive({
      if(input$map_input == "Default"){
        tables_for_mapping()$ele_site_avg
      } else if(input$map_input == "Strata"){
        tables_for_mapping()$ele_site_avg
      } else if(input$map_input == "Vegetation Class"){
        tables_for_mapping()$ele_site_avg
      } else if(input$map_input == "Year"){
        tables_for_mapping()$ele_year_avg
      } else if(input$map_input == "Month"){
        tables_for_mapping()$ele_month_avg
      }
    })

    #display map data tables
    output$map_df <- renderDataTable({
      #validate if tables have been loaded
      validate(
        need(input$map_input, "Load your tables to view them!"),
      )
      #display data table
      datatable(display_this_df())
    })

    #______________________________________
    ##### HELP #####

    #sound_check_documentation
    output$sound_check_documentation <- renderText({
      rd = system.file("man", "sound_check_function.Rd", package = "elpR2")
      temp = tempfile("docs")
      Rd2HTML(rd, out = temp)
      HTML(read_file(temp))
    })

    #exclude_files_documentation
    output$exclude_files_documentation <- renderText({
      rd = system.file("man", "sound_exclude_function.Rd", package = "elpR2")
      temp = tempfile("docs")
      Rd2HTML(rd, out = temp)
      HTML(read_file(temp))
    })

    #restructure_documentation
    output$restructure_documentation <- renderText({
      rd = system.file("man", "restructure_rumble_function.Rd", package = "elpR2")
      temp = tempfile("docs")
      Rd2HTML(rd, out = temp)
      HTML(read_file(temp))
    })

    #gun_restructure_documentation
    output$gun_restructure_documentation <- renderText({
      rd = system.file("man", "restructure_gunshot_function.Rd", package = "elpR2")
      temp = tempfile("docs")
      Rd2HTML(rd, out = temp)
      HTML(read_file(temp))
    })

    #general_restructure_documentation
    output$general_restructure_documentation <- renderText({
      rd = system.file("man", "restructure_general_function.Rd", package = "elpR2")
      temp = tempfile("docs")
      Rd2HTML(rd, out = temp)
      HTML(read_file(temp))
    })

    #summary_documentation
    output$summary_documentation <- renderText({
      rd = system.file("man", "data_summaries_function.Rd", package = "elpR2")
      temp = tempfile("docs")
      Rd2HTML(rd, out = temp)
      HTML(read_file(temp))
    })
  }

  #_______________________________________________________________________________
  #### RUN THE APP ####
  shinyApp(ui = ui, server = server)
}

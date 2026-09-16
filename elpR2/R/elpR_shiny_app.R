#Purpose of script: To make a shiny app for the elpR package.
#Author: Jidapa Janpathompong
#Date created: April 2025
#Date last updated: April 22, 2026

#Notes: This script is organized by sections in the UI:
#ABOUT
#1. Basic Info
#2. Sound Check
#3. Exclude Files
#4. Restructure
#5. Data Summaries
#6. Results
#HELP

#' Shiny App for elpR
#'
#' @author Jidapa Janpathompong
#' @description This function creates a shiny app that allows users to run the other
#' functions in this package (sound_check_function(), exclude_sounds_function(),
#' restructure_rumble_function(), restructure_gunshot_function(),
#' restructure_general_function(), and data_summaries_function()), and visualize
#' their data.
#'
#' @returns A shiny app
#'
#' @export

elpRApp <- function(){
  #load libraries
  library(shiny)
  library(bslib) #shiny layout functions
  library(shinyFiles) #shinyDirChoose(), parseDirPath()
  library(gbRd) #Rd_fun()
  library(tools) #Rd2HTML()
  library(readr) #read_files()
  library(shinyjs) #useShinyjs()
  library(ggplot2)
  library(Hmisc) #for loading monthly plots
  library(leaflet)
  library(dplyr) #summarize()
  library(mapview) #mapshot()
  library(webshot) #mapshot()
  if(webshot::is_phantomjs_installed() == FALSE){
    webshot::install_phantomjs()
  } #related to downloading leaflet maps
  library(webshot2) #downloading help documentation
  library(DT) #datatable()
  library(stringr) #str_c()

  #define helper function to add info icons next to input widgets
  add_info <- function(input_widget, arg_name){
    widget_with_icon <- div(
      class = "info_icon_div",
      input_widget,
      tags$span(
        tags$i(
          class = "glyphicon glyphicon-info-sign",
          title = paste("see", arg_name, "argument", sep=" ")
        )
      )
    )
    return(widget_with_icon)
  }

  #_______________________________________________________________________________
  #### DEFINE UI ####
  ui <- page_fillable(
    #HTML styling
    tags$head(
      tags$style(HTML("
      .title {
      font-size: 40px;
      color: #3c6a00;
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
      right: 235px;
      }
      .image_yang {
      position: absolute;
      top: 15px;
      right: 20px;
      }
      #eng_button {
      position: absolute;
      top: 15px;
      right: 445px;
      padding: 10px;
      padding-left: 15px;
      padding-right: 15px;
      border-radius: 0%;
      }
      #french_button {
      position: absolute;
      top: 15px;
      right: 365px;
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
      .info_icon_div {
      display: flex;
      align-items: center;
      margin-bottom: -15px;
      }"))
    ),

    theme = bs_theme(
      bg = "white",
      fg = "black",
      primary = "#3c6a00"
    ),

    #______________________________________
    ##### header #####
    div(class = "title", "ElpR App"),
    div(uiOutput("eng_ui")),
    div(uiOutput("french_ui")),
    div(class = "image", imageOutput("logo")),
    div(class = "image_yang", imageOutput("logo_yang")),
    div(uiOutput("subtitle")),
    div(class = "line", hr()),

    #______________________________________
    ##### body #####
    useShinyjs(),
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
uiOutput("card_1_1_parent_dir"),
add_info(textInput("parent_dir_text_in", label = "Or paste the parent folder path", value = ""), "parent_dir"),
add_info(shinyDirButton(id = "parent_dir_in", label = NULL, icon = icon("folder-open"), title = "Choose the parent files_for_elpR folder"), "parent_dir"),
textOutput("parent_dir_recieved"),
add_info(textInput("deployment_name_in", label = uiOutput("card_1_1_dep_name"), value = "kk_202405_may"), "deployment_name"),
add_info(textInput("deployment_num_in", label = uiOutput("card_1_1_dep_num"), value = "02"), "deployment_num"),
add_info(textInput("disk_ID_in", label = uiOutput("card_1_1_disk_ID"), value = "00"), "disk_ID"),
add_info(fileInput("sites_in", label = uiOutput("card_1_1_sites")), "sites")
          ),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12,12), md = c(9,12), lg = c(6,12))
        )
      ),

      ###### 2. Sound Check ######
      tabPanel(
        "2. Sound Check",
        layout_columns(
          card(
            card_header(textOutput("card_1_2")),

            textOutput("card_1_2_sound_path"),
            add_info(shinyDirButton(id = "sound_path_in", label = NULL, icon = icon("folder-open"), title = "Choose folder containing sound files"), "x"),
            textOutput("sound_path_recieved"),
            add_info(numericInput("fileDurationMin_in", label = textOutput("card_1_2_file_dur"), value = 14000), "fileDurationMin"),
            add_info(numericInput("sample_rate_in", label = textOutput("card_1_2_samp_rate"), value = 8000), "sample_rate"),
            add_info(selectInput("sound_file_ext_in", label = textOutput("card_1_2_file_ext"), choices = list(".wav", ".flac", ".aiff")), "sound_file_ext"),
          ),

          card(
            card_header(textOutput("card_2_2")),

            actionButton("check_sound_check_info_in", textOutput("card_2_2_check")),
            tableOutput("check_sound_check_info_output"),
            input_task_button("run_sound_check_in", textOutput("card_2_2_run")),
            textOutput("card_2_2_preview"),
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
        "3. Exclude Files",
        layout_columns(
          card(
            card_header(textOutput("card_1_3")),

            textOutput("card_1_3_path"),
            add_info(shinyDirButton(id = "extra_sounds_in", label = NULL, icon = icon("folder-open"), title = "Choose folder to output extra sounds"), "extra_sounds_folder"),
            textOutput("extra_sounds_recieved"),
            add_info(radioButtons("have_swift_files_in", label = textOutput("card_1_3_have_swift"),
                                  choices = list("Yes", "No"), selected = "No"), "have_SwiftFiles"),
            uiOutput("merge_swift_files_output")
          ),

          card(
            card_header(textOutput("card_2_3")),

            actionButton("check_exclude_files_info_in", textOutput("card_2_3_check")),
            tableOutput("check_exclude_files_info_output"),
            input_task_button("run_exclude_files_in", textOutput("card_2_3_run")),
            textOutput("card_2_3_preview"),
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
        "4. Restructure",
        layout_columns(
          textOutput("tab_4_pills"),

          navset_pill(
            ###### > rumbles ######
            tabPanel(
              "Rumbles",
              layout_columns(
                "\n",

                card(
                  card_header(textOutput("card_1_4_1")),

                  add_info(textInput("sample_rate_chr_in", label = textOutput("card_1_4_1_samp_rate"), value = "8kHz"), "sample_rate"),
                  add_info(radioButtons("three_rand_days_in", label = textOutput("card_1_4_1_rand"),
                                        choices = list("Yes", "No"), selected = "Yes"), "three_rand_days"),
                  add_info(radioButtons("min_23hours_in", label = textOutput("card_1_4_1_min_hrs"),
                                        choices = list("Yes", "No"), selected = "Yes"), "min_23hrs"),
                  add_info(textInput("score_column_name_in", label = textOutput("card_1_4_1_col_name"), value = "Score"), "score_column_name"),
                  add_info(selectInput("detector_in", label = textOutput("card_1_4_1_detect"), choices = list("HoriHarm", "FruitPunchAI", "Stanford Detector")), "Detector"),
                  add_info(numericInput("detector_score_in", label = textOutput("card_1_4_1_detect_score"), value = 0.2), "Detector_ScoreThreshold"),
                  add_info(numericInput("filter_score_in", label = textOutput("card_1_4_1_filt_score"), value = 0.4), "Filter_ScoreThreshold")
                ),

                card(
                  card_header(textOutput("card_2_4_1")),

                  actionButton("check_restructure_info_in", textOutput("card_2_4_1_check")),
                  tableOutput("check_restructure_info_output"),
                  input_task_button("run_restructure_in", textOutput("card_2_4_1_run")),
                  textOutput("card_2_4_1_preview"),
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
                  card_header(textOutput("card_1_4_2")),

                  add_info(textInput("sample_rate_chr_gun_in", label = textOutput("card_1_4_2_samp_rate"), value = "8kHz"), "sample_rate"),
                  add_info(selectInput("detector_gun_in", textOutput("card_1_4_2_detect"), choices = list("DTDguns8")), "Detector"),
                  add_info(numericInput("detector_score_gun_in", label = textOutput("card_1_4_2_detect_score"), value = 0.53), "Detector_ScoreThreshold"),
                  add_info(numericInput("filter_score_gun_in", label = textOutput("card_1_4_2_filt_score"), value = 0.53), "Filter_ScoreThreshold")
                ),

                card(
                  card_header(textOutput("card_2_4_2")),

                  actionButton("check_gun_restructure_info_in", textOutput("card_2_4_2_check")),
                  tableOutput("check_gun_restructure_info_output"),
                  input_task_button("run_gun_restructure_in", textOutput("card_2_4_2_run")),
                  textOutput("card_2_4_2_preview"),
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
                  card_header(textOutput("card_1_4_3")),

                  textOutput("card_1_4_3_path"),
                  add_info(shinyDirButton(id = "general_merge_in", label = NULL, icon = icon("folder-open"), title = "Choose folder containing tables to merge"), "path"),
                  textOutput("general_merge_recieved"),
                  add_info(uiOutput("recursive_out"), "recursive")
                ),

                card(
                  card_header(textOutput("card_2_4_3")),

                  actionButton("check_general_restructure_info_in", textOutput("card_2_4_3_check")),
                  tableOutput("check_general_restructure_info_output"),
                  input_task_button("run_general_restructure_in", textOutput("card_2_4_3_run")),
                  textOutput("card_2_4_3_preview"),
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
        "5. Data Summaries",
        layout_columns(
          card(
            card_header(textOutput("card_1_5")),

add_info(textInput("project_name_in", label = uiOutput("card_1_5_proj"), value = "PNNN"), "project_name"),
add_info(textInput("deployment_nums_in", label = uiOutput("card_1_5_dep_num"), value = "01-20"), "deployment_num"),
add_info(selectInput("summary_detector_in", uiOutput("card_1_5_detect"), choices = list("HoriHarm", "FruitPunchAI", "Stanford Detector", "DTDguns8")), "detector_name"),
add_info(textInput("event_count_column_in", label = uiOutput("card_1_5_event_count"), value = "Count"), "event_count_column"),
textOutput("card_1_5_out_path"),
add_info(shinyDirButton(id = "summary_folder_in", label = NULL, icon = icon("folder-open"), title = "Choose folder to output data summaries"), "output"),
            textOutput("summary_folder_recieved"),
            textOutput("card_1_5_tables_path"),
            add_info(shinyDirButton(id = "selection_tables_folder_in", label = NULL, icon = icon("folder-open"), title = "Choose folder containing selection tables"), "ele_tables"),
            textOutput("selection_tables_folder_recieved"),
            textOutput("card_1_5_zero_path"),
            add_info(shinyDirButton(id = "zero_selection_tables_folder_in", label = NULL, icon = icon("folder-open"), title = "Choose folder containing zero-day selection tables"), "zero_txt"),
            textOutput("zero_selection_tables_folder_recieved")
          ),

          card(
            card_header(textOutput("card_2_5")),

            add_info(radioButtons("sound_check_include_in", label = textOutput("card_2_5_sound_include"),
                                  choices = list("Yes", "No"), selected = "No"), "sound_check_include"),
            textOutput("card_2_5_sound_path"),
            uiOutput("sound_check_include_output"),
            textOutput("sound_check_folder_recieved"),
            add_info(radioButtons("use_only_sites_provided_in", label = textOutput("card_2_5_sites_include"),
                                  choices = list("Yes", "No"), selected = "No"), "use_only_sites_provided"),
            uiOutput("use_only_sites_provided_output"),
            add_info(radioButtons("rand_dates_needed_in", label = textOutput("card_2_5_rand"),
                                  choices = list("Yes", "No"), selected = "No"), "rand_dates_needed"),
add_info(radioButtons("events_bad_sound_remove_in", label = uiOutput("card_2_5_min_hrs"),
                                  choices = list("Yes", "No"), selected = "No"), "events_bad_sound_remove")
          ),

          card(
            card_header(textOutput("card_3_5")),

            actionButton("check_summary_info_in", textOutput("card_3_5_check")),
            tableOutput("check_summary_info_output"),
            input_task_button("run_summary_in", textOutput("card_3_5_run")),
            textOutput("card_3_5_preview"),
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
              textOutput("tab_6_pill_1"),
              layout_columns(
                "\n",

                card(
                  card_header(textOutput("card_1_6_1")),

                  textOutput("card_1_6_1_path"),
                  shinyDirButton(id = "saved_plots_folder_in", label = NULL, icon = icon("folder-open"), title = "Choose folder containing .Rds file"),
                  textOutput("saved_plots_folder_recieved"),
                  textInput("saved_plots_file_in", label = textOutput("card_1_6_1_rds"), value = "saved_plots.Rds"),
                  input_task_button("load_plots", textOutput("card_1_6_1_run")),
                ),

                card(
                  min_height = "500px",
                  layout_sidebar(
                    sidebar = sidebar(
                      p(uiOutput("card_2_6_1_plot")),
                      p(id = "placeholder"),
                      p(uiOutput("card_2_6_1_download")),
                      radioButtons("download_ext_in", label = textOutput("card_2_6_1_format"),
                                   choices = list(".png", ".eps"), selected = ".png"),
                    ),
                    plotOutput("plots_output"),
                    downloadButton("download_plots", textOutput("card_2_6_1_save"))
                  )
                ),

                col_widths = breakpoints(sm = c(12,12,12), md = c(12,9,12), lg = c(12,6,12))
              )
            ),

            ###### > maps ######
            tabPanel(
              textOutput("tab_6_pill_2"),
              layout_columns(
                "\n",

                card(
                  card_header(textOutput("card_1_6_2")),

                  textOutput("card_1_6_2_path"),
                  shinyDirButton(id = "saved_tables_folder_in", label = NULL, icon = icon("folder-open"), title = "Choose folder containing data summary tables"),
                  textOutput("saved_tables_folder_recieved"),
                  fileInput("site_lat_long_map", label = textOutput("card_1_6_2_sites")),
                  input_task_button("load_tables", textOutput("card_1_6_2_run")),
                ),

                card(
                  min_height = "600px",
                  layout_sidebar(
                    sidebar = sidebar(
                      p(uiOutput("card_2_6_2_map")),
                      p(id = "placeholder2")
                    ),
                    leafletOutput("map_output"),
                    downloadButton("download_maps", textOutput("card_2_6_2_save")),
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
            multiple = FALSE, #not working since I made the panels dynamic :(
            uiOutput("help_sound"),
            uiOutput("help_exclude"),
            uiOutput("help_rumble"),
            uiOutput("help_gunshot"),
            uiOutput("help_general"),
            uiOutput("help_data"),
            uiOutput("help_plots"),
            uiOutput("help_maps")
          ),
          uiOutput("download_all_help"),

          "\n",
          "\n",
          "\n",

          col_widths = breakpoints(sm = c(12,12,12), md = c(10,10,12), lg = c(10,10,12))
        )
      )
    )
  )

  #_______________________________________________________________________________
  #### DEFINE SERVER LOGIC ####

  server <- function(input, output) {
    #______________________________________
    ##### header #####
    #render logos
    output$logo <- renderImage({
      list(src = "inst/elp_logo.png",
           contentType = "image/png",
           width = 100,
           height = 80)
    }, deleteFile = FALSE)

    output$logo_yang <- renderImage({
      list(src = "inst/yang_logo.png",
           contentType = "image/png",
           width = 215,
           height = 70)
    }, deleteFile = FALSE)

    #initialize/set variables for translation buttons
    output$subtitle <- renderText({subtitle_eng})
    output$tab_about <- renderText({tab_about_eng})
    output$tab_1 <- renderText({tab_1_eng})
    output$tab_6 <- renderText({tab_6_eng})
    output$tab_help <- renderText({tab_help_eng})

    output$header_welcome <- renderUI({header_welcome_eng})
    output$text_welcome <- renderUI({text_welcome_eng})
    output$header_contributers <- renderUI({header_contributers_eng})
    output$text_contributers <- renderUI({text_contributers_eng})

    output$card_1_1 <- renderText({card_1_1_eng})
output$card_1_1_parent_dir <- renderUI({card_1_1_parent_dir_eng})
output$card_1_1_dep_name <- renderUI({card_1_1_dep_name_eng})
output$card_1_1_dep_num <- renderText({card_1_1_dep_num_eng})
output$card_1_1_disk_ID <- renderText({card_1_1_disk_ID_eng})
output$card_1_1_sites <- renderText({card_1_1_sites_eng})
output$choose_a_folder <- renderText({choose_a_folder_eng})
output$parent_dir_recieved <- renderText({parent_dir_recieved_eng()})

    output$card_1_2 <- renderText({card_1_2_eng})
    output$card_1_2_sound_path <- renderText({card_1_2_sound_path_eng})
    output$card_1_2_file_dur <- renderText({card_1_2_file_dur_eng})
    output$card_1_2_samp_rate <- renderText({card_1_2_samp_rate_eng})
    output$card_1_2_file_ext <- renderText({card_1_2_file_ext_eng})
    output$card_2_2 <- renderText({card_2_2_eng})
    output$card_2_2_check <- renderText({card_2_2_check_eng})
    output$card_2_2_run <- renderText({card_2_2_run_eng})
    output$card_2_2_preview <- renderText({card_2_2_preview_eng})
    output$sound_path_recieved <- renderText({sound_path_recieved_eng()})

    output$card_1_3 <- renderText({card_1_3_eng})
    output$card_1_3_path <- renderText({card_1_3_path_eng})
    output$card_1_3_have_swift <- renderText({card_1_3_have_swift_eng})
    output$card_1_3_merge_swift <- renderText({card_1_3_merge_swift_eng})
    output$card_2_3 <- renderText({card_2_3_eng})
    output$card_2_3_check <- renderText({card_2_2_check_eng})
    output$card_2_3_run <- renderText({card_2_3_eng})
    output$card_2_3_preview <- renderText({card_2_2_preview_eng})
    output$extra_sounds_recieved <- renderText({extra_sounds_recieved_eng()})

    output$tab_4_pills <- renderText({tab_4_pills_eng})
    output$card_1_4_1 <- renderText({card_1_4_1_eng})
    output$card_1_4_1_samp_rate <- renderText({card_1_4_1_samp_rate_eng})
    output$card_1_4_1_rand <- renderText({card_1_4_1_rand_eng})
    output$card_1_4_1_min_hrs <- renderText({card_1_4_1_min_hrs_eng})
    output$card_1_4_1_col_name <- renderText({card_1_4_1_col_name_eng})
    output$card_1_4_1_detect <- renderText({card_1_4_1_detect_eng})
    output$card_1_4_1_detect_score <- renderText({card_1_4_1_detect_score_eng})
    output$card_1_4_1_filt_score <- renderText({card_1_4_1_filt_score_eng})
    output$card_2_4_1 <- renderText({card_2_4_1_eng})
    output$card_2_4_1_check <- renderText({card_2_2_check_eng})
    output$card_2_4_1_run <- renderText({card_2_4_1_eng})
    output$card_2_4_1_preview <- renderText({card_2_2_preview_eng})

    output$card_1_4_2 <- renderText({card_1_4_2_eng})
    output$card_1_4_2_samp_rate <- renderText({card_1_4_2_samp_rate_eng})
    output$card_1_4_2_detect <- renderText({card_1_4_2_detect_eng})
    output$card_1_4_2_detect_score <- renderText({card_1_4_2_detect_score_eng})
    output$card_1_4_2_filt_score <- renderText({card_1_4_2_filt_score_eng})
    output$card_2_4_2 <- renderText({card_2_4_2_eng})
    output$card_2_4_2_check <- renderText({card_2_2_check_eng})
    output$card_2_4_2_run <- renderText({card_2_4_2_eng})
    output$card_2_4_2_preview <- renderText({card_2_2_preview_eng})

    output$card_1_4_3 <- renderText({card_1_4_3_eng})
    output$card_1_4_3_path <- renderText({card_1_4_3_path_eng})
    output$general_merge_recieved <- renderText({general_merge_recieved_eng()})
    output$recursive_out <- renderUI({card_1_4_3_recur_eng})
    output$card_2_4_3 <- renderText({card_2_4_3_eng})
    output$card_2_4_3_check <- renderText({card_2_2_check_eng})
    output$card_2_4_3_run <- renderText({card_2_4_3_eng})
    output$card_2_4_3_preview <- renderText({card_2_2_preview_eng})

    output$card_1_5 <- renderText({card_1_5_eng})
output$card_1_5_proj <- renderUI({card_1_5_proj_eng})
output$card_1_5_dep_num <- renderUI({card_1_5_dep_num_eng})
output$card_1_5_detect <- renderUI({card_1_5_detect_eng})
output$card_1_5_event_count <- renderUI({card_1_5_event_count_eng})
output$card_1_5_out_path <- renderUI({card_1_5_out_path_eng})
output$card_1_5_tables_path <- renderUI({card_1_5_tables_path_eng})
    output$selection_tables_folder_recieved <- renderText({selection_tables_folder_recieved_eng()})
    output$card_1_5_zero_path <- renderText({card_1_5_zero_path_eng})
    output$zero_selection_tables_folder_recieved <- renderText({zero_selection_tables_folder_recieved_eng()})
    output$card_2_5 <- renderText({card_2_5_eng})
    output$card_2_5_sound_include <- renderText({card_2_5_sound_include_eng})
    output$card_2_5_sites_include <- renderText({card_2_5_sites_include_eng})
    observeEvent(input$sound_check_include_in, {
      if(input$sound_check_include_in == "Yes" & translate_val() == 0){
        output$card_2_5_sound_path <- renderText({card_2_5_sound_path_eng})
        output$sound_check_folder_recieved <- renderText({sound_check_folder_recieved_eng()})
      } else if(input$sound_check_include_in == "Yes" & translate_val() == 1){
        output$card_2_5_sound_path <- renderText({card_2_5_sound_path_french})
        output$sound_check_folder_recieved <- renderText({sound_check_folder_recieved_french()})
      }
    })
    output$card_2_5_sites <- renderText({card_2_5_sites_eng})
    output$card_2_5_rand <- renderText({card_2_5_rand_eng})
    output$card_2_5_min_hrs <- renderText({card_2_5_min_hrs_eng})
    output$card_3_5 <- renderText({card_3_5_eng})
    output$card_3_5_check <- renderText({card_2_2_check_eng})
    output$card_3_5_run <- renderText({card_3_5_eng})
    output$card_3_5_preview <- renderText({card_2_2_preview_eng})

    output$tab_6_pill_1 <- renderText({tab_6_pill_1_eng})
    output$card_1_6_1 <- renderText({card_1_6_1_eng})
    output$card_1_6_1_path <- renderText({card_1_6_1_path_eng})
    output$saved_plots_folder_recieved <- renderText({saved_plots_folder_recieved_eng()})
    output$card_1_6_1_rds <- renderText({card_1_6_1_rds_eng})
    output$card_1_6_1_run <- renderText({card_1_6_1_run_eng})
    output$card_1_6_1_message <- renderText({card_1_6_1_message_eng})
    output$card_2_6_1_plot <- renderUI({card_2_6_1_plot_eng})
    output$card_2_6_1_plot_input <- renderText({card_2_6_1_plot_input_eng})
    output$card_2_6_1_facet <- renderText({card_2_6_1_facet_eng})
    output$card_2_6_1_night <- renderText({card_2_6_1_night_eng})
    output$card_2_6_1_overlay <- renderText({card_2_6_1_overlay_eng})
    output$card_2_6_1_download <- renderUI({card_2_6_1_download_eng})
    output$card_2_6_1_format <- renderText({card_2_6_1_format_eng})
    output$card_2_6_1_save <- renderText({card_2_6_1_save_eng})

    output$tab_6_pill_2 <- renderText({tab_6_pill_2_eng})
    output$card_1_6_2 <- renderText({card_1_6_1_eng})
    output$card_1_6_2_path <- renderText({card_1_6_2_path_eng})
    output$saved_tables_folder_recieved <- renderText({saved_tables_folder_recieved_eng()})
    output$card_1_6_2_sites <- renderText({card_1_6_2_sites_eng})
    output$card_1_6_2_run <- renderText({card_1_6_2_run_eng})
    output$card_2_6_2_map <- renderUI({card_2_6_2_map_eng})
    output$card_2_6_2_map_input <- renderText({card_2_6_2_map_input_eng})
    output$card_2_6_2_year <- renderText({card_2_6_2_year_eng})
    output$card_2_6_2_month <- renderText({card_2_6_2_month_eng})
    output$card_2_6_2_save <- renderText({card_2_6_1_save_eng})

    output$help_sound <- renderUI({help_sound_eng()})
    output$help_exclude <- renderUI({help_exclude_eng()})
    output$help_rumble <- renderUI({help_rumble_eng()})
    output$help_gunshot <- renderUI({help_gunshot_eng()})
    output$help_general <- renderUI({help_general_eng()})
    output$help_data <- renderUI({help_data_eng()})
    output$help_plots <- renderUI({help_plots_eng()})
    output$help_maps <- renderUI({help_maps_eng()})
    output$download_all_help <- renderUI({downloadButton("download_all_help_eng", label = renderText({card_2_6_1_save_eng}))})

    translate_val <- reactiveVal(0)
    unclicked_color <- "background-color: white; color: #404040"
    clicked_color <- "background-color: #d49337; color: white"

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
      output$tab_6 <- renderText({tab_6_eng})
      output$tab_help <- renderText({tab_help_eng})

      output$header_welcome <- renderUI({header_welcome_eng})
      output$text_welcome <- renderUI({text_welcome_eng})
      output$header_contributers <- renderUI({header_contributers_eng})
      output$text_contributers <- renderUI({text_contributers_eng})

      output$card_1_1 <- renderText({card_1_1_eng})
      output$card_1_1_parent_dir <- renderUI({card_1_1_parent_dir_eng})
      output$card_1_1_dep_name <- renderUI({card_1_1_dep_name_eng})
      output$card_1_1_dep_num <- renderText({card_1_1_dep_num_eng})
      output$card_1_1_disk_ID <- renderText({card_1_1_disk_ID_eng})
      output$card_1_1_sites <- renderText({card_1_1_sites_eng})
      output$choose_a_folder <- renderText({choose_a_folder_eng})
      output$parent_dir_recieved <- renderText({parent_dir_recieved_eng()})

      output$card_1_2 <- renderText({card_1_2_eng})
      output$card_1_2_sound_path <- renderText({card_1_2_sound_path_eng})
      output$card_1_2_file_dur <- renderText({card_1_2_file_dur_eng})
      output$card_1_2_samp_rate <- renderText({card_1_2_samp_rate_eng})
      output$card_1_2_file_ext <- renderText({card_1_2_file_ext_eng})
      output$card_2_2 <- renderText({card_2_2_eng})
      output$card_2_2_check <- renderText({card_2_2_check_eng})
      output$card_2_2_run <- renderText({card_2_2_run_eng})
      output$card_2_2_preview <- renderText({card_2_2_preview_eng})
      output$sound_path_recieved <- renderText({sound_path_recieved_eng()})

      output$card_1_3 <- renderText({card_1_3_eng})
      output$card_1_3_path <- renderText({card_1_3_path_eng})
      output$card_1_3_have_swift <- renderText({card_1_3_have_swift_eng})
      output$card_1_3_merge_swift <- renderText({card_1_3_merge_swift_eng})
      output$card_2_3 <- renderText({card_2_3_eng})
      output$card_2_3_check <- renderText({card_2_2_check_eng})
      output$card_2_3_run <- renderText({card_2_3_eng})
      output$card_2_3_preview <- renderText({card_2_2_preview_eng})
      output$extra_sounds_recieved <- renderText({extra_sounds_recieved_eng()})

      output$tab_4_pills <- renderText({tab_4_pills_eng})
      output$card_1_4_1 <- renderText({card_1_4_1_eng})
      output$card_1_4_1_samp_rate <- renderText({card_1_4_1_samp_rate_eng})
      output$card_1_4_1_rand <- renderText({card_1_4_1_rand_eng})
      output$card_1_4_1_min_hrs <- renderText({card_1_4_1_min_hrs_eng})
      output$card_1_4_1_col_name <- renderText({card_1_4_1_col_name_eng})
      output$card_1_4_1_detect <- renderText({card_1_4_1_detect_eng})
      output$card_1_4_1_detect_score <- renderText({card_1_4_1_detect_score_eng})
      output$card_1_4_1_filt_score <- renderText({card_1_4_1_filt_score_eng})
      output$card_2_4_1 <- renderText({card_2_4_1_eng})
      output$card_2_4_1_check <- renderText({card_2_2_check_eng})
      output$card_2_4_1_run <- renderText({card_2_4_1_eng})
      output$card_2_4_1_preview <- renderText({card_2_2_preview_eng})

      output$card_1_4_2 <- renderText({card_1_4_2_eng})
      output$card_1_4_2_samp_rate <- renderText({card_1_4_2_samp_rate_eng})
      output$card_1_4_2_detect <- renderText({card_1_4_2_detect_eng})
      output$card_1_4_2_detect_score <- renderText({card_1_4_2_detect_score_eng})
      output$card_1_4_2_filt_score <- renderText({card_1_4_2_filt_score_eng})
      output$card_2_4_2 <- renderText({card_2_4_2_eng})
      output$card_2_4_2_check <- renderText({card_2_2_check_eng})
      output$card_2_4_2_run <- renderText({card_2_4_2_eng})
      output$card_2_4_2_preview <- renderText({card_2_2_preview_eng})

      output$card_1_4_3 <- renderText({card_1_4_3_eng})
      output$card_1_4_3_path <- renderText({card_1_4_3_path_eng})
      output$general_merge_recieved <- renderText({general_merge_recieved_eng()})
      output$recursive_out <- renderUI({card_1_4_3_recur_eng})
      output$card_2_4_3 <- renderText({card_2_4_3_eng})
      output$card_2_4_3_check <- renderText({card_2_2_check_eng})
      output$card_2_4_3_run <- renderText({card_2_4_3_eng})
      output$card_2_4_3_preview <- renderText({card_2_2_preview_eng})

      output$card_1_5 <- renderText({card_1_5_eng})
      output$card_1_5_proj <- renderUI({card_1_5_proj_eng})
      output$card_1_5_dep_num <- renderUI({card_1_5_dep_num_eng})
      output$card_1_5_detect <- renderUI({card_1_5_detect_eng})
      output$card_1_5_event_count <- renderUI({card_1_5_event_count_eng})
      output$card_1_5_out_path <- renderUI({card_1_5_out_path_eng})
      output$card_1_5_tables_path <- renderUI({card_1_5_tables_path_eng})
      output$selection_tables_folder_recieved <- renderText({selection_tables_folder_recieved_eng()})
      output$card_1_5_zero_path <- renderText({card_1_5_zero_path_eng})
      output$zero_selection_tables_folder_recieved <- renderText({zero_selection_tables_folder_recieved_eng()})
      output$card_2_5 <- renderText({card_2_5_eng})
      output$card_2_5_sound_include <- renderText({card_2_5_sound_include_eng})
      output$card_2_5_sites_include <- renderText({card_2_5_sites_include_eng})
      if(input$sound_check_include_in == "Yes"){
        output$card_2_5_sound_path <- renderText({card_2_5_sound_path_eng})
        output$sound_check_folder_recieved <- renderText({sound_check_folder_recieved_eng()})
      }
      output$card_2_5_sites <- renderText({card_2_5_sites_eng})
      output$card_2_5_rand <- renderText({card_2_5_rand_eng})
      output$card_2_5_min_hrs <- renderText({card_2_5_min_hrs_eng})
      output$card_3_5 <- renderText({card_3_5_eng})
      output$card_3_5_check <- renderText({card_2_2_check_eng})
      output$card_3_5_run <- renderText({card_3_5_eng})
      output$card_3_5_preview <- renderText({card_2_2_preview_eng})

      output$tab_6_pill_1 <- renderText({tab_6_pill_1_eng})
      output$card_1_6_1 <- renderText({card_1_6_1_eng})
      output$card_1_6_1_path <- renderText({card_1_6_1_path_eng})
      output$saved_plots_folder_recieved <- renderText({saved_plots_folder_recieved_eng()})
      output$card_1_6_1_rds <- renderText({card_1_6_1_rds_eng})
      output$card_1_6_1_run <- renderText({card_1_6_1_run_eng})
      output$card_1_6_1_message <- renderText({card_1_6_1_message_eng})
      output$card_2_6_1_plot <- renderUI({card_2_6_1_plot_eng})
      output$card_2_6_1_plot_input <- renderText({card_2_6_1_plot_input_eng})
      output$card_2_6_1_facet <- renderText({card_2_6_1_facet_eng})
      output$card_2_6_1_night <- renderText({card_2_6_1_night_eng})
      output$card_2_6_1_overlay <- renderText({card_2_6_1_overlay_eng})
      output$card_2_6_1_download <- renderUI({card_2_6_1_download_eng})
      output$card_2_6_1_format <- renderText({card_2_6_1_format_eng})
      output$card_2_6_1_save <- renderText({card_2_6_1_save_eng})

      output$tab_6_pill_2 <- renderText({tab_6_pill_2_eng})
      output$card_1_6_2 <- renderText({card_1_6_1_eng})
      output$card_1_6_2_path <- renderText({card_1_6_2_path_eng})
      output$saved_tables_folder_recieved <- renderText({saved_tables_folder_recieved_eng()})
      output$card_1_6_2_sites <- renderText({card_1_6_2_sites_eng})
      output$card_1_6_2_run <- renderText({card_1_6_2_run_eng})
      output$card_2_6_2_map <- renderUI({card_2_6_2_map_eng})
      output$card_2_6_2_map_input <- renderText({card_2_6_2_map_input_eng})
      output$card_2_6_2_year <- renderText({card_2_6_2_year_eng})
      output$card_2_6_2_month <- renderText({card_2_6_2_month_eng})
      output$card_2_6_2_save <- renderText({card_2_6_1_save_eng})

      output$help_sound <- renderUI({help_sound_eng()})
      output$help_exclude <- renderUI({help_exclude_eng()})
      output$help_rumble <- renderUI({help_rumble_eng()})
      output$help_gunshot <- renderUI({help_gunshot_eng()})
      output$help_general <- renderUI({help_general_eng()})
      output$help_data <- renderUI({help_data_eng()})
      output$help_plots <- renderUI({help_plots_eng()})
      output$help_maps <- renderUI({help_maps_eng()})
      output$download_all_help <- renderUI({downloadButton("download_all_help_eng", label = renderText({card_2_6_1_save_eng}))})
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
      output$tab_6 <- renderText({tab_6_french})
      output$tab_help <- renderText({tab_help_french})

      output$header_welcome <- renderUI({header_welcome_french})
      output$text_welcome <- renderUI({text_welcome_french})
      output$header_contributers <- renderUI({header_contributers_french})
      output$text_contributers <- renderUI({text_contributers_french})

      output$card_1_1 <- renderText({card_1_1_french})
      output$card_1_1_parent_dir <- renderUI({card_1_1_parent_dir_french})
      output$card_1_1_dep_name <- renderText({card_1_1_dep_name_french})
      output$card_1_1_dep_num <- renderText({card_1_1_dep_num_french})
      output$card_1_1_disk_ID <- renderText({card_1_1_disk_ID_french})
      output$card_1_1_sites <- renderText({card_1_1_sites_french})
      output$card_1_1_parent_dir <- renderUI({card_1_1_parent_dir_french})
      output$choose_a_folder <- renderText({choose_a_folder_french})
      output$parent_dir_recieved <- renderText({parent_dir_recieved_french()})

      output$card_1_2 <- renderText({card_1_2_french})
      output$card_1_2_sound_path <- renderText({card_1_2_sound_path_french})
      output$card_1_2_file_dur <- renderText({card_1_2_file_dur_french})
      output$card_1_2_samp_rate <- renderText({card_1_2_samp_rate_french})
      output$card_1_2_file_ext <- renderText({card_1_2_file_ext_french})
      output$card_2_2 <- renderText({card_2_2_french})
      output$card_2_2_check <- renderText({card_2_2_check_french})
      output$card_2_2_run <- renderText({card_2_2_run_french})
      output$card_2_2_preview <- renderText({card_2_2_preview_french})
      output$sound_path_recieved <- renderText({sound_path_recieved_french()})

      output$card_1_3 <- renderText({card_1_3_french})
      output$card_1_3_path <- renderText({card_1_3_path_french})
      output$card_1_3_have_swift <- renderText({card_1_3_have_swift_french})
      output$card_1_3_merge_swift <- renderText({card_1_3_merge_swift_french})
      output$card_2_3 <- renderText({card_2_3_french})
      output$card_2_3_check <- renderText({card_2_2_check_french})
      output$card_2_3_run <- renderText({card_2_3_french})
      output$card_2_3_preview <- renderText({card_2_2_preview_french})
      output$extra_sounds_recieved <- renderText({extra_sounds_recieved_french()})

      output$tab_4_pills <- renderText({tab_4_pills_french})
      output$card_1_4_1 <- renderText({card_1_4_1_french})
      output$card_1_4_1_samp_rate <- renderText({card_1_4_1_samp_rate_french})
      output$card_1_4_1_rand <- renderText({card_1_4_1_rand_french})
      output$card_1_4_1_min_hrs <- renderText({card_1_4_1_min_hrs_french})
      output$card_1_4_1_col_name <- renderText({card_1_4_1_col_name_french})
      output$card_1_4_1_detect <- renderText({card_1_4_1_detect_french})
      output$card_1_4_1_detect_score <- renderText({card_1_4_1_detect_score_french})
      output$card_1_4_1_filt_score <- renderText({card_1_4_1_filt_score_french})
      output$card_2_4_1 <- renderText({card_2_4_1_french})
      output$card_2_4_1_check <- renderText({card_2_2_check_french})
      output$card_2_4_1_run <- renderText({card_2_4_1_french})
      output$card_2_4_1_preview <- renderText({card_2_2_preview_french})

      output$card_1_4_2 <- renderText({card_1_4_2_french})
      output$card_1_4_2_samp_rate <- renderText({card_1_4_2_samp_rate_french})
      output$card_1_4_2_detect <- renderText({card_1_4_2_detect_french})
      output$card_1_4_2_detect_score <- renderText({card_1_4_2_detect_score_french})
      output$card_1_4_2_filt_score <- renderText({card_1_4_2_filt_score_french})
      output$card_2_4_2 <- renderText({card_2_4_2_french})
      output$card_2_4_2_check <- renderText({card_2_2_check_french})
      output$card_2_4_2_run <- renderText({card_2_4_2_french})
      output$card_2_4_2_preview <- renderText({card_2_2_preview_french})

      output$card_1_4_3 <- renderText({card_1_4_3_french})
      output$card_1_4_3_path <- renderText({card_1_4_3_path_french})
      output$general_merge_recieved <- renderText({general_merge_recieved_french()})
      output$recursive_out <- renderUI({card_1_4_3_recur_french})
      output$card_2_4_3 <- renderText({card_2_4_3_french})
      output$card_2_4_3_check <- renderText({card_2_2_check_french})
      output$card_2_4_3_run <- renderText({card_2_4_3_french})
      output$card_2_4_3_preview <- renderText({card_2_2_preview_french})

      output$card_1_5 <- renderText({card_1_5_french})
      output$card_1_5_proj <- renderUI({card_1_5_proj_french})
      output$card_1_5_dep_num <- renderUI({card_1_5_dep_num_french})
      output$card_1_5_detect <- renderUI({card_1_5_detect_french})
      output$card_1_5_event_count <- renderUI({card_1_5_event_count_french})
      output$card_1_5_out_path <- renderUI({card_1_5_out_path_french})
      output$card_1_5_tables_path <- renderUI({card_1_5_tables_path_french})
      output$selection_tables_folder_recieved <- renderText({selection_tables_folder_recieved_french()})
      output$card_1_5_zero_path <- renderText({card_1_5_zero_path_french})
      output$zero_selection_tables_folder_recieved <- renderText({zero_selection_tables_folder_recieved_french()})
      output$card_2_5 <- renderText({card_2_5_french})
      output$card_2_5_sound_include <- renderText({card_2_5_sound_include_french})
      output$card_2_5_sites_include <- renderText({card_2_5_sites_include_french})
      if(input$sound_check_include_in == "Yes"){
        output$card_2_5_sound_path <- renderText({card_2_5_sound_path_french})
        output$sound_check_folder_recieved <- renderText({sound_check_folder_recieved_french()})
      }
      output$card_2_5_sites <- renderText({card_2_5_sites_french})
      output$card_2_5_rand <- renderText({card_2_5_rand_french})
      output$card_2_5_min_hrs <- renderText({card_2_5_min_hrs_french})
      output$card_3_5 <- renderText({card_3_5_french})
      output$card_3_5_check <- renderText({card_2_2_check_french})
      output$card_3_5_run <- renderText({card_3_5_french})
      output$card_3_5_preview <- renderText({card_2_2_preview_french})

      output$tab_6_pill_1 <- renderText({tab_6_pill_1_french})
      output$card_1_6_1 <- renderText({card_1_6_1_french})
      output$card_1_6_1_path <- renderText({card_1_6_1_path_french})
      output$saved_plots_folder_recieved <- renderText({saved_plots_folder_recieved_french()})
      output$card_1_6_1_rds <- renderText({card_1_6_1_rds_french})
      output$card_1_6_1_run <- renderText({card_1_6_1_run_french})
      output$card_1_6_1_message <- renderText({card_1_6_1_message_french})
      output$card_2_6_1_plot <- renderUI({card_2_6_1_plot_french})
      output$card_2_6_1_plot_input <- renderText({card_2_6_1_plot_input_french})
      output$card_2_6_1_facet <- renderText({card_2_6_1_facet_french})
      output$card_2_6_1_night <- renderText({card_2_6_1_night_french})
      output$card_2_6_1_overlay <- renderText({card_2_6_1_overlay_french})
      output$card_2_6_1_download <- renderUI({card_2_6_1_download_french})
      output$card_2_6_1_format <- renderText({card_2_6_1_format_french})
      output$card_2_6_1_save <- renderText({card_2_6_1_save_french})

      output$tab_6_pill_2 <- renderText({tab_6_pill_2_french})
      output$card_1_6_2 <- renderText({card_1_6_1_french})
      output$card_1_6_2_path <- renderText({card_1_6_2_path_french})
      output$saved_tables_folder_recieved <- renderText({saved_tables_folder_recieved_french()})
      output$card_1_6_2_sites <- renderText({card_1_6_2_sites_french})
      output$card_1_6_2_run <- renderText({card_1_6_2_run_french})
      output$card_2_6_2_map <- renderUI({card_2_6_2_map_french})
      output$card_2_6_2_map_input <- renderText({card_2_6_2_map_input_french})
      output$card_2_6_2_year <- renderText({card_2_6_2_year_french})
      output$card_2_6_2_month <- renderText({card_2_6_2_month_french})
      output$card_2_6_2_save <- renderText({card_2_6_1_save_french})

      output$help_sound <- renderUI({help_sound_french()})
      output$help_exclude <- renderUI({help_exclude_french()})
      output$help_rumble <- renderUI({help_rumble_french()})
      output$help_gunshot <- renderUI({help_gunshot_french()})
      output$help_general <- renderUI({help_general_french()})
      output$help_data <- renderUI({help_data_french()})
      output$help_plots <- renderUI({help_plots_french()})
      output$help_maps <- renderUI({help_maps_french()})
      output$download_all_help <- renderUI({downloadButton("download_all_help_french", label = renderText({card_2_6_1_save_french}))})
    })

    #______________________________________
    ##### ABOUT #####
    #subtitle
    subtitle_eng <- "To facilitate the use of the elpR app"
    subtitle_french <- "Pour faciliter l'utilisation du package elpR"

    #tab labels
    tab_about_eng <- "About"
    tab_1_eng <- "1. Basic Info"
    tab_6_eng <- "6. Results!"
    tab_help_eng <- "HELP"

    tab_about_french <- "À Propos"
    tab_1_french <- "1. Informations de Base"
    tab_6_french <- "6. Résultats!"
    tab_help_french <- "AIDE"

    #English body
    header_welcome_eng <- HTML("<h2>Welcome!</h2>")
    text_welcome_eng <- HTML(
      "<p>This app was designed to help facilitate the use of the elpR package.
      The app was created using <a href = 'https://shiny.posit.co/'>Shiny</a>, a package used for building interactive web interfaces in R.
      <br/>
      <br/>The elpR R package contains tools for processing and analyzing elephant rumble detector output.
      This package was originally made at the <a href = 'https://www.elephantlisteningproject.org/'>Elephant Listening Project</a> for the team and their collaborators.
      <br/>
      <br/>The pages listed on the sidebar reflect the chronological order that the elpR functions should be used in.
      Visit the HELP page to learn more about each function.</p>")
    header_contributers_eng <- HTML("<h2>Contributers</h2>")
    text_contributers_eng <- HTML(
      "<p><b>Bobbi Estabrook</b>: Author and Creator of the elpR R package.
      <br/><b>Jidapa Janpathompong</b>: Creator of the elpR app.</p>"
    )

    #French body
    header_welcome_french <- HTML("<h2>Bienvenue!</h2>")
    text_welcome_french <- HTML(
      "<p>Cette application a été conçue pour faciliter l'utilisation du package elpR.
      Elle a été créée avec <a href = 'https://shiny.posit.co/'>Shiny</a>, un package permettant de développer des interfaces web interactives en R.
      <br/>
      <br/>Le package R elpR contient des outils pour le traitement et l'analyse des données issues du détecteur de grondements d'éléphants.
      Ce package a été initialement développé par l'équipe du projet <a href = 'https://www.elephantlisteningproject.org/'>Elephant Listening Project</a> pour ses collaborateurs.
      <br/>
      <br/>Les pages listées dans la barre latérale correspondent à l'ordre chronologique d'utilisation des fonctions d'elpR.
      Consultez la page d'aide pour en savoir plus sur chaque fonction.</p>")
    header_contributers_french <- HTML("<h2>Contributeurs</h2>")
    text_contributers_french <- HTML(
      "<p><b>Bobbi Estabrook</b>: Auteure et créatrice du package R elpR.
      <br/><b>Jidapa Janpathompong</b>: Créatrice de l’application elpR.</p>"
    )

    #______________________________________
    ##### definitions #####

    #needed for retrieving folder input paths
    volumes = getVolumes()()

    #______________________________________
    ##### 1. Basic Info #####

    #retrieve the user-selected parent directory
    shinyDirChoose(input, "parent_dir_in", roots = volumes)
    parent_dir_path <- reactive({
      pasted_path <- trimws(if (is.null(input$parent_dir_text_in)) "" else input$parent_dir_text_in)
      if (nzchar(pasted_path)) {
        normalizePath(pasted_path, winslash = "/", mustWork = FALSE)
      } else {
        parseDirPath(volumes, input$parent_dir_in)
      }
    })
    parent_dir_recieved_eng <- reactive({paste("Path received: ", parent_dir_path())})
    parent_dir_recieved_french <- reactive({paste("Chemin reçu: ", parent_dir_path())})

    #English & French translations
    card_1_1_eng <- "Input Basic Information"
card_1_1_parent_dir_eng <- HTML("<p><b>INPUT 1:</b> Choose the parent files_for_elpR folder</p>")
card_1_1_dep_name_eng <- HTML("<p><b>INPUT 1:</b> Deployment name</p>")
card_1_1_dep_num_eng <- HTML("<p><b>INPUT 2:</b> Deployment number</p>")
card_1_1_disk_ID_eng <- HTML("<p><b>INPUT 3:</b> Disk ID</p>")
card_1_1_sites_eng <- HTML("<p><b>INPUT 4:</b> Sites .txt file</p>")

card_1_1_french <- "Saisir des informations de base"
card_1_1_parent_dir_french <- HTML("<p><b>ENTRÉE 1:</b> Choisissez le dossier parent files_for_elpR</p>")
card_1_1_dep_name_french <- HTML("<p><b>ENTRÉE 1:</b> Nom du déploiement</p>")
card_1_1_dep_num_french <- HTML("<p><b>ENTRÉE 2:</b> Numéro de déploiement</p>")
card_1_1_disk_ID_french <- HTML("<p><b>ENTRÉE 3:</b> ID de disque</p>")
card_1_1_sites_french <- HTML("<p><b>ENTRÉE 4:</b> Fichier .txt des sites</p>")

#retrieve folder input path
shinyDirChoose(input, "parent_dir_in", roots = volumes)
parent_dir_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$parent_dir_in))})
parent_dir_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$parent_dir_in))})

    #create list of user inputs
    inputs_basic_info <- reactive({
      list(
        sites = input$sites_in$datapath,
        parent_dir = parseDirPath(volumes, input$parent_dir_in)
      )
    })

    #______________________________________
    ##### 2. Sound Check #####

    #English & French translations
    card_1_2_eng <- "Input Sound Check Information"
    card_1_2_sound_path_eng <- "Choose folder containing sound files: "
    card_1_2_file_dur_eng <- "File duration (minutes): "
    card_1_2_samp_rate_eng <- "Sample rate (Hz): "
    card_1_2_file_ext_eng <- "Sound file extention: "
    card_2_2_eng <- "Run Sound Check"
    card_2_2_check_eng <- "Check Your Info" #also used on other tabs
    card_2_2_run_eng <- "Run Sound Check"
    card_2_2_preview_eng <- "Results preview: " #also used on other tabs

    card_1_2_french <- "Saisir les Informations Sound Check"
    card_1_2_sound_path_french <- "Choisissez le dossier contenant les fichiers audio: "
    card_1_2_file_dur_french <- "Durée du fichier (minutes): "
    card_1_2_samp_rate_french <- "Sample rate (Hz): "
    card_1_2_file_ext_french <- "Fréquence d'échantillonnage (Hz): "
    card_2_2_french <- "Exécuter Sound Check"
    card_2_2_check_french <- "Vérifie tes Informations"
    card_2_2_run_french <- "Exécuter Sound Check"
    card_2_2_preview_french <- "Aperçu des résultats: "

    #retrieve folder input path
    shinyDirChoose(input, "sound_path_in", roots = volumes)
    sound_path_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$sound_path_in))})
    sound_path_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$sound_path_in))})

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
      req(parent_dir_path())
      result <- sound_check_function(
        x = parseDirPath(volumes, input$sound_path_in),
event_count_column = input$event_count_column_in,
selection_tables = parseDirPath(volumes, input$selection_tables_folder_in),
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

    #English & French translations
    card_1_3_eng <- "Input Exclude Files Information"
    card_1_3_path_eng <- "Choose folder to output extra sounds: "
    card_1_3_have_swift_eng <- "Do you have Swift files in the sounds folder?"
    card_1_3_merge_swift_eng <- "Do you want to merge the Swift files?"
    card_2_3_eng <- "Run Exclude Files"

    card_1_3_french <- "Saisir les Informations Exclude Files"
    card_1_3_path_french <- "Choisissez le dossier de destination des sons supplémentaires: "
    card_1_3_have_swift_french <- "Avez-vous des fichiers Swift dans le dossier des sons?"
    card_1_3_merge_swift_french <- "Souhaitez-vous fusionner les fichiers Swift?"
    card_2_3_french <- "Exécuter Exclude Files"

    #retrieve folder input path
    shinyDirChoose(input, "extra_sounds_in", roots = volumes)
    extra_sounds_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$extra_sounds_in))})
    extra_sounds_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$extra_sounds_in))})

    #render more inputs based on radio button selections
    ##RADIO BUTTON: have_swift_files
    observeEvent(input$have_swift_files_in, {
      if(input$have_swift_files_in == "Yes"){
        shinyjs::show("merge_swift_files_output")
        output$merge_swift_files_output <- renderUI({
          add_info(radioButtons("merge_swift_files_in", label = textOutput("card_1_3_merge_swift"),
                       choices = list("Yes", "No"), selected = "No"), "merge_swift_files")
        })
      }
      else if(input$have_swift_files_in == "No"){
        shinyjs::hide("merge_swift_files_output")
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
      req(parent_dir_path())
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
parent_dir = parent_dir_path(),
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

    #English & French translations
    tab_4_pills_eng <- "Choose type of data: "
    card_1_4_1_eng <- "Input Rumble Detector Information"
    card_1_4_1_samp_rate_eng <- "Sample rate: "
    card_1_4_1_rand_eng <- "Do you want 3 random days per week?"
    card_1_4_1_min_hrs_eng <- "Does your project require a minimum of 23 hours per day?"
    card_1_4_1_col_name_eng <- "Score column name: "
    card_1_4_1_detect_eng <- "Detector used: "
    card_1_4_1_detect_score_eng <- "Detector score: "
    card_1_4_1_filt_score_eng <- "Filter score: "
    card_2_4_1_eng <- "Run Rumble Restructure"

    tab_4_pills_french <- "Choisissez le type de données: "
    card_1_4_1_french <- "Saisir les Informations Rumble Detector"
    card_1_4_1_samp_rate_french <- "Taux d'échantillonnage: "
    card_1_4_1_rand_french <- "Voulez-vous 3 jours aléatoires par semaine?"
    card_1_4_1_min_hrs_french <- "Votre projet nécessite-t-il un minimum de 23 heures par jour?"
    card_1_4_1_col_name_french <- "Nom de la colonne de score: "
    card_1_4_1_detect_french <- "Détecteur utilisé: "
    card_1_4_1_detect_score_french <- "Score du détecteur: "
    card_1_4_1_filt_score_french <- "Score de filtre: "
    card_2_4_1_french <- "Exécuter Rumble Restructure"

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
      req(parent_dir_path())
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
parent_dir = parent_dir_path(),
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

    #English & French translations
    card_1_4_2_eng <- "Input Gunshot Detector Information"
    card_1_4_2_samp_rate_eng <- "Sample rate: "
    card_1_4_2_detect_eng <- "Detector used: "
    card_1_4_2_detect_score_eng <- "Detector score: "
    card_1_4_2_filt_score_eng <- "Filter score: "
    card_2_4_2_eng <- "Run Gunshot Restructure"

    card_1_4_2_french <- "Saisir les Informations Gunshot Detector"
    card_1_4_2_samp_rate_french <- "Taux d'échantillonnage: "
    card_1_4_2_detect_french <- "Détecteur utilisé: "
    card_1_4_2_detect_score_french <- "Score du détecteur: "
    card_1_4_2_filt_score_french <- "Score de filtre: "
    card_2_4_2_french <- "Exécuter Gunshot Restructure"

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
      req(parent_dir_path())
      result <- restructure_gunshot_function(
parent_dir = parent_dir_path(),
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

    #English & French translations
    card_1_4_3_eng <- "Input General Information"
    card_1_4_3_path_eng <- "Choose folder containing selection tables to merge: "
    card_1_4_3_recur_eng <- radioButtons("recursive_in", label = "How are the selection tables organized?",
                                         choices = list("In one folder", "In multiple subfolders"))
    card_2_4_3_eng <- "Run General Restructure"

    card_1_4_3_french <- "Saisir les Informations General"
    card_1_4_3_path_french <- "Choisissez le dossier contenant les tables de sélection à fusionner: "
    card_1_4_3_recur_french <- radioButtons("recursive_in", label = "Comment les tableaux de sélection sont-ils organisés?",
                                         choices = list("Dans un dossier", "Dans plusieurs sous-dossiers"))
    card_2_4_3_french <- "Exécuter General Restructure"

    #retrieve folder input path
    shinyDirChoose(input, "general_merge_in", roots = volumes)
    general_merge_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$general_merge_in))})
    general_merge_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$general_merge_in))})

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
      if(input$recursive_in == "In one folder" | input$recursive_in == "Dans un dossier"){
        recursive_in_value = FALSE
      } else if(input$recursive_in == "In multiple subfolders" | input$recursive_in == "Dans plusieurs sous-dossiers"){
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

    #English & French translations
    card_1_5_eng <- "Input Data Summaries Information"
card_1_5_proj_eng <- HTML("<p><b>INPUT 1:</b> Project name</p>")
card_1_5_dep_num_eng <- HTML("<p><b>INPUT 2:</b> Deployment number(s)</p>")
card_1_5_detect_eng <- HTML("<p><b>INPUT 3:</b> Detector used</p>")
card_1_5_event_count_eng <- HTML("<p><b>INPUT 4:</b> Event-count column name</p>")
card_1_5_tables_path_eng <- HTML("<p><b>INPUT 5:</b> Choose folder containing selection tables</p>")
card_1_5_zero_path_eng <- HTML("<p><b>INPUT 6:</b> Choose folder containing zero-day selection tables</p>")
    card_2_5_eng <- "Input OPTIONAL Data Summaries Information"
    card_2_5_sound_include_eng <- "Do you want to include a sound check file?"
    card_2_5_sound_path_eng <- "Choose folder containing sound check files: "
    card_2_5_sites_include_eng <- "Do you only want to include sites listed in a sites file?"
    card_2_5_sites_eng <- "Sites .txt file with latitude and longitude: "
    card_2_5_rand_eng <- "Do you need random dates?"
    card_2_5_min_hrs_eng <- "Do you want to exclude sounds <23 hours?"
    card_3_5_eng <- "Run Data Summaries"

    card_1_5_french <- "Saisir les Informations Data Summaries"
card_1_5_proj_french <- HTML("<p><b>ENTRÉE 1:</b> Nom du projet</p>")
card_1_5_dep_num_french <- HTML("<p><b>ENTRÉE 2:</b> Numéro(s) de déploiement</p>")
card_1_5_detect_french <- HTML("<p><b>ENTRÉE 3:</b> Détecteur utilisé</p>")
card_1_5_event_count_french <- HTML("<p><b>ENTRÉE 4:</b> Nom de la colonne de comptage des événements</p>")
card_1_5_tables_path_french <- HTML("<p><b>ENTRÉE 5:</b> Choisissez le dossier contenant les tables de sélection</p>")
card_1_5_zero_path_french <- HTML("<p><b>ENTRÉE 6:</b> Choisissez le dossier contenant les tables de sélection zero-day</p>")
    card_2_5_french <- "Saisir les informations facultatives Data Summaries"
    card_2_5_sound_include_french <- "Souhaitez-vous inclure un fichier de Sound Check?"
    card_2_5_sound_path_french <- "Choisissez le dossier contenant les fichiers de Sound Check: "
    card_2_5_sites_include_french <- "Souhaitez-vous inclure uniquement les sites répertoriés dans un fichier de sites?"
    card_2_5_sites_french <- "Fichier .txt des sites contenant la latitude et la longitude: "
    card_2_5_rand_french <- "Avez-vous besoin de dates aléatoires?"
    card_2_5_min_hrs_french <- "Souhaitez-vous exclure les sons de moins de 23 heures?"
    card_3_5_french <- "Exécuter Data Summaries"

    #retrieve folder input paths
    shinyDirChoose(input, "summary_folder_in", roots = volumes)
    summary_folder_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$summary_folder_in))})
    summary_folder_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$summary_folder_in))})

    shinyDirChoose(input, "selection_tables_folder_in", roots = volumes)
    selection_tables_folder_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$selection_tables_folder_in))})
    selection_tables_folder_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$selection_tables_folder_in))})

    shinyDirChoose(input, "zero_selection_tables_folder_in", roots = volumes)
    zero_selection_tables_folder_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$zero_selection_tables_folder_in))})
    zero_selection_tables_folder_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$zero_selection_tables_folder_in))})

    #render more inputs based on radio button selections
    ##RADIO BUTTON: sound_check_include
    observeEvent(input$sound_check_include_in, {
      if(input$sound_check_include_in == "Yes"){
        shinyjs::show("card_2_5_sound_path")
        shinyjs::show("sound_check_include_output")
        shinyjs::show("sound_check_folder_recieved")
        output$sound_check_include_output <- renderUI({
          add_info(shinyDirButton(id = "sound_check_folder_in", label = NULL, icon = icon("folder-open"), title = "Choose folder containing sound check files"), "sound_checks")
        })
      } else if(input$sound_check_include_in == "No"){
        shinyjs::hide("card_2_5_sound_path")
        shinyjs::hide("sound_check_include_output")
        shinyjs::hide("sound_check_folder_recieved")
      }
    })
    shinyDirChoose(input, "sound_check_folder_in", roots = volumes)
    sound_check_folder_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$sound_check_folder_in))})
    sound_check_folder_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$sound_check_folder_in))})

    ##RADIO BUTTON: use_only_sites_provided
    observeEvent(input$use_only_sites_provided_in, {
      if(input$use_only_sites_provided_in == "Yes"){
        shinyjs::show("use_only_sites_provided_output")
        output$use_only_sites_provided_output <- renderUI({
          add_info(fileInput("sites_lat_long_in", label = textOutput("card_2_5_sites")), "site_late_long")
        })
      } else{
        shinyjs::hide("use_only_sites_provided_output")
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
event_count_column = input$event_count_column_in,
selection_tables = parseDirPath(volumes, input$selection_tables_folder_in),
        zero_txt = parseDirPath(volumes, input$zero_selection_tables_folder_in),
        sound_check_include = input$sound_check_include_in,
        sound_checks = sound_check_folder_value,
        use_only_sites_provided = input$use_only_sites_provided_in,
        site_lat_long = sites_lat_long_value,
        rand_dates_needed = input$rand_dates_needed_in,
        events_bad_sound_remove = input$events_bad_sound_remove_in
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
      req(parent_dir_path())
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
      if(input$events_bad_sound_remove_in == "Yes"){
        events_bad_sound_remove_value = "y"
      } else if(input$events_bad_sound_remove_in == "No"){
        events_bad_sound_remove_value = "n"
      }

      result <- data_summaries_function(
parent_dir = parent_dir_path(),
project_name = input$project_name_in,
deployment_num = input$deployment_nums_in,
detector_name = input$summary_detector_in,
event_count_column = input$event_count_column_in,
selection_tables = parseDirPath(volumes, input$selection_tables_folder_in),
        zero_txt = parseDirPath(volumes, input$zero_selection_tables_folder_in),
        sound_check_include = sound_check_include_value,
        sound_checks = sound_check_folder_value,
        use_only_sites_provided = use_only_sites_provided_value,
        site_lat_long = sites_lat_long_value,
        rand_dates_needed = rand_dates_needed_value,
        events_bad_sound_remove = events_bad_sound_remove_value
      )
      output$run_summary_output <- renderPrint({
        result
      })
    })

    #______________________________________
    ##### 6. Results! #####

    ###### > plots ######

    #English & French translations
    tab_6_pill_1_eng <- "Plots"
    card_1_6_1_eng <- "Input File Information"
    card_1_6_1_path_eng <- "Choose folder containing .Rds file with saved plots: "
    card_1_6_1_rds_eng <- "Name of .Rds file: "
    card_1_6_1_run_eng <- "Load Plots"
    card_1_6_1_message_eng <- "Loading plots..."
    card_2_6_1_plot_eng <- HTML("<p><b>Plot Options</b></p>")
    card_2_6_1_plot_input_eng <- "Select plot to view: "
    card_2_6_1_facet_eng <- "Categorize by: "
    card_2_6_1_night_eng <- "Annotate for: "
    card_2_6_1_overlay_eng <- "Display: "
    card_2_6_1_download_eng <- HTML("<p><b>Download Options</b></p>")
    card_2_6_1_format_eng <- "File format to download: "
    card_2_6_1_save_eng <- "Download"

    tab_6_pill_1_french <- "Graphiques"
    card_1_6_1_french <- "Informations sur le fichier d'entrée"
    card_1_6_1_path_french <- "Choisissez le dossier contenant le fichier .Rds: "
    card_1_6_1_rds_french <- "Nom du fichier .Rds: "
    card_1_6_1_run_french <- "Charger des Graphiques"
    card_1_6_1_message_french <- "Chargement des graphiques..."
    card_2_6_1_plot_french <- HTML("<p><b>Options de Graphique</b></p>")
    card_2_6_1_plot_input_french <- "Sélectionnez le graphique à afficher: "
    card_2_6_1_facet_french <- "Catégoriser par: "
    card_2_6_1_night_french <- "Annoter pour: "
    card_2_6_1_overlay_french <- "Afficher: "
    card_2_6_1_download_french <- HTML("<p><b>Options de Téléchargement</b></p>")
    card_2_6_1_format_french <- "Format de fichier à télécharger: "
    card_2_6_1_save_french <- "Télécharger"

    #retrieve folder input path
    shinyDirChoose(input, "saved_plots_folder_in", roots = volumes)
    saved_plots_folder_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$saved_plots_folder_in))})
    saved_plots_folder_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$saved_plots_folder_in))})

    #load plots from data summaries
    saved_plots <- reactiveVal({})
    observeEvent(input$load_plots, {
req(parent_dir_path())
#load plots into environment from user inputted file path
saved_plots(
  readRDS(paste0(parent_dir_path(),
                 "/data_summaries/summary_plots/",
                 input$project_name_in,
                 "_dep",input$deployment_nums_in,
                 "_",input$summary_detector_in,
                 "_saved_plots.Rds"))
)
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
            label = textOutput("card_2_6_1_plot_input"),
            choices = list("Total Rumbles",
                           "Night vs Day Rumbles",
                           "Rumbles per Hour",
                           "Rumbles per Day",
                           "Rumbles per Week",
                           "Rumbles per Month"),
            selected = NULL)
        )
      )
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
            selectInput("facet_wrap_input", label = textOutput("card_2_6_1_facet"),
              choices = list("Default", "Site", "Strata", "Vegetation Class"), selected = "Default")
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
            selectInput("night_annot_input", label = textOutput("card_2_6_1_night"),
              choices = list("Default", "Night Rumbles"), selected = "Default")
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
            selectInput("overlay_input", label = textOutput("card_2_6_1_overlay"),
              choices = list("Default", "Overlay"), selected = "Default")
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

    #English & French translations
    tab_6_pill_2_eng <- "Maps"
    card_1_6_2_path_eng <- "Choose folder containing data summary tables: "
    card_1_6_2_sites_eng <- "Sites .txt file: "
    card_1_6_2_run_eng <- "Load Tables"
    card_2_6_2_map_eng <- HTML("<p><b>Map Options</b></p>")
    card_2_6_2_map_input_eng <- "Map average daily rumbles by: "
    card_2_6_2_year_eng <- "View year: "
    card_2_6_2_month_eng <- "View month: "

    tab_6_pill_2_french <- "Cartes"
    card_1_6_2_path_french <- "Choisissez le dossier contenant les tables data summaries: "
    card_1_6_2_sites_french <- "Fichier .txt des sites: "
    card_1_6_2_run_french <- "Charger les Tableaux"
    card_2_6_2_map_french <- HTML("<p><b>Options de Carte</b></p>")
    card_2_6_2_map_input_french <- "Carte moyenne quotidienne rumbles par"
    card_2_6_2_year_french <- "Voir l'année: "
    card_2_6_2_month_french <- "Afficher le mois: "

    #retrieve folder input path
    shinyDirChoose(input, "saved_tables_folder_in", roots = volumes)
    saved_tables_folder_recieved_eng <- reactive({paste("Path recieved: ", parseDirPath(volumes, input$saved_tables_folder_in))})
    saved_tables_folder_recieved_french <- reactive({paste("Chemin reçu: ", parseDirPath(volumes, input$saved_tables_folder_in))})

    #load plots from data summaries
    tables_for_mapping <- reactiveVal(list())
    observeEvent(input$load_tables, {
req(parent_dir_path())
req(input$site_lat_long_map)

#run maps_function
result <- tryCatch(
  maps_function(
    folder_path = paste0(parent_dir_path(),
                         "/data_summaries/summary_tables/"),
    site_lat_long = input$site_lat_long_map$datapath
  ),
  error = function(error) {
    message("Maps failed: ", conditionMessage(error))
    showNotification(
      paste("Maps failed:", conditionMessage(error)),
      type = "error",
      duration = NULL
    )
    NULL
  }
)
req(result)
maps_function_output(result)

      #display drop-down menu for choosing maps
      removeUI(selector = "#map_input_div", multiple = TRUE, immediate = TRUE)
      insertUI(
        selector = "#placeholder2",
        where = "beforeBegin",
        ui = div(
          id = "map_input_div",
          selectInput("map_input", label = textOutput("card_2_6_2_map_input"),
            choices = list("Default", "Strata", "Vegetation Class", "Year", "Month"), selected = NULL)
        )
      )
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

req(maps_function_output())
req(maps_function_output()$event_year_avg)

ele_year_avg <- maps_function_output()$event_year_avg
        years_vector <- sort(unique(ele_year_avg$Year))

        insertUI(
          selector = "#placeholder2",
          where = "beforeBegin",
          ui = div(
            id = "year_slider_input_div",
            sliderInput("year_slider_input", label = textOutput("card_2_6_2_year"),
              min = min(years_vector), max = max(years_vector), value = min(years_vector), step = 1)
          )
        )
      }
      if(input$map_input == "Month"){

req(maps_function_output())
req(maps_function_output()$event_month_avg)

ele_month_avg <- maps_function_output()$event_month_avg
        months_vector <- sort(unique(ele_month_avg$Month))

        insertUI(
          selector = "#placeholder2",
          where = "beforeBegin",
          ui = div(
            id = "month_slider_input_div",
            sliderInput("month_slider_input", label = textOutput("card_2_6_2_month"),
              min = min(months_vector), max = max(months_vector), value = min(months_vector), step = 1)
          )
        )
      }
    })

    #select maps based on drop-down menu selections
    display_this_map <- reactive({
ele_site_avg <- maps_function_output()$event_site_avg
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
ele_year_avg <- maps_function_output()$event_year_avg
color_by_Events <- colorNumeric(palette = "viridis",
                                         domain = range(ele_year_avg$`Avg Events`))

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
            radius = ~sqrt(`Avg Events`) * 8,  # Adjust the multiplier to change circle sizes
            popup = ~paste("Site:", Site, "<br>Avg Events:", `Avg Events`),
            color = ~color_by_Events(`Avg Events`),
            fillOpacity = 0.7,
            group = paste(input$year_slider_input)
          )

        #View map
        m
      } else if(input$map_input == "Month"){
ele_month_avg <- maps_function_output()$event_month_avg
color_by_Events <- colorNumeric(palette = "viridis",
                                         domain = range(ele_month_avg$`Avg Events`))

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
            radius = ~sqrt(`Avg Events`) * 8,  # Adjust the multiplier to change circle sizes
            popup = ~paste("Site:", Site,
                           "<br>Avg Events:", `Avg Events`),
            color = ~color_by_Events(`Avg Events`),
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
maps_function_output()$event_site_avg
      } else if(input$map_input == "Strata"){ 
        maps_function_output()$event_site_avg
      } else if(input$map_input == "Vegetation Class"){ 
        maps_function_output()$event_site_avg
      } else if(input$map_input == "Year"){ 
        maps_function_output()$event_year_avg
      } else if(input$map_input == "Month"){ 
        maps_function_output()$event_month_avg
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

    #English & French translations
    help_sound_eng <- reactive({accordion_panel(
      "What does \"Sound Check\" do?",
      downloadButton("download_sound_check_help", label = NULL),
      htmlOutput("sound_check_documentation"))})
    help_exclude_eng <- reactive({accordion_panel(
      "What does \"Exclude Files\" do?",
      downloadButton("download_exclude_files_help", label = NULL),
      htmlOutput("exclude_files_documentation"))})
    help_rumble_eng <- reactive({accordion_panel(
      "What does \"Restructure\" for Rumbles do?",
      downloadButton("download_restructure_help", label = NULL),
      htmlOutput("restructure_documentation"))})
    help_gunshot_eng <- reactive({accordion_panel(
      "What does \"Restructure\" for Gunshots do?",
      downloadButton("download_gun_restructure_help", label = NULL),
      htmlOutput("gun_restructure_documentation"))})
    help_general_eng <- reactive({accordion_panel(
      "What does \"Restructure\" for General do?",
      downloadButton("download_general_restructure_help", label = NULL),
      htmlOutput("general_restructure_documentation"))})
    help_data_eng <- reactive({accordion_panel(
      "What does \"Data Summaries\" do?",
      downloadButton("download_summary_help", label = NULL),
      htmlOutput("summary_documentation"))})
    help_plots_eng <- reactive({accordion_panel(
      "What does \"Results!\" for Plots do?",
      downloadButton("download_plots_help_eng", label = NULL),
      htmlOutput("plots_documentation_eng"))})
    help_maps_eng <- reactive({accordion_panel(
      "What does \"Results!\" for Maps do?",
      downloadButton("download_maps_help_eng", label = NULL),
      htmlOutput("maps_documentation_eng"))})

    help_sound_french <- reactive({accordion_panel(
      "Que fait \"Sound Check\"?",
      downloadButton("download_sound_check_help", label = NULL),
      htmlOutput("sound_check_documentation"))})
    help_exclude_french <- reactive({accordion_panel(
      "Que fait \"Exclude Sounds\"?",
      downloadButton("download_exclude_files_help", label = NULL),
      htmlOutput("exclude_files_documentation"))})
    help_rumble_french <- reactive({accordion_panel(
      "Que fait \"Restructure\" pour Rumbles?",
      downloadButton("download_restructure_help", label = NULL),
      htmlOutput("restructure_documentation"))})
    help_gunshot_french <- reactive({accordion_panel(
      "Que fait \"Restructure\" pour Gunshots?",
      downloadButton("download_gun_restructure_help", label = NULL),
      htmlOutput("gun_restructure_documentation"))})
    help_general_french <- reactive({accordion_panel(
      "Que fait \"Restructure\" pour General?",
      downloadButton("download_general_restructure_help", label = NULL),
      htmlOutput("general_restructure_documentation"))})
    help_data_french <- reactive({accordion_panel(
      "Que fait \"Data Summaries\"?",
      downloadButton("download_summary_help", label = NULL),
      htmlOutput("summary_documentation"))})
    help_plots_french <- reactive({accordion_panel(
      "Que fait \"Résultats!\" pour Graphiques?",
      downloadButton("download_plots_help_french", label = NULL),
      htmlOutput("plots_documentation_french"))})
    help_maps_french <- reactive({accordion_panel(
      "Que fait \"Résultats!\" pour Cartes?",
      downloadButton("download_maps_help_french", label = NULL),
      htmlOutput("maps_documentation_french"))})

    rd_path <- function(filename) {
      installed_path <- system.file("man", filename, package = "elpR2")
      if (nzchar(installed_path) && file.exists(installed_path)) {
        installed_path
      } else {
        local_path <- file.path("man", filename)
        if (!file.exists(local_path)) {
          stop("Documentation file not found: ", filename)
        }
        local_path
      }
    }

    #sound_check_documentation
    sound_check_help <- reactive({
      rd = rd_path("sound_check_function.Rd")
      temp_html = tempfile(fileext = ".html")
      Rd2HTML(rd, out = temp_html)
      list(
        html_path = temp_html,
        html_content = HTML(read_file(temp_html))
      )
    })
    output$download_sound_check_help <- downloadHandler(
      filename = function(){
        "sound_check_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = sound_check_help()$html_path, file = file)
      }
    )
    output$sound_check_documentation <- renderText({
      sound_check_help()$html_content
    })

    #exclude_files_documentation
    exclude_files_help <- reactive({
      rd = rd_path("sound_exclude_function.Rd")
      temp_html = tempfile(fileext = ".html")
      Rd2HTML(rd, out = temp_html)
      list(
        html_path = temp_html,
        html_content = HTML(read_file(temp_html))
      )
    })
    output$download_exclude_files_help <- downloadHandler(
      filename = function(){
        "exclude_files_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = exclude_files_help()$html_path, file = file)
      }
    )
    output$exclude_files_documentation <- renderText({
      exclude_files_help()$html_content
    })

    #restructure_documentation
    restructure_help <- reactive({
      rd = rd_path("restructure_rumble_function.Rd")
      temp_html = tempfile(fileext = ".html")
      Rd2HTML(rd, out = temp_html)
      list(
        html_path = temp_html,
        html_content = HTML(read_file(temp_html))
      )
    })
    output$download_restructure_help <- downloadHandler(
      filename = function(){
        "rumble_restructure_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = restructure_help()$html_path, file = file)
      }
    )
    output$restructure_documentation <- renderText({
      restructure_help()$html_content
    })

    #gun_restructure_documentation
    gun_restructure_help <- reactive({
      rd = rd_path("restructure_gunshot_function.Rd")
      temp_html = tempfile(fileext = ".html")
      Rd2HTML(rd, out = temp_html)
      list(
        html_path = temp_html,
        html_content = HTML(read_file(temp_html))
      )
    })
    output$download_gun_restructure_help <- downloadHandler(
      filename = function(){
        "gunshot_restructure_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = gun_restructure_help()$html_path, file = file)
      }
    )
    output$gun_restructure_documentation <- renderText({
      gun_restructure_help()$html_content
    })

    #general_restructure_documentation
    general_restructure_help <- reactive({
      rd = rd_path("restructure_general_function.Rd")
      temp_html = tempfile(fileext = ".html")
      Rd2HTML(rd, out = temp_html)
      list(
        html_path = temp_html,
        html_content = HTML(read_file(temp_html))
      )
    })
    output$download_general_restructure_help <- downloadHandler(
      filename = function(){
        "general_restructure_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = general_restructure_help()$html_path, file = file)
      }
    )
    output$general_restructure_documentation <- renderText({
      general_restructure_help()$html_content
    })

    #summary_documentation
    summary_help <- reactive({
      rd = rd_path("data_summaries_function.Rd")
      temp_html = tempfile(fileext = ".html")
      Rd2HTML(rd, out = temp_html)
      list(
        html_path = temp_html,
        html_content = HTML(read_file(temp_html))
      )
    })
    output$download_summary_help <- downloadHandler(
      filename = function(){
        "data_summaries_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = summary_help()$html_path, file = file)
      }
    )
    output$summary_documentation <- renderText({
      summary_help()$html_content
    })

    #plots_documentation
    plots_help <- reactive({
      temp_html_eng = tempfile(fileext = ".html")
      temp_html_french = tempfile(fileext = ".html")
      html_content_eng = HTML(
        "<h2>Plot Results</h2>
        <h3>Description</h3>
        <p>This page displays the plots generated by the Data Summaries function.
        First, select the folder containing your Data Summaries output, and load in the data.
        Next, use the options on the sidebar to view and download the plots.</p>
        <h3>Inputs</h3>
        <ul><li>Folder containing the .rds file generated by the Data Summaries function.</li></ul>
        <h3>Outputs</h3>
        <ul><li>Plots in the .rds file generated by the Data Summaries function.
        (See \"What does \'Data Summaries\' do?\" for more details.)</li></ul>
        <h3>Author(s)</h3>
        <p>Jidapa Janpathompong</p>")
      html_content_french = HTML(
        "<h2>Résultats du Graphique</h2>
        <h3>Description</h3>
        <p>Cette page affiche les graphiques générés par la fonction Data Summaries.
        Tout d'abord, sélectionnez le dossier contenant les données générées par la fonction Data Summaries, puis chargez-y les données.
        Ensuite, utilisez les options de la barre latérale pour visualiser et télécharger les graphiques.</p>
        <h3>Entrées</h3>
        <ul><li>Dossier contenant le fichier .rds généré par la fonction Data Summaries.</li></ul>
        <h3>Sorties</h3>
        <ul><li>Graphiques du fichier .rds généré par la fonction Data Summaries.
        (Pour plus de détails, consultez la section \"Que fait \'Data Summaries\'?\")</li></ul>
        <h3>Auteurs</h3>

        <p>Jidapa Janpathompong</p>")
      cat(html_content_eng, file = temp_html_eng)
      cat(html_content_french, file = temp_html_french)
      list(
        html_path_eng = temp_html_eng,
        html_path_french = temp_html_french,
        html_content_eng = html_content_eng,
        html_content_french = html_content_french
      )
    })
    output$download_plots_help_eng <- downloadHandler(
      filename = function(){
        "plots_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = plots_help()$html_path_eng, file = file)
      }
    )
    output$download_plots_help_french <- downloadHandler(
      filename = function(){
        "plots_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = plots_help()$html_path_french, file = file)
      }
    )
    output$plots_documentation_eng <- renderText({
      plots_help()$html_content_eng
    })
    output$plots_documentation_french <- renderText({
      plots_help()$html_content_french
    })

    #maps_documentation
    maps_help <- reactive({
temp_html_eng = tempfile(fileext = ".html")
temp_html_french = tempfile(fileext = ".html")
html_content_eng = HTML(
  "<h2>Map Results</h2>
  <h3>Description</h3>
  <p>This page displays maps using data generated by the Data Summaries function.
  First, select the folder containing your Data Summaries output, and load in the data.
  Next, use the options on the sidebar to view and download the maps.</p>
  <h3>Inputs</h3>
  <ul><li>Folder containing the data tables generated by the Data Summaries function.</li>
  <li>A .txt file listing sites in data.</li></ul>
  <h3>Outputs</h3>
  <ul>
    <li>Maps using data generated by the Data Summaries function. Specifically uses:
    <ul>
      <li>*Rumbles_Site_Weekly_Summaries.txt</li>
      <li>*Rumbles_ZeroDays_soundExcluded_3randDaysOnly_MonthlyMean_Site.txt</li>
    </ul></li>
  </ul>
  <h3>Author(s)</h3>
  <p>Jidapa Janpathompong</p>")
html_content_french = HTML(
  "<h2>Résultats de la Carte</h2>
  <h3>Description</h3>
  <p>Cette page affiche des cartes réalisées à partir des données générées par la fonction Data Summaries.
  Sélectionnez d'abord le dossier contenant les données générées par la fonction Data Summaries, puis chargez-y les données.
  Ensuite, utilisez les options de la barre latérale pour visualiser et télécharger les cartes.</p>
  <h3>Entrées</h3>
  <ul><li>Dossier contenant les tableaux de données générés par la fonction Data Summaries.</li>
  <li>Un fichier .txt listant les sites dans les données.</li></ul>
  <h3>Sorties</h3>
  <ul>
    <li>Cartes utilisant les données générées par la fonction Data Summaries. Utilise plus précisément:
    <ul>
      <li>*Rumbles_Site_Weekly_Summaries.txt</li>
      <li>*Rumbles_ZeroDays_soundExcluded_3randDaysOnly_MonthlyMean_Site.txt</li>
    </ul></li>
  </ul>
  <h3>Auteurs</h3>
  <p>Jidapa Janpathompong</p>")
cat(html_content_eng, file = temp_html_eng)
cat(html_content_french, file = temp_html_french)
      list(
        html_path_eng = temp_html_eng,
        html_path_french = temp_html_french,
        html_content_eng = html_content_eng,
        html_content_french = html_content_french
      )
    })
    output$download_maps_help_eng <- downloadHandler(
      filename = function(){
        "maps_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = maps_help()$html_path_eng, file = file)
      }
    )
    output$download_maps_help_french <- downloadHandler(
      filename = function(){
        "maps_help.pdf"
      },
      content = function(file){
        webshot2::webshot(url = maps_help()$html_path_french, file = file)
      }
    )
    output$maps_documentation_eng <- renderText({
      maps_help()$html_content_eng
    })
    output$maps_documentation_french <- renderText({
      maps_help()$html_content_french
    })

    #all documentation
    all_help <- reactive({
      combined_html <- str_c(
        HTML(
          "<h1>ElpR App HELP Documents</h1>
          <h2>Table of Contents</h2>
          <ul>
            <li>Sound Check</li>
            <li>Exclude Bad Sounds</li>
            <li>Rumble Selection Table Restructure</li>
            <li>Gunshot Selection Table Restructure</li>
            <li>Merge Selection Tables</li>
            <li>Data Summaries</li>
            <li>Plot Results</li>
            <li>Map Results</li>
          </ul>"
        ),
        sound_check_help()$html_content,
        exclude_files_help()$html_content,
        restructure_help()$html_content,
        gun_restructure_help()$html_content,
        general_restructure_help()$html_content,
        summary_help()$html_content,
        sep = HTML("<div style='page-break-after: always;'></div>")
      )
      combined_html_eng <- str_c(
        combined_html,
        plots_help()$html_content_eng,
        maps_help()$html_content_eng,
        sep = HTML("<div style='page-break-after: always;'></div>")
      )
      combined_html_french <- str_c(
        combined_html,
        plots_help()$html_content_french,
        maps_help()$html_content_french,
        sep = HTML("<div style='page-break-after: always;'></div>")
      )
      temp_html_eng = tempfile(fileext = ".html")
      temp_html_french = tempfile(fileext = ".html")
      cat(combined_html_eng, file = temp_html_eng)
      cat(combined_html_french, file = temp_html_french)
      list(
        html_path_eng = temp_html_eng,
        html_path_french = temp_html_french,
        html_content_eng = HTML(read_file(temp_html_eng)),
        html_content_french = HTML(read_file(temp_html_french))
      )
    })
    output$download_all_help_eng <- downloadHandler(
      filename = function(){
        "help_documentation.pdf"
      },
      content = function(file){
        webshot2::webshot(url = all_help()$html_path_eng, file = file)
      }
    )
    output$download_all_help_french <- downloadHandler(
      filename = function(){
        "help_documentation.pdf"
      },
      content = function(file){
        webshot2::webshot(url = all_help()$html_path_french, file = file)
      }
    )

  }

  #_______________________________________________________________________________
  #### RUN THE APP ####
  shinyApp(ui = ui, server = server)
  # app_object <- shinyApp(ui = ui, server = server)
  # runApp(appDir = app_object, launch.browser = TRUE)
}

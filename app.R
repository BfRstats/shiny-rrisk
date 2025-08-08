library(shiny)
#shinyjs
#DT
#visNetwork
#shinyAce
#igraph
#ggplot2
#rmarkdown
#knitr
#flextable
#R6

shiny_rrisk_version <- "alpha v18.4"

source("rrisk/rriskModelClass.R", chdir = TRUE)
source("rrisk/rrisk_distributions/rrisk_distributions_plot_pdf.R")

source("shiny_rrisk_modules/handling_menu_bar/menu_bar_ui.R")
source("shiny_rrisk_modules/handling_menu_bar/menu_bar_server.R")
source("shiny_rrisk_modules/handling_menu_bar/menu_bar_utils.R")

source("shiny_rrisk_modules/handling_nodes/nodes_ui.R")
source("shiny_rrisk_modules/handling_nodes/nodes_server.R")
source("shiny_rrisk_modules/handling_nodes/nodes_utils.R")

source("shiny_rrisk_modules/handling_global_objects/global_objects_ui.R")
source("shiny_rrisk_modules/handling_global_objects/global_objects_server.R")
source("shiny_rrisk_modules/handling_global_objects/global_objects_utils.R")

source("shiny_rrisk_modules/handling_simulation_run/simulation_run_ui.R")
source("shiny_rrisk_modules/handling_simulation_run/simulation_run_server.R")
source("shiny_rrisk_modules/handling_simulation_run/simulation_run_utils.R")

source("shiny_rrisk_modules/handling_documentation/documentation_ui.R")
source("shiny_rrisk_modules/handling_documentation/documentation_server.R")
source("shiny_rrisk_modules/handling_documentation/documentation_utils.R")

source("utility_functions/set_parametric_dist_choices.R")
source("utility_functions/show_disclaimer_manual_and_contact_dialog.R")
source("utility_functions/get_shiny_R6_class.R")
source("utility_functions/try_cast_as_numeric.R")
source("utility_functions/cast_csv_as_vector.R")
source("utility_functions/do_efsa_eke_fit.R")
source("utility_functions/build_action_buttons.R")
source("utility_functions/parse_action_buttons.R")
source("utility_functions/remove_tabs.R")
source("utility_functions/read_rrisk_dist_export_file.R")
source("utility_functions/get_proper_file_name.R")

source("myShinyAlert/myShinyAlert.R", chdir = TRUE)
source("myShinyAlert/utils.R", chdir = TRUE)

#===============================================================================
ui <- fluidPage(
  shinyjs::useShinyjs(),  # Set up shinyjs
  # MENU BAR
  menu_bar_ui(shiny_rrisk_version),
  htmltools::br(), 
  htmltools::br(), 
  htmltools::br(),
  # MODEL NAME
  textAreaInput(
    inputId = "text_model_name",
    label   = "Model name",
    value   = "no model name",
    width   = "500px",
    height  = "5ch"),
  htmltools::hr(),
  # ACTION PANELS
  tabsetPanel(
    id = "main_tab",
    tabPanel( # MODEL CREATION
      title = htmltools::HTML("Model Creation"),
      value = "p1",
      htmltools::hr(),
      # interactive DAG of model and interactive model table
      nodes_ui(),
      htmltools::hr()
    ),
    tabPanel( # DEFINE GLOBAL OBJECTS
      title = htmltools::HTML("Fixed Parameters & Functions"),
      value = "p2",
      htmltools::hr(),
      # table with global objects, such as functions or parameter
      global_objects_ui(),
      htmltools::hr()
    ),
    tabPanel( # SIMULATION AND RESULTS
      title = htmltools::HTML("Simulation"),
      value = "p3", 
      htmltools::hr(),
      # for simulation run
      simulation_run_ui(),
      htmltools::hr()
    ),
    tabPanel( # ADDITIONAL DOCUMENTATIONS
      title = htmltools::HTML("Documentation and Reporting"),
      value = "p4",
      htmltools::hr(),
      # ui elements for documentaion
      documentation_ui(),
      htmltools::hr()
    ),
  )
)

server <- function(input, output, session) 
{
  # get an instance of a shiny compatible rriskModelClass
  # 1.) get shinyRriskClass by extending rriskModelClass. shiny needs
  #     a reactive object. The Function get_shiny_R6_class() adds that.
  # 2.) instantiate a shinyRriskClass object with $new()
  # 3.) activate reactivity of this object with $reactive()
  rrisk_model <- get_shiny_R6_class(
    class_name   = "shinyRriskClass", 
    parent_class = rriskModelClass)$new(MAX_MC1D    = 1e5, 
                                        MAX_MC2D_1D = 1e4, 
                                        MAX_MC2D_2D = 1e3)$reactive()

  # max size for model results
  max_memory_allowed <- 256 # MB
  
  param_dist_info <- isolate(rrisk_model()$get_param_dist_info())
  # needed for add/change node dialog
  # for entry fields and entry names of parameter of each 
  # parametric distribution
  parametric_dists <- sapply(X        = param_dist_info,
                             FUN      = function(x) x$def,
                             simplify = FALSE)
  # needed for add/change node dialog for selection of parametric distributions
  param_dist_choices <- set_parametric_dist_choices(param_dist_info)
  
  # directory for temporary files for the shiny rrisk session
  temp_dir <- tempdir(check = TRUE)
  
  # NOTE: I DO NOT USE SHINYs moduleServer FUNCTION HERE, AS IT
  # IS EASIER HERE TO JUST USE SIMPLE FUNCTIONS; THERE IS NO REAL NEED FOR
  # A NEW SCOPING UNIT. ESPECIALLY WHEN CERTAIN INPUTS NEED TO BE CALLED/SET
  # IN SEVERAL SERVER FUNCTIONS
  
  # need to come first, otherwise visNetwork in this function will mess with
  # the downloadHandler in menu_bar_server
  # (this persists also when using moduleServer; it must be something
  # with visNetwork, as igraph does not have this issue. But with igraph
  # a simple plot is generated, while visNetwork is a interactive .js plug-in)
  nodes_server(input, output, session,
               rrisk_model, param_dist_choices, parametric_dists, param_dist_info)
  
  menu_bar_server(input, output, session, rrisk_model, temp_dir)
  
  global_objects_server(input, output, rrisk_model)
  
  run_sim_server(input, output, rrisk_model, max_memory_allowed)
  
  documentation_server(input, output, rrisk_model)
  
  session$onSessionEnded(
    function() {
      # delete all temp png files
      #file.remove(list.files(temp_dir, full.names = TRUE, 
      #                       pattern = "^rriskModelFile.*.png"))
      stopApp()
    }
  )
}

shinyApp(ui, server)


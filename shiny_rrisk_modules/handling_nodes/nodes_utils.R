get_dist_input <- function(input, param_dist_info) 
{
  # get name of distribution
  dist_name <- input[["param_dist_name"]]
  # if there is no name, return empty result
  if (is.null(dist_name)) return(NULL)
  # get the parameter of the distribution
  dist_params <- names(param_dist_info[[dist_name]])
  
  # get the input for each parameter of the distribution 
  result <- list()
  for (i in seq_along(dist_params)) {
    param_name <- dist_params[i]
    param <- input[[paste0("mc_input_", i)]]

    # if this param is optional and has its initial value, skip it
    if (param_dist_info[[dist_name]][[param_name]]$optional &&
        as.name(param_dist_info[[dist_name]][[param_name]]$init) == param)
      next
    
    if (is.character(param)) {
      # if this param is a vector, and it is just a string of comma separated 
      # values, break it into a vector
      if (isTRUE(param_dist_info[[dist_name]][[param_name]]$is_vector)) {
        param <- cast_csv_as_vector(param)
      }
      # try to cast string input into type numeric;
      # if it does not work, let it be a string
      param <- try_cast_as_numeric(param)
    }
    result[[param_name]] <- param
  }
  result
}

get_add_change_dialog <- function(node_name, node_type, node_info = NULL,
                                  node_group = NULL, is_add_dialog = TRUE) 
{
  if (is_add_dialog) {
    dialog_name     <- "Add new item"
    label_ok_button <- "Add"
  } else {
    dialog_name     <- "Change item"
    label_ok_button <- "Change"
  }
  
  if (is.null(node_info)) {
    unit_name    <- "" 
    node_descr   <- "" 
    source_descr <- ""
  } else {
    unit_name    <- node_info$unit
    node_descr   <- node_info$descr
    source_descr <- node_info$source
  }
  
  if (is.null(node_group))
    group_names <- ""
  else
    group_names <- node_group
  
  modalDialog(
    title = dialog_name,
    # ui elements
    htmltools::div(
      id = "main_add_change_ui_element",
      fluidRow(
        column(width = 6,
               textInput(inputId = "node_name",
                         label   = "Name (required)",
                         value   = node_name))
      ),
      fluidRow(
        column(width = 6,
               textInput(inputId = "unit_name",
                         label   = "Unit",
                         value   = unit_name)),
        column(width = 6,
               textInput(inputId = "group_names",
                         label   = "Grouping names",
                         value   = group_names))),
        textAreaInput(inputId = "source_descr",
                      label   = "Source",
                      value   = source_descr,
                      width   = '100%'),
        textAreaInput(inputId = "node_description",
                      label   = "Description",
                      value   = node_descr,
                      width   = '100%'),
      htmltools::hr(),
      selectInput(inputId  = "node_type", 
                  label    = "Choose item definition (required):",
                  choices  = list(
                    "nothing"                    = "",
                    "user defined"               = "user_defined",
                    "parametric distribution"    = "param_dist",
                    "bootstrapping"              = "bootstrap",
                    "rrisk distributions import" = "rrisk_dist_import"),
                  selected = node_type)
    ),
    size = "l",
    footer = htmltools::tagList(
      modalButton("Cancel"),
      actionButton(inputId = "btn_add_and_change_item",
                   label   = label_ok_button)
    )
  )
}

add_inputs_to_modal_dialog <- function(node_type = "", 
                                       item, 
                                       param_dist_choices) 
{
  # remove all possible inserted ui elements
  removeUI(selector = "div#user_defined_function_ui_elements")
  removeUI(selector = "div#parametric_distribution_ui_elements")
  removeUI(selector = "div#parametric_dist_inputs")
  removeUI(selector = "div#bootstrap_ui_elements")
  #removeUI(selector = "div#efsa_eke_ui_elements")
  removeUI(selector = "div#rrisk_dist_import_ui_elements")
  
  # insert ui elements for 
  # (a) user defined code, or 
  # (b) parametric distribution
  # (c) bootstrap
  if (node_type == "user_defined") {
    
    insertUI(selector = "#main_add_change_ui_element",
             where    = "afterEnd",
             ui       = htmltools::div(
               id = "user_defined_function_ui_elements",
               textInput(inputId = "node_code",
                         label   = "Item Code",
                         value   = item$code,
                         width   = "100%")
             )
    )
    
  } else if (node_type == "param_dist") {
    
    insertUI(selector = "#main_add_change_ui_element",
             where    = "afterEnd",
             ui       = htmltools::div(
               id = "parametric_distribution_ui_elements",
               selectInput(inputId  = "param_dist_name",
                           label    = "Choose parametric distribution",
                           choices  = param_dist_choices,
                           selected = item$param_dist$name)
             )
    )
    add_inputs_for_mc_modal_dialog(item$param_dist$def, 
                                   item$mc_dim)
    
  } else if (node_type == "bootstrap") {

    selected_item <- if (is.null(item$mc_dim) ||
                         item$mc_dim == 1) "Variability" else "Uncertainty"

    insertUI(
      selector = "#main_add_change_ui_element",
      where    = "afterEnd",
      ui       = htmltools::div(
        id = "bootstrap_ui_elements",
        htmltools::hr(),
        # select if bootstrap node represents variable or
        # uncertain variables
        radioButtons(inputId  = "btn_bootstrap_uncert",
                     label    = "Distribution represents:",
                     choices  = c("Variability", "Uncertainty"),
                     selected = selected_item,
                     inline   = TRUE),
        # open csv with data
        fileInput(inputId = "open_bootstrap_data_file",
                  label   = "open data",
                  accept  = c("text/csv",
                              "text/comma-separated-values",
                              ".csv")),
        # visualization and modifing of data
        textAreaInput(inputId = "bootstrap_values",
                      label   = "Provided data",
                      value   = paste(item$bootstrap_data$data,
                                      collapse = ", "),
                      width   = "100%"),
        # table with user supplied bootstrap statistics
        DT::DTOutput(outputId = "table_bootstrap_summary_stat")
      )
    )
    
  } else if (node_type == "rrisk_dist_import") {
    
    insertUI(
      selector = "#main_add_change_ui_element",
      where    = "afterEnd",
      ui       = htmltools::div(
        id = "rrisk_dist_import_ui_elements",
        htmltools::hr(),
        # open csv with data
        fileInput(inputId  = "open_rrisk_dist_export_file",
                  label    = "open rrisk-distributions-export file",
                  multiple = FALSE,
                  accept   = c("application/json",
                               ".rriskdistex")),
        # if the rrisk_dist_object already exist, then show UI elements
        if (!is.null(item$info$rrisk_dist_fit)) {
          set_fitted_dist_input_ui(item$info$rrisk_dist_fit, var_uncert = 1) 
        }
      )
    )
    
  }
}

add_inputs_for_mc_modal_dialog <- function(dist_params, 
                                           var_uncert = 1) 
{
  # remove previous mc param dist ui element
  removeUI(selector = "div#parametric_dist_inputs")
  # insert mc param dist ui element
  insertUI(
    selector = "#parametric_distribution_ui_elements",
    where    = "afterEnd",
    ui       = htmltools::div(
      id = "parametric_dist_inputs",
      htmltools::hr(),
      get_parametric_dist_input_ui(dist_params, var_uncert))
  )
}

get_parametric_dist_input_ui <- function(dist_params, 
                                         var_uncert = 1) 
{
  if (length(dist_params) == 0) return()
  
  selected_item <- if (var_uncert == 1) "Variability" else "Uncertainty"
  
  fluidRow(
    column(
      width = 5,
      radioButtons(inputId  = "btn_var_uncert",
                   label    = "Distribution represents:",
                   choices  = c("Variability", "Uncertainty"),
                   selected = selected_item,
                   inline   = TRUE),
      mapply(
        function(i, label_name, this_value) {
          if (is.logical(this_value)) {
            checkboxInput(inputId = paste0("mc_input_", i), 
                          label   = label_name, 
                          value   = this_value)
          } else {
            textInput(inputId = paste0("mc_input_", i),
                      label   = label_name,
                      value   = toString(this_value))
          }
        },
        seq_along(dist_params), names(dist_params), dist_params,
        SIMPLIFY = FALSE
      )
    ),
    column(
      width = 6,
      plotOutput(outputId = "pdf_plot")
    )
  )
}

set_fitted_dist_input_ui <- function(rrisk_dist_fit, 
                                     var_uncert = 1) 
{
  selected_item <- if (var_uncert == 1) "Variability" else "Uncertainty"
  
  fluidPage(
    fluidRow(
      if (rrisk_dist_fit$data_type == "pdf") {
        shinyjs::disabled(
          textAreaInput(
            inputId = "rrisk_dist_fit_pdf",
            label   = "user provided data",
            value   = paste(rrisk_dist_fit$provided_data, collapse = ", "),
            width   = "100%"
          )
        )
      } else if (rrisk_dist_fit$data_type == "cdf") {
        
      },
      htmltools::hr()
    ),
    fluidRow(
      column(
        width = 5,
        #htmltools::tags$strong(rrisk_dist_fit$selected_fit$fitted_dist_name),
        htmltools::HTML(paste0("Fitted distribution: <b>",
                               rrisk_dist_fit$selected_fit$fitted_dist_name,
                               "</b><br><br>")),
        radioButtons(inputId  = "btn_var_uncert",
                     label    = "Distribution represents:",
                     choices  = c("Variability", "Uncertainty"),
                     selected = selected_item,
                     inline   = TRUE),
        
        mapply(
          function(i, label_name, this_value) {
            shinyjs::disabled(
              textInput(inputId = paste0("mc_input_", i),
                        label   = label_name,
                        value   = toString(this_value))
            )
          },
          seq_along(rrisk_dist_fit$selected_fit$par), 
          names(rrisk_dist_fit$selected_fit$par), rrisk_dist_fit$selected_fit$par,
          SIMPLIFY = FALSE
        )
      ),
      column(
        width = 6,
        plotOutput(outputId = "pdf_plot")
      )
    )
  )
}

create_model_table <- function(df, colnames) 
{
  # generate action button for data frame
  actionButtons <- mapply(
    FUN = build_action_buttons,
    seq_len(nrow(df)), df$node_type == "implicit",
    MoreArgs = list(item = "item")
  )
  # remove entry node_type from data frame
  df$node_type <- NULL
  # sort data frame according to colnames
  if (nrow(df) > 0) df <- df[,names(colnames)]
  # add ID-Numbers (first column) and action buttons (last column) to data frame
  df <- cbind.data.frame(seq_len(nrow(df)), df, Actions = actionButtons)
  
  DT::datatable(
    data      = df,
    # Need to disable escaping for html as string to work
    escape    = FALSE,
    #colnames  = c("ID" = 1),
    colnames  = c("ID", unname(colnames), "Actions"),
    rownames  = FALSE,
    selection = 'single',
    options   = list(
      paging      = FALSE
    ),
    callback = DT::JS(
      "table.on('dblclick', 'td',", 
      "  function() {",
      "    const data = table.row(this).data();",
      "    Shiny.setInputValue('model_table_dblclick', {node_name: data[1]});",
      "    Shiny.setInputValue('model_table_dblclick', {dt_row: data[0]});",
      "  }",
      ");"
    )
  )
}

create_bootstrap_summary_stat_table <- function(input_table)
{
  DT::datatable(data      = input_table,
                colnames  = c("variable name", "summary statistic"),
                selection = 'none', 
                rownames  = TRUE,
                editable  = TRUE, 
                filter    = "none",
                options   = list(dom = 't', ordering = FALSE),
                caption   = "Input for Bootstrap summary functions")
}

build_bootstrap_df <- function(bootstrap_sum_stats)
{
  n <- 0
  if (length(bootstrap_sum_stats) < 5) n <- 5 - length(bootstrap_sum_stats)
  df_sum_stat <- list(var_name = c(names(bootstrap_sum_stats), rep("", n)),
                      sum_stat = c(unlist(bootstrap_sum_stats), rep("", n)))
  list2DF(df_sum_stat)
}
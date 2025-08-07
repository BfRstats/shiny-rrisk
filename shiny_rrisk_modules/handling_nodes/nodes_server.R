nodes_server <- function(input, output, session,
                         rrisk_model, param_dist_choices, parametric_dists, param_dist_info)
{
  item                <- reactiveValues()
  replace_node        <- FALSE #reactiveVal()
  bootstrap_table     <- reactiveVal()
  efsa_eke_plot_data  <- reactiveVal()
  node_clicked        <- reactiveVal(FALSE)
  clicked_node_number <- 0
  rrisk_dist_fit      <- reactiveVal()
  
  #---BEGIN: node modal dialog--------------------------------------------------
  # ADD item modal dialog
  observeEvent(
    eventExpr   = input$btn_add_new_item,
    handlerExpr = {
      # initialize item object
      item$content <- list(node_name  = "",
                           node_type  = "",
                           code       = "",
                           param_dist = list(name = ""),
                           mc_dim     = 1)
      # initialize bootstrap table
      bootstrap_table(data.frame(var_name  = rep("", 5),
                                 sum_stats = rep("", 5)))
      # remove rrisk_dist_fit
      rrisk_dist_fit(NULL)
      # set flag that this is a ADD node modal dialog
      replace_node <<- FALSE
      # show the actual ADD node modal dialog
      showModal(get_add_change_dialog(node_name     = "",
                                      node_type     = "",
                                      is_add_dialog = TRUE))
    }
  )
  
  # CHANGE item modal dialog
  observeEvent(
    ignoreInit  = TRUE,
    eventExpr   = node_clicked(),
    handlerExpr = {
      item_num <- clicked_node_number
      result <- rrisk_model()$get_node_content(node_num = as.integer(item_num),
                                               silent   = TRUE)
      # set flag to indicate we want to change an item
      replace_node <<- TRUE
      # if this node is a bootstrap node, set bootstrap table
      if (result$node_content$node_type == "bootstrap") {
        bootstrap_table(
          build_bootstrap_df(result$node_content$bootstrap_data$op)
        )
      } else if (result$node_content$node_type == "param_dist") {
          #!is.null(result$node_content$info$selected_fit)) {
        rrisk_dist_fit(result$node_content$info$rrisk_dist_fit)
        
        if (!is.null(result$node_content$info$rrisk_dist_fit)) {
          result$node_content$node_type <- "rrisk_dist_import"
        }
      }
      
      item$content <- result$node_content
      
      # show modal CHANGE ITEM
      showModal(get_add_change_dialog(
        node_name     = result$node_content$node_name,
        node_type     = result$node_content$node_type,
        node_info     = result$node_content$info,
        node_group    = toString(result$node_content$group),
        is_add_dialog = FALSE)
      )
      # add UI elements, if we have parametric distribution item
      add_inputs_to_modal_dialog(result$node_content$node_type,
                                 result$node_content,
                                 param_dist_choices)
      # disable selection wheel for bootstrap result nodes
      shinyjs::toggleState(
        id        = "node_type",
        condition = result$node_content$node_type != "bootstrap_result"
      )
    }
  )
  
  # adding ui element to ADD/CHANGE modal dialog
  # adding ui element for selecting item type and user defined function
  observeEvent(
    eventExpr   = input$node_type,
    handlerExpr = {
      # add additional UI elements to ADD NODE Dialog
      add_inputs_to_modal_dialog(input$node_type,
                                 item$content,
                                 param_dist_choices)
    }
  )
  
  # adding ui element to Add/CHANGE modal dialog
  # adding ui element for parametric distribution
  observeEvent(
    eventExpr   = input$param_dist_name,
    handlerExpr = {
      if (!is.null(item$content$param_dist$name) &&
          nzchar(item$content$param_dist$name) &&
          input$param_dist_name == item$content$param_dist$name) {
        dist_params        <- item$content$param_dist$def
        node_var_or_uncert <- item$content$mc_dim
      } else {
        dist_params <- rrisk_model()$get_default_dist_params(input$param_dist_name)
        node_var_or_uncert <- 1
      }
      add_inputs_for_mc_modal_dialog(dist_params, node_var_or_uncert)
    }
  )
  
  # ADD a new node or CHANGE an existing node
  observeEvent(
    eventExpr   = input$btn_add_and_change_item,
    handlerExpr = {
      
      if (isFALSE(input$add_and_change_dialog)) return()
      
      node_code      <- NULL
      param_dist_def <- NULL
      bootstrap      <- NULL
      mc_dim         <- NULL
      extra_descr    <- NULL
      
      info <- list(unit   = input$unit_name,
                   source = input$source_descr,
                   descr  = input$node_description)
      
      if (input$node_type == "user_defined") {
        
        node_code <- input$node_code
        
      } else if (input$node_type == "param_dist") {
        
        param_dist_def <- list(name = input$param_dist_name,
                               def  = get_dist_input(input,
                                                     parametric_dists))
        item$content$param_dist <- param_dist_def
        
        mc_dim <- ifelse(input$btn_var_uncert == "Variability", 1L, 2L)
        item$content$mc_dim <- mc_dim
        
      } else if (input$node_type == "bootstrap") {
        
        bootstrap_sum_stats <- bootstrap_table()
        
        bootstrap_op <- list()
        for (i in seq_len(nrow(bootstrap_sum_stats))) {
          # if not var_name, and no sum_stat is given, skip entry
          if (nchar(trimws(bootstrap_sum_stats$sum_stat[i])) == 0 &&
              nchar(trimws(bootstrap_sum_stats$var_name[i])) == 0) next
          bootstrap_op <- c(bootstrap_op,
                            setNames(list(bootstrap_sum_stats$sum_stat[i]),
                                     bootstrap_sum_stats$var_name[i]))
        }
        
        bootstrap <- list(
          data = try_cast_as_numeric(
            cast_csv_as_vector(input$bootstrap_values)),
          op   = bootstrap_op
        )
        
        mc_dim <- ifelse(input$btn_bootstrap_uncert == "Variability", 1L, 2L)
        
      } else if (input$node_type == "rrisk_dist_import") {
        
        param_dist_def <- list(
          name = rrisk_dist_fit()$selected_fit$fitted_dist_name,
          def  = rrisk_dist_fit()$selected_fit$par
        )
        
        # get MC dim, aka variability (1 MC-Dim), or Uncertainty (2 MC-Dim)
        mc_dim <- if (input$btn_var_uncert == "Variability") 
                    1L 
                  else 
                    2L
        item$content$mc_dim <- mc_dim
        
        # add rrisk_dist_fit object to info list
        info$rrisk_dist_fit <- rrisk_dist_fit()
        
      }
      
      if (replace_node) {
        result <- rrisk_model()$change_node(
          node_name     = item$content$node_name,
          new_node_name = input$node_name,
          user_def_expr = node_code,
          param_dist    = param_dist_def,
          bootstrap     = bootstrap,
          info          = info,
          group         = trimws(strsplit(input$group_names, ",")[[1]]),
          mc_dim        = mc_dim,
          silent        = TRUE
        )
      } else {
        result <- rrisk_model()$add_node(
          node_name     = input$node_name,
          user_def_expr = node_code,
          param_dist    = param_dist_def,
          bootstrap     = bootstrap,
          info          = info,
          group         = trimws(strsplit(input$group_names, ",")[[1]]),
          mc_dim        = mc_dim,
          replace_implicit_node = FALSE,
          silent        = TRUE
        )
      }
      
      if (result$is_ok) {
        item$content$node_name <- input$node_name
        item$content$node_type <- input$node_type
        removeModal()
      } else {
        myShinyAlert(
          title = "Error in user input",
          text  = result$error_message,
          type  = "error")
      }
    }
  )
  
  # for plotting the pdf of a parametric distribution in the
  # ADD/CHANGE NODE dialog
  output$pdf_plot <- renderPlot({
    if (input$node_type == "param_dist") {
      
      # pdf plot param dist
      # get dist name
      dist_name <- input$param_dist_name
      # in case there is no dist name, return without doing a thing
      if (is.null(dist_name)) return()
      # get param names
      param_names <- names(parametric_dists[[dist_name]])
      # try to get values for each param name
      params <- mapply(
        FUN = function(param_name, i, this_dist) 
        {
          param <- input[[paste0("mc_input_", i)]]
          if (isTRUE(parametric_dists[[this_dist]][[param_name]]$is_vector)) {
            param <- cast_csv_as_vector(param)
          }
          try_cast_as_numeric(param)
        },
        param_names, seq_along(param_names),
        MoreArgs = list(this_dist = dist_name),
        SIMPLIFY = FALSE
      )
      # now plot
      plot_pdf(input$param_dist_name, params, 
               xlab = paste0(input$node_name, " [", input$unit_name, "]"))
      
    } else if (input$node_type == "rrisk_dist_import") {
      
      # plot pdf for fitted distribution
      if (is.null(rrisk_dist_fit())) return(NULL)
      
      # plot histogram
      if (rrisk_dist_fit()$data_type == "pdf") {
        # plot histogram for pdf
        hist(
          x    = rrisk_dist_fit()$provided_data, 
          freq = FALSE,
          main = "",
          xlab = paste0(input$node_name, " [", input$unit_name, "]")
        )
      } else if (rrisk_dist_fit()$data_type == "cdf") {
        # plot histogram for cdf
        diff_x <- diff(rrisk_dist_fit()$provided_data)
        diff_y <- diff(rrisk_dist_fit()$provided_data_cdf)
        slope <- diff_y / diff_x
        
        is_finite <- is.finite(slope)
        slope <- slope[is_finite]
        diff_x <- diff_x[is_finite]
        
        plot(x    = NULL, 
             y    = NULL, 
             xlim = range(rrisk_dist_fit()$provided_data), 
             ylim = c(0, 1.1 * max(slope)),
             xlab = "x",
             ylab = "Density")
        
        n <- length(rrisk_dist_fit()$provided_data)-1
        x <- rrisk_dist_fit()$provided_data[1:n]
        x <- x[is_finite]
        rect(xleft   = x,
             ybottom = rep(0, length(diff_x)),
             xright  = x + diff_x,
             ytop    = slope,
             col     = "gray")
      }
      
      # get name and params of fitted dist
      param_dist_name <- rrisk_dist_fit()$selected_fit$fitted_dist_name
      params <- as.list(rrisk_dist_fit()$selected_fit$par)
      # plot fitted data
      result <- get_pdf_data_for_plotting(param_dist_name,
                                          params)
      lines(result$x, result$y, col = "red", lwd = 2, type = result$type)
      
    }
  })
  #---END: node modal dialog----------------------------------------------------
  
  # DELETE node
  observeEvent(
    eventExpr   = input$btn_delete_item,
    handlerExpr = {
      item_num <- parseActionButton(input$btn_delete_item)
      myShinyAlert(
        title             = "Delete item?",
        text              = "Do you want to delete item?",
        type              = "info",
        showCancelButton  = TRUE,
        confirmButtonText = "Delete",
        animation         = FALSE,
        callbackR         = function(value) {
          if (isTRUE(value)) {
            rrisk_model()$remove_node(node_num = item_num)
          }
        })
    }
  )
  
  #---BEGIN: bootstrap handlings------------------------------------------------
  # load bootstrap data
  observeEvent(
    eventExpr   = input$open_bootstrap_data_file,
    handlerExpr = {
      infile <- input$open_bootstrap_data_file
      if (is.null(infile)) return(NULL)
      bootstrap_data <- read.csv(infile$datapath, header = TRUE)
      updateTextInput(
        session = session,
        inputId = "bootstrap_values",
        value   = paste0(bootstrap_data[[1]], collapse = ", ")
      )
    } 
  )
  
  # render table bootstrap summary stat for ADD/change node
  output$table_bootstrap_summary_stat <- DT::renderDT(
    expr   = create_bootstrap_summary_stat_table(bootstrap_table()),
    server = FALSE
  )
  
  # table bootstrap summary stat for ADD/change node is editable
  # the edit for each cell must be stored in object bootstrap_table
  observeEvent(
    eventExpr   = input$table_bootstrap_summary_stat_cell_edit,
    handlerExpr = {
      bootstrap_table(
        DT::editData(bootstrap_table(),
                     input$table_bootstrap_summary_stat_cell_edit)
      )
    }
  )
  #---END: bootstrap handlings--------------------------------------------------
  
  #---BEGIN: handle rrisk-dist-export file--------------------------------------
  # load rrisk-dist-export file
  observeEvent(
    eventExpr   = input$open_rrisk_dist_export_file,
    handlerExpr = {
      
      infile <- input$open_rrisk_dist_export_file
      if (is.null(infile)) return(NULL)
      
      # get rrisk_dist_fit
      result <- read_rrisk_dist_export_file(infile, param_dist_info)
      
      if (result$is_ok) {
        
        # if successful add rrisk_dist_fit to reactive Value
        rrisk_dist_fit(result$fit_dist)
        # add UI elements for displaying rrisk-dist results
        add_inputs_for_rrisk_dist_import(result$fit_dist, var_uncert = 1)
      
      } else {
        
        myShinyAlert(
          title = "Could not open file",
          text  = result$error_message,
          type  = "error"
        )
        
      }
    } 
  )
  #---END: handle rrisk-dist-export file----------------------------------------

  # check if model is ok
  observeEvent(
    eventExpr   = input$btn_check_graph,
    handlerExpr = {
      result <- rrisk_model()$check_model(silent = TRUE)
      if (result$is_ok)
        myShinyAlert(
          title = "Your model seems to be ok.",
          text  = "Could not find errors.",
          type  = "success")
      else
        myShinyAlert(
          title = "Error in your model",
          text  = result$error_message,
          type  = "error")
    }
  )
  
  #---BEGIN: handling of model graph--------------------------------------------
  output$model_DAG <- visNetwork::renderVisNetwork({
    set.seed(42)
    model_graph <- rrisk_model()$get_model_graph(as = "visNetwork")
    #model_graph <- rrisk_model()$get_model_graph(
    #  as            = "visNetwork",
    #  grouped:graph = input$plot_collapsed_groups_model_graph
    #)
    graph <- visNetwork::visNetwork(nodes = model_graph$nodes,
                                    edges = model_graph$edges) |>
      visNetwork::visNodes(physics = FALSE) |>
      visNetwork::visEdges(arrows = list(to = list(enabled = TRUE)),
                           physics = TRUE) |>
      visNetwork::visHierarchicalLayout(
        direction            = "LR",
        enabled              = input$plot_style_model_graph,
        #blockShifting        = TRUE,
        #edgeMinimization     = TRUE,
        #parentCentralization = TRUE,
        sortMethod           = "directed",
        shakeTowards         = "leaves")
    if (input$plot_collapsed_groups_model_graph) {
      graph <- visNetwork::visClusteringByGroup(
        graph  = graph,
        groups = rrisk_model()$get_groups()
      )
    } else {
      graph <- graph |> visNetwork::visEvents(
        click       = paste0("function(n) {
                             Shiny.setInputValue('node_clicked', n);}"),
        doubleClick = paste0("function(n) {
                             Shiny.setInputValue('node_doubleclicked', n);}"))
    }
    graph
  })

  observe({
    node_names <- rrisk_model()$get_node_names()
    visNetwork::visNetworkProxy(shinyId = "model_DAG") |>
      visNetwork::visSelectNodes(id = node_names[input$model_table_rows_selected])
  })

  observeEvent(
    eventExpr = input$node_clicked,
    handlerExpr = {
      # check if a node with id is clicked
      if (length(input$node_clicked$nodes) == 0) return()
      # get the the node id (here: name)
      selected_node <- input$node_clicked$nodes[[1]]
      # get vector with all node names
      node_names <- rrisk_model()$get_node_names()
      # get the position of selected node name in vector
      node_num <- which(selected_node == node_names)
      # select row in model data table
      DT::selectRows(proxy = proxy_data_table,
                     selected = node_num)
    }
  )

  observeEvent(
    eventExpr = input$node_doubleclicked,
    handlerExpr = {
      if (!input$plot_collapsed_groups_model_graph &&
          length(input$node_doubleclicked$nodes) > 0) {
        # get the the node id (here: name)
        selected_node <- input$node_clicked$node[[1]]
        # get vector with all node names
        node_names <- rrisk_model()$get_node_names()
        # get the position of selected node name in vector
        node_num <- which(selected_node == node_names)
        # select row in model data table
        clicked_node_number <<- node_num
        node_clicked(!node_clicked())
      }
    }
  )
  #---END: handling of model graph----------------------------------------------
  
  #---BEGIN: handling for model table-------------------------------------------
  proxy_data_table <- DT::dataTableProxy(outputId = "model_table")
  
  # show model table
  output$model_table <- DT::renderDT(
    expr = {
      create_model_table(
        df       = rrisk_model()$get_df_nodes(
                     info_elements = c("unit", "descr")
                   ),
        colnames = c(node_name   = "Name", 
                     node_group  = "Group", 
                     node_code   = "Definition", 
                     unit        = "Unit", 
                     node_mc_dim = "MC Dim", 
                     descr       = "Description")
      )
    },
    server = TRUE
  )
  
  # double click in row of model table
  observeEvent(
    eventExpr = input$model_table_dblclick,
    handlerExpr = {
      clicked_node_number <<- input$model_table_dblclick$dt_row
      node_clicked(!node_clicked())
    }
  )
  #---END: handling of model table----------------------------------------------
}
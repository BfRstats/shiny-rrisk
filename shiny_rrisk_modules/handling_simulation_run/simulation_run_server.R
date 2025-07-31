run_sim_server <- function(input, output, rrisk_model, max_memory_allowed)
{
  #run_simulation_clicked <- reactiveVal(0)
  
  # show simulation parameter model dialog
  observeEvent(
    eventExpr   = input$btn_set_parameter,
    handlerExpr = {
      sim_param_list <- rrisk_model()$get_simulation_params()
      is_2DMC <- rrisk_model()$is_2DMC()
      showModal(simulation_parameter_dialog(sim_param_list, is_2DMC))
    }
  )
  
  # get result from simulation parameter modal dialog
  observeEvent(
    eventExpr   = input$btn_change_sim_param,
    handlerExpr = {
      is_2DMC <- rrisk_model()$is_2DMC()
      # set MC settings
      if (is_2DMC) { 
        # 2D MC
        result <- rrisk_model()$set_simulation_params(
          MC2D   = list(ITER1D        = input$sim_param_value_1D,
                        ITER2D        = input$sim_param_value_2D,
                        SAMPLING_TYPE = input$btn_sampling_type),
          silent = TRUE
        )
      } else { 
        # 1D MC
        result <- rrisk_model()$set_simulation_params(
          MC1D   = list(ITER1D        = input$sim_param_value_1D,
                        SAMPLING_TYPE = input$btn_sampling_type),
          silent = TRUE
        )
      }
      # if MC setting was OK, set the other parameter
      if (result$is_ok) {
        if (is_2DMC) {
          result <- rrisk_model()$set_simulation_params(
            seed          = input$set_seed_value,
            quantiles     = try_cast_as_numeric(
                              cast_csv_as_vector(input$txt_quantiles)),
            quantiles_2D  = try_cast_as_numeric(
                              cast_csv_as_vector(input$txt_quantiles_2D)),
            quantile_type = as.integer(input$selected_quantile_type),
            silent        = TRUE
          )
        } else {
          result <- rrisk_model()$set_simulation_params(
            seed          = input$set_seed_value,
            quantiles     = try_cast_as_numeric(
                              cast_csv_as_vector(input$txt_quantiles)),
            quantile_type = as.integer(input$selected_quantile_type),
            silent        = TRUE
          )
        }
      }
      # if there is an error show error message,
      # otherwise close dialog
      if (result$is_ok)
        removeModal()
      else
        myShinyAlert(
          title = "Error in setting simulation parameter",
          text  = result$error_message,
          type  = "error")
    }
  )
  
  # run simulation
  observeEvent(
    eventExpr   = input$btn_run_simulation,
    handlerExpr = {
      #run_simulation_clicked(run_simulation_clicked() + 1)
      # check if model is not to big
      memory_needed <- rrisk_model()$get_memory_usage_for_graph("MB")
      if (memory_needed > max_memory_allowed) {
        myShinyAlert(
          title = "Model needs to much memory",
          text  = paste("Model needs", round(memory_needed, 1), "MB,",
                        "but only", max_memory_allowed, "MB",
                        "are available. Reduce number of Iterations."),
          type  = "error"
        )
        return()
      }
      # show running dialog
      myShinyAlert(
        title = "Simulation is running",
        text  = "Simulation is running, please wait",
        type  = "info"
      )
      # remove old result tabs
      remove_tabs()
      # set timer for simulation run
      start <- Sys.time()
      # now run the simulation
      result <- rrisk_model()$run_simulation(silent = TRUE)
      # close previous "show running dialog" alert
      closeAlert()
      # check if simulation was successful
      if (result$is_ok) {
        # SIMULATION SUCCESS
        # show simulation run time
        myShinyAlert(
          title = "Simulation run finished",
          text  = paste("Run time:", 
                        round(run_time <- Sys.time() - start, 2), 
                        attr(run_time, "units"), 
                        "\nPreparing plots, please wait."),
          type  = "info"
        )
        # set timer for creating graphics
        plot_width <- "50%"
        start <- Sys.time()
        # get nodes names for result tabs
        end_nodes   <- rrisk_model()$get_end_nodes()
        other_nodes <- setdiff(rrisk_model()$get_results(node_names_only = TRUE),
                               end_nodes)
        # add general plots tab
        appendTab(
          inputId = "result_tabs",
          tab     = tabPanel(
            title = "general results",
            tags$hr(),
            selectInput(
              inputId = "select_nodes_for_result_plots",
              label   = "Node",
              choices  = list(
                "End node"    = as.list(end_nodes),
                "Other nodes" = as.list(other_nodes)
              ),
              selected = end_nodes[1]
            ),
            uiOutput(outputId = "result_table"),
            checkboxInput(inputId = "as_log10",
                          label   = "as log10"),
            plotOutput(outputId = "result_plot_hist",
                       width    = "50%",
                       click    = "plot_hist_click"),
            verbatimTextOutput("plot_hist_info"),
            plotOutput(outputId = "result_plot_ecdf",
                       width    = "50%",
                       click    = "plot_ecdf_click"),
            verbatimTextOutput("plot_ecdf_info"),
            plotOutput(outputId = "result_plot_conv",
                       width    = "50%")
          )
        )
        # add correlation plots tab
        appendTab(
          inputId = "result_tabs",
          tab     = tabPanel(
            title = "sensitivity plot",
            tags$hr(),
            tagList(
              fluidRow(
                column(
                  width = 2,
                  selectInput(inputId = "select_sensitivity_analysis",
                              label   = "Sensitivity analysis",
                              choices = c("Pearson"  = "pearson", 
                                          "Spearman" = "spearman",
                                          "R2"       = "R2"))
                ),
                column(
                  width = 1,
                  htmltools::p("for")
                ),
                column(
                  width = 2,
                  selectInput(inputId  = "select_node_for_sensitivity_analysis",
                              label    = "Node",
                              choices  = list(
                                "End node"    = as.list(end_nodes),
                                "Other nodes" = as.list(other_nodes)
                              ),
                              selected = end_nodes[1])
                )
              ),
              plotOutput(outputId = "sensitivity_plot",
                         width    = "50%")
            )
          )
        )
        # show the tab with one of the output nodes
        updateTabsetPanel(inputId  = "result_tabs",
                          selected = "general results")
        # close previous "show simulation run time" alert
        closeAlert()
      } else {
        # SIMULATION FAILED
        myShinyAlert(
          title = "Error occured during simulation run",
          text  = result$error_message,
          type  = "error")
      }
    }
  )
  
  # print results table for selected node 
  output$result_table <- renderUI({
    node_name <- input$select_nodes_for_result_plots
    get_table(rrisk_model()$get_result_summary(node_name))
  }) |> bindEvent(input$btn_run_simulation, 
                  input$select_nodes_for_result_plots)

  # plot histgramm for selected node
  output$result_plot_hist <- renderCachedPlot(
    expr = {
      rrisk_model()$plot_histogram(input$select_nodes_for_result_plots,
                                   plot   = FALSE,
                                   log    = input$as_log10,
                                   silent = TRUE)$plot
    },
    cacheKeyExpr = {
      list(input$select_nodes_for_result_plots,
           input$as_log10,
           input$btn_run_simulation)
    }
  )
  #output$result_plot_hist <- renderPlot(
  # expr = {
  #   rrisk_model()$plot_histogram(input$select_nodes_for_result_plots,
  #                                plot   = FALSE,
  #                                log    = input$as_log10,
  #                                silent = TRUE)$plot
  # }
  #) |> #bindCache(input$select_nodes_for_result_plots, input$as_log10) |>
  #bindEvent(input$btn_run_simulation,
  #          input$select_nodes_for_result_plots,
  #          input$as_log10)
  
  # add text for values selected interactively by user in histogram
  output$plot_hist_info <- renderText(
  {
    x <- if (is.null(input$plot_hist_click$x))
           ""
         else
           signif(input$plot_hist_click$x, 3)
    
    y <- if (is.null(input$plot_hist_click$y))
           ""
         else
           signif(input$plot_hist_click$y, 3)
    
    paste0(input$select_nodes_for_result_plots, " = ", x, "\n", 
           "density = ", y)
  })
  
  # render ECDF plot
  output$result_plot_ecdf <- renderCachedPlot(
    expr = {
    rrisk_model()$plot_ecdf(input$select_nodes_for_result_plots,
                            plot   = FALSE,
                            log    = input$as_log10,
                            silent = TRUE)$plot
    },
    cacheKeyExpr = {
      list(input$select_nodes_for_result_plots,
           input$as_log10,
           input$btn_run_simulation)
    }
  )

  # plot interactivly selected values of user of ECDF plot
  output$plot_ecdf_info <- renderText(
  {
    x <- if (is.null(input$plot_ecdf_click$x))
           ""
         else
           signif(input$plot_ecdf_click$x, 3)

    y <- if (is.null(input$plot_ecdf_click$y))
           ""
         else
           signif(input$plot_ecdf_click$y, 3)

    paste0(input$select_nodes_for_result_plots, " = ", x, "\n",
           "ecdf value = ", y)
  })

  # plot convergence
  output$result_plot_conv <- renderCachedPlot(
    expr = {
      rrisk_model()$plot_convergence(input$select_nodes_for_result_plots,
                                     plot   = FALSE,
                                     log    = input$as_log10,
                                     silent = TRUE)$plot
    },
    cacheKeyExpr = {
      list(input$select_nodes_for_result_plots,
           input$as_log10,
           input$btn_run_simulation
           #run_simulation_clicked()
           )
    }
  )

  output$sensitivity_plot <- renderCachedPlot(
    expr = {
      node_name <- input$select_node_for_sensitivity_analysis
      method_name <- input$select_sensitivity_analysis
      rrisk_model()$get_correlation_plot(node_name,
                                         method_name,
                                         silent = TRUE)
    },
    cacheKeyExpr = {
      list(input$select_node_for_sensitivity_analysis,
           input$select_sensitivity_analysis,
           input$btn_run_simulation
           #run_simulation_clicked()
           )
    }
  )

}
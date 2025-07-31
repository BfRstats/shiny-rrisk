# SERVER
menu_bar_server <- function(input, output, session, rrisk_model, temp_dir)
{
  model_name <- ""
  
  # menu reactivity
  observeEvent(
    eventExpr = input$menu_bar,
    handlerExpr = {
      
      preset_model <- switch(
        input$menu_bar,
        "HEV"       = "Exposure_HEV_in_liver_sausages.rrisk",
        "MINAS"     = "Minas_Cheese_Listeriose_QMRA.rrisk",
        "POUILLOT"  = "Pouillot_2DMC_toy_model.rrisk",
        "ALIZARIN"  = "Alizarin-red-s-in-eel.rrisk",
        "YERSINIA"  = "Yersinia_enterocolitica_in_minced_meat.rrisk",
        "ASPARAGUS" = "Pest_risk_assessment_of_Elasmopalpus_lignosellus_for_EU.rrisk",
        NULL
      )
      open_preset_model(rrisk_model, preset_model)
      
      if (input$menu_bar == "NEW") {
        new_model_dialog(rrisk_model)
      } else if (input$menu_bar == "DISCLAIMER") {
        show_disclaimer_manual_and_contact_dialog()
      }
      
      updateNavbarPage(
        session  = session,
        inputId  = "menu_bar",
        selected = "1"
      )
    }
  )
  
  # open model
  observeEvent(
    eventExpr   = input$open_model_file,
    handlerExpr = {
      open_rrisk_model_file(rrisk_model, input$open_model_file$datapath)
    }
  )
  
  observeEvent(
    eventExpr   = input$text_model_name,
    handlerExpr = {
      model_name <<- input$text_model_name
    }
  )
  
  # internal function
  get_proper_file_name <- function(file_name, file_ext)
  {
      if (nzchar(trimws(file_name))) {
        # remove "."
        file_name <- gsub("\\.", "", file_name)
        # remove "*"
        file_name <- gsub("\\*", "", file_name)
        # replace white space with underscore
        file_name <- gsub(" ", "_", file_name)
        # add file extension
        file_name <- paste0(file_name, ".", file_ext)
        # return proper file name
      } else {
        # no file_name given; set dfeault file_name
        file_name <- paste0("shiny_rrisk_model.", file_ext)
      }
      file_name
  }
  
  # save model by downloading it
  output$download_model <- downloadHandler(
    filename = function()
    {
      get_proper_file_name(model_name, "rrisk")
    }, 
    content  = function(file) 
    {
      rrisk_model()$set_model_name(model_name)
      rrisk_model()$set_model_description(input$text_model_description)
      rrisk_model()$save_model(file, silent = TRUE)
    },
    contentType = "application/json"
  )

  # compile and download report
  output$download_report <- downloadHandler(
    filename = function()
    {
      get_proper_file_name(model_name, "docx")
    }, 
    content  = function(report_file_path) 
    {
      # show running dialog
      myShinyAlert(
        title = "Compiling report",
        text  = "Compiling report is running, please wait",
        type  = "info"
      )
      start <- Sys.time()
      # check if model is empty
      # if so, try to run simulation
      if (is.null(rrisk_model()$get_results(node_names_only = TRUE))) {
       result <- rrisk_model()$run_simulation(silent = TRUE)
       if (!result$is_ok) {
         closeAlert()
         myShinyAlert(
           title = "Error in model",
           text  = result$error_message,
           type  = "error"
         )
         return()
       }
      }
      rrisk_model()$set_model_name(model_name)
      rrisk_model()$set_model_description(input$text_model_description)
      result <- rrisk_model()$compile_report(
        report_file_path,
        template_file_path = "www/report_template/shiny-rrisk-report-template.rmd",
        silent             = TRUE
      )
      closeAlert()
      if (result$is_ok) {
        myShinyAlert(
          title = "Compiling report finished",
          text  = paste("Run time:",
                        round(run_time <- Sys.time() - start, 2),
                        attr(run_time, "units")),
          type  = "info"
        )
      } else {
        myShinyAlert(
          title = "Could not compile report file",
          text  = result$error_message,
          type  = "error"
        )
      }
    },
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document "
  )
  
  # save results by downloading them
  output$download_results <- downloadHandler(
    filename = function()
    {
      get_proper_file_name(model_name, "zip")
    }, 
    content  = function(file) 
    {
      myShinyAlert(
        title = "Preparing results for download",
        text  = "Getting results, please wait",
        type  = "info"
      )
      # check if model is empty
      # if so, try to run simulation
      if (is.null(rrisk_model()$get_results(node_names_only = TRUE))) {
        result <- rrisk_model()$run_simulation(silent = TRUE)
        if (!result$is_ok) {
          closeAlert()
          myShinyAlert(
            title = "Error in model. Cannot produce results.",
            text  = result$error_message,
            type  = "error"
          )
          return()
        }
      }
      # get results
      results <- rrisk_model()$get_results()
      if (is.null(results)) {
        closeAlert()
        myShinyAlert(
           title = "No results available",
           text  = result$error_message,
           type  = "error"
         )
         return()
      }
      # create temporary folder for building the results file
      temp_files_folder <- "results_folder"
      dir.create(file.path(temp_dir, temp_files_folder))
      # build for each node an csv file in the temporary folder
      for (node_name in names(results)) {
        file_name <- paste0(node_name, ".csv")
        df_node <- as.data.frame(results[[node_name]])
        
        if (ncol(df_node) == 1)
          colnames(df_node) <- node_name
        else
          colnames(df_node) <- paste0(node_name, ".",
                                      seq_len(ncol(df_node)))
        
        write.csv(x         = df_node,
                  file      = file.path(temp_dir, temp_files_folder,
                                        file_name),
                  row.names = FALSE)
      }
      # zip csv files
      ans <- zip::zip(file, temp_files_folder, root = temp_dir)
      unlink(file.path(temp_dir, temp_files_folder), recursive = TRUE)
      closeAlert()
    },
    contentType = "application/zip"
  )
}
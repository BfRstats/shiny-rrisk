# UTILS
open_rrisk_model_file <- function(rrisk_model, model_file_path)
{
  #old_nodes_with_results <- rrisk_model()$get_results(node_names_only = TRUE)
  
  result <- rrisk_model()$open_model(model_file_path,
                                     silent = TRUE)
  
  if (result$is_ok) {
    
    remove_tabs()

    updateTextInput(inputId = "text_model_description",
                    value   = rrisk_model()$get_model_description())
    
    updateTextInput(inputId = "text_model_name",
                    value   = rrisk_model()$get_model_name())    
    
    updateTabsetPanel(inputId  = "main_tab",
                      selected = "p1")
  } else {
    myShinyAlert(
      title = "Could not open model file",
      text  = result$error_message,
      type  = "error")
  }
  
  if (isTRUE(result$warning)) {
    myShinyAlert(
      title = "Iteration in file exceed allowed iterations",
      text  = result$warning_message,
      type  = "warning")
  }
}

open_preset_model <- function(rrisk_model, model_file_path)
{
  if (is.null(model_file_path)) return()
  
  model_file_path <- paste0("www/preset_models/", model_file_path)
  
  open_rrisk_model_file(rrisk_model, model_file_path)
}

new_model_dialog <- function(rrisk_model)
{
  if (rrisk_model()$is_model_empty()) return()
  
  myShinyAlert(
    title             = "Create new model?",
    text              = "Do you want to create a new model?",
    type              = "info",
    showCancelButton  = TRUE,
    confirmButtonText = "New model",
    callbackR         = function(value) {
      if (isTRUE(value)) {
        # remove old result tabs
        remove_tabs()
        # clear model
        rrisk_model()$clear_model()
        # reset this input values with shinyjs, because visNetwork is 
        # interfering with shiny
        shinyjs::reset("text_model_name")
        shinyjs::reset("text_model_description")
        #updateTextInput(session = session,
        #                inputId = "text_model_description",
        #                value   = "")
        #updateTextInput(session = session,
        #                inputId = "text_model_name",
        #                value   = "no model name")    
      }
    }
  )
}
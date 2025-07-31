global_objects_server <- function(input, output, rrisk_model)
{
  replace_global_obj <- reactiveVal()
  remind_obj_name <- reactiveVal()
  
  # for displaying modal dialog to set a fixed parameter or function
  observeEvent(
    eventExpr   = input$btn_add_new_global_obj,
    handlerExpr = {
      replace_global_obj(FALSE)
      showModal(add_change_global_object_dialog())
    }
  )
  
  # CHANGE global object modal dialog
  observeEvent(
    eventExpr   = input$global_obj_table_dblclick,
    handlerExpr = {
      # get object name
      obj_name <- input$global_obj_table_dblclick$obj_name
      global_obj <- rrisk_model()$get_global_object(obj_name = obj_name)
      replace_global_obj(TRUE)
      remind_obj_name(global_obj$name)
      showModal(add_change_global_object_dialog(global_obj,
                                                is_add_dialog = FALSE))
    }
  )
  
  # DELETE global object modal dialog
  observeEvent(
    eventExpr   = input$btn_delete_global_obj,
    handlerExpr = {
      item_num <- parseActionButton(input$btn_delete_global_obj)
      myShinyAlert(
        title             = "Delete global object?",
        text              = "Do you want to delete global object?",
        type              = "info",
        showCancelButton  = TRUE,
        confirmButtonText = "Delete",
        callbackR         = function(value) {
          if (isTRUE(value))
            rrisk_model()$remove_global_object(obj_num = item_num)
        }
      )
    }
  )
  
  # ADD/CHANGE button of modal dialog for global object was pressed
  observeEvent(
    eventExpr   = input$btn_add_change_global_obj,
    handlerExpr = {
      if (replace_global_obj()) {
        result <- rrisk_model()$change_global_object(
          obj_name     = remind_obj_name(),
          new_obj_name = input$global_object_name,
          definition   = input$global_object_definition,
          info         = list(unit   = input$global_object_unit,
                              source = input$global_object_source,
                              descr  = input$global_object_descr),
          silent       = TRUE
        )
      } else {
        result <- rrisk_model()$add_global_object(
          obj_name   = input$global_object_name,
          definition = input$global_object_definition,
          info       = list(unit   = input$global_object_unit,
                            source = input$global_object_source,
                            descr  = input$global_object_descr),
          silent     = TRUE
        )
      }
      if (result$is_ok)
        removeModal()
      else
        myShinyAlert(
          title     = "Error in your object",
          text      = result$error_message,
          type      = "error"
        )
    }
  )
  
  output$global_obj_table <- DT::renderDT(
    expr = {
      create_global_object_table(rrisk_model()$get_global_object_list(),
                                 c("Name", "Definition", "Unit", "Description"))
    }
  )
  
}

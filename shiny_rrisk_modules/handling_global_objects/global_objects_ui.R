global_objects_ui <- function()
{
  tagList(
    DT::DTOutput(outputId = "global_obj_table"),
    htmltools::hr(),
    actionButton(inputId = "btn_add_new_global_obj",
                 label   = "Add new parameter or function")
  )
}
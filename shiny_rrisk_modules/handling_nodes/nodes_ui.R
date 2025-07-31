nodes_ui <- function()
{
  htmltools::tagList(
    actionButton(inputId = "btn_add_new_item",
                 label   = "Add new node"),
    actionButton(inputId = "btn_check_graph",
                 label   = "Check model"),
    fluidRow(
      column(
        width = 2,
        checkboxInput(inputId = "plot_style_model_graph", 
                      label   = "stretch model graph"),
      ),
     column(
       width = 10, 
       checkboxInput(inputId = "plot_collapsed_groups_model_graph",
                     label   = "collapse groups")
     )
    ),
    htmltools::hr(),
    #plotOutput(outputId = "model_DAG"),
    visNetwork::visNetworkOutput(outputId = "model_DAG",
                                 width    = "auto",
                                 height   = "500px"),
    DT::DTOutput(outputId = "model_table")
  )
}
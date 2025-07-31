simulation_run_ui <- function()
{
  tagList(
    actionButton(inputId = "btn_set_parameter",
                 label   = "Set run parameter"),
    actionButton(inputId = "btn_run_simulation",
                 label   = "Run simulation"),
    # htmltools::br(), htmltools::br(),
    # textInput(inputId = "txt_quantiles",
    #           label   = "quantiles",
    #           value   = "0.05, 0.25, 0.5, 0.75, 0.95"),
    # selectInput(inputId = "seletct_quantile_type",
    #             label = "type",
    #             choices = paste(1:9),
    #             selected = "8",
    #             selectize = FALSE),
    htmltools::hr(),
    tabsetPanel(id   = "result_tabs", 
                type = "pills")
  )
}
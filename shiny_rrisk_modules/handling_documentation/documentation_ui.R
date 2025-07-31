documentation_ui <- function()
{
  tagList(
    htmltools::h4("Author List"),
    htmltools::br(),
    DT::DTOutput(outputId = "author_table"),
    htmltools::hr(),
    actionButton(inputId = "btn_add_author",
                 label   = "Add author"),
    htmltools::hr(),
    textAreaInput(inputId = "text_model_description",
                  label   = "Model Description",
                  width   = '100%',
                  height  = '400px'),
  )
}
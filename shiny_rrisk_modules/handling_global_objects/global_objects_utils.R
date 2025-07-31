#===BEGIN: GLOBAL OBJECT MODULE=================================================
add_change_global_object_dialog <- function(global_obj = list(),
                                            is_add_dialog = TRUE)
{
  if (is_add_dialog) {
    add_change_btn_text <- "Add Parameter or Function"
    modal_dialog_title  <- "Add fixed Parameter or Function"
    object_code         <- "Vectorize(function(x) {x}, USE.NAMES = FALSE)"
  } else {
    add_change_btn_text <- "Change Parameter or Function"
    modal_dialog_title  <- "Change fixed Parameter or Function"
    object_code         <- global_obj$definition
  }
  
  modalDialog(
    title = modal_dialog_title,
    fluidRow(
      column(width = 6,
             textInput(inputId = "global_object_name",
                       label   = "Name",
                       value   = global_obj$name)),
      column(width = 6,
             textInput(inputId = "global_object_unit",
                       label   = "Unit",
                       value   = global_obj$info$unit))),
    textAreaInput(inputId = "global_object_source",
                  label   = "Source",
                  value   = global_obj$info$source,
                  width   = "100%"),
    textAreaInput(inputId = "global_object_descr",
                  label   = "Description",
                  value   = global_obj$info$descr,
                  width   = "100%"),
    shinyAce::aceEditor(outputId = "global_object_definition", 
                        mode     = "r", 
                        height   = "200px", 
                        value    = object_code),
    size   = "l",
    footer = tagList(modalButton("Cancel"),
                     actionButton(inputId = "btn_add_change_global_obj",
                                  label   = add_change_btn_text))
  )
}

create_global_object_table <- function(global_object_list, colnames)
{
  toHTML <- function(input_string)
  {
    input_string <- gsub("\\n", "<\\br>", input_string)
    input_string <- gsub("  ", "&ensp;", input_string)
    HTML(input_string)
  }
  
  if (length(global_object_list) > 0) {
    df <- sapply(
      X   = global_object_list,
      FUN = function(global_obj) 
      {
        if (is.null(global_obj$info)) {
          descr <- ""
          unit  <- ""
        } else {
          descr <- global_obj$info$descr
          unit  <- global_obj$info$unit
        }
        c(global_obj$name, toHTML(global_obj$definition), unit, descr)
      },
      USE.NAMES = FALSE
    )
    df <- as.data.frame(t(df))
    rownames(df) <- NULL
    colnames(df) <- colnames
  } else {
    df <- list2DF(setNames(object = vector("list", length(colnames)), 
                           nm     = colnames))
  }
  
  actionButtons <- sapply(
    X   = seq_len(nrow(df)),
    FUN = build_action_buttons,
    FALSE, "global_obj"
  )
  
  df <- cbind.data.frame(df, Actions = actionButtons)
  
  DT::datatable(
    data      = df,
    # Need to disable escaping for html as string to work
    escape    = FALSE,
    colnames  = colnames(df),
    selection = 'single',
    options   = list(columnDefs = list(list(width = '30%',
                                           targets = c(2,4))),
                     paging     = FALSE),
    callback = DT::JS(
      "table.on('dblclick', 'td',", 
      "  function() {",
      "    const data = table.row(this).data();",
      "    Shiny.setInputValue('global_obj_table_dblclick', {list_num: data[0]});",
      "    Shiny.setInputValue('global_obj_table_dblclick', {obj_name: data[1]});",
      "  }",
      ");"
    )
  )
}
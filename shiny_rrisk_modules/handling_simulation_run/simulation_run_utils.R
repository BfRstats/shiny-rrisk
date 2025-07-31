simulation_parameter_dialog <- function(sim_param_list, is_2DMC)
{
  if (is_2DMC) {
    # 2D MC
    mc_text    <- "Number of Monte Carlo iterations for variability"
    mc_value   <- sim_param_list[["2DMC"]][["ITER1D"]]
    mc_quantiles <- "Quantiles for variability"
    sampling_type <- sim_param_list[["2DMC"]][["SAMPLING_TYPE"]]
  } else {
    # 1D MC
    mc_text    <- "Number of Monte Carlo iterations"
    mc_value   <- sim_param_list[["1DMC"]][["ITER1D"]]
    mc_quantiles <- "Quantiles"
    sampling_type <- sim_param_list[["1DMC"]][["SAMPLING_TYPE"]]
  }
  
  modalDialog(
    title = "Simulation Parameter",
    radioButtons(inputId  = "btn_sampling_type",
                 label    = "Sampling technique",
                 choices  = list("Monte-Carlo"         = "MC", 
                                 "mid latin hypercube" = "LHS"),#,
                 #"Sobol sequence"      = "sobol"),
                 selected = sampling_type,
                 inline   = TRUE),
    tags$hr(),
    numericInput(inputId = "sim_param_value_1D",
                 label   = mc_text,
                 value   = mc_value,
                 min     = 1,
                 max     = 100000),
    if (is_2DMC) {
      numericInput(inputId = "sim_param_value_2D",
                   label   = paste("Number of Monte Carlo iterations",
                                   "for uncertainty"),
                   value   = sim_param_list[["2DMC"]][["ITER2D"]],
                   min     = 1,
                   max     = 1000)
    },
    numericInput(inputId = "set_seed_value",
                 label   = "Seed value",
                 value   = sim_param_list[["SEED"]]),
    htmltools::hr(),
    textInput(inputId = "txt_quantiles",
              label   = mc_quantiles,
              value   = paste0(sim_param_list[["QUANTILES"]],
                              collapse = ", "),
              width   = "100%"),
    if (is_2DMC) {
      textInput(inputId = "txt_quantiles_2D",
                label   = "Quantiles for uncertainty",
                value   = paste0(sim_param_list[["QUANTILES_2D"]],
                                 collapse = ", "),
                width   = "100%")
    },
    selectInput(inputId   = "selected_quantile_type",
                label     = "type",
                choices   = paste(1:9),
                selected  = sim_param_list[["QUANTILE_TYPE"]],
                selectize = FALSE),
    footer = tagList(modalButton("Cancel"),
                     actionButton(inputId = "btn_change_sim_param", 
                                  label   = "Change Parameter"))
  )
}

get_plots <- function(model, node_name, plot_width)
{
  result_output <- if (min(model$get_results(node_name)) > 0) {
    tabsetPanel(
      id = paste0("result_plots_", node_name),
      tabPanel(
        title = "linear",
        plotOutput(outputId = paste0("plot_conv_", node_name),
                   width    = plot_width),
        plotOutput(outputId = paste0("plot_hist_", node_name),
                   width    = plot_width),
        plotOutput(outputId = paste0("plot_ecdf_", node_name),
                   width    = plot_width)),
        tabPanel(
          title = "log10",
          plotOutput(outputId = paste0("plot_log10_conv_", node_name),
                     width    = plot_width),
          plotOutput(outputId = paste0("plot_log10_hist_", node_name),
                     width    = plot_width),
          plotOutput(outputId = paste0("plot_log10_ecdf_", node_name),
                     width    = plot_width))
    )
  } else {
    tagList(
      plotOutput(outputId = paste0("plot_conv_", node_name),
                 width    = plot_width),
      plotOutput(outputId = paste0("plot_hist_", node_name),
                 width    = plot_width),
      plotOutput(outputId = paste0("plot_ecdf_", node_name),
                 width    = plot_width)
    )
  }

  result_output
}

get_table <- function(summary_input) 
{
  #if (attr(summary_input, "summary_type") == "1DMC") {
  if (nrow(summary_input) == 1) {
    tags$table(
      style = "width:85%",
      tags$tr(
        tagList(
          lapply(
            X   = names(summary_input),
            FUN = function(name) tags$th(name)
          )
        )
      ),
      tags$tr(
        tagList(
          lapply(
            X   = summary_input,
            FUN = function(value) tags$td(value)
          )
        )
      )
    )
  #} else if (attr(summary_input, "summary_type") == "2DMC") {
  } else {#if (nrow(summary_input) == 3) {
    tags$table(
      style = "width:85%",
      tags$tr(
        tagList(
          lapply(
            X   = c(" ", colnames(summary_input)),
            FUN = function(name) tags$th(name)
          )
        )
      ),
      tagList(
        lapply(
          X   = c(rownames(summary_input)),
          FUN = function(row_name) {
            tags$tr(
              tagList(
                lapply(
                  X   = c(row_name, summary_input[row_name,]),
                  FUN = function(x) tags$td(x)
                )
              )
            )
           }
        )
      )
    )
  }
}
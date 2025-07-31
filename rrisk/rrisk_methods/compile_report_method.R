rriskModelClass$set("public", 
  "compile_report", function(
    report_file_path, 
    template_file_path = "rrisk_report_templates/rrisk_report_template.rmd",
    silent = FALSE) 
  {
    result <- private$check_model_before_reporting(report_file_path,
                                                   silent)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    #---BEGIN: prepare data for rmarkdown---------------------------------------
    # prepare node list for rmarkdown file
    node_list <- sapply(
      X       = names(private$node_list),
      FUN     = function(node_name) {
        list(name = node_name,
             code = gsub(r"(\*)", r"(\\*)",
                         private$node_list[[node_name]]$display_code),
             info = private$node_list[[node_name]]$info,
             type = private$node_list[[node_name]]$type,
             bootstrap_data = private$node_list[[node_name]]$bootstrap_data)
      },
      simplify = FALSE
    )
    
    #---BEGIN: prepare data for global objects---------------------------------- 
    # prepare data frame for global objects for rmarkdown file
    empty <- rep("", length(private$global_expr))
    df_global_obj <- data.frame(
      name   = names(private$global_expr),
      expr   = unlist(private$global_expr_display,
                      use.names = FALSE),
      unit   = empty,
      source = empty,
      descr  = empty
    )
    
    # fill the data frame for global objects with optional info
    i <- 0
    for (global_obj_name in df_global_obj$name) {
      i <- i + 1
      if (!is.null(private$global_expr_info[[global_obj_name]])) {
        df_global_obj$unit[i]   <- private$global_expr_info[[global_obj_name]]$unit
        df_global_obj$source[i] <- private$global_expr_info[[global_obj_name]]$source
        df_global_obj$descr[i]  <- private$put_line_breaks(
                                     private$global_expr_info[[global_obj_name]]$descr, 50)
      }
    }
    #---END: prepare data for global objects------------------------------------
    
    # get MC modeling settings
    # check if 1SMC oder 2DMC
    mc_dim <- sapply(
      X         = private$node_list,
      FUN       = function(x) ifelse(is.null(x$mc_dim), 1, x$mc_dim),
      USE.NAMES = FALSE
    )
    if (all(mc_dim == 1)) {
      # 1DMC
      mc_sim_settings <- append(list("MC_TYPE" = "1DMC"),
                                private$param_list[["1DMC"]])
    } else if (any(mc_dim == 2)) {
      # 2D MC
      mc_sim_settings <- append(list("MC_TYPE" = "2DMC"),
                                private$param_list[["2DMC"]])
    } else {
      # no MC element in model
      mc_sim_settings <- list("MC_TYPE" = "none")
    }
    
    mc_sim_settings$MC_SEED <- private$param_list[["SEED"]]
    
    summary_result_list <- self$get_result_summaries()
    
    # create png file of model graph plot as a temp file
    model_graph_plot_file <- file.path(private$temp_dir_path,
                                       "model_graph_plot.png")
    png(filename = model_graph_plot_file, width = 2*480, height = 2*480)
      self$plot_model_graph(plot_groups = TRUE)
    dev.off()
    
    # create plots for node results
    graph_path_list <- private$build_plots_for_nodes()
    
    #create plots for correlations for end nodes
    cor_plot_files <- list()
    for (end_node in self$get_end_nodes()) {
      cor_plot_files[[end_node]] <- file.path(private$temp_dir_path,
                                              paste0("cor_plot_", end_node, 
                                                     ".png"))
      png(filename = cor_plot_files[[end_node]], width = 2*480, height = 2*480)
       self$get_correlation_plot(end_node)
      dev.off()
    }
    
    df_nodes <- self$get_df_nodes(info_elements = c("unit", "descr"))
    colnames(df_nodes) <- c("var name", "group", "code", "unit", "MC dim", 
                            "description", "node type")
    #---END: prepare data for rmarkdown-----------------------------------------
    # compile report
    rmarkdown::render(
      input       = template_file_path,
      output_file = report_file_path,
      params      = list(report_title   = trimws(private$model_name),
                         author_list    = private$author_list,
                         model_descr    = private$model_descr,
                         mc_settings    = mc_sim_settings,
                         model_graph    = model_graph_plot_file,
                         node_graphs    = graph_path_list,
                         cor_graphs     = cor_plot_files,
                         df_nodes       = df_nodes,#self$get_df_nodes(),
                         node_list      = node_list,
                         end_nodes      = self$get_end_nodes(),
                         df_global_obj  = df_global_obj,
                         result_summary = summary_result_list),
      clean       = TRUE,
      intermediates_dir = private$temp_dir_path,
      quiet       = FALSE
    )
    # remove temp files
    unlink(model_graph_plot_file)
    for (plot_type in graph_path_list)
      for (graph_path in plot_type)
        unlink(graph_path)
    
    invisible(result)
  }
)

#---BEGIN: util methods for public method compile_report------------------------
rriskModelClass$set("private", 
  "check_model_before_reporting", function(report_file_path, silent = TRUE) 
  {
    #check model
    result <- private$check_all(
      private$is_error(self$is_model_empty(),
                       "Model is empty"),
      private$is_model_fully_specified(),
      private$are_all_functions_specified_or_allowed(),
      private$is_single_graph(),
      private$is_graph_a_dag(),
      private$is_model_stochastic(),
      private$is_correct_file_extension(report_file_path, 
                                        ext = c("", "docx"))
    )
    if (!result$is_ok && !silent) stop(result$error_message)
    invisible(result)
  }
)

rriskModelClass$set("private", 
  "build_plots_for_nodes", function() 
  {
    if (length(private$result_list) == 0) return()
    
    #plot_node_names <- c()
    #for (node_name in names(private$node_expr))
    #  if (!is.null(private$result_list[[node_name]]))
    #    plot_node_names <- c(plot_node_names, node_name)
    plot_node_names <- names(private$result_list)
    
    plot_types <- c("convergence", "log10_convergence", "histogram", 
                    "log10_histogram", "ecdf", "log10_ecdf")
    graph_path_list <- list()
    for (plot_type in plot_types) {
      tmp <- as.list(file.path(private$temp_dir_path,
                               paste0(plot_node_names, "_", 
                                      seq_along(plot_node_names),
                                      "_", plot_type, ".png")))
      names(tmp) <- plot_node_names
      graph_path_list[[plot_type]] <- tmp
    }
    
    # plot graphs
    for (node_name in plot_node_names) 
    {
      # plot convergence
      png(filename = graph_path_list[["convergence"]][[node_name]])
        self$plot_convergence(node_name = node_name)
      dev.off()
      
      # plot histgram
      png(filename = graph_path_list[["histogram"]][[node_name]])
        self$plot_histogram(node_name = node_name)
      dev.off()
      
      # plot ecdf
      png(filename = graph_path_list[["ecdf"]][[node_name]])
        self$plot_ecdf(node_name = node_name)
      dev.off()
      
      if (min(private$result_list[[node_name]]) <= 0) next
      
      # plot log10 convergence
      png(filename = graph_path_list[["log10_convergence"]][[node_name]])
        self$plot_convergence(node_name = node_name, log = TRUE)
      dev.off()
      
      # plot log 10 histgram
      png(filename = graph_path_list[["log10_histogram"]][[node_name]])
        self$plot_histogram(node_name = node_name, log = TRUE)
      dev.off()
      
      # plot log ecdf
      png(filename = graph_path_list[["log10_ecdf"]][[node_name]])
        self$plot_ecdf(node_name = node_name, log = TRUE)
      dev.off()
    }
    
    graph_path_list
  }
)

rriskModelClass$set("private", "put_line_breaks",
  function(text, width_output = 70) {
    # split the text into words
    words <- strsplit(text, split = " ")[[1]]
    # max number of words in text
    # (this number will change, as \n-words are added)
    max_words <- length(words)
    # number of chars in one line
    chars_in_line <- 0
    # counter for the current word in words
    next_word <- 1
    # go through all the words
    while (next_word <= max_words) {
      # how many chars of words are in one line
      chars_in_line <- chars_in_line + nchar(words[next_word])
      if (chars_in_line >= width_output) {
        # has the line to many words, then take the last word to add a break
        # otherwise use the current word position to add the break
        if (chars_in_line > width_output)
          set_break_at <- next_word - 1
        else
          set_break_at <- next_word
        # add a break at break point
        words <- append(words, "\n", set_break_at)
        # reset counter for number of chars in line
        # as one character is added for space character anyway, set it to -1
        # so it becomes zero in the end
        chars_in_line <- -1
        # the words vector contains now one additional entry,
        # therefore the max number of entries in this vector must be
        # increased by one
        max_words <- max_words + 1
      }
      # go to the next word
      next_word <- next_word + 1
      # add one space charater between words
      chars_in_line <- chars_in_line + 1
    }
    
    paste(words, sep = "", collapse = " ")
  }
)
#---END: util methods for public method compile_report--------------------------

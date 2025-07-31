#---BEGIN: public main methods "save_model", and "open_model"-------------------
# MAIN METHOD save_model
rriskModelClass$set("public", "save_model",
  function(file_path, silent = FALSE) 
  {
    result <- private$is_correct_file_extension(file_path, 
                                                ext = c("", "rrisk"))
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
        
    
    if (tools::file_ext(file_path) == "") 
      file_path <- paste0(file_path, ".rrisk")
    
    model_representation <- list(
      file_version      = 4L,
      #file_version      = 5L,
      #file_type        = "shiny-rrisk",
      node_list         = private$node_list,
      param_list        = private$param_list,
      global_expr       = private$global_expr_display,
      global_expr_info  = private$global_expr_info,
      author_list       = private$author_list,
      model_descr       = private$model_descr,
      model_name        = private$model_name
    )
    
    # readable json file
    write(x    = jsonlite::serializeJSON(x      = model_representation,
                                         pretty = TRUE), 
          file = file_path)
    
    invisible(result)
  }
)

# MAIN METHOD open_model
rriskModelClass$set("public", "open_model",
  function(file_path, silent = FALSE) 
  {
    result <- private$check_this(object = file_path,
                                 preset_args(
                                   private$is_correct_file_extension, 
                                   ext = "rrisk"
                                 ),
                                 private$is_file_readable)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    result$warning <- FALSE
    result$warning_message <- ""
    
    #---BEGIN: read model file--------------------------------------------------
    model_path <- file(file_path, "r")
    model_data <- tryCatch(jsonlite::unserializeJSON(readLines(model_path)),
                           error = function(e) "no-json")
    close(model_path)
    if (is.character(model_data) &&
        model_data == "no-json") {
      if (silent)
        return(list(is_ok = FALSE,
                    error_message = "Could not read file."))
      else
        stop("Could not read file.")
    }
    #---END: read model file----------------------------------------------------
    
    # set slots in model with model_data data
    file_version <- as.integer(model_data$file_version)
    private$node_list <- if (file_version == 1L) {
      sapply(
        X   = names(model_data$node_expr),
        FUN = function(node_name) {
          new_node <- private$get_node_template()
          tmp <- deparse(model_data$node_expr[[node_name]])
          tmp <- paste(trimws(tmp), collapse = "")
          new_node$code           <- tmp
          new_node$display_code   <- model_data$node_expr_display[[node_name]]
          new_node$type <- if (is.null(model_data$node_type_list[[node_name]])) 
                             "implicit" 
                           else 
                             model_data$node_type_list[[node_name]]
          new_node$dist_def       <- model_data$node_dist_def[[node_name]]
          new_node$mc_dim         <- model_data$dist_mc_dim[[node_name]]
          new_node$info           <- model_data$node_info[[node_name]]
          new_node$group          <- model_data$node_group_list[[node_name]]
          new_node$bootstrap_data <- model_data$bootstrap_data[[node_name]]
          new_node
        },
        simplify = FALSE
      )
    } else if (file_version == 2L || file_version == 3L) {
      sapply(
        X        = model_data$node_list,
        FUN      = function(this_node) 
        {
          code <- deparse(this_node$exec_code)
          code <- paste(trimws(code), collapse = "")
          this_node$code <- code
          this_node$exec_code <- NULL
          this_node
        },
        simplify = FALSE
      )
    } else if (file_version == 4L) {
      model_data$node_list
    } else if (file_version == 5L) {
      if (model_data$file_type != "shiny-rrisk") {
        return(list(is_ok = FALSE,
                    error_message = paste("This is not a shiny-rrisk file")))
      }
      model_data$node_list
    } else {
      # ERROR in file version
      return(list(is_ok = FALSE,
                  error_message = paste("The file version '", file_version,
                                        "' is not supported")))
    }
    
    # add gloabl expression and their info
    private$global_expr_display <- model_data$global_expr
    private$global_expr_info    <- model_data$global_expr_info
    
    # check code global expr
    result <- private$check_global_expr_of_input_file()
    if (!result$is_ok) {
      self$clear_model()
      if (silent)
        return(result)
      else
        stop(result$error_message)
    }

    # pars and eval global expressions
    private$global_expr <- sapply(
      X        = private$global_expr_display,
      FUN      = function(x) eval(str2lang(x)),
      simplify = FALSE
    )
    
    # check code in node list
    result <- private$check_node_expr_of_input_file()
    if (!result$is_ok) {
      self$clear_model()
      if (silent)
        return(result)
      else
        stop(result$error_message)
    }

    # parse the code in each node
    private$exec_code <- sapply(
      X   = names(private$node_list),
      FUN = function(this_node_name)
      {
        str2lang(private$node_list[[this_node_name]]$code)
      },
      simplify = FALSE
    )
    
    # add other descriptions
    private$author_list         <- model_data$author_list
    private$model_descr         <- model_data$model_descr
    private$model_name          <- model_data$model_name
    
    #---BEGIN: set and check parameter------------------------------------------
    private$param_list <- model_data$param_list

    # for 1D MC
    if (private$param_list[["1DMC"]][["ITER1D"]] > private$default_param_list[["1DMC"]][["MAX_ITER1D"]]) {
      private$param_list[["1DMC"]]$ITER1D <- private$default_param_list[["1DMC"]][["MAX_ITER1D"]]
      # throw warning
      result$warning <- TRUE
      result$warning_message <- paste(
        result$warning_message,
        paste("Iteration of model is larger than allowed.", 
              "Reduced to allowed iteration of",
              private$default_param_list[["1DMC"]][["MAX_ITER1D"]], ".\n")
      )
    }
    
    # for 2D MC
    if (private$param_list[["2DMC"]][["ITER1D"]] > private$default_param_list[["2DMC"]][["MAX_ITER1D"]]) {
      private$param_list[["2DMC"]]$ITER1D <- private$default_param_list[["2DMC"]][["MAX_ITER1D"]]
      # throw warning
      result$warning <- TRUE
      result$warning_message <- paste(
        result$warning_message,
        paste("Iteration of model for 1dim of", 
              "2D-MC is larger than allowed.", 
              "Reduced to allowed iteration of",
              private$default_param_list[["2DMC"]][["MAX_ITER1D"]], ".\n")
      )
    }
    
    if (private$param_list[["2DMC"]][["ITER2D"]] > private$default_param_list[["2DMC"]][["MAX_ITER2D"]]) {
      private$param_list[["2DMC"]]$ITER2D <- private$default_param_list[["2DMC"]][["MAX_ITER2D"]]
      # throw warning
      result$warning <- TRUE
      result$warning_message <- paste(
        result$warning_message,
        paste("Iteration of model for 2dim of", 
              "2D-MC is larger than allowed.", 
              "Reduced to allowed iteration of",
              private$default_param_list[["2DMC"]][["MAX_ITER2D"]], ".")
      )
    }
    
    if (isTRUE(result$warning) && !silent)
      warning(result$warning_message)
    
    # is.null test just for now! 
    # Normally these params are always in the model file
    if (is.null(model_data$param_list[["SEED"]]))
      private$param_list[["SEED"]] <- private$default_param_list[["SEED"]]

    if (is.null(model_data$param_list[["QUANTILES"]]))
      private$param_list[["QUANTILES"]] <- private$default_param_list[["QUANTILES"]]
    
    if (is.null(model_data$param_list[["QUANTILES_2D"]]))
      private$param_list[["QUANTILES_2D"]] <- private$default_param_list[["QUANTILES_2D"]]
    
    if (is.null(model_data$param_list[["QUANTILE_TYPE"]]))
      private$param_list[["QUANTILE_TYPE"]] <- private$default_param_list[["QUANTILE_TYPE"]]
    #---END: set and check parameter--------------------------------------------
  
    private$update_edge_list()
    
    # reset the results
    private$result_list <- list()
    
    private$tickle_shiny()
    
    invisible(result)
  }
)

rriskModelClass$set("public", 
  "clear_model", function()
  {
    private$node_list           <- list()
    private$edge_list           <- list()
    private$exec_code           <- list()
    private$param_list          <- private$init_param_list()
    private$global_expr         <- list()
    private$global_expr_display <- list()
    private$global_expr_info    <- list()
    private$author_list         <- list()
    private$model_name          <- "no model name"
    private$model_descr         <- ""
    private$result_list         <- list()
    
    private$tickle_shiny()
  }
)
#---END: public main methods "save_model", and "open_model"---------------------

rriskModelClass$set("private",
  "check_global_expr_of_input_file", function()
  {
    # init in case no gloabl expr.
    result <- private$is_error(FALSE, "")
    # 
    allowed_func_names <- c(private$function_white_list_1,
                            private$function_white_list_2)
    #
    for (this_expr in private$global_expr_display) {
      
      result <- private$check_this(
        object = setNames(this_expr, "definition"),
        private$is_character,
        private$is_correct_syntax,
        preset_args(
          private$uses_allowed_functions,
          allowed_functions = allowed_func_names
        ),
        preset_args(
          private$is_pure_expression,
          allowed_functions = allowed_func_names
        )
      )
      
      if (!result$is_ok) break
    }
    
    result
  }
)

rriskModelClass$set("private",
  "check_node_expr_of_input_file", function()
  {
    # init in case of no nodes
    result <- private$is_error(FALSE, "")
    #
    allowed_func_names <- c(private$function_white_list_1,
                            private$get_global_func_names(),
                            private$dist_func_names,
                            "do_bootstrapping", "list", "cbind")
    #
    for (this_node_name in names(private$node_list)) {
      
      result <- private$check_this(
        object = setNames(private$node_list[[this_node_name]]$code, 
                          "user_def_expr"),
        private$is_character,
        preset_args(
          private$is_correct_syntax,
          check_for_reserved_words = FALSE
        ),
        preset_args(
          private$uses_allowed_functions,
          allowed_functions = allowed_func_names
        )
      )
      
      if (!result$is_ok) break
    }
    
    result
  }
)

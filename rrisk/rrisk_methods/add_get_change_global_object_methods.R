#---BEGIN: public methods "set_global_object", "get_global_object",
#          "get_global_object_list", "remove_global_object"---------------------
# MAIN METHOD set_global_object
rriskModelClass$set("public", 
  "add_global_object", function(obj_name, definition, info = NULL, 
                                silent = FALSE) 
  {
    # if definition is a function, convert it to a string for checking
    if (is.function(definition))
      definition <- deparse1(definition, collapse = "\n")
    
    # check input
    result <- private$check_add_global_object(obj_name, definition, info)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    # remove trailing blanks from object name
    obj_name <- trimws(obj_name)
    
    # parse and evaluate the expression
    private$global_expr[[obj_name]] <- eval(str2lang(definition))
      
    # format the expression for display
    private$global_expr_display[[obj_name]] <- definition
    # this tidy source does not work as good as i hoped for
    # private$global_expr_display[[obj_name]] <- formatR::tidy_source(
    #   text          = definition,
    #   comment       = TRUE,
    #   intent        = 3,
    #   brace.newline = TRUE,
    #   arrow         = TRUE,
    #   width.cutoff  = 80,
    #   args.newline  = TRUE,
    #   output        = FALSE)$text.tidy
    
    # add object description
    private$global_expr_info[[obj_name]] <- info
    
    # if the added global object is not a function, i.e., parametric a fixed 
    # value, and if an implicit node of the same name of this global object 
    # already exist, then remove this implicit node from  the node_list, and 
    # rebuild the edge_list
    if (!is.function(private$global_expr[[obj_name]]) &&
        !is.null(private$node_list[[obj_name]]) &&
        private$node_list[[obj_name]]$type == "implicit") {
      private$node_list[[obj_name]] <- NULL
      private$update_edge_list()
    }
    
    private$tickle_shiny()
    
    invisible(result)
  }
)

# MAIN METHOD change_global_object
rriskModelClass$set("public", 
  "change_global_object", function(obj_name, new_obj_name = obj_name, 
                                   definition = NULL, info = NULL, 
                                   silent = FALSE) 
  {
    # check input
    result <- private$check_change_global_object(obj_name,
                                                 new_obj_name,
                                                 definition,
                                                 info)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    # remove trailing blanks from object name
    obj_name     <- trimws(obj_name)
    new_obj_name <- trimws(new_obj_name)
    
    # get old global object
    old_obj <- self$get_global_object(obj_name = obj_name, silent = silent)
    
    if (is.null(definition)) definition <- old_obj$definition
    if (is.null(info)) info <- old_obj$info
    
    # remove old global object
    self$remove_global_object(obj_name = obj_name, silent = silent)
    
    # set new global object
    self$add_global_object(obj_name   = new_obj_name, 
                           definition = definition, 
                           info       = info, 
                           silent     = silent)
    
    private$tickle_shiny()
    
    invisible(result)
  }
)

# MAIN METHOD remove_global_object
rriskModelClass$set("public", 
  "remove_global_object", function(obj_name = NULL, obj_num = NULL, 
                                   silent = FALSE) 
  {
    # check input
    result <- private$checks_for_global_object(obj_name, obj_num)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (is.null(obj_name))
      obj_name <- names(private$global_expr)[obj_num]
    else
      obj_name <- trimws(obj_name)
    
    is_not_function <- !is.function(private$global_expr[[obj_name]])
    
    private$global_expr[[obj_name]]         <- NULL
    private$global_expr_display[[obj_name]] <- NULL
    private$global_expr_info[[obj_name]]    <- NULL
    
    # if removed global object is not a function, i.e., parametric a fixed value,
    # update edge list, and add an implicit node for the obj_name, 
    # in case the removed name obj_name is already used already by the user.
    if (is_not_function) {
      private$update_edge_list()
      # is there a node name in edge list, that does not exist in the node list?
      source_node_names <- c()
      for (source_nodes in private$edge_list) {
        source_node_names <- c(source_node_names, source_nodes)
      }
      implicit_node_names <- setdiff(source_node_names, 
                                     names(private$node_list))
      # add nodes that are in the edge list to the node list as implicit node
      for (implicit_node_name in implicit_node_names) {
        private$node_list[[implicit_node_name]] <- private$get_node_template()
      }
    }

    private$tickle_shiny()
    
    invisible(result)
  }
)

# MAIN METHOD get_global_object
rriskModelClass$set("public", 
  "get_global_object", function(obj_name = NULL, obj_num = NULL, silent = FALSE) 
  {
    # check input
    result <- private$checks_for_global_object(obj_name, obj_num)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (is.null(obj_name))
      obj_name <- names(private$global_expr)[obj_num]
    else
      obj_name <- trimws(obj_name)
    
    list("name"       = obj_name,
         "info"       = private$global_expr_info[[obj_name]],
         "definition" = private$global_expr_display[[obj_name]])
  }
)

# MAIN METHOD get_global_object_list
rriskModelClass$set("public", 
  "get_global_object_list", function() 
  {
    sapply(
      X        = names(private$global_expr), 
      FUN      = self$get_global_object, 
      simplify = FALSE
    )
  }
)
#---END: public methods "set_global_object", "get_global_object",
#       "get_global_object_list", "remove_global_object"------------------------

#---BEGIN: private methods for public methods "set_global_object", 
#          "get_global_object", "get_global_object_list", 
#          "remove_global_object"-----------------------------------------------
rriskModelClass$set("private", 
  "check_add_global_object", function(obj_name, definition = NULL, info = NULL) 
  {
    private$check_all(
      private$check_this(object = setNames(obj_name, "object name"),
                         #private$is_not_missing,
                         private$is_character,
                         private$is_valid_name,
                         preset_args(
                          private$is_unique_param_name,
                          def = definition
                         )),
      private$check_this(object = setNames(definition, "definition"),
                         #private$is_not_missing,
                         #private$is_proper_param_type,
                         private$is_character,
                         private$is_correct_syntax,
                         preset_args(
                           private$uses_allowed_functions,
                           allowed_functions = c(private$function_white_list_1,
                                                 private$function_white_list_2)
                         ),
                         preset_args(
                           private$is_pure_expression,
                           allowed_functions = c(private$function_white_list_1,
                                                 private$function_white_list_2)
                         )),
      if (!is.null(info)) {
        private$is_correct_info_list(info)
      }
    )
  }
)

rriskModelClass$set("private", 
  "check_change_global_object", function(obj_name, new_obj_name = NULL, 
                                         definition = NULL, info = NULL) 
  {
    private$check_all(
      private$check_this(object = setNames(obj_name, "object_name"),
                         #private$is_not_missing,
                         private$is_character,
                         preset_args(
                           private$is_known_name,
                           known_names = names(private$global_expr)
                         )),
      if (!identical(trimws(obj_name), trimws(new_obj_name))) {
        private$check_this(object = setNames(new_obj_name, "new_object_name"),
                           #private$is_not_missing,
                           private$is_character,
                           private$is_valid_name,
                           preset_args(
                             private$is_unique_param_name,
                             def = definition
                           ))
      },
      if (!is.null(definition)) {
        private$check_this(object = setNames(definition, "definition"),
                           #private$is_not_missing,
                           #private$is_proper_param_type,
                           private$is_character,
                           private$is_correct_syntax,
                           preset_args(
                             private$uses_allowed_functions,
                             allowed_functions = c(private$function_white_list_1,
                                                   private$function_white_list_2)
                           ),
                           preset_args(
                             private$is_pure_expression,
                             allowed_functions = c(private$function_white_list_1,
                                                   private$function_white_list_2)
                           ))
      },
      if (!is.null(info)) {
        private$is_correct_info_list(info)
      }
    )
  }
)

rriskModelClass$set("private", 
  "checks_for_global_object", function(obj_name, obj_num) 
  {
    private$check_all(
      private$is_error(is.null(obj_name) && is.null(obj_num),
                       "Either obj_name, or obj_num must be set."),
      private$is_error(!is.null(obj_name) && !is.null(obj_num),
                       "Only obj_name, or obj_num must be set"),
      if (!is.null(obj_name)) {
        private$check_this(object = setNames(obj_name, "obj_name"),
                           private$is_character,
                           preset_args(
                             private$is_known_name,
                             known_names = names(private$global_expr)
                           ))
      },
      if (!is.null(obj_num)) {
        private$check_this(object = setNames(obj_num, "obj_num"),
                           private$is_numeric,
                           preset_args(
                             private$is_in_range,
                             max_range = length(private$global_expr)
                           ))
      }
    )
  }
)
#---END: private methods for public methods "set_global_object", 
#        "get_global_object", "get_global_object_list", "remove_global_object"--

#---BEGIN: main public methods "add_node", "remove_node", "change_node",
#          "get_df_nodes", and "get_node_content"-------------------------------
# MAIN METHOD add_node
rriskModelClass$set("public", 
  "add_node", function(node_name, user_def_expr = NULL, param_dist = NULL, 
                       bootstrap = NULL, info = NULL, group = NULL, mc_dim = 1L, 
                       replace_implicit_node = TRUE, silent = FALSE,
                       is_bootstrap_item = FALSE) 
  {
    # check user input for errors
    result <- private$check_add_node_input(node_name, replace_implicit_node,
                                           user_def_expr, param_dist, 
                                           bootstrap, info, mc_dim)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    # node name is there and all is fine
    # now remove trailing blanks
    node_name <- trimws(node_name)
    
    # get template for new node; it is a list
    new_node <- private$get_node_template()
    
    # fill the new node
    if (!is.null(user_def_expr)) {
      # node contains a user defined expression
      new_node$type         <- "user_defined"
      new_node$code         <- user_def_expr
      new_node$display_code <- user_def_expr
      if (is_bootstrap_item) new_node$mc_dim <- mc_dim
      
    } else if (!is.null(param_dist)) {
      # node contains information for a parametric distribution
      new_node$type         <- "param_dist"
      new_node$code         <- private$create_dist_code(
        dist_name = param_dist$name,
        dist_def  = param_dist$def,
        n         = ifelse(mc_dim == 1L, "ITER1D", "ITER2D")
      )
      new_node$display_code <- private$create_dist_display_code(
        dist_name = param_dist$name,
        dist_def  = param_dist$def
      )
      new_node$dist_def     <- param_dist
      new_node$mc_dim       <- mc_dim # 1 ("variable") or 2 ("uncertain")
      
    } else if (!is.null(bootstrap)) {
      # node contains information for bootstrapping
      new_node$type <- "bootstrap"
      # create node expression for bootstrapping
      # First: get all functions connected to this node
      bs_sum_funcs <- sapply(
        X   = bootstrap$op,
        FUN = function(op) op
      )
      # Second: prepare, parse, and add bootstrap node expression for this node
      new_node$code <- paste0(
        "do_bootstrapping(", 
        "n = ", ifelse(mc_dim == 1L, "ITER1D", "ITER2D"),
        ", input = ", node_name, ",",
        paste0("func_list = list(", 
               paste(bs_sum_funcs,"=",bs_sum_funcs,
               collapse = ", "), ")"),
        ")"
      )
      # create display code for bootstrapping
      new_node$display_code   <- "bootstrap_generator()"
      
      new_node$bootstrap_data <- private$build_bootstrap_entry(bootstrap)
      new_node$mc_dim         <- mc_dim # 1 ("variable") or 2 ("uncertain")

      # add read nodes from bootstrap$op, if bootstrap is not NULL
      bootstrap_result_names <- csv_to_vec(names(bootstrap$op))
      for (op_name in bootstrap_result_names) {
        self$add_node(node_name             = op_name, 
                      user_def_expr         = paste0(node_name, 
                                                     "[['", op_name, "']]"), 
                      group                 = group,
                      mc_dim                = mc_dim,
                      replace_implicit_node = TRUE, 
                      silent                = silent,
                      is_bootstrap_item     = TRUE)
        # these nodes not are in fact implicit and should not be changed
        # by the user
        private$node_list[[op_name]]$type <- "bootstrap_result"
      }
      
    }
    
    # if there is some node info, add it to the node info list
    if (!is.null(info))
      new_node$info <- info
    
    # if there is a grouping label for nodes, add it
    if (!is.null(group) &&
        any(nzchar(trimws(group)))) {
      new_node$group <- csv_to_vec(group)
    }
    
    # add new node to node list
    private$node_list[[node_name]] <- new_node
    
    # add evaluated code
    private$exec_code[[node_name]] <- str2lang(new_node$code)
    
    # add implicit source nodes to node list,
    # if the node expression implies that
    private$add_implicit_source_nodes(new_node$display_code)
    
    # update edge list
    private$update_edge_list()
    
    # for shiny only, to make R6 class reactive
    private$tickle_shiny()
    
    invisible(result)
  }
)

# MAIN METHOD change_node
rriskModelClass$set("public", 
  "change_node", function(node_name, new_node_name = node_name, 
                          user_def_expr = NULL, param_dist = NULL, 
                          bootstrap = NULL, info = NULL, group = NULL, 
                          mc_dim = NULL, reliability = NULL, silent = FALSE) 
  {
    # check if input is ok
    result <- private$check_change_node_input(node_name, new_node_name, 
                                              user_def_expr, param_dist,
                                              bootstrap,
                                              info, mc_dim)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    # remove trailing blanks from character input
    node_name     <- trimws(node_name)
    new_node_name <- trimws(new_node_name)
    
    # If new_node_name is different from old node_name, check if other down
    # stream nodes use this node.
    # If so, then change in the exec. code, and display code the old
    # node_name to new_node_name.
    # If the downstream node is a parm_dist node, replace the old node_name
    # also for the necessary entries in the list def in the list dist_def.
    if (node_name != new_node_name) {
      private$change_node_name_in_other_nodes(node_name, new_node_name)
    }
    
    # get old node data  
    node_content <- self$get_node_content(node_name = node_name)
    
    # get node type
    node_type <- private$node_list[[node_name]]$type
    
    # replace not defined node data with the old node data
    if (is.null(user_def_expr) && 
        is.null(param_dist) &&
        is.null(bootstrap)) {
      user_def_expr <- node_content$code
      param_dist    <- node_content$param_dist
      bootstrap     <- node_content$bootstrap_data
    } 
    if (is.null(info))   info   <- node_content$info
    if (is.null(group))  group  <- node_content$group
    if (is.null(mc_dim)) mc_dim <- node_content$mc_dim
    
    # remove old node
    result <- self$remove_node(node_name = node_name, silent = silent)
    
    # if there were no problems when removing old node, add new node
    if (result$is_ok) {
      result <- self$add_node(
        node_name         = new_node_name, 
        user_def_expr     = user_def_expr, 
        param_dist        = param_dist,
        bootstrap         = bootstrap,
        info              = info, 
        group             = group,
        mc_dim            = mc_dim,
        silent            = silent,
        is_bootstrap_item = ifelse(node_type == "bootstrap_result", TRUE, FALSE)
      )
      
      if (node_type == "bootstrap_result")
        private$node_list[[new_node_name]]$type <- "bootstrap_result"
    }
    
    invisible(result)
  }
)

# MAIN METHOD remove_node
rriskModelClass$set("public", 
  "remove_node", function(node_name = NULL, node_num = NULL, silent = FALSE) 
  {
    # check if input is ok
    result <- private$check_node_name_and_node_num(node_name, node_num)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    if (is.null(node_name)) {
      node_name <- names(private$node_list)[node_num]
    } else
      node_name <- trimws(node_name)
    
    # remove downstream node that are connected with an upstream bootstrap node
    if (private$node_list[[node_name]]$type == "bootstrap") {
      # collect names of bootstrap result vars connected to this node
      bs_result_node_names <- NULL
      for (sum_stat in private$node_list[[node_name]]$bootstrap_data$sum_stats){
        bs_result_node_names <- c(bs_result_node_names, sum_stat$result_vars)
      }
      # remove nodes of bootstrap result vars
      for (boot_var_name in bs_result_node_names) {
        private$node_list[[boot_var_name]] <- NULL
      }
      private$update_edge_list()
    }
    
    # Is this node is implied by another node (i.e. is source node of another 
    # node?), and not a parameter (i.e. not a function) of the global object 
    # list? If yes, changed the node to be a implicit node, otherwise delete 
    # the node
    if (private$is_source_node(node_name)) {
      # is implicit node
      # this node is implied by an down stream node
      private$node_list[[node_name]] <- private$get_node_template()
    } else {
      # is explicit node
      private$node_list[[node_name]] <- NULL
    }
    
    # delete all implicit source nodes of this node
    for (source_node_name in private$edge_list[[node_name]]) {
      if (private$node_list[[source_node_name]]$type == "implicit") {
        private$node_list[[source_node_name]] <- NULL
      }
    }
    
    # check for all nodes, if a node has implicit source nodes; 
    # if so, add them
    for (node in private$node_list)
      private$add_implicit_source_nodes(node$display_code)
    
    # remove evaluated code
    private$exec_code[[node_name]] <- NULL

    # update edge list
    private$update_edge_list()
    
    # tell shiny R6 object changed
    private$tickle_shiny()
    
    invisible(result)
  }
)

# MAIN METHOD get_node_content
rriskModelClass$set("public", 
  "get_node_content", function(node_name = NULL, node_num = NULL, 
                               silent = FALSE) 
  {
    # check input
    result <- private$check_node_name_and_node_num(node_name, node_num)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    if (is.null(node_name))
      node_name <- names(private$node_list)[node_num]
    else
      node_name <- trimws(node_name)
    
    node <- private$node_list[[node_name]]
    # node content if node is a for explicit nodes
    node_content <- list(node_name  = node_name,
                         node_type  = node$type,
                         info       = node$info,
                         group      = node$group)
    
    if (node$type == "user_defined") {
      node_content$code <- node$display_code
    } else if (node$type == "param_dist") {
      node_content$param_dist <- self$get_complete_dist_list(node_name)
      node_content$mc_dim     <- node$mc_dim
    } else if (node$type == "bootstrap") {
      node_content$bootstrap_data <- private$get_bootstrap_input_list(node_name)
      node_content$mc_dim         <- node$mc_dim
    } else if (node$type == "bootstrap_result") {
      node_content$code   <- node$display_code
      node_content$mc_dim <- node$mc_dim
    }
    
    if (silent) {
      result$node_content <- node_content
      invisible(result)
    } else
      node_content
  }
)

# MAIN METHOD get_df_nodes
rriskModelClass$set("public", 
  "get_df_nodes", function(get_pretty_code = TRUE, info_elements = NULL)
  {
    
    if (missing(info_elements) || is.null(info_elements)) {
      info_elements <- NULL
      for (node in private$node_list) {
        info_elements <- c(info_elements, names(node$info))
      }
      info_elements <- unique(info_elements)
    }
    
    col_names <- c("node_name", "node_group", "node_code", "node_mc_dim", 
                   "node_type", info_elements)
    df_nodes <- data.frame(matrix(ncol = length(col_names),
                                  nrow = 0))
    colnames(df_nodes) <- col_names

    # fill node data frame
    for (node_name in names(private$node_list)) {
      
      node <- private$node_list[[node_name]]
      
      # grouping name for node
      if (is.null(node$group))
        node_group <- "--"
      else
        node_group <- paste(node$group, collapse = ", ")
      
      # code of node
      if (get_pretty_code)
        node_expr <- node$display_code
      else
        node_expr <- node$code
      
      # MC dimension of node
      if (is.null(node$mc_dim))
        node_mc_dim <- "--"
      else
        node_mc_dim <- if (node$mc_dim == 1L) "V" else "U"
      
      this_df_node <- data.frame(
        node_name   = node_name,
        node_group  = node_group,
        node_code   = node_expr,
        node_mc_dim = node_mc_dim,
        node_type   = node$type
      )
      
      # get infos about node
      if (!is.null(info_elements)) {
        info <- sapply(
          X   = info_elements,
          FUN = function(info_element)
          {
            this_info <- node$info[[info_element]]
            if (is.null(this_info) ||
                !nzchar(trimws(this_info)))
              "--"
            else
              trimws(this_info)
          },
          simplify = FALSE
        )
        this_df_node <- cbind.data.frame(this_df_node, list2DF(info))
      }
 
      # add row with information of this node to node data frame
      df_nodes <- rbind.data.frame(df_nodes, this_df_node)
    }
    

    df_nodes
  }
)

rriskModelClass$set("public",
  "get_node_names", function()
  {
    names(private$node_list)
  }
)

rriskModelClass$set("public",
  "get_groups", function()
  {
    result <- c()
    for (node in private$node_list) {
      if (!is.null(node$group) && nzchar(node$group))
        result <- c(result, node$group)
    }
    result
  }
)

#---END: main public methods "add_node", "remove_node", and "change_node"
#        "get_df_nodes", and "get_node_content"---------------------------------

#---BEGIN: check functions for node methods-------------------------------------
rriskModelClass$set("private", 
  "check_add_node_input", function(node_name, replace_implicit_node, 
                                   user_def_expr, param_dist, bootstrap, info, 
                                   mc_dim) 
  {
    private$check_all(
      # check if input for node name is fine
      private$check_this(object = setNames(node_name, "node_name"),
                         private$is_character,
                         private$is_valid_name,
                         preset_args(
                           private$is_unique_node_name,
                           exception_implicit_node = replace_implicit_node
                         )),
      # either user_def_expr, or param_dist needs, or bootstrap needs
      # to be defined
      private$is_error(all(is.null(user_def_expr), 
                           is.null(param_dist),
                           is.null(bootstrap)),
                       paste("Either user_def_expr, param_dist, or bootstrap", 
                             "must be defined.")),
      # Only one of these can be defined: user_def, or, param_dist, or bootstrap.
      # Not more.
      private$is_error(sum(!sapply(list(user_def_expr,
                                        param_dist,
                                        bootstrap), is.null)) > 1,
                       paste("Only user_def_expr, or param_dist, or bootstrap", 
                             "must be defined.")),
      # if user_def_expr is set, check if user defined expression is ok
      if (!is.null(user_def_expr)) {
        private$check_this(object = setNames(user_def_expr, 
                                             "user defined expression"),
                           private$is_character,
                           private$is_correct_syntax,
                           preset_args(
                             private$uses_allowed_functions,
                             allowed_functions = c(private$function_white_list_1,
                                                   private$get_global_func_names())
                           )
                           )
      },
      # if param_dist is set, check if input for parametric distribution is ok
      if (!is.null(param_dist)) {
        private$check_param_dist(param_dist)
      },
      # is param_dist is set, check if mc_dim is 1, or 2
      if (!is.null(param_dist)) {
        private$check_this(object = setNames(ifelse(is.null(mc_dim), "", mc_dim), 
                                             "mc_dim"),
                           private$is_integer,
                           preset_args(
                             private$is_in_range,
                             max_range = 2
                           ))
      },
      if (!is.null(bootstrap)) {
        # check bootstrap data
        private$check_all(
          # is bootstrap data of type numeric?
          private$is_error(!is.numeric(bootstrap$data),
                           "Boostrap data must be given as comma seperated values."),
          # are bootstrap var_names ok?
          private$are_bootstrap_var_names_ok(names(bootstrap$op), 
                                             replace_implicit_node)
          # are the bootstrap functions allowed?
        )
      },
      # if info list is set, check input for info
      if (!is.null(info)) {
        private$is_correct_info_list(info)
      }
    )
  }
)

rriskModelClass$set("private", 
  "check_change_node_input", function(node_name, new_node_name, user_def_expr, 
                                      param_dist, bootstrap, info, mc_dim) 
  {
    private$check_all(
      # check the input for node name
      private$check_this(object = setNames(node_name, "node name"),
                         #private$is_not_missing,
                         private$is_character,
                         preset_args(
                           private$is_known_name,
                           known_names = names(private$node_list)
                         )),
      # if the input for new node name is different from old node name,
      # check the input for new node name
      if (trimws(node_name) != trimws(new_node_name)) {
        private$check_this(object = setNames(new_node_name, 
                                             "new node name"),
                           private$is_character,
                           private$is_valid_name,
                           preset_args(
                             private$is_unique_node_name,
                             exception_implicit_node = TRUE
                           ))
      },
      # check if user defined expression is OK
      if (!is.null(user_def_expr)) {
        private$check_this(object = setNames(user_def_expr, 
                                             "user_def_expr"),
                           private$is_character,
                           private$is_correct_syntax,
                           preset_args(
                             private$uses_allowed_functions,
                             allowed_functions = c(private$function_white_list_1,
                                                   private$get_global_func_names())
                           )
                           )
      },
      if (!is.null(bootstrap)) {
        # check bootstrap data
        private$check_all(
          # is bootstrap data of type numeric?
          private$is_error(!is.numeric(bootstrap$data),
                           "Boostrap data must be given as comma seperated values."),
          # are bootstrap var_names ok?
          private$are_bootstrap_var_names_ok(names(bootstrap$op), FALSE, FALSE)
        )
      },
      # check if input for parametric distribution is ok
      if (!is.null(param_dist)) {
        private$check_param_dist(param_dist)
      },
      # check if mc_dim is 1, or 2
      if (!is.null(mc_dim)) {
        private$check_this(object = setNames(mc_dim, "mc_dim"),
                           private$is_integer,
                           preset_args(
                             private$is_in_range,
                             max_range = 2
                           ))
      },
      # check input for info
      if (!is.null(info)) {
        private$is_correct_info_list(info)
      }
    )
  }
)

rriskModelClass$set("private", 
  "check_node_name_and_node_num", function(node_name, node_num) 
  {
    private$check_all(
      # either node_name, or node_num needs to specified, 
      # but not both at the same time
      private$is_error(is.null(node_name) && is.null(node_num),
                       "Either node_name or node_num must be specified."),
      private$is_error(!is.null(node_name) && !is.null(node_num),
                       "Only node_name or node_num have to be specified."),
      # if node_name is specified, check input for node_name
      if (!is.null(node_name)) {
        private$check_this(object = setNames(node_name, "node_name"),
                           private$is_character,
                           preset_args(
                             private$is_known_name,
                             known_names = names(private$node_list)
                           ))
      },
      # if node_num is specified, check input for node_num
      if (!is.null(node_num)) {
        private$check_this(object = setNames(node_num, "node_num"),
                           private$is_integer,
                           preset_args(
                             private$is_in_range,
                             max_range = length(private$node_list)
                           ))
      }
    )
  }
)

rriskModelClass$set("private", 
  "check_param_dist", function(param_dist) 
  {
    check_all_def_entries_syntax <- function()
    {
      # check param expr
      for (param_name in names(param_dist$def)) {
        this_param <- as.character(param_dist$def[[param_name]])
        result <- private$check_this(object = setNames(this_param,
                                                       param_name),
                                     private$is_correct_syntax,
                                     preset_args(
                                       private$uses_allowed_functions,
                                       allowed_functions = c(private$function_white_list_1,
                                                             private$get_global_func_names())
                                     )
                                     )
        if (!result$is_ok) break
      }
      result
    }
    
    private$check_all(
      # check if dist name is of type character, and actually exists
      private$check_this(object = setNames(param_dist$name, 
                                            "dist name"),
                         private$is_character,
                         preset_args(
                           private$is_known_name,
                           known_names = names(private$param_dist_def)
                         )),
      private$is_valid_param_dist_list(setNames(names(param_dist),
                                                "param_dist")),
      private$check_this(object = names(param_dist$def),
                         private$has_no_duplication,
                         preset_args(
                           private$has_no_unknown_params,
                           known_params = names(private$param_dist_def[[param_dist$name]]$def)
                         ),
                         preset_args(
                           private$has_no_non_optional_params_missing,
                           this_params = private$param_dist_def[[param_dist$name]]$def
                         )),
      # check param expr
      check_all_def_entries_syntax(),
      private$are_params_in_range(
        user_input = param_dist$def,
        dist_info  = private$param_dist_def[[param_dist$name]]
      )
    )
  }
)
#---END: check functions for node methods---------------------------------------

#---BEGIN: Utility functions for node methods-----------------------------------
rriskModelClass$set("private",
  "get_node_template", function()
  {
    new_node <- list(
      type           = "implicit",
      code           = "NaN",
      display_code   = "",
      info           = NULL,
      group          = NULL,
      mc_dim         = NULL,
      dist_def       = NULL,
      bootstrap_data = NULL
    )
    
    new_node
  }
)

# is used also by public method "open_model", "add_node", "remove_node"
rriskModelClass$set("private",
  "update_edge_list", function()
  {
    private$edge_list <- sapply(
      X        = private$node_list,
      FUN      = function(node) private$get_source_nodes(node$display_code),
      simplify = FALSE
    )
  }
)

rriskModelClass$set("private", 
  "create_dist_code", function(dist_name, dist_def, n)
  {
    # creates string for function of parametric distribution with format:
    # <name of function for param dist>(n=n, 
    #                                   <name of param1>=<value of param1>, 
    #                                   <more params>,
    #                                   type=SAMPLING_TYPE)
    # Example1: rrisk_rpoisson(n=ITER1D,lambda=10,type=SAMPLING_TYPE)
    # Example2: rrisk_rpoisson(n=ITER1D,lambda=portion_size*dose,
    #                          type=SAMPLING_TYPE)
    string_dist_def <- paste(
      names(dist_def),
      lapply(
        X   = dist_def,
        FUN = function(x)
        {
          if (length(x) == 1)
            # return single value
            x
          else if (is.numeric(x))
            # return vector
            paste0("c(", toString(x), ")")
          else
            # return matrix; this will be eventually be evaluated as matrix
            paste0("cbind(", toString(x), ")")
        }
      ),
      collapse = ",",
      sep      = "="
    )
    dist_func_formals <- paste0("(n=", n, ",", string_dist_def, 
                                ",type=SAMPLING_TYPE)")
    # create function call string
    paste0(private$dist_func_names[[dist_name]], dist_func_formals)
  }
)

# is used also by public method "open_model", "add_node"
rriskModelClass$set("private",
  "create_dist_display_code", function(dist_name, dist_def)
  {
    string_dist_def <- paste(
      names(dist_def),
      lapply(
        X   = dist_def,
        FUN = function(x)
        {
          if (length(x) == 1)
            # return single value
            x
          else if (is.numeric(x))
            # return vector
            paste0("c(", toString(x), ")")
          else
            # return matrix; this will be eventually be evaluated as matrix
            paste0("cbind(", toString(x), ")")
        }
      ),
      collapse = ", ",
      sep      = " = "
    )
    # create function call for displaying only
    paste0(dist_name, "(", string_dist_def, ")")
  }
)

# is called by private methods "update_edge_list", and "add_implicit_source_nodes"
rriskModelClass$set("private", 
  "get_source_nodes", function(node_code) 
  {
    df_parseData <- utils::getParseData(parse(text        = node_code,
                                              keep.source = TRUE))
    source_nodes <- with(
      data = df_parseData,
      expr = text[token == "SYMBOL" &
                  !(text %in% c(private$reserved_words,
                                names(private$param_list[["2DMC"]]),
                                names(private$global_expr_display)))]
    )
    unique(source_nodes)
  }
)

rriskModelClass$set("private", 
  "add_implicit_source_nodes", function(node_code) 
  {
    # if the node code implies source nodes that still do not
    # exist, add these source nodes with node code of "NaN"
    for (source_node_name in private$get_source_nodes(node_code)) {
      if (is.null(private$node_list[[source_node_name]])) { 
        # then this node does not exist
        private$node_list[[source_node_name]] <- private$get_node_template()
      }
    }
  }
)

rriskModelClass$set("private", 
  "is_source_node", function(node_name) 
  {
    result <- FALSE
    for (source_node_names in private$edge_list) {
      if (node_name %in% source_node_names) {
        result <- TRUE
        break
      }
    }
    result
  }
)

rriskModelClass$set("private", 
  "build_bootstrap_entry", function(bootstrap_data)
  {
    result <- lapply(
      X   = names(bootstrap_data$op),
      FUN = function(entry_name)
      {
        # build named list entry
        list(func_name   = bootstrap_data$op[[entry_name]],
             result_vars = trimws(strsplit(entry_name, ",")[[1]]))
      }
    )
    list(data      = bootstrap_data$data,
         sum_stats = result)
  }
)

rriskModelClass$set("private", 
  "get_bootstrap_input_list", function(node_name)
  {
    # get the operation table as a list
    # entry: result_vars = name_of_function
    op <- list()
    for (sum_stats in private$node_list[[node_name]]$bootstrap_data$sum_stats) {
      tmp <- setNames(object = list(sum_stats$func_name),
                      nm     = paste(sum_stats$result_vars, collapse = ", "))
      op <- c(op, tmp)
    }
    # return list with bootstrap information
    list(
      data = private$node_list[[node_name]]$bootstrap_data$data,
      op   = op
    )
  }
)

rriskModelClass$set("private",
  "change_node_name_in_other_nodes", function(node_name, new_node_name)
  {
    # The new_node_name is different from old node_name, check if other down
    #  stream nodes use this node. If so, then change in the exec. code, and 
    #  display code the old node_name to new_node_name.
    # If the downstream node is a param_dist node, replace the old node_name
    # also for the necessary entries in the list def in the list dist_def.
    # 
    # go over edge_list
    for (this_node_name in names(private$edge_list)) {
      # Is the old node_name a source node for this_node_name?
      # If so, change the old node_name to new_node_name. Otherwise go to the 
      # next node in edge_list.
      if (!(node_name %in% private$edge_list[[this_node_name]])) next
      # display code
      tmp <- private$node_list[[this_node_name]]$display_code
      tmp <- replace_node_name(node_name, new_node_name, tmp)
      private$node_list[[this_node_name]]$display_code <- tmp
      # exec code
      tmp <- private$node_list[[this_node_name]]$code
      tmp <- replace_node_name(node_name, new_node_name, tmp)
      private$node_list[[this_node_name]]$code <- tmp
      # if node is a param. distribution node, change also the dist def
      if (private$node_list[[this_node_name]]$type == "param_dist") {
        # loop over entry of list def in list dist_def
        # def is a string, or a vector of strings
        def <- private$node_list[[this_node_name]]$dist_def$def
        for (def_name in names(def)) {
          # grepl will be true for any sub-string matching the pattern:
          # e.g., node_name = "W_b" will match also with "W_bc". But the
          # content of def can be also an expression; e.g., "W_b * time".
          # The function replace_node_name matches correctly only to the correct
          # node_name.
          if (any(grepl(pattern = node_name, x = def[[def_name]]))) {
            # the def content can be string, or a vector of strings
            tmp <- c()
            for (this_def in def[[def_name]]) {
              tmp <- c(tmp,
                       replace_node_name(node_name, 
                                         new_node_name, 
                                         this_def))
            }
            private$node_list[[this_node_name]]$dist_def$def[[def_name]] <- tmp
          }
        }
      }
    }
    private$update_edge_list()
  }
)

rriskModelClass$set("private",
  "get_global_func_names", function()
  {
    func_names <- c()
    for (global_obj_name in names(private$global_expr)) {
      if (is.function(private$global_expr[[global_obj_name]])) {
        func_names <- c(func_names, global_obj_name)
      }
    }
    func_names
  }
)
#---END: Utility functions for node methods-------------------------------------

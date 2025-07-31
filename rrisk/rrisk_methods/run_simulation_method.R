#---BEGIN: public main method "run_simulation"----------------------------------
# MAIN METHOD run_simulation
rriskModelClass$set("public", 
  "run_simulation", function(target_node = NULL, silent = FALSE) 
  {
    # check if model graph is ok
    result <- self$check_model(target_node, silent)
    if (!result$is_ok)
      if (silent)
        return(invisible(result))
      else
        stop(result$error_message)
    
    # set seed for random number generation
    set.seed(private$param_list[["SEED"]])
    
    mc_dim <- NULL
    for (node in private$node_list)
      if (!is.null(node$mc_dim))
        mc_dim <- c(mc_dim, node$mc_dim)
    
    if (all(mc_dim == 1) ||
        all(mc_dim == 2)) {
      # 1D MC run
      result <- private$do_1DMC_run(target_node, silent)
    } else { 
      # 2D MC run
      result <- private$do_2DMC_run(target_node, silent)
    }
    
    invisible(result)
  }
)
#---END: public main method "run_simulation"------------------------------------

#---BEGIN: private methods for public method "run_simulation"-------------------
rriskModelClass$set("private", 
  "do_1DMC_run", function(target_node, silent)
  {
    # prepare iteration parameter list
    # a 1D model can contain only nodes of type "variable", i.e. mc_dim=1,
    # or only node of type "uncertain", i.e. mc_dim=2
    iter_params <- private$param_list[["1DMC"]]
    iter_params[["ITER2D"]] <- private$param_list[["1DMC"]][["ITER1D"]]

    # do the 1D MC simulation run
    private$result_list <- tryCatch(
      expr    = private$run_1D_simulation(target_node, iter_params),
      error   = function(e) e,
      warning = function(w) w
    )
    
    # check if warning or error occurred during simulation
    result <- private$is_error(
      any(c("error", "warning") %in% attr(private$result_list, "class")),
      paste("Error occured during simulation run:", private$result_list)
    )
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
      
    # extract all non-constant node items 
    # (i.e. discard all node results with only constants)
    is_vector <- vapply(
      X         = private$result_list, 
      FUN       = function(x) length(x) > 1,
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )
    
    # remove all bootstrap nodes
    is_not_bootstrap <- vapply(
      X         = names(private$result_list),
      FUN       = function(node_name) 
      {
        if (private$node_list[[node_name]]$type == "bootstrap")
          FALSE
        else
          TRUE
      },
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )
      
    # set the clean result list
    private$result_list <- private$result_list[is_vector & is_not_bootstrap]
    
    result
  }
)

rriskModelClass$set("private",
  "do_2DMC_run", function(target_node, silent)
  {
    iter_params <- private$param_list[["2DMC"]]
    iter_params[["ITER2D"]] <- 1
    
    # do the 2D MC simulation run
    private$result_list <- replicate(
      n        = private$param_list[["2DMC"]][["ITER2D"]],
      expr     = tryCatch(
        private$run_1D_simulation(target_node, iter_params),
        error   = function(e) e,
        warning = function(w) w
      ),
      simplify = FALSE
    )
    
    # check if warning or error occurred during simulation in at least
    # one run of the 2DMC
    for (this_result in private$result_list) {
      result <- private$is_error(
        any(c("error", "warning") %in% attr(this_result, "class")),
        paste("Error occured during simulation run:", this_result)
      )
      if (!result$is_ok)
        if (silent)
          return(result)
        else
          stop(result$error_message)
    }
    
    # Transform the current result list object, from a list where every entry
    # contains the 1D MC results of every node for each 2D MC iteration, 
    # into a result list, where the names of the list entries are the node
    # names, and the entries are the combined results per node over all 
    # 2D MC iterations.
    #  
    # 1D MC nodes (mc_dim = 1) are then vectors; every element is result 
    # from a 2D MC iteration.
    # 
    # 2D MC nodes (mc_dim = 2) are then matrices; every column is the result
    # from a 2D MC iteration.
    #
    # This function does this now:
    private$result_list <- sapply(
      X   = names(private$node_list),
      FUN = function(node_name) {
        sapply(X   = private$result_list, 
               FUN = function(x) x[[node_name]])
      }, 
      simplify = FALSE
    )
    
    # exclude 1D vectors with no variance, i.e. containing only constants
    is_not_constant <- vapply(
      X   = names(private$result_list),
      FUN = function(node_name) {
        this_result <- private$result_list[[node_name]]
        if (is.vector(this_result) &&
            !is.list(this_result) &&
            min(this_result) == max(this_result))
          FALSE
        else
          TRUE
      },
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )
    
    # remove all bootstrap nodes
    is_not_bootstrap <- vapply(
      X         = names(private$result_list),
      FUN       = function(node_name) 
      {
        if (private$node_list[[node_name]]$type == "bootstrap")
          FALSE
        else
          TRUE
      },
      FUN.VALUE = logical(1),
      USE.NAMES = FALSE
    )
    
    # set the clean result list
    private$result_list <- private$result_list[is_not_constant & 
                                               is_not_bootstrap]
    
    result
  }
)

rriskModelClass$set("private", 
  "run_1D_simulation", function(target_node = NULL, param_list) 
  {
    # prepare result list to collect everything
    node_names <- names(private$node_list)
    n <- length(node_names)
    
    # local run-time object for variables needed to be safed between calls 
    # of evaluate_model
    run_obj <- new.env()
    run_obj$visited_node <- setNames(object = rep(FALSE, n),
                                     nm     = node_names)
    run_obj$result_list  <- setNames(object = vector(mode   = "list", 
                                                     length = n),
                                     nm     = node_names)
    run_obj$param_list   <- param_list
    
    # RUN SIMULATION FOR EACH END NODE
    if (is.null(target_node))
      for (end_node in self$get_end_nodes())
        tmp <- private$evaluate_model(end_node, run_obj)
    else
      tmp <- private$evaluate_model(target_node, run_obj)
    
    run_obj$result_list
  }
)

rriskModelClass$set("private", 
  "evaluate_model", function(target_node, run_obj) 
  {
    # if node was already visited, just give back the node result
    if (run_obj$visited_node[target_node])
      return(run_obj$result_list[[target_node]])
    # mark this target_node as visited
    # run_obj is an external environment, therefore the results are stored 
    # outside of the scope of this function
    run_obj$visited_node[target_node] <- TRUE
    # evaluate the source nodes recursively over all source nodes
    # of this target node
    source_node_results <- sapply(
      X        = private$edge_list[[target_node]],
      FUN      = function(this_source_node)
      {
        private$evaluate_model(this_source_node, run_obj)
      },
      simplify = FALSE
    )
    # add other lists to source node result list
    source_node_results <- c(
      source_node_results, 
      run_obj$param_list, 
      private$global_expr,
      setNames(object = list(private$node_list[[target_node]]$bootstrap_data),
               nm     = target_node)
    )
    # evaluate code of target node with result of all variables 
    # of its source nodes, and with all parameters
    result <- eval(expr  = private$exec_code[[target_node]],
                   envir = source_node_results)
    # store result in run-time object run_obj
    # it is an external environment, therefore the results are stored 
    # outside of the scope of this function
    run_obj$result_list[[target_node]] <- result
    # return result; is needed, as it is a recursive function
    result
  }
)
#---END: private methods for public method "run_simulation"---------------------
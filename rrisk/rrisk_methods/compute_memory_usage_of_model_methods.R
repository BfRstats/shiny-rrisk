#---BEGIN: public main method "get_memory_usage_for_graph"----------------------
rriskModelClass$set("public", 
  "get_memory_usage_for_graph", function(unit = c("bit", "MB", "GB")) 
  {
    if (missing(unit))
      unit <- "bit"
    # obtain a list that contains for every node if it is V (T|F),
    # and if it is U (T|F)
    result <- private$get_mc_dims_for_all_nodes()
    # compute needed memory to hold results for alls nodes
    needed_memory <- private$compute_memory_usage(result, unit)
    
    needed_memory
  }
)
#---END: public main method "get_memory_usage_for_graph"------------------------

#---BEGIN: private methods for public method "get_memory_usage_for_graph"-------
rriskModelClass$set("private", 
  "get_mc_dims_for_all_nodes", function() 
  { 
    # driver for 'get_mc_dims'
    # prepare result list to collect everything
    node_names <- names(private$node_list)
    n <- length(node_names)
    # local run-time object for variables needed to be safed between calls 
    # of evaluate_model
    run_obj <- new.env()
    run_obj$visited_node <- setNames(object = rep(FALSE, n),
                                     nm     = node_names)
    run_obj$result_list <- setNames(object = vector(mode = "list", length = n),
                                    nm     = node_names)

    for (end_node in self$get_end_nodes())
      tmp <- private$get_mc_dims(end_node, run_obj)
    
    run_obj$result_list
  }
)

rriskModelClass$set("private", 
  "get_mc_dims", function(this_node, run_obj) 
  {
    # if node was already visited, just give back the node result
    if (run_obj$visited_node[this_node])
      return(run_obj$result_list[[this_node]])
    # mark this target_node as visited
    # it is an external environment, therefore the results are stored externally
    run_obj$visited_node[this_node] <- TRUE
    
    # set the plain node MC Dim for this node, without any inherited
    # MC Dim from source nodes
    this_node_mc_dim <- list(dim1 = FALSE, dim2 = FALSE)
    mc_dim <- private$node_list[[this_node]]$mc_dim
    if (!is.null(mc_dim)) this_node_mc_dim[[mc_dim]] <- TRUE
    
    # get names of all source nodes for this target node
    source_node_names <- private$edge_list[[this_node]]
    # evaluate the source nodes recursively
    source_node_results <- sapply(
      X        = source_node_names,
      FUN      = function(this_source_node)
        private$get_mc_dims(this_source_node, run_obj),
      simplify = FALSE
    )

    # evaluate results
    for (this_result in source_node_results) {
      if (this_result$dim1)
        this_node_mc_dim$dim1 <- TRUE
      if (this_result$dim2)
        this_node_mc_dim$dim2 <- TRUE
    }
    
    # store result in run-time object
    # it is an external environment, therefore the results are stored externally
    run_obj$result_list[[this_node]] <- this_node_mc_dim
    # return result
    this_node_mc_dim
  }
)

rriskModelClass$set("private", 
  "compute_memory_usage", function(mc_dim_list, unit) 
  {
    if (self$is_2DMC()) { # 2D MC
      dim1_iter <- private$param_list[["2DMC"]]$ITER1D
      dim2_iter <- private$param_list[["2DMC"]]$ITER2D
    } else {# 1D MC
      # for models with only variable nodes
      dim1_iter <- private$param_list[["1DMC"]]$ITER1D
      # for models with only uncertain nodes
      dim2_iter <- private$param_list[["1DMC"]]$ITER1D
    }

    total <- 0
    for (list_entry in mc_dim_list) {
      if (list_entry$dim1 && !list_entry$dim2)
        total <- total + dim1_iter # this nodes result vec is iter1D long
      else if (!list_entry$dim1 && list_entry$dim2)
        total <- total + dim2_iter # this nodes result vec is iter2D long
      else if (list_entry$dim1 && list_entry$dim2)
        total <- total + dim1_iter * dim2_iter # this nodes result vec is iter1D * iter2D long
    }

    total <- total * 64 # R uses 64 bit for numeric type
    if (unit == "MB")
      total <- total * 1.25e-7
    else if (unit == "GB")
      total <- total * 1.25e-10
    
    total
  }
)
#---END: private methods for public method "get_memory_usage_for_graph"---------

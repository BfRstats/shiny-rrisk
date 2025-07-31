rriskModelClass$set("public", 
  "get_param_dist_info", function() 
  {
    private$param_dist_def
    #sapply(
    #  X        = private$node,
    #  FUN      = function(x) x$def,
    #  simplify = FALSE
    #)
  }
)

rriskModelClass$set("public", 
  "get_param_dist_list", function() 
    private$param_dist_def
)

rriskModelClass$set("public",
  "get_param_dist_display_names", function()
  {
    sapply(X         = private$param_dist_def,
           FUN       = function(x) x$display_name,
           simplify  = TRUE,
           USE.NAMES = FALSE)
  }
)

rriskModelClass$set("public", 
  "get_default_dist_params", function(param_dist_name) 
  {
    sapply(X        = private$param_dist_def[[param_dist_name]]$def,
           FUN      = function(x) x$init,
           simplify = FALSE)
  }
)

rriskModelClass$set("public", 
  "get_complete_dist_list", function(node_name) 
  {
    # get the parameters for this distribution
    #this_params <- private$node_dist_def[[node_name]]
    #if (is.null(private$node_list[[node_name]]$param_dist_def)) return(NULL)
    this_params <- private$node_list[[node_name]]$dist_def
    dist_name <- this_params$name
    # get the missing definition entry names
    missing_def_names <- setdiff(names(private$param_dist_def[[dist_name]]$def),
                                 names(this_params$def))
    # get the missing entries as a complete list
    for (def_name in missing_def_names)
      this_params$def[[def_name]] <- private$param_dist_def[[dist_name]]$def[[def_name]]$init
    this_params
  }
)
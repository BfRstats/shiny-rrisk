#library(R6)
#library(igraph)
#library(rmarkdown)
#library(ggplot2)
#library(formatR)
#library(flextable)

source("rrisk_distributions/rrisk_distributions.R")
source("rrisk_distributions/rrisk_distributions_definitions.R")

#source("rrisk_utility_functions/rrisk_utility_functions.R")
source("rrisk_utility_functions/do_bootstrapping.R")
source("rrisk_utility_functions/preset_args_of_function.R")
source("rrisk_utility_functions/vec_switch.R")
source("rrisk_utility_functions/csv_to_vec.R")
source("rrisk_utility_functions/replace_node_name.R")

r_dist_names <- c("unif", "binom", "nbinom", "geom", "hyper", "pois", "beta", 
                  "norm", "lnorm", "cauchy", "exp", "gamma", "weibull")

# R6-Class for the rrisk model
rriskModelClass <- R6::R6Class(classname = "rriskModelClass",
  private = list(
    # named list with all nodes
    # each entry of list node_list, is a list with the following entries:
    # - type: Type is of type 'character' with one of the following values:
    #   - "implicit"
    #   - "user_defined"
    #   - "param_dist"
    #   - "bootstrap"
    #   - "bootstrap_result"
    # - exec_code: is of type 'language'
    # - display_code: is of type 'character'
    # - info: is a named list, or NULL
    #   in shiny rrisk this is usually:
    #   - source: where the information came from
    #   - unit: unit of the result of the node
    #   - descr: description of the node
    # - group: is a vector of type 'character', or NULL
    # - mc_dim: is a integer, or NULL
    # - dist_def: is a named list, or NULL; if list then it contains the name
    # of the distribution, and a named list "def" with the values or expressions
    # for the parametric distribution
    # - bootstrap_data: is a named list, or NULL
    node_list             = list(),
    # edge_list: named list with edges for the nodes, i.e. the graph
    # the names of the list are nodes other nodes are pointing to
    # the entry is a vector with the node names that are pointing to the node,
    # i.e. who are source nodes of this node
    # Example:
    # $this_node
    # [1] "other_node", "another_node"
    # $other_node
    # [1] "yet_another_node"
    # final source nodes do not appear as names of the edge_list 
    # (here: another_node, yet_another_node)
    # end nodes (or roots) do not appear as entries of the edge_list
    # (here: this_node)
    edge_list             = list(),
    # named list with all the nodes and their evaluated code for execution,
    # internal use only for running the simulation
    exec_code             = list(),
    # named list for general parameters needed that can be 
    # changed by the user
    param_list            = list(),
    # named list of general parameters needed, but cannot be changed
    # by users
    default_param_list    = list(),
    # global_expr: named list of user defined functions and params
    # global_expr_display: named list of user defined functions and params as
    # character (it is a formated strinf using formatR)
    # global_expr_info: name list with extra info for user defined functions 
    # and params
    # global_obj_list      = list(),
    global_expr           = list(),
    global_expr_display   = list(),
    global_expr_info      = list(),
    # dist_func_names: function names of random number generators for 
    # parametric distributions; not for change
    dist_func_names       = list(),
    # named list of definitions of the used parametric distributions
    # not for change
    param_dist_def        = list(),
    # white list of general functions of allowed functions
    function_white_list_1 = c(), # scalar vectorized functions
    function_white_list_2 = c(), # other functions
    function_white_list_bs = c(), # allowed intrinsic function for bootstrapping
    reserved_words        = c(),
    #
    author_list           = list(),
    model_name            = "no model name",
    model_descr           = "",
    result_list           = list(), # contains the list with computed results
    temp_dir_path         = NULL,
    # prototype function for shiny Rrisk R6 class; makes R6 responsive
    tickle_shiny          = function() NULL
  ),
  public = list(
    initialize = function(MAX_MC1D    = 100000, # max. iterations 1D MC
                          MAX_MC2D_1D = 5000,   # max. iter. 2D MC first dimension
                          MAX_MC2D_2D = 1000)   # max. iter. 2D MC second dimension
    {
      # set available parametric distributions
      private$param_dist_def <- get_param_dist_def()
      private$dist_func_names <- sapply(
        X        = private$param_dist_def,
        FUN      = function(x) x$function_call,
        simplify = FALSE
      )
      # default values for simulation
      # user can change values in param_list only up to this values
      private$default_param_list <- list(
        "1DMC" = list("MAX_ITER1D" = MAX_MC1D),
        "2DMC" = list("MAX_ITER1D" = MAX_MC2D_1D,
                      "MAX_ITER2D" = MAX_MC2D_2D),
        "QUANTILES"     = c(0.05, 0.25, 0.5, 0.75, 0.95),
        "QUANTILES_2D"  = c(0.05, 0.95),
        "QUANTILE_TYPE" = 8,
        "SEED"          = 42
      )
      # set MC sampling params
      private$param_list <- private$init_param_list()
      # reserved words
      private$reserved_words <- c("ITER1D", "ITER2D", "SAMPLING_TYPE")
      # set white list for general functions
      #private$restrict_functions <- restrict_functions
      # white list of allowed functions
      # 1. list: functions allowed for nodes, and global objects
      # these function are vectorized scalar functions, so f: R^n -> R^n
      # 2. list: functions allowed only within global functions
      # these global functions are then again vectorized scalar functions of
      # type f: R^n -> R^n.
      # The functions inside the global functions are most of 
      # type f: R^n -> R; f: R -> R^n
      private$function_white_list_1 <- c(
        # functions allowed for user defined node expressions and global objects
        # only vectorized scalar functions of type f: R -> R
        # ATTENTION: the combine function c breaks this, but is here for 
        # convenience
        "log", "logb", "log10", "log2", "log1p", "exp", "expm1", "sqrt", "abs",
        "sign",
        "sin", "cos", "tan", "acos", "asin", "atan", "atan2", "cospi", "sinpi", 
        "tanpi", "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
        "beta", "lbeta", "gamma", "lgamma", "psigamma", "digamma", "trigamma", 
        "choose", "lchoose", "factorial", "lfactorial", 
        "trunc", "ceiling", "floor", "round", "signif",
        "ifelse", "c", "vec_switch")
      private$function_white_list_2 <- c(
        # functions only allowed for global objects, not save for usage in nodes
        # f: R^n -> R
        "sum", "prod", "mean", "median", "sd", "max", "min", "length",
        # f: R -> R^n
        "seq_len", "rep", "seq", "vector",
        # other functions that should not be used directly in nodes
        "cumsum", "cummax", "cummin", "cumprod",
        "range", "all", "any", "which",
        "lapply", "sapply", "mapply", "vapply", "apply", "replicate",
        "simplify2array",
        "Vectorize", "Reduce", "Filter", "Find", "Map", "Negate", "Position",
        "sort", "order", "rank",
        "list", "as.list", "seq_along", "switch",
        "sample", "sample.int",
        paste0("d", r_dist_names),
        paste0("p", r_dist_names),
        paste0("q", r_dist_names),
        paste0("r", r_dist_names),
        "data.frame", "ecdf",  
        "is.infinite", "is.na", "is.nan", "is.null",
        "lm", "coef", "optim")
      private$function_white_list_bs <- c(
        # intrinsic functions allowed for summary statistics for bootstrapping
        # f: R^n -> R
        "sum", "prod", "mean", "median", "sd", "max", "min"
      )
      # create a tempdir for storing temporary files
      private$temp_dir_path <- tempdir()
      # prevent debug-mode when a stop is found
      options(error = NULL)
    }
    #finalize = function() {
    #}
  )
)

# source and add methods to rriskModelClass
source("rrisk_methods/add_get_change_remove_node_methods.R")
source("rrisk_methods/add_get_change_remove_author_methods.R")
source("rrisk_methods/add_get_change_global_object_methods.R")
source("rrisk_methods/plot_model_graph_methods.R")
source("rrisk_methods/plot_convergence_histogram_ecdf_methods.R")
source("rrisk_methods/check_model_methods.R")
source("rrisk_methods/set_get_simulation_params_methods.R")
source("rrisk_methods/save_open_clear_model_methods.R")
source("rrisk_methods/run_simulation_method.R")
source("rrisk_methods/compute_memory_usage_of_model_methods.R")
source("rrisk_methods/compile_report_method.R")
source("rrisk_methods/get_param_dist_methods.R")
source("rrisk_methods/set_get_model_description_methods.R")
source("rrisk_methods/set_get_model_name_methods.R")
source("rrisk_methods/get_results_methods.R")
source("rrisk_methods/get_correlations_methods.R")
source("rrisk_methods/atomic_check_methods.R")


rriskModelClass$set("public", 
  "is_model_empty", function()
  {
    length(private$node_list) == 0
  }
)

rriskModelClass$set("public", 
  "is_2DMC", function()
  {
    is_2dmc_model <- FALSE
    is_1d <- FALSE
    is_2d <- FALSE
    for (node in private$node_list) {
      if (is.null(node$mc_dim)) next
      if (node$mc_dim == 1) is_1d <- TRUE
      if (node$mc_dim == 2) is_2d <- TRUE
      if (is_1d && is_2d) {
        is_2dmc_model <- TRUE
        break
      }
    }
    is_2dmc_model
  }
)

# method is used by private methods "get_model_graph", "run_1D_simulation", 
# "get_mc_dims_for_all_nodes", "check_if_is_single_graph"
rriskModelClass$set("public", 
  "get_end_nodes", function() 
  {
    # end nodes have no edges (arrows) pointing away from them,
    # only edges (arrows) pointing to them
    # i.e. they are not source nodes for other nodes
    # In the edge list, they do not appear as an entry
    nodes_in_edge_list <- NULL
    for (entry in private$edge_list)
      nodes_in_edge_list <- c(nodes_in_edge_list, entry)

    nodes_in_node_list <- names(private$node_list)
    nodes_in_node_list[!(nodes_in_node_list %in% nodes_in_edge_list)]
  }
)

#---BEGIN: private utility methods shared with other methods--------------------
# private method called by public method "check_model", and 
# private method "is_unique_node_name"
rriskModelClass$set("private", 
  "get_empty_nodes", function() 
  {
    empty_nodes <- NULL
    for (node_name in names(private$node_list))
      if (private$node_list[[node_name]]$type == "implicit")
        empty_nodes <- c(empty_nodes, node_name)
    empty_nodes
  }
)

rriskModelClass$set("private",
  "init_param_list", function()
  {
    list(
      "1DMC" = list("ITER1D"        = private$default_param_list[["1DMC"]][["MAX_ITER1D"]],
                    "SAMPLING_TYPE" = "MC"),
      "2DMC" = list("ITER1D"        = private$default_param_list[["2DMC"]][["MAX_ITER1D"]],
                    "ITER2D"        = private$default_param_list[["2DMC"]][["MAX_ITER2D"]],
                    "SAMPLING_TYPE" = "MC"),
      "QUANTILES"     = private$default_param_list[["QUANTILES"]],
      "QUANTILES_2D"  = private$default_param_list[["QUANTILES_2D"]],
      "QUANTILE_TYPE" = private$default_param_list[["QUANTILE_TYPE"]],
      "SEED"          = private$default_param_list[["SEED"]]
    )
  }
)
#---END: private utility methods shared with other methods----------------------
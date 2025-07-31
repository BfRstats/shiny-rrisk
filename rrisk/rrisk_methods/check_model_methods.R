#---BEGIN: public method "check_model"------------------------------------------
rriskModelClass$set("public", 
  "check_model", function(target_node = NULL, silent = FALSE) 
  {
    #check model
    result <- private$check_all(
      if (!is.null(target_node)) {
        private$check_this(object = setNames(target_node, target_node),
                           private$is_character,
                           preset_args(
                             private$is_known_name,
                             known_names = names(private$node_list)
                           ))
      },
      private$is_error(self$is_model_empty(),
                       "Model is empty"),
      private$is_model_fully_specified(),
      private$are_all_functions_specified_or_allowed(),
      private$is_single_graph(),
      private$is_graph_a_dag(),
      private$is_model_stochastic()
    )
    
    if (!result$is_ok && !silent) stop(result$error_message)
    invisible(result)
  }
)
#---END: public method "check_model"--------------------------------------------
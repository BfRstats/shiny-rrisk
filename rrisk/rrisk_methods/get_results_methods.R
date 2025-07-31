rriskModelClass$set("public", 
  "get_results", function(node_name = NULL, node_names_only = FALSE)#,
                          #silent = FALSE) 
  {
    # result <- private$check_input_for_get_results(node_name)
    # if (!result$is_ok)
    #   if (silent)
    #     return(result)
    #   else
    #     stop(result$error_message)
    # 
    # result_list <- if (node_names_only) {
    #                  if (is.null(node_name))
    #                    names(private$result_list)
    #                  else
    #                    names(private$result_list[[node_name]])
    #                } else {
    #                  if (is.null(node_name))
    #                    private$result_list
    #                  else
    #                    private$result_list[[node_name]]
    #                }
    # 
    # result$result_list <- result_list
    
    if (node_names_only) {
      if (is.null(node_name))
        names(private$result_list)
      else
        names(private$result_list[[node_name]])
    } else {
      if (is.null(node_name))
        private$result_list
      else
        private$result_list[[node_name]]
    }
    
  }
)

rriskModelClass$set("public", 
  "get_result_summary", function(node_name, quantiles = NULL, 
                                 quantiles_2D = NULL, quantile_type = NULL,
                                 silent = FALSE) 
  {
    result <- private$check_input_for_result_summary(
      node_name, quantiles, quantile_type #, quantiles_2D
    )
    #result <- private$check_this(object = node_name,
    #                             private$is_character,
    #                             #private$is_known_node_name,
    #                             private$is_result_available)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (is.null(quantiles)) 
      quantiles <- private$param_list[["QUANTILES"]]

    if (is.null(quantiles_2D)) 
      quantiles_2D <- private$param_list[["QUANTILES_2D"]]
        
    if (is.null(quantile_type))
      quantile_type <- private$param_list[["QUANTILE_TYPE"]]
    
    unit <- trimws(private$node_list[[node_name]]$info$unit)
    if (length(unit) == 0 || !nzchar(unit)) unit <- "--"
    
    if (is.vector(private$result_list[[node_name]])) {
      # 1D MC result
      node_summary <- c(
        "mean"   = mean(private$result_list[[node_name]]),
        "median" = median(private$result_list[[node_name]]),
        "sd"     = sd(private$result_list[[node_name]]),
        quantile(
          x     = private$result_list[[node_name]],
          probs = quantiles,
          #names = FALSE,
          type  = quantile_type
        )
      )
      node_summary <- signif(node_summary, 3)
      # convert to data.frame
      df_node_summary <- as.data.frame(t(as.vector(node_summary)))
      colnames(df_node_summary) <- names(node_summary)
      df_node_summary <- cbind.data.frame(
        df_node_summary,
        data.frame("unit" = unit)
      )
    } else if (is.matrix(private$result_list[[node_name]])) {
      # 2D MC result
      node_summary <- apply(
        X      = private$result_list[[node_name]], 
        MARGIN = 2, 
        FUN    = function(x) c(mean   = mean(x),
                               median = median(x),
                               sd     = sd(x),
                               quantile(x     = x,
                                        probs = quantiles,
                                        type  = quantile_type))
      )
      node_summary <- apply(
        X      = node_summary, 
        MARGIN = 1, 
        FUN    = function(x) signif(c(mean(x), median(x), sd(x),
                                      quantile(x, 
                                               probs = quantiles_2D,
                                               type  = quantile_type)), 
                                    digits = 3)
      )

      # convert to data.frame
      df_node_summary <- as.data.frame(node_summary)
      rownames(df_node_summary) <- c("mean", "median", "sd", 
                                     paste0(100*quantiles_2D, "%"))

      df_node_summary <- cbind.data.frame(
        df_node_summary,
        data.frame("unit" = rep(unit, nrow(df_node_summary)))
      )
    } else {
      df_node_summary <- data.frame()
    }
    
    if (silent) {
      result$summary <- df_node_summary
      invisible(result)
    } else
      df_node_summary
  }
)

rriskModelClass$set("public", 
  "get_result_summaries", function() 
  {
    # get results for the each node
    sapply(X        = names(private$result_list),
           FUN      = function(node_name) self$get_result_summary(node_name),
           simplify = FALSE)
  }
)

#===============================================================================

rriskModelClass$set("private",
  "check_input_for_result_summary", 
  function(node_name, quantiles, quantile_type)
  {
    tmp <- private$check_all(
      private$check_this(object = setNames(node_name, "node name"),
                         private$is_character,
                         #private$is_known_node_name,
                         private$is_result_available),
      if (!is.null(quantiles)) {
        private$check_all(
          private$is_error(
            !is.numeric(quantiles), 
            paste("Check your input for quantiles.",
                  "It must be a value, or comma seperated list of values.")
          ),
          private$is_error(
            min(quantiles) < 0 ||
            max(quantiles) > 1,
            "Quantiles must be in the range from 0 to 1."
          )
        )
      },
      if (!is.null(quantile_type)) {
        private$check_this(
          object = setNames(quantile_type, "quantile type"),
          private$is_integer,
          preset_args(private$is_in_range, max_range = 9)
        )
      }
    )
    tmp
  }
)


#---BEGIN: main public methods "get_simulation_params", "set_simulation_params"-
# MAIN METHOD get_simulation_params
rriskModelClass$set("public", "get_simulation_params",
  function()
  {
    private$param_list
  }
)

# MAIN METHOD set_simulation_params
rriskModelClass$set("public", "set_simulation_params",
  function(MC1D = NULL, MC2D = NULL, seed = NULL,
           quantiles = NULL, quantiles_2D = NULL, quantile_type = NULL, 
           silent = FALSE) 
  {
    result <- private$check_parameter_input(MC1D, MC2D, seed,
                                            quantiles, quantiles_2D,
                                            quantile_type)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (!is.null(MC1D))
      for (entry in names(MC1D))
        private$param_list[["1DMC"]][[entry]] <- MC1D[[entry]]
      
    if (!is.null(MC2D))
      for (entry in names(MC2D))
        private$param_list[["2DMC"]][[entry]] <- MC2D[[entry]]
    
    if (!is.null(seed))
      private$param_list[["SEED"]] <- seed
    
    if (!is.null(quantiles))
      private$param_list[["QUANTILES"]] <- quantiles
    
    if (!is.null(quantiles_2D))
      private$param_list[["QUANTILES_2D"]] <- quantiles_2D
    
    if (!is.null(quantile_type))
      private$param_list[["QUANTILE_TYPE"]] <- quantile_type
    
    invisible(result)
  }
)
#---END: main public methods "get_simulation_params", "set_simulation_params"---

#---BEGIN: private method for public method "set_simualtion_params"-------------
rriskModelClass$set("private", "check_parameter_input",
  function(MC1D, MC2D, seed, quantiles, quantiles_2D, quantile_type) 
  {
    private$check_all(
      if (!is.null(MC1D)) {
        private$check_all(
          private$is_error(!is.list(MC1D), "MC1D must be  a list."),
          private$is_error(
            any(this <- !(names(MC1D) %in% names(private$param_list[["1DMC"]]))),
            paste("List for MC1D contains unknown entries:", names(MC1D)[this],
                  collapse = " ")
          ),
          private$is_number(setNames(MC1D[["ITER1D"]], "MC iterations")),
          private$is_in_range(
            setNames(MC1D[["ITER1D"]], "MC iterations"),
            max_range = private$default_param_list[["1DMC"]][["MAX_ITER1D"]]
          ),
          if (!is.null(MC1D[["SAMPLING_TYPE"]])) {
            private$is_error(
              !(MC1D[["SAMPLING_TYPE"]] %in% c("MC", "LHS")),
              paste("Your input for the sampling_type is unknown.",
                    "Allowed inputs are 'MC', 'LHS'.")
            )
          }
        )
      },
      if (!is.null(MC2D)) {
        private$check_all(
          private$is_error(!is.list(MC2D), "MC2D must be  a list."),
          private$is_error(
            any(this <- !(names(MC2D) %in% names(private$param_list[["2DMC"]]))),
            paste("List for MC2D contains unknown entries:", names(MC2D)[this],
                  collapse = " ")
          ),
          private$is_number(setNames(MC2D[["ITER1D"]], "iter1d")),
          private$is_in_range(
            setNames(MC2D[["ITER1D"]], "iter1d"),
            max_range = private$default_param_list[["2DMC"]][["MAX_ITER1D"]]
          ),
          private$is_number(setNames(MC2D[["ITER2D"]], "iter2d")),
          private$is_in_range(
            setNames(MC2D[["ITER2D"]], "iter2d"),
            max_range = private$default_param_list[["2DMC"]][["MAX_ITER2D"]]
          ),
          if (!is.null(MC2D[["SAMPLING_TYPE"]])) { 
            private$is_error(
              !(MC2D[["SAMPLING_TYPE"]] %in% c("MC", "LHS")),
              paste("Your input for the sampling_type is unknown.",
                    "Allowed inputs are 'MC', 'LHS'.")
            )
          }
        )
      },
      if (!is.null(seed)) {
        private$is_number(seed)
      },
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
      if (!is.null(quantiles_2D)) {
        private$check_all(
          private$is_error(
            !is.numeric(quantiles_2D), 
            paste("Check your input for quantiles 2D.",
                  "It must be a value, or comma seperated list of values.")
          ),
          private$is_error(
            min(quantiles_2D) < 0 ||
            max(quantiles_2D) > 1,
            "Quantiles 2D must be in the range from 0 to 1."
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
  }
)
#---END: private method for public method "set_simulation_params"---------------

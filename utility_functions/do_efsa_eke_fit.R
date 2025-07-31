do_efsa_eke_fit <- function(quantile_data, cdf_data, modPERT = TRUE)
{
  #---BEGIN: check input--------------------------------------------------------
  result <- list(is_ok = TRUE, error_message = "")
  
  if (is.character(quantile_data)) {
    result$is_ok <- FALSE
    result$error_message <- paste("Input is wrong for quantile data:",
                                  paste(quantile_data, collapse = ","), "\n",
                                  "Use comma seperated values.",
                                  "E.g.: 0, 0.5, 1.0")
    return(result)
  }

  if (is.character(cdf_data)) {
    result$is_ok <- FALSE
    result$error_message <- paste("Input is wrong for cdf data:",
                                  paste(cdf_data, collapse = ","), "\n",
                                  "Use comma seperated values.",
                                  "E.g.: 0, 0.5, 1.0")
    return(result)
  }

  if (length(quantile_data) < 3 || length(cdf_data) < 3) {
    result$is_ok <- FALSE
    result$error_message <- "Not enough data provided"
    return(result)
  }

  if (length(quantile_data) != length(cdf_data)) {
    result$is_ok <- FALSE
    result$error_message <- "Quantile and CDF must be of same length"
    return(result)
  }

  if (is.unsorted(quantile_data) || is.unsorted(cdf_data)) {
    result$is_ok <- FALSE
    result$error_message <- "Input must be of increasing order"
    return(result)
  }

  if (any(cdf_data < 0) || any(cdf_data > 1)) {
    result$is_ok <- FALSE
    result$error_message <- "cdf_data must be >= 0 & <= 1"
    return(result)
  }
  #---END: check input----------------------------------------------------------
  
  # do fit
  fit <- fit_modPERT_to_data_for_EFSA_EKE(quantile_data, cdf_data, modPERT)
  #print(fit)
  
  x <- seq(from       = min(min(quantile_data), fit$def$min), 
           to         = max(max(quantile_data), fit$def$max), 
           length.out = 1000)
  
  # return result
  list(is_ok = TRUE,
       error_message = "",
       fit = fit,
       cdf = list(x = x,
                  y = rrisk_pmodpert(x, 
                                     min   = fit$def$min,
                                     mode  = fit$def$mode,
                                     max   = fit$def$max,
                                     shape = fit$def$shape)),
       pdf = list(x = x,
                  y = rrisk_dmodpert(x,
                                     min   = fit$def$min,
                                     mode  = fit$def$mode,
                                     max   = fit$def$max,
                                     shape = fit$def$shape)))
}

# function for fitting EFSA-EKE to modPERT distribution
fit_modPERT_to_data_for_EFSA_EKE <- function(quantile_data, cdf_data, 
                                             modPERT = TRUE)
{
  start_params <- c()
  lower <- c()

  if (min(cdf_data) > 0) {
    start_params <- c(start_params,
                      min = min(quantile_data))
    lower <- c(lower,
               min = -Inf)
  } else {
    min_val <- min(quantile_data)
  }

  start_params <- c(start_params,
                    mode = median(quantile_data))
  lower <- c(lower,
             mode = -Inf)

  if (max(cdf_data) < 1) {
    start_params <- c(start_params,
                      max = max(quantile_data))
    lower <- c(lower,
               max = -Inf)
  } else {
    max_val <- max(quantile_data)
  }

  if (modPERT) {
     start_params <- c(start_params,
                       shape = 4)
     lower <- c(lower,
                shape = 0)
  }
   
  result <- stats::optim(
    par = start_params,
    fn  = function(params, x, y) 
    {
      min_temp <- if (is.na(params["min"])) min_val else params["min"]
      max_temp <- if (is.na(params["max"])) max_val else params["max"]
      mode_temp <- params["mode"]
      shape_temp <- if (modPERT) params["shape"] else 4
      
      # check if the params are plausible
      if (mode_temp < min_temp ||
          mode_temp > max_temp ||
          min_temp > max_temp ||
          shape_temp < 0) return(1e9)
        
      pred_y <- rrisk_pmodpert(
        x, 
        min   = min_temp, 
        mode  = params["mode"], 
        max   = max_temp,
        shape = shape_temp
      )
      #result <- sum(abs(pred_y - y))
      result <- sum((pred_y - y)^2)
      if (is.nan(result)) result <- 1e9
      result
    }, 
    gr = NULL, 
    quantile_data, cdf_data,
    lower = lower,
    #method = "Nelder-Mead"
    method = "L-BFGS-B"
  )
  
  param_values <- signif(result$par, digits = 3)
  
  list(name = "pert",
       def  = list(
         min   = if (is.na(param_values["min"])) min_val else param_values["min"],
         mode  = param_values["mode"],
         max   = if (is.na(param_values["max"])) max_val else param_values["max"],
         shape = if (modPERT) param_values["shape"] else 4
       )
  )
}


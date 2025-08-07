read_rrisk_dist_export_file <- function(infile, param_dist_info)
{
  result <- list(is_ok = TRUE, error_message = "")
  
  model_path <- file(infile$datapath, "r")
  fit_dist <- tryCatch(jsonlite::unserializeJSON(readLines(model_path)),
                     error = function(e) "no-json")
  close(model_path)
  
  if (is.character(fit_dist) &&
      fit_dist == "no-json") {
    
    result$is_ok <- FALSE
    result$error_message <- paste("Could not open file:", fit_dist)
  
  } else if (fit_dist$file_type != "rrisk-distributions-export") {
    
    result$is_ok <- FALSE
    result$error_message <- paste("Wrong file type\n", 
                                  "Expected 'rrisk-distributions-export',",
                                  "got:", fit_dist$file_type)
    
  } else {

    fit_dist$selected_fit <- within(
      fit_dist$selected_fit,
      {
        # extend fitted params with missing params
        this_dist <- param_dist_info[[fitted_dist_name]]$def
        missing_params <- unlist(sapply(
          X        = this_dist[!(names(this_dist) %in% names(par))],
          FUN      = function(x) x$init,
          simplify = FALSE
        ))
        par <- c(par, missing_params)
        rm(this_dist, missing_params)
        # sort names of params
        matched_par <- match(names(par), 
                             names(param_dist_info[[fitted_dist_name]]$def))
        par <- par[order(matched_par)]
        rm(matched_par)
        # set display name (SHOULD BE DONE BY RRISK DISTRIBUTIONS?)
        fitted_display_dist_name <- param_dist_info[[fitted_dist_name]]$display_name
      }
    )
    
    result$fit_dist <- fit_dist
  }
  
  result
}
#---BEGIN: public methods "plot_convergence", "plot_histogram", and "plot_ecdf"-
# MAIN METHOD plot_convergence
rriskModelClass$set("public", 
  "plot_convergence", function(node_name, log = FALSE, plot = TRUE,
                               silent = FALSE, max_value = 1500) 
  {
    # check for input 'node_name', and 'log'
    result <- private$checks_for_plotting(node_name, log)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (is.matrix(private$result_list[[node_name]])) {
      # get results for 2D MC plot
      m <- nrow(private$result_list[[node_name]])
      xlab_text <- "Iterations for 1D"
      # average over second dimension (uncertainty)
      x <- rowMeans(private$result_list[[node_name]])
    } else if (is.vector(private$result_list[[node_name]])) {
      # get results for 1D MC plot
      m <- length(private$result_list[[node_name]])
      xlab_text <- "Iterations"
      x <- private$result_list[[node_name]]
    }
    
    # cumsum expierences an overflow, when
    # the vector mean_converge is of type integer and very large (ca. 50000)
    # converting to numeric (i.e. 64bit float) prevents this
    if (is.integer(x)) x <- as.numeric(x)
    
    # reduce the number of points, if m is larger than max_value
    if (m > max_value &&
        m > 101)
      seq_m <- c(seq(1,100), round(seq(from = 101, to = m, length.out = 900)))
    else
      seq_m <- seq_len(m)
    
    # compute mean for each iteration step
    mean_converge <- cumsum(x)[seq_m] / seq_m
    last_mean_value <- mean_converge[length(mean_converge)]
    # compute 95 % confidence interval for each iteration step
    # 1. compute empirical variance for each iteration step
    ci_mean <- (cumsum(x^2)[seq_m] - seq_m * mean_converge^2) / (seq_m - 1)
    # 2. now compute 95 % CI of mean 
    # (assuming it is symmetric, which does not hold for all variables)
    ci_mean <- qt(0.975, seq_m) * sqrt(ci_mean/seq_m)
    
    # upper and lower confidence interval band
    upper <- mean_converge + ci_mean
    lower <- mean_converge - ci_mean
    rm(ci_mean, x)
    
    plot_range <- range(mean_converge, na.rm = TRUE)
    is_infinite <- is.infinite(plot_range)
    plot_range[is_infinite] <- c(-1e6,1e6)[is_infinite]
    rm(is_infinite)
    
    p <- ggplot2::ggplot(NULL, ggplot2::aes(x     = seq_m,
                                            y     = mean_converge,
                                            ymin  = plot_range[1],
                                            ymax  = plot_range[2],
                                            color = "red")) + 
         ggplot2::geom_line(ggplot2::aes(color = "red"))
                      
    if (log) {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = ifelse(lower < 0, 
                                                               1e-60, lower), 
                                                 ymax = upper),
                                    fill  = "red",
                                    color = "grey",
                                    alpha = 0.4) +
           ggplot2::scale_y_log10()
    } else {
      p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, 
                                                 ymax = upper),
                                    fill  = "red",
                                    color = "grey",
                                    alpha = 0.4)
    }
    
    ylab_name <- node_name
    unit_name <- trimws(private$node_info[[node_name]]$unit)
    if (length(unit_name) > 0 && # no character(0)
        nzchar(unit_name))       # no ""
      ylab_name <- paste0(ylab_name, " [", unit_name, "]")
    
    p <- p + ggplot2::geom_hline(yintercept = last_mean_value,
                                 linetype   = "longdash",
                                 linewidth  = 0.25) +
         ggplot2::xlab("Number of iterations") +
         ggplot2::ylab(ylab_name) +
         ggplot2::ggtitle("convergence plot") +
         ggplot2::theme(legend.position = "none")
    
    if (plot) 
      print(p)
    else
      result$plot <- p
    
    invisible(result)
  }
)

# MAIN METHOD plot_histogram
rriskModelClass$set("public", 
  "plot_histogram", function(node_name, breaks = NULL, xlim = NULL, 
                             log = FALSE, plot = TRUE, silent = FALSE) 
  {
    result <- private$checks_for_plotting(node_name, log, breaks)# xlim
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (is.null(breaks)) {
      if (self$is_2DMC() &&
          isTRUE(private$node_list[[node_name]]$mc_dim == 2)) {
        # breaks for 2D-MC and node with MC-dim = 2
        breaks <- private$param_list[["2DMC"]][["ITER2D"]] / 20
        #if (breaks < 30) breaks <- 30
      } else {
        # breaks for 1D-MC, and 2D-MC + node with MC-dim = 1 
        breaks <- 75
      }
    }
    
    xlab_name <- node_name
    unit_name <- trimws(private$node_list[[node_name]]$info$unit) 
    if (length(unit_name) > 0 &&
        nzchar(unit_name))
      xlab_name <- paste0(xlab_name, " [", unit_name, "]")
    
    p <- ggplot2::ggplot(NULL, 
                         ggplot2::aes(
                           as.vector(private$result_list[[node_name]]))) + 
         ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                 bins  = breaks, 
                                 color = "darkgrey", 
                                 fill  = "gray") +
         ggplot2::ggtitle("histogram") +
         ggplot2::xlab(xlab_name)
    
    if (!is.null(xlim)) p <- p + ggplot2::xlim(xlim[1], xlim[2])
    
    if (log) p <- p + ggplot2::scale_x_log10()
    
    if (plot) 
      print(p)
    else
      result$plot <- p
    
    invisible(result)
  }
)

# MAIN METHOD plot_ecdf
rriskModelClass$set("public", 
  "plot_ecdf", function(node_name, xlim = NULL, log = FALSE, plot = TRUE, 
                        quantiles_2D = NULL, quantile_type = NULL,
                        silent = FALSE) 
  {
    result <- private$checks_for_plotting(node_name, log)#, quantiles_2D, quantile_type, xlim
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    if (is.null(quantiles_2D)) 
      quantiles_2D <- private$param_list[["QUANTILES_2D"]]
    
    if (is.null(quantile_type))
      quantile_type <- private$param_list[["QUANTILE_TYPE"]]
    
    #---BEGIN: internal functions-----------------------------------------------
    get_lines_and_segments <- function(q)
    {
      result <- list(line = c(),
                     segment = list())
      while(length(q) > 0) {
        tmp <- q[1]
        q <- q[!(q %in% tmp)]
        if (length(q) == 0) {
          result$line <- c(result$line, paste0("Qu", 100*tmp))
        } else {
          tmp2 <- q[q == (1 - tmp)]
          if (length(tmp2) == 0) {
            result$line <- c(result$line, paste0("Qu", 100*tmp))
          } else {
            result$segment <- c(result$segment, 
                                list(paste0("Qu", 100*c(tmp, tmp2))))
            q <- q[!(q %in% tmp2)]
          }
        }
      }
      result
    }
    
    compute_2DMC_ecdf <- function(mc_results,
                                  ecdf_range, quantiles_2D, quantile_type)
    {
      mm <- ncol(mc_results)
      nn <- length(ecdf_range)
      # get ecdf for each 2D MC iteration (aka column)
      mat_result <- matrix(nrow = nn, ncol = mm)
      for (i in seq_len(mm)) {
        mat_result[,i] <- ecdf(mc_results[,i])(ecdf_range)
      }
      # compute quantiles for each ecdf value (aka row)
      # loop over first 2D MC dimension
      mat_ecdf <- matrix(nrow = nn, ncol = length(quantiles_2D))
      for (i in seq_len(nn)) {
        mat_ecdf[i,] <- quantile(mat_result[i,],
                                 probs = quantiles_2D,
                                 type  = quantile_type,
                                 names = FALSE)
      }
      # build data frame for ggplot
      df <- data.frame(ecdf_range, mat_ecdf, rowMeans(mat_result))
      colnames(df) <- c("ecdf_range", paste0("Qu", 100*quantiles_2D), "mean")
      # return data frame
      df
    }
    #---END: internal functions-------------------------------------------------
    
    # number of points of ecdf curve
    nn <- 1000
    
    if (is.vector(private$result_list[[node_name]])) {
      # 1D MC node results
      p <- ggplot2::ggplot(NULL, 
                           ggplot2::aes(private$result_list[[node_name]])) +
           ggplot2::stat_ecdf(geom = "step", n = nn)
    } else {
      # if node results is a matrix -> contains variability and uncertainty
      # 2D MC node results
      
      # set the values for which we want to compute the ecdf-value
      # from the ecdf function
      ecdf_range <- seq(from       = min(private$result_list[[node_name]]), 
                        to         = max(private$result_list[[node_name]]), 
                        length.out = nn)
      # get 2D MC ecdf curves
      df <- compute_2DMC_ecdf(private$result_list[[node_name]],
                              ecdf_range, quantiles_2D, quantile_type)
      
      plot_lines_and_segments <- get_lines_and_segments(quantiles_2D)
      
      # plot 2D ecdf curves
      p <- ggplot2::ggplot(df)

      # add segments: confidence bands
      for (that in plot_lines_and_segments$segment) {
        p <- p + ggplot2::geom_ribbon(ggplot2::aes(x    = !!ecdf_range,
                                                   ymin = !!df[,that[1]], 
                                                   ymax = !!df[,that[2]]),
                                      fill  = "grey",
                                      color = "grey",
                                      alpha = 0.4)
      }
      
      # add lines
      for (this in c("mean", plot_lines_and_segments$line)) {
        p <- p + ggplot2::geom_line(ggplot2::aes(x = !!ecdf_range,
                                                 y = !!df[,this]),
                                    linetype = if (this == "mean") 1 else 2)
      }

      p <- p + ggplot2::coord_cartesian(ylim = c(0,1))
    }
    
    # build x-label
    xlab_name <- node_name
    unit_name <- trimws(private$node_list[[node_name]]$info$unit) 
    if (length(unit_name) > 0 &&
        nzchar(unit_name)) {
      xlab_name <- paste0(xlab_name, " [", unit_name, "]")
    }
    
    # add titles and labels
    p <- p + ggplot2::ggtitle("ecdf plot") +
         ggplot2::ylab("ecdf") +
         ggplot2::xlab(xlab_name) +
         ggplot2::theme(legend.position = "none")
    
    # zoom if needed
    if (!is.null(xlim)) p <- p + ggplot2::xlim(xlim[1], xlim[2])
    
    # log10 scaled if needed
    if (log) p <- p + ggplot2::scale_x_log10()
    
    if (plot)
      print(p)
    else
      result$plot <- p

    invisible(result)
  }
)
#---END: public methods "plot_convergence", "plot_histogram", and "plot_ecdf"---

#---BEGIN: private utility method for public methods "plot_convergence",
#          "plot_histogram", and "plot_ecdf"------------------------------------
rriskModelClass$set("private", 
  "checks_for_plotting", function(node_name, log = FALSE, breaks = NULL) 
  {
    result <- private$is_not_missing(node_name, "node_name")
    if (!result$is_ok) return(result)
    private$check_all(
      private$is_known_name(setNames(node_name, "node_name"),
                            known_names = names(private$node_list)
                            #known_names = names(private$node_expr)
                            ),
      # are there any results for this node name
      private$is_result_available(node_name),
      # if data shall be plotted logarithmic, is that possible?
      private$is_error(log && min(private$result_list[[node_name]]) < 0,
                       paste("Results for node", dQuote(node_name), 
                             "can not be plotted logarithmically.")),
      # if the breaks are set, are they in the right range?
      private$is_error(!is.null(breaks) && (breaks < 1 || breaks > 1e6),
                       "Breaks need to be between 1 and 1e6.")
    )
  }
)
#---END: private utility method for public methods "plot_convergence",
#        "plot_histogram", and "plot_ecdf"--------------------------------------
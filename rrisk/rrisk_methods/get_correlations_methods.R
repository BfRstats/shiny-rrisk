#---BEGIN: main public methods "get_correlations", "get_correlation_plot"-------
rriskModelClass$set("public",
  "get_correlations", function(target_node, method = "spearman",
                               silent = FALSE)
  {
    result <- private$check_for_getting_correlations(target_node, method)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    # get node names to do correlations with this target node
    source_node_names <- private$get_nodes_for_correlation(target_node)

    # prepare results data frame
    df_cor <- data.frame(var = source_node_names,
                         cor = rep(0, length(source_node_names)))
    row.names(df_cor) <- source_node_names
        
    if (self$is_2DMC()) {
      # 2D MC result
      
      # add lower and upper ci band to result data frame
      df_cor$lower <- rep(0, length(source_node_names))
      df_cor$upper <- rep(0, length(source_node_names))
      
      for (source_node_name in source_node_names) {
        
        all_cor <- private$get_all_cor(
          x        = private$result_list[[target_node]], 
          y        = private$result_list[[source_node_name]],
          method   = method
        )
        
        df_cor[source_node_name, "cor"] <- mean(all_cor)
        df_cor[source_node_name, c("lower", "upper")] <- quantile(
          x     = all_cor, 
          probs = c(0.025, 0.975), 
          names = FALSE
        )
      }
    } else {
      # 1D MC result
      for (source_node_name in source_node_names) {
        df_cor[source_node_name, "cor"] <- cor(
          x      = private$result_list[[target_node]], 
          y      = private$result_list[[source_node_name]],
          method = method
        )
      }
    }
    
    # order correlations from largest to lowest
    df_cor <- df_cor[order(abs(df_cor$cor)),]
    rownames(df_cor) <- NULL
    
    result$df_cor <- df_cor
    
    result
  }
)

rriskModelClass$set("public",
  "get_correlation_plot", function(target_node, method = "spearman",
                                   plot = TRUE, silent = FALSE)
  {
    if (is.null(private$result_list)) return()

    result <- private$check_for_getting_correlations(target_node, method)
    
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    df_cor <- self$get_correlations(target_node, method)$df_cor
    
    # reorder is needed, as ggplot2 likes to be Mr. Smartass
    p <- ggplot2::ggplot(df_cor, ggplot2::aes(x = reorder(var, abs(cor)), 
                                              y = cor)) + 
         ggplot2::geom_col(width = 0.5)
    
    if (self$is_2DMC()) {
      p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, 
                                                   ymax = upper))
    }
    
    plot_text <- switch(
      EXPR = method,
      "pearson"  = "Pearson's correlation coef. for\n",
      "kendall"  = "Kendall's correlation coef. for\n",
      "spearman" = "Spearman's rho statistic for\n"
    )
    plot_text <- paste(plot_text, target_node)
    
    # make the plot more pretty
    p <- p + ggplot2::coord_flip() +
         ggplot2::ylim(-1,1) +
         ggplot2::xlab("") +
         ggplot2::ylab(plot_text) +
         ggplot2::theme(
           axis.text    = ggplot2::element_text(size = 20),
           axis.title.x = ggplot2::element_text(size = 20)
         )

    if (plot) 
      print(p)
    else
      result$plot <- p
    
    invisible(result)
  }
)
#---END: main public methods "get_correlations", "get_correlation_plot"---------

#---BEGIN: check functions for sensitivity analysis methods---------------------
rriskModelClass$set("private",
  "check_for_getting_correlations", function(target_node, method_name)
  {
    private$check_all(
      # check if target_node is fine
      private$check_this(object = setNames(target_node, "node name"),
                         private$is_character,
                         preset_args(
                           private$is_known_name,
                           known_names = names(private$node_list)
                         )
      ),
      private$is_error(
        !(method_name %in% c("pearson", "kendall", "spearman")),
        paste("Method '", method_name, "' is unknown.")
      ),
      # there are results for other nodes
      private$is_error(
        length(private$result_list) <= 1,
        "No results available"
      )
    )
  }
)
#---END: check functions for sensitivity analysis methods-----------------------

#---BEGIN: Utility functions for sensitiviy analysis methods--------------------
rriskModelClass$set("private",
  "get_nodes_for_correlation", function(target_node)
  {
    # get node names for y to do correlations with
    source_node_names <- setdiff(names(private$result_list), target_node)
    # exclude nodes who are incompatible with x
    target_mc_dim <- private$node_list[[target_node]]$mc_dim
    if (is.null(target_mc_dim)) target_mc_dim <- 1L
    is_compatible <- sapply(
      X   = source_node_names,
      FUN = function(source_node_name)
      {
        source_mc_dim <- private$node_list[[source_node_name]]$mc_dim
        if (is.null(source_mc_dim)) source_mc_dim <- 1L
        # return result of logical expression
        !(target_mc_dim != source_mc_dim &&
          is.vector(private$result_list[[target_node]]) &&
          is.vector(private$result_list[[source_node_name]]))
      },
      USE.NAMES = FALSE
    )
    source_node_names[is_compatible]
  }
)

rriskModelClass$set("private",
  "get_all_cor", function(x, y, method)
  {
    # This is for the 2D MC sensitivity analysis
    
    # internal function
    my_cor <- function(x, y, method)
    {
      if (min(x) == max(x))
        0
      else if (min(y) == max(y))
        0
      else
        cor(x, y, method = method)
    }
    
    # In a 1D MC model, all node results are either vectors, or scalars.
    # Scalars are removed from the node results.
    #
    # In a 2D MC model, all nodes on the first MC dim are matrices 
    # (nrow = first MC dim, ncol = second MC dim). 
    # A node is on the first MC dim, if it is mc_dim = 1, or any upstream node
    # of this node is of mc_dim = 1.
    # A node is 
    
    # 1D + 1 or 2D: length is equal to length or nrow
    # 2D + 1 or 2D: length is equal to lenth or nrow
    # 2D + 2D: ncol is equal
    if (is.vector(x) && is.vector(y)) {
      # x is 1D, y is 1D
      all_cor <- my_cor(x, y, method)
    } else if ((is.vector(x) && is.matrix(y)) ||
               (is.matrix(x) && is.vector(y))) {
      # in the 2D MC, the 1D object is always on the second MC dim.
      if (is.vector(x)) {
        # x is 1D, y is 2D
        this_x <- y # 2D object
        this_y <- x # 1D object
      } else {
        # x is 2D, y is 1D
        this_x <- x # 2D object
        this_y <- y # 1D object
      }
      all_cor <- apply(
        X      = this_x,
        MARGIN = 1,
        FUN    = function(xx) my_cor(x = xx, y = this_y, method = method)
      )
    } else if (is.matrix(x) && is.matrix(y)) {
      # x is 2D, y is 2D
      # over cols, i.e., 2. MC dim
      # we get uncertainty of correlation of two variable nodes
      all_cor <- rep(0, ncol(x))
      for (i in seq_along(all_cor)) {
        all_cor[i] <- my_cor(x[,i], y[,i], method)
      }
      # all_cor <- mapply(
      #   FUN       = function(xx, yy, method)
      #   {
      #     my_cor(x = xx, y = yy, method = method)
      #   },
      #   asplit(x, MARGIN = 1), asplit(y, MARGIN = 1),
      #   MoreArgs  = list(method = method),
      #   USE.NAMES = FALSE
      # )
      # oder richtig so?
      # all_cor <- rep(0, nrow(x))
      # for (i in seq_along(all_cor)) {
      #   all_cor[i] <- my_cor(x[i,], y[i,], method)
      # }
    } else {
      stop("ERROR in get_all_cor: neither x nor y are either vector nor matrix")
    }
    
    all_cor
  }
)
#---END: Utility functions for sensitiviy analysis methods----------------------

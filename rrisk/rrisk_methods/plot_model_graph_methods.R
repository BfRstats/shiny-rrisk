#---BEGIN: public method "plot_model_graph"-------------------------------------
# MAIN METHOD plot_model_graph
rriskModelClass$set("public", 
  "plot_model_graph", function(plot_groups = TRUE, 
                               plot_if_model_is_empty = FALSE) 
  {
    if (length(private$node_list) > 0) {
      plot(x           = self$get_model_graph(), 
           mark.groups = if (plot_groups) private$get_node_group_list() else list(),
           layout      = igraph::layout_with_kk)
      legend("topleft",
             legend = c("end node", "source node", "implicit node"),
             col    = "black",
             pt.bg  = c("cyan", "white", "red"),
             pch    = 21)
    } else if (plot_if_model_is_empty) {
      plot.window(xlim = c(0,1), ylim = c(0,1))
      text(0.5, 0.5, "No data available for model plot")
    } else {
      message("No data available for model plot")
    }
  }
)

rriskModelClass$set("public", 
  "get_model_graph", function(as = "igraph") 
  {
    dag <- NULL
    
    node_names <- names(private$node_list)
    
    if (as == "igraph") {
      dag <- igraph::graph_from_data_frame(d        = private$get_df_edges(),
                                           vertices = node_names)
    
      # find and color end nodes
      end_nodes <- self$get_end_nodes()
      for (end_node in end_nodes)
        igraph::V(dag)[end_node]$color <- "cyan"
    
      # find and color empty nodes
      empty_nodes <- private$get_empty_nodes()
      for (empty_node in empty_nodes)
        igraph::V(dag)[empty_node]$color <- "red"
      
      attr(dag, "graph_type") <- "igraph"
      
    } else if (as == "visNetwork") {
      shapes <- setNames(object = rep("dot", length(node_names)),
                         nm = node_names)
      end_nodes <- self$get_end_nodes()
      empty_nodes <- private$get_empty_nodes()
      shapes[end_nodes] <- "star"
      shapes[empty_nodes] <- "square"
      
      nodes <- data.frame(
        id    = node_names,
        label = node_names,
        group = sapply(
          X   = node_names,
          FUN = function(node_name)
          {
            if (is.null(private$node_list[[node_name]]$group))
              "--"
            else
              paste(private$node_list[[node_name]]$group, collapse = " ")
          }
        ),
        shape = shapes, 
        title = sapply(
          X   = node_names,
          FUN = function(node_name)
          {
            if (is.null(private$node_list[[node_name]]$info))
              ""
            else
              private$node_list[[node_name]]$info$descr
          }
        )
      )
      
      edges <- list(from = NULL, to = NULL)
      for (node_name in node_names) {
        source_nodes <- private$edge_list[[node_name]]
        edges$from <- c(edges$from, source_nodes)
        edges$to   <- c(edges$to, rep(node_name, length(source_nodes)))
      }
      
      dag <- list(nodes = nodes, edges = list2DF(edges))

      attr(dag, "graph_type") <- "visNetwork"
    }
    
    dag
  }
)
#---END: public method "plot_model_graph"---------------------------------------

#---BEGIN: private utility methods for public method "plot_model_graph"---------
rriskModelClass$set("private", 
  "get_node_group_list", function() 
  {
    # get the group names
    unique_groups <- NULL
    for (node in private$node_list)
      unique_groups <- c(unique_groups, node$group)
    unique_groups <- unique(unique_groups)
    
    # create group list
    # it contains the vertices, aka nodes, of that group
    group_node_list <- setNames(object = vector(mode = "list",
                                                length = length(unique_groups)),
                                nm     = unique_groups)
    # go through all node names in the node_group_list,
    # and get the group in which they are.
    # Collect this node_name in that group list entry
    for (node_name in names(private$node_list)) {
      for (group in private$node_list[[node_name]]$group) {
        group_node_list[[group]] <- c(group_node_list[[group]], node_name)
      }
    }
    
    group_node_list
  }
)

rriskModelClass$set("private", 
  "get_df_edges", function() 
  {
    df_edges   <- data.frame(from = character(),
                             to   = character())
    node_names <- names(private$edge_list)
    for (i in seq_along(private$edge_list)) {
      df_edges <- rbind(df_edges,
                        data.frame(from = private$edge_list[[i]],
                                   to   = rep(node_names[i],
                                              length(private$edge_list[[i]]))))
    }
    df_edges
  }
)
#---END: private utility methods for public method "plot_model_graph"-----------

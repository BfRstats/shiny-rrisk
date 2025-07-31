set_parametric_dist_choices <- function(param_dist_info)
{
  dist_name_list <- list("nothing" = "")
  for (dist_name in names(param_dist_info)) {
    display_name <- param_dist_info[[dist_name]]$display_name
    dist_type    <- param_dist_info[[dist_name]]$type
    dist_name_list[[dist_type]] <- c(dist_name_list[[dist_type]],
                                     setNames(object = list(dist_name),
                                              nm     = display_name))
  }
  dist_name_list
}
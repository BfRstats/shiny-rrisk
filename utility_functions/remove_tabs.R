remove_tabs <- function()
{
  #for (node_name in node_names) {
  #  removeTab(inputId = "result_tabs", target  = node_name)
  #}
  removeTab(inputId = "result_tabs", target = "general results")
  removeTab(inputId = "result_tabs", target = "sensitivity plot")
}
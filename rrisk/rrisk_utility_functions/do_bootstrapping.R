# this function does the bootstrapping, using user provided data and summary
# statistic functions.
# n = number of bootstrap samples
# input = list with entries: 
#   data        = bootstrap data
#   func_name   = name of the function call
#   result_vars = vector of result var names
# func_list = a named list with the summary statistic functions
do_bootstrapping <- function(n, input, func_list)
{
  # size of original data sample
  m <- length(input$data)
  # collect names of all bootstrap result variables
  result_var_names <- NULL
  for (sum_stat in input$sum_stats) {
    result_var_names <- c(result_var_names, sum_stat$result_vars)
  }
  # set up result list
  result_list <- sapply(
    X        = result_var_names,
    FUN      = function(x) rep(NaN, n),
    simplify = FALSE
  )
  # do bootstrapping
  for (i in seq_len(n)) {
    # get ith bootstrap sample
    bootstrap_sample <- sample(x = input$data, size = m, replace = TRUE)
    # compute all statistics for ith bootstrap sample
    for (sum_stat in input$sum_stats) {
      # compute jth statistic for ith bootstrap sample
      result_tmp <- func_list[[sum_stat$func_name]](bootstrap_sample)
      # first get result var names of jth statistic
      result_vars <- sum_stat$result_vars
      # store result for ith bootstrap sample
      if (is.list(result_tmp)) {
        # result has more than one variable
        for (result_name in result_vars) {
          result_list[[result_name]][i] <- result_tmp[[result_name]]
        }
      } else {
        # result has only one variable
        result_list[[result_vars]][i] <- result_tmp
      }
    }
  }
  # return the result
  result_list
}
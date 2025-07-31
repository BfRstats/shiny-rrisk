# vectorized version of switch for the user
# choice must be a vector of integer numbers
# the number refer to the input in ...
vec_switch <- function(choice, ...)
{
  alternatives <- list(...)
  result <- rep(NA, length(choice))
  for (i in seq_along(choice)) result[i] <- alternatives[[choice[i]]][i]
  result
}
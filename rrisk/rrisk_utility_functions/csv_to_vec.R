# x is a charcter vector, e.g. x = c("1, 2", "3 ")
# final result is a vector, e.g. finaL_result c("1", "2", "3")
csv_to_vec <- function(x)#, type = "char")
{
  # split result
  split_results <- strsplit(x, ",")
  # concatenation to vector and trim white space of split results
  final_result <- NULL
  for (this_split in split_results) {
    final_result <- c(final_result,
                      trimws(this_split, which = "both"))
  }
  # return final results
  final_result
}
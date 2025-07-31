# i created this function as i am not patient enough to deal with reg expr
# and gregexpr, regmatch, or gsub etc.. I just wanted it finished and be ready
replace_node_name <- function(old_name, new_name, input_string) {
  # search patterns
  only_letters  <- c(letters, LETTERS)
  alpha_numeric <- c(letters, LETTERS, as.character(0:9), "_")
  # split the string into letters
  string_vec <- strsplit(input_string, "")[[1]]
  n <- length(string_vec)
  # output string
  output_string <- c()
  i <- 1
  while (i <= n) {
    # get next letter
    letter <- string_vec[i]
    if (letter %in% only_letters) {
      # found a new word
      word <- c(letter)
      i <- i + 1
      while (i <= n) {
        # get next letter
        letter <- string_vec[i]
        if (letter %in% alpha_numeric) {
          word <- c(word, letter)
          i <- i + 1
        } else {
          break
        }
      }
      # fuse the letters to a string
      word <- paste(word, collapse = "")
      # check if new word is the one we are looking for.
      # If so, replace it with the new word.
      if (identical(word, old_name)) {
        output_string <- c(output_string, new_name)
      } else {
        output_string <- c(output_string, word)
      }
    } else {
      output_string <- c(output_string, letter)
      i <- i + 1
    }
  }
  # return fused output string
  paste(output_string, collapse = "")
}
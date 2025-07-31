try_cast_as_numeric <- function(string)
{
  number <- suppressWarnings(as.numeric(string))
  if (any(is.na(number)))
    string
  else
    number
}

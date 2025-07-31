cast_csv_as_vector <- function(string)
{
  # Primitive approach because I could not think of anything
  parsed_string <- tryCatch(parse(text = string),
                            error = function(e) e)
  if (is.expression(parsed_string))
    # assume it is an expression; do not change a thing
    string
  else
    #  assume string is csv; split csv into vector
    trimws(strsplit(string, ",")[[1]])
}
detectQuote = function(filename, skip) {
  # data.table::fread has argument quote.
  # On some computers the quotes in the files are
  # not recognised, to catch this first try to check whether this is the case:
  quote = "\""
  Dtest = NULL
  try(expr = {Dtest = data.table::fread(input = filename,
                                        header = FALSE, sep = ",", skip = skip,
                                        nrows = 20, quote = quote)}, silent = TRUE)
  if (length(Dtest) == 0) {
    quote = ""
  } else {
    if (nrow(Dtest) <= 1) {
      quote = "" 
    }
  }
  return(quote)
}
getExtension <- function(filename){ 
  # Extract file extension
  ex <- unlist(strsplit(basename(filename), split = "[.]"))
  if (length(ex) < 2) stop(paste0("Cannot recognise extension from '", filename, "' as filename, please check"), call. = FALSE)
  return(ex[length(ex)])
}




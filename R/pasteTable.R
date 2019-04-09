#' pasteTable
#'
#' Function to move table from clipboard to R
#'
#' @export

pasteTable <- function(sep = "\t", header = FALSE, ...){
  read.table("clipboard", sep = sep, header = header, ...)
}

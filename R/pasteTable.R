#' pasteTable
#'
#' Function to move table from clipboard to R
#'
#' @param sep table separator value. e.g. tab, space, comma, etc.
#' @param header whether or not to assign the first row to column names. default FALSE
#' @param ... Additional parameters passed to utils::read.table
#'
#' @export

pasteTable <- function(sep = "\t", header = FALSE, ...){
  utils::read.table("clipboard", sep = sep, header = header, ...)
}

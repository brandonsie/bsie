#' copyTable
#'
#' write data frame to clipboard
#'
#' @param df Data frame to copy to clipboard.
#' @param sep Separator
#' @param row.names Logical whether or not to include row names
#' @param ... Additional parameters passed to write.table
#'
#' @export



copyTable <- function(df, sep = "\t", row.names = FALSE, ...){
  utils::write.table(df, "clipboard", sep = sep, row.names = row.names, ...)
}

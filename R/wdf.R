#' wdf
#'
#' Write Data Frame
#'
#' Write data frame to console without column names or row names.
#'
#' @param df Data frame to write.
#'
#' @export

wdf <- function(df){
  df %>% as.data.frame %>% write.table(col.names = FALSE, row.names = FALSE)
}



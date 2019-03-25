#' lapply as.character to data frame columns to convert factor to character
#'
#' @param df Data frame to convert.
#'
#' @export

deFactor <- function(df){
  df[] <- lapply(df, as.character)
  return(df)
}
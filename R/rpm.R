#' rpm
#' scale data frame so each column sums to 1e6 (by default)
#'
#' @param reads Input data frame to rescale
#' @param scale Value to which each column should be scaled. Default 1e6
#' @param ignore_cols Column names to exclude from calculation (e.g. id column)
#'
#' @export


rpm <- function(reads, scale = 1e6, ignore_cols = NULL){
  if(!is.null(ignore_cols)){
    ignore_df <- reads %>% subset(select = ignore_cols)
    reads <- reads %>% dplyr::select(-tidyselect::all_of(ignore_cols))

  }

  rpm <- reads %>% apply(2, function(x){
    x * scale / sum(x)
  }) %>% as.data.frame
  if(!is.null(ignore_cols)){
    rpm <- dplyr::bind_cols(ignore_df, rpm)
  }

  return(rpm)

}


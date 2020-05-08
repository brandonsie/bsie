#' rpm
#' scale data frame so each column sums to 1e6 (by default)
#'
#' @param reads Input data frame to rescale
#' @param scale Value to which each column should be scaled. Default 1e6
#'
#' @export


rpm <- function(reads, scale = 1e6){
  return(reads %>% as.data.frame %>% apply(2, function(x){
    x * scale / sum(x)
  }))

}


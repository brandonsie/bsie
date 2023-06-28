#' useful functions for other packages that i should remember to use more often
#'
#' tidyr::crossing is like base::expand.grid. all unique combinations of elements from 2 vectors
#' @export


crossing <- function(...){
  tidyr::crossing(...)
}



#' cipher
#' Calls base::chartr to scramble input based on a set key and seed.
#' For amino acids, single letter codes are: GPAVLIMCFYWHKRQNEDST
#' @param input Character vector of strings to encode.
#' @param seed Parameter to pass to set.seed for reproducibility.
#' @param key Poissible characters to found in input and to include in output.
#' @export

ciper <- function(input, seed, key = c(LETTERS)){
  `%>%` <- magrittr::`%>%`
  set.seed(seed)
  scr <- key %>% strsplit(NULL) %>% unlist %>% sample %>% paste(collapse="")
  return(chartr(key,scr,input))
}

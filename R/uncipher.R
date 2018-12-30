#' uncipher
#' Calls base::chartr to unscramble input based on a set key and seed.
#' For amino acids, single letter codes are: GPAVLIMCFYWHKRQNEDST
#' @param input Character vector of strings to encode.
#' @param seed Parameter to pass to set.seed for reproducibility.
#' @param key Poissible characters to found in input and to include in output.
#' @export

uncipher <- function(input, seed, key = c(LETTERS)){
  set.seed(seed)
  scr <- key %>% strsplit(NULL) %>% unlist %>% sample %>% paste(collapse="")
  return(chartr(scr,key,input))
}
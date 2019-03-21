#' rml
#'
#' ReMove List
#'
#' Wrapper to remove objects in environment
#'
#' @export

rml <- function(){
  remove(list=ls())
}
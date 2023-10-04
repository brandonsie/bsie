#' functions for reading and writing data
#'
#' fread fwrite wrappers
#'
#' read_csv
#' @param file directory path to csv
#' @param ... additional parameters to pass to readr::read_csv
#'
#' @export


read_csv <- function(file, ...){
  readr::read_csv(file = file, col_types = readr::cols(), ...)
}

#' functions for reading and writing data
#'
#' fread fwrite wrappers
#'
#' fr
#' @param file directory path to table
#' @param ... additional parameters to pass to data.table::fread
#'
#' @export
#'
fr <- function(file, ...){
  data.table::fread(file = file, ...) %>% tibble::as_tibble()
}

#' functions for reading and writing data
#'
#' fread fwrite wrappers
#' fw
#' @param x table to write
#' @param file directory path destination to write
#' @param ... additional parameters to pass to data.table::fwrite
#'
#' @export

fw <- function(x, file, ...){
  data.table::fwrite(x = x, file = file, ...)
}
#' functions for reading and writing data
#'
#' fread fwrite wrappers
#'
#' @export


read_csv <- function(file, ...){
  readr::read_csv(file = file, col_types = readr::cols(), ...)
}

#' functions for reading and writing data
#'
#' fread fwrite wrappers
#'
#' @export
#'
fr <- function(file, ...){
  data.table::fread(file = file, ...) %>% tibble::as_tibble()
}

#' functions for reading and writing data
#'
#' fread fwrite wrappers
#'
#' @export

fw <- function(x, file, ...){
  data.table::fwrite(x = x, file = file, ...)
}
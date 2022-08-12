# functions for reading and writing data



read_csv <- function(...){
  readr::read_csv(col_types = readr::cols(), ...)
}

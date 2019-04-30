#' pasteWinpath gsub wrapper. By default changes \ to / from windows paths from cliboard.
#'
#' @param sub Optional gsub to replace '\\' with in converted path. Set to '\\\\' to ignore.
#'
#' @export

pasteWinpath <- function(sub = "/"){
  path <- readClipboard()
  path <- gsub("\\\\", sub, path)
  return(path)
}

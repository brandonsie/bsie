#' copyWinpath
#'
#' write converted windows path (from \ to /) to clipboard
#'
#' @param sub Optional gsub to replace '\\' with in converted path. Set to '\\\\' to ignore.
#'
#' @export

copyWinpath <- function(sub = "/"){
  path <- pasteWinpath(sub)
  utils::writeClipboard(path)
  return(path)
}

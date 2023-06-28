#' rpm
#' scale data frame so each column sums to 1e6 (by default)
#'
#' @param reads Input data frame to rescale
#' @param scale Value to which each column should be scaled. Default 1e6
#' @param ignore_cols Column names to exclude from calculation (e.g. id column)
#'
#' @export


rpm <- function(reads, scale = 1e6, ignore_cols = NULL){
  if(!is.null(ignore_cols)){
    # ignore_df <- reads %>% base::subset(select = ignore_cols)
    # reads <- reads %>% dplyr::select(-tidyselect::all_of(ignore_cols))

    reads_colnames <- colnames(reads)
    cols_to_ignore <- reads_colnames %in% ignore_cols

    ignore_df <- reads[cols_to_ignore]
    reads <- reads[!cols_to_ignore]


  }

  rpm_df <- apply(reads, 2, function(x){x * scale / sum(x)})
  rpm_df <- as.data.frame(rpm_df)
  # rpm_df <- apply(reads, 2, function(x){x * scale / sum(x)}) %>% as.data.frame()
  if(!is.null(ignore_cols)){
    rpm_df <- dplyr::bind_cols(ignore_df, rpm_df)
  }

  return(rpm_df)

}


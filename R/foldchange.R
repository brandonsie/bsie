#' foldchange
#' Calculate fold change from rpm scaled data.
#'
#' @param rpm_control Reads per million (counts per million) data frame of control data from which to determine bins (e.g. beads only). Peptide order should match that of rpm_sample.
#' @param rpm_sample Reads per million (counts per million) data frame of sample data for which to calculate z-scores. Peptide order should match that of rpm_control
#' @param binsize Number of peptides per bin (default 300)
#' @param ignore_cols Column names to exclude from calculation (e.g. id column)
#'
#'
#' @export


foldchange <- function(rpm_control, rpm_sample, binsize = 300,
                        ignore_cols = c()){

  if(length(ignore_cols) > 0){
    ignore_df <- rpm_sample %>% subset(select = ignore_cols)
    rpm_sample <- rpm_sample %>% dplyr::select(-tidyselect::all_of(ignore_cols))
  }

  # for each peptide, calculate fold change compared to mean of rpm_control
  foldchange <- data.frame(matrix(NA, nrow = nrow(rpm_sample), ncol = ncol(rpm_sample)))
  rownames(foldchange) <- rownames(rpm_sample)
  colnames(foldchange) <- colnames(rpm_sample)

  control_means <- rpm_control %>% apply(1, mean)
  foldchange <- rpm_sample / control_means



  if(length(ignore_cols) > 0){
    foldchange <- dplyr::bind_cols(ignore_df, foldchange)
  }


  return(foldchange)
}


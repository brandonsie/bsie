#' new_pro_fun
#'
#' calculate prosum, promax, pro_breadth -- like polyclonal but not redundancy collapsed
#' @param zscore_df data frame of zscores
#' @param protein_split character vector ewqual to number of rows in zscore df indicating which peptides belong to which proteins
#' @param method whether to sum values across peptides, max, or take number of peptides above a threshold
#' @param z_thresh threhold above which to count as scoring for method="breadth
#'
#' @export

new_pro_fun <- function(zscore_df, protein_split, method = c("sum", "max", "breadth"), z_thresh = 5){
  # input zscore data frame and annotation split
  # annotate zscore df
  # split by protein
  # summarize for each protein. method = c(sum, max)
  # zscore df tibble. protein_split factor vector

  zscore_df_list <- zscore_df %>% split(protein_split)
  zscore_df_list <- zscore_df_list[]

  if(method == "sum"){
    zscore_df_list_summary <- zscore_df_list %>% lapply(function(x) apply(x, 2, function(x) sum(x)))
  } else if(method == "max"){
    zscore_df_list_summary <- zscore_df_list %>% lapply(function(x) apply(x, 2, function(x) max(x)))
  } else if(method == "breadth"){
    zscore_df_list_summary <- zscore_df_list %>% lapply(function(x) apply(x, 2, function(x) sum(x>z_thresh)))
  } else{
    stop("error, invalid method for new_pro_fun")
  }

  zscore_df_summary <- zscore_df_list_summary %>% dplyr::bind_rows(.id = "protein")
}

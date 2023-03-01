#' collapse_df_reps
#'
#' collapse columns in df which chare an identity index provided in the vector pid
#' @param df data frame or tibble with observations as columns
#' @param pid patient id, character vector with length equal to number of columns in df. replicate columns are indicated by same value in corresponding elements of pid
#' @param method what kind of collapsing to do. min = take minimum value. mean = take mean of values. clean = take mean, but if either value is below clean_thresh, set final valut o zero. cor: compute correlation across all features for data. required duplicates.
#' @param clean_thresh for method="clean". threshold value below which if any replicate exhibits this value, set output to 0
#' @param ignore_cols columns in df to exclude from colapse (e.g. feature id)
#' @param show_progress logical indicating whether or not to show progress bar
#'
#' @export


collapse_df_reps <-
  `%>%` <- magrittr::`%>%`
  function(df,
           pid,
           method = c("min", "mean", "clean", "cor"),
           clean_thresh = 2.5,
           ignore_cols = NULL, show_progress = TRUE) {

    if (length(ignore_cols) > 0) {
      ignore_df <- df %>% subset(select = ignore_cols)
      df <- df %>% dplyr::select(-tidyselect::all_of(ignore_cols))
      pid = pid[!(pid%in% ignore_cols)]
    }


    # z df data frame
    # pid indicator which columns are tehcnical replicates e.g. 123456_w4
    unique_pid <- unique(pid)
    if(method[1] == "cor"){
      df_collapse <-
        data.frame(matrix(nrow = 1, ncol = length(unique_pid)))
    } else{
      df_collapse <-
        data.frame(matrix(nrow = nrow(df), ncol = length(unique_pid)))

    }
    colnames(df_collapse) <- unique_pid
    if(show_progress) pb <- txtProgressBar(0, length(unique_pid), style = 3)
    for (i in 1:length(unique_pid)) {
      if(show_progress) setTxtProgressBar(pb, i)
      # find corresponding columns of uncollapsed input df and produce subset data frame
      pid_match <- pid == unique_pid[i]
      if(sum(pid_match > 1)){
        replicates_df <- df[, pid_match]
      } else{
        replicates_df <- df[,pid_match] %>% as.data.frame
      }


      if (method[1] == "min") {
        collapsed_vector <- replicates_df %>% apply(1, min)
      } else if (method[1] == "mean") {
        collapsed_vector <- replicates_df %>% apply(1, mean)
      } else if (method[1] == "clean") {
        collapsed_vector <- replicates_df %>% apply(1, function(x) {
          if (min(x, na.rm = TRUE) < clean_thresh) {
            0
          } else{
            mean(x, na.rm = TRUE)
          }
        })
      } else if(method[1] == "cor"){
        collapsed_vector <- cor(replicates_df[[1]], replicates_df[[2]])
      }
      df_collapse[, i] <- collapsed_vector

    }


    if (length(ignore_cols) > 0 & method[1] != "cor") {
      df_collapse <- dplyr::bind_cols(ignore_df, df_collapse)
    }

    df_collapse
  }

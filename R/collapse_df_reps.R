#' collapse_df_reps
#'
#' collapse columns in df which share an identity index provided in the vector pid
#'
#' @param df data frame or tibble with observations as columns
#' @param pid patient id, character vector with length equal to number of columns in df. replicate columns are indicated by same value in corresponding elements of pid
#' @param method what kind of collapsing to do. min = take minimum value. mean = take mean of values. clean = take mean, but if either value is below clean_thresh, set final valut o zero. cor: compute correlation across all features for data. required duplicates.
#' @param clean_thresh for method="clean". threshold value below which if any replicate exhibits this value, set output to 0
#' @param ignore_cols columns in df to exclude from colapse (e.g. feature id)
#' @param show_progress logical indicating whether or not to show progress bar
#'
#' @export


collapse_df_reps <-
  function(df,
           pid,
           method = c("min", "mean", "clean", "cor"),
           clean_thresh = 2.5,
           ignore_cols = NULL, show_progress = TRUE) {


    `%>%` <- magrittr::`%>%`

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
    if(show_progress) pb <- utils::txtProgressBar(0, length(unique_pid), style = 3)
    for (i in 1:length(unique_pid)) {
      if(show_progress) utils::setTxtProgressBar(pb, i)
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
        collapsed_vector <- stats::cor(replicates_df[[1]], replicates_df[[2]])
      }
      df_collapse[, i] <- collapsed_vector

    }


    if (length(ignore_cols) > 0 & method[1] != "cor") {
      df_collapse <- dplyr::bind_cols(ignore_df, df_collapse)
    }

    df_collapse
  }


#' zclean_longitudinal
#'
#' cleans longitudinal data. similar to collapse_df_reps(method = "clean") except that zscores below clean_thresh (z_thresh) but above a lower threshold (z_thresh_2) are sitll counted if a previous sample from that person scored above z_thresh.
#'
#' @param z_min minimum zscore from collapse_df_reps(method = "min")
#' @param z_mean mean zscore from collapse_df_reps(method = "mean")
#' @param sample_list named list. each list element name is patient ID (e.g. 123456). Each list element containcs a character vector of patient+timepoint labels in order. (e.g. 123456_t1, 123456_t2, 123456_t3).
#' @param z_thresh if z > z_thresh, take mean
#' @param z_thresh2 if z_thresh2 < z < z_thresh, take mean if previous timepoint z > z_thresh, otherwise set to zero. If z < z_thresh, set to zero.
#' @param show_progress logical indicating whether or not to show progress bar
#'
#' @export



zclean_longitudinal <- function(z_min, z_mean, sample_list, z_thresh = 3.5, z_thresh2 = 2, show_progress = TRUE){
  # sample_list. named list. name = patietnt id. list element contents = sample_timepoint IDs eg. 192026: 192026_Pre, 192026_w0, 192026_w4, ...
  # for each sample,
  # if z < 2; set to zero.
  # if 2 <= z < 3.5; check if previous timeoint > 3.5 ( from smaple list)
  # if yes, take mean. if no, set to zero
  # if 3.5 <= z , take mean


  temp_z_clean <- z_mean

  if(show_progress) prog <- utils::txtProgressBar(0, ncol(z_mean), style = 3)

  (1:ncol(z_mean)) %>% lapply(function(j){
    if(show_progress) utils::setTxtProgressBar(prog, j)
    this_sample <- names(z_mean)[j] #192026_Pre
    this_patient <- names(sample_list)[grepl(this_sample, sample_list)] #192026
    this_series <- sample_list[[this_patient]] # 192026_Pre 192026_w0, 192026_w4, ...

    this_position <- (1:length(this_series))[this_series == this_sample]
    if(this_position == 1){
      this_previous_samples <- NULL
    } else{
      this_previous_samples <- this_series[1:(this_position-1)]
    }

    # set initial values
    this_z_mean <- z_mean[[this_sample]]
    this_z_min <- z_min[[this_sample]]
    this_z_clean <- this_z_mean # temporarily set

    # calclulate
    # if z < 2; set to zero.
    this_z_clean[this_z_min < z_thresh2] <- 0

    # if 2 <= z < 3.5; check if previous timeoint > 3.5 ( from smaple list)
    # if yes, take mean. if no, set to zero
    if(!is.null(this_previous_samples)){
      previous_z_min_df <- z_min[this_previous_samples]
      previous_z_min_max <- previous_z_min_df %>% apply(1, max) # get highest scoreing minimum at each peptide across previous timepoints

      this_z_clean[(this_z_min < z_thresh) & (this_z_min >= z_thresh2) & (previous_z_min_max < z_thresh)] <- 0
    }

    # if 3.5 <= z , take mean (already taken)

    this_z_clean %>% as.data.frame() %>% stats::setNames(this_sample)

  }) %>% dplyr::bind_cols()






}

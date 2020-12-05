#' zscore_bins
#' Calculate zscores from rpm scaled data. Adapted from Michael Mina et al. 2019 DOI: 10.1126/science.aay6485
#'
#' @param rpm_control Reads per million (counts per million) data frame of control data from which to determine bins (e.g. beads only). Peptide order should match that of rpm_sample.
#' @param rpm_sample Reads per million (counts per million) data frame of sample data for which to calculate z-scores. Peptide order should match that of rpm_control
#' @param binsize Number of peptides per bin (default 300)
#' @param include_percentile Peptide percentiles range to include in mean and sd calculation (default 0.05, 0.95)
#' @param ignore_cols Column names to exclude from calculation (e.g. id column)
#' @param export_bins Logical. if true, include a column called ranks indicating bins based on rpm_control values
#' @param sd_min. Default value NULL. If set to a numeric value or vector, can be used to impose a floor to the standard deviation. If a single value is given, the same floor is applied to the standard deviation of each bin. If a vector is provided, each value is applied sequentially.
#'
#'
#' @export


zscore_bins <- function(rpm_control, rpm_sample, binsize = 300,
                        include_percentile = c(0.05, 0.95),
                        ignore_cols = c(), export_bins = TRUE,
                        sd_min = NULL){

  if(length(ignore_cols) > 0){
    ignore_df <- rpm_sample %>% subset(select = ignore_cols)
    rpm_sample <- rpm_sample %>% dplyr::select(-tidyselect::all_of(ignore_cols))
  }

  # Rank order RPM from controls (e.g. beads-only IP, input library)
  control_ranks <- rpm_control %>% as.data.frame %>% apply(1, sum) %>% "*"(-1) %>% rank
  unique_ranks <- control_ranks %>% unique %>% sort

  # Bin peptides by above ranks. Min # of peptides per bin == binsize
  bins <- list()
  this_bin_index <- c()
  impose_sd_floor <- ifelse(is.null(sd_min), FALSE,TRUE)

 for(i in 1:length(unique_ranks)){
    ## First bin epitopes with identical ranks from beads only rankings
    ## If any bin contains fewer than 300 epitopes,
    ##   add epitopes w/ adjacent ranks until each bin >= 300 epitopes

    index_this_rank <- c(1:length(control_ranks))[control_ranks == unique_ranks[i]]
    this_bin_index <- c(this_bin_index, index_this_rank)
    if(length(this_bin_index)>=300){
      bins[[length(bins) + 1]] <- this_bin_index
      this_bin_index <- c()
    }
  }

    # From each bin, for each sample, take sample RPMs. Discard top bottom 5%.
    # From middle 90% calc mean, sd
    bin_mean <- bin_sd <- matrix(nrow = length(bins), ncol = ncol(rpm_sample))
    bin_ids <- vector(length = nrow(rpm_sample))
    pb <- utils::txtProgressBar(0, length(bins)*2, style = 3)
    for(i in 1:nrow(bin_mean)){
      utils::setTxtProgressBar(pb, i)
      for(j in 1:ncol(bin_mean)){
        this_bin_rpm <- rpm_sample[bins[[i]],j] %>% as.matrix %>% as.vector %>% sort
        this_bin_rpm_mid <- this_bin_rpm[
          round(length(this_bin_rpm)*include_percentile[1],0):
            round(length(this_bin_rpm)*include_percentile[2],0)
        ]
        bin_mean[i,j] <- mean(this_bin_rpm_mid)
        bin_sd[i,j] <- stats::sd(this_bin_rpm_mid)

        if(impose_sd_floor){
          if(length(sd_min) > 1){
          bin_sd[i,j] <- sd_min[i]
          } else{
            bin_sd[i,j] <- max(bin_sd[i,j], sd_min)
          }
        }
      }
      bin_ids[bins[[i]]] <- i
    }

    # For each RPM from each bin, Z = (RPM - mean)/sd
    zscores <- data.frame(matrix(NA, nrow = nrow(rpm_sample), ncol = ncol(rpm_sample)))
    rownames(zscores) <- rownames(rpm_sample)
    colnames(zscores) <- colnames(rpm_sample)
    pb <- utils::txtProgressBar(0, length(bins)*2, style = 3)
    for(i in 1:length(bins)){
      utils::setTxtProgressBar(pb, i+length(bins))
      for(j in 1:ncol(zscores)){
        if(bin_mean[i,j] == 0 & bin_sd[i,j] == 0){ # added this in case all bin had mean sd of zero. basically ignore that bin
          zscores[bins[[i]],j] <- 0
        } else{
          zscores[bins[[i]],j] <- (rpm_sample[bins[[i]],j] - bin_mean[i,j])/bin_sd[i,j]
        }

      }
    }

    if(export_bins){
      zscores <- dplyr::bind_cols(bin = bin_ids, zscores)
    }
    if(length(ignore_cols) > 0){
      zscores <- dplyr::bind_cols(ignore_df, zscores)
    }


    return(zscores)
}


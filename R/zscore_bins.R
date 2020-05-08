#' zscore_bins
#' Calculate zscores from rpm scaled data. Adapted from Michael Mina et al. 2019 DOI: 10.1126/science.aay6485
#'
#' @param rpm_control Reads per million (counts per million) data frame of control data from which to determine bins (e.g. beads only). Peptide order should match that of rpm_sample.
#' @param rpm_sample Reads per million (counts per million) data frame of sample data for which to calculate z-scores. Peptide order should match that of rpm_control
#' @param binsize Number of peptides per bin (default 300)
#' @param include_percentile Peptide percentiles range to include in mean and sd calculation (default 0.05, 0.95)
#'
#'
#' @export


zscore_bins <- function(rpm_control, rpm_sample, binsize = 300,
                        include_percentile = c(0.05, 0.95)){

  # Rank order RPM from controls (e.g. beads-only IP, input library)
  control_ranks <- rpm_control %>% apply(1, sum) %>% "*"(-1) %>% rank
  unique_ranks <- control_ranks %>% unique %>% sort

  # Bin peptides by above ranks. Min # of peptides per bin == binsize
  bins <- list()
  this_bin_index <- c()
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

    # From each bin, take sample RPMs. Discard top bottom 5%.
    # From middle 90% calc mean, sd
    bin_mean <- bin_sd <- vector()
    for(i in 1:length(bins)){
      this_bin_rpm <- rpm_sample[bins[[i]],] %>% as.matrix %>% as.vector %>% sort
      this_bin_rpm_mid <- this_bin_rpm[
        round(length(this_bin_rpm)*include_percentile[1],0):
          round(length(this_bin_rpm)*include_percentile[2],0)
      ]
      bin_mean[i] <- mean(this_bin_rpm_mid)
      bin_sd[i] <- sd(this_bin_rpm_mid)
    }

    # For each RPM from each bin, Z = (RPM - mean)/sd
    zscores <- data.frame(matrix(NA, nrow = nrow(rpm_sample), ncol = ncol(rpm_sample)))
    rownames(zscores) <- rownames(rpm_sample)
    colnames(zscores) <- colnames(rpm_sample)
    for(i in 1:length(bins)){
      if(bin_mean[i] == 0 & bin_sd[i] == 0){ # added this in case all bin had mean sd of zero. basically ignore that bin
        zscores[bins[[i]],] <- 0
      } else{
        zscores[bins[[i]],] <- (rpm_sample[bins[[i]],] - bin_mean[i])/bin_sd[i]
      }
    }
    return(zscores)
}


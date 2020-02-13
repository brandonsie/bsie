#' Input jpg heatmap, color scale, table dimensions. Output matrix of corresponding values.
#'
#' @param heatmap Either image array or path to cropped jpg containing heatmap color info
#' @param scale Either image array or path to cropped jpg containing color scale info
#' @param scale_range Numeric vector of length two containing in order the topmost and bottommost (or leftmost and rightmost) value in the color scale.
#' @param scale_direction Character vector, "vertical" or "horizontal" describing color scale orientation.
#' @param table_dim Numeric vector of length two containing in order the number of rows and number of columns in the heatmap image
#' @param n_samples Number of pixels to sample per cell. By default, heatmapExtract estimates the central pixel of each cell based on the heatmap pixel dimensions and table_dim. If n_samples is set > 1, then heatmapExtract will average across multiple near-central pixels.
#' @param verbose Logical, if true then debugging information will be printed.
#'
#' @export

heatmapExtract <- function(heatmap, scale, scale_range = c(1,0),
                           scale_direction = "vertical", table_dim,
                           n_samples = 1, verbose = TRUE){

  # Read in heatmap and scale from paths if necessary
  if(class(heatmap) == "character") heatmap <- jpeg::readJPEG(heatmap)
  if(class(scale) == "character") scale <- jpeg::readJPEG(scale)

  # Process image arrays to Hex arrays
  heatmap_val <- rgb( heatmap[,,1], heatmap[,,2], heatmap[,,3])
  heatmap_mat <- matrix(heatmap_val, dim(heatmap)[1], dim(heatmap)[2] )
  if(verbose) print(paste("Heatmap dimensions (px):", paste(dim(heatmap_mat), collapse = ", ")))

  scale_val <- rgb( scale[,,1], scale[,,2], scale[,,3])
  scale_mat <- matrix(scale_val, dim(scale)[1], dim(scale)[2] )
  if(verbose) print(paste("Color scale dimensions (px):", paste(dim(scale_mat), collapse = ",")))


  # Calculate central pixel
  cell_height <- dim(heatmap_mat)[1] / table_dim[1]
  cell_width <- dim(heatmap_mat)[2] / table_dim[2]
  central_pixel <- c(cell_height/2, cell_width/2)

  if(verbose) print(paste0(
    "Table dimensions (n): ", paste(table_dim, collapse = ", "),
    "; Cell dimensions (px): ", cell_height, ", ", cell_width,
    "; Central pixel: ", paste(central_pixel, collapse = ", ")
  ))

  # Calculate multiple central pixels if n_samples > 1
  if(n_samples == 1){
    starting_points <- list(central_pixel)
  } else if(n_samples > 1){
    n_addl_l <- floor((n_samples-1)/2)
    min_start <- central_pixel - n_addl_l

    starting_points <- list()
    for(i in 1:n_samples){
      increment <- -n_addl_l
      starting_points[[i]] <- central_pixel + increment
      increment <- increment + 1
    }

    if(verbose) print(
      paste(
        "Central pixels: ",
        paste(
          lapply(
            starting_points,
            FUN = function(x)
              paste0("[", paste(x, collapse = ","), "]")) %>% unlist,
          collapse = ", "
        )
      )
    )

  } else stop("n_samples must be an integer >= 1")


  # get hex values for each n_samples values per cell
  heatmap_sample_mats <- list()
  for(i in 1:n_samples){
    heatmap_sample_mats[[i]] <- heatmap_mat[
      seq.int(starting_points[[i]][1],
              nrow(heatmap_mat),
              cell_height),
      seq.int(starting_points[[i]][2],
              ncol(heatmap_mat),
              cell_width)
    ]
  }

  # convert hex values to RGB for heatmap
  heatmap_sample_rgb <- list()
  for(i in 1:n_samples){
    this_col2rgb <- col2rgb(heatmap_sample_mats[[i]])
    this_rgb_mat_list <- list()
    for(j in 1:3){this_rgb_mat_list[[j]] <- this_col2rgb[j,]}
    heatmap_sample_rgb[[i]] <- this_rgb_mat_list
  }

  heatmap_avg_rgb <- list()
  for(i in 1:3){
    heatmap_avg_rgb[[i]] <- heatmap_sample_rgb %>%
      lapply(FUN = function(x) x[[i]]) %>%
      lapply(as.data.frame) %>%
      (dplyr::bind_cols) %>%
      apply(1, mean)
  }

  # convert hex values to RGB for colorscale
  if(scale_direction == "vertical"){
    scale_midline <- dim(scale_mat)[[2]]/2 %>% round
    scale_hex_vector <- scale_mat[,scale_midline]

  } else if(scale_direction == "horizontal"){
    scale_midline <- dim(scale_mat)[[1]]/2 %>% round
    scale_hex_vector <- scale_mat[scale_midline,]

  } else stop("scale_direction must be either 'vertical' or 'horizontal'")

  scale_rgb <- col2rgb(scale_hex_vector)
  scale_rgb_list <- list()
  for(i in 1:3){scale_rgb_list[[i]] <- scale_rgb[i,]}

  scale_values <- seq(scale_range[1], scale_range[2],
                      length.out = ncol(scale_rgb))

  # map heatmap to color scale values
  value_mat <- matrix(nrow = table_dim[1], ncol = table_dim[2])
  for(i in 1:product(table_dim)){
      this_r <- heatmap_avg_rgb[[1]][i]
      this_g <- heatmap_avg_rgb[[2]][i]
      this_b <- heatmap_avg_rgb[[3]][i]

      dist_r <- abs(scale_rgb_list[[1]] - this_r)
      dist_g <- abs(scale_rgb_list[[2]] - this_g)
      dist_b <- abs(scale_rgb_list[[3]] - this_b)

      dist_sum <- dist_r + dist_g + dist_b
      min_dist_pos <- c(1:length(dist_sum))[dist_sum == min(dist_sum)][1]

      value_mat[i] <- scale_values[min_dist_pos]
  }

  # return matrix
  return(value_mat)

}
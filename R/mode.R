#' calculate mode from numeric vector (e.g. to indicate peak of density plot)
#' takes numeric vector, outputx x axis value of y axis peak
#' https://stackoverflow.com/questions/13133297/calculating-peaks-in-histograms-or-density-functions
#' ... for use in ggridges::geom_density_ridges to ignore second parameter of quantiles
#' @export

mode_fun <- function(vector, ...){
  d <- density(vector)
  mode = d$x[d$y == max(d$y)]

}
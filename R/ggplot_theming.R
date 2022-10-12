#' commonly used ggplot theme customization
#' @export

bms_theme <-
  ggplot2::theme_light() +
  ggplot2::theme(
    text = ggplot2::element_text(size = 15),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(color = "black"),
    plot.title = ggplot2::element_text(size = 25, face = "bold", hjust = 0.5)
  )
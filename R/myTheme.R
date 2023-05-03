#' My usual ggplot theme
#'
#' @return ggplot theme
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' p + myTheme()
myTheme <- function() {
  theme <- theme_bw() + theme(panel.grid = element_blank())
}

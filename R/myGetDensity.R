#' Get density of scatter plot by 2D KDE
#'
#' @param x X coordination
#' @param y Y coordination
#' @param nbins Number of bins to break into
#' @param ... other paramerters passed to kede2d
#'
#' @return Density for each dot
#' @export
#'
#' @import MASS
#'
#' @examples
#' myGetDensity(plot.data$x, plot.data$y)
myGetDensity <- function(x, y, nbins = 100, ...) {
  dens <- MASS::kde2d(x, y, n = nbins, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

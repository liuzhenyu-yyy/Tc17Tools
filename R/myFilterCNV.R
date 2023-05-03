#' Filter single-cell CNV by region length
#'
#' @param segcopy Cell-by-bin matrix of copy number
#' @param nbins Cutoff for number of continious bins with CNV
#'
#' @return Filtered cell-by-bin matrix of copy number
#' @export
#'
#' @examples
#' myFilterCNV(segcopy, nbins = 10)
myFilterCNV <- function(segcopy, nbins = 10) {
  vector2length <- function(vector) {
    temp <- rle(vector)
    return(rep(temp$length, temp$length))
  }

  seglen <- segcopy
  segcopy.filter <- segcopy

  for (one in 1:ncol(seglen)) {
    seglen[, one] <- vector2length(seglen[, one])
  }

  segcopy.filter[seglen <= nbins & segcopy != 2] <- 2
  return(segcopy.filter)
}

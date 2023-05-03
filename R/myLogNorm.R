#' Normalize read/UMI count matrix
#'
#' @param read.count Read count/UMI matrix
#' @param scale.factor Scale factor
#' @param log Perform log transformation or not
#'
#' @return Normalized count matrix
#' @export
#'
#' @examples
#' myLogNorm(rc, 1e6, TRUE)
myLogNorm <- function(read.count, scale.factor = 10000, log = TRUE) {
  read.count <- t(read.count) / colSums(read.count) * scale.factor
  read.count.nor <- as.data.frame(t(read.count))
  if (log) {
    read.count.nor <- log1p(read.count.nor)
  }
  return(read.count.nor)
}

#' Summarize sensitivity from scRNA-seq read/UMI count matrix
#'
#' @param rc Read count matrix
#' @param n Number of quantile bins to break into
#' @param cutoff Minimal number of reads/UMIs for detection
#'
#' @return Data.frame with quantile breaks, accululated genes and standard deviation among cells
#' @export
#'
#' @importFrom BiocGenerics sd
#'
#' @examples
#' myGeneSensitivity(rc, 20, 1)
myGeneSensitivity <- function(rc, n = 20, cutoff = 1) {
  Process_Cell <- function(rc_cell, breaks) {
    rc_cell <- rc_cell[rc_cell > cutoff]
    Gene_Total <- length(rc_cell)
    rc_cell.nor <- log10(rc_cell * 1e5 / sum(rc_cell))

    breaks <- seq(0, 5, 5 / n)
    breaks[1] <- min(rc_cell.nor)
    frequency <- unlist(lapply(
      breaks,
      function(x) {
        return(sum(rc_cell.nor <= x) / Gene_Total)
      }
    ))
    return(frequency)
  }

  freq <- apply(rc, 2, Process_Cell)
  res <- data.frame(
    "breaks" = seq(0, 5, 5 / n),
    "frequency" = apply(freq, 1, mean),
    "sd" = apply(freq, 1, sd)
  )
  return(res)
}

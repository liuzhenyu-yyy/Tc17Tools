#' Plot single-cell copy number dot plot
#'
#' @param segref Number of normalized read count of genomic bin
#' @param segcopy Copy number of genomic bin
#' @param bininfo Metadata for genomic bin, bininfo$CHR indicates chromosome
#' @param sex "XX" for female and "XY" for male
#' @param cell Name of the single cell
#' @param top Maximum copy number to be plotted
#' @param return Reture plot or not
#'
#' @return ggplot object of single cell CN profile
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' myPlotCNProfile(segref$sc1, segcopy$sc1, bininfo, cell = "sc1")
myPlotCNProfile <- function(segref, segcopy, bininfo,
                            sex = "XX", cell = "Cell", top = 8, return = FALSE) {
  x <- y <- mid <- name <- NULL # just to avoid globalVariables check for devtools
  # check input
  if (length(segref) != length(segcopy)) {
    print("Error: SegRef and SegCopy differ in length. Exit!")
    return(0)
  }
  if (length(segref) != nrow(bininfo)) {
    print("Error in bin information. Exit!")
    return(0)
  }

  print(paste("Generating CN plot for", cell))

  # calculate chromosome boundary
  bininfo$CHR <- factor(bininfo$CHR, levels = unique(bininfo$CHR))
  bounds <- data.frame(table(bininfo$CHR))
  bounds$end <- 0
  for (one in 1:nrow(bounds)) {
    bounds[one, "end"] <- sum(bounds$Freq[1:one])
  }
  bounds$mid <- bounds$end - (bounds$Freq) * 0.5
  bounds$name <- gsub("chr", "", bounds$Var1)

  # normalize for X chromosome
  if (sex == "XY") {
    bin.sex <- grep("X|Y", bininfo$CHR)
    segref[bin.sex] <- segref[bin.sex] * 0.5
  }

  # generate CNV data
  nbins <- nrow(bininfo)
  clouds <- data.frame(x = 1:nbins, y = segref)
  amp <- data.frame(x = which(segcopy > 2), y = segcopy[which(segcopy > 2)])
  del <- data.frame(x = which(segcopy < 2), y = segcopy[which(segcopy < 2)])
  flat <- data.frame(x = which(segcopy == 2), y = segcopy[which(segcopy == 2)])

  # plot
  p <- ggplot() +
    geom_point(data = clouds, aes(x = x, y = y), color = "gray75", size = 1) +
    geom_point(data = flat, aes(x = x, y = y), size = 0.5) +
    geom_point(data = amp, aes(x = x, y = y), size = 0.5, color = "red") +
    geom_point(data = del, aes(x = x, y = y), size = 0.5, color = "blue") +
    geom_text(data = bounds, aes(x = mid, y = -top * .05, label = name), size = 2) +
    labs(title = paste("Copy number profile of cell:", cell, "\nSex chromosome:", sex)) +
    scale_x_continuous(limits = c(0, nbins), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-top * .1, top), expand = c(0, 0)) +
    geom_vline(xintercept = bounds$end, lty = 2, size = 0.2) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    theme(axis.text.x = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    xlab(NULL) +
    ylab(NULL) +
    theme(title = element_text(size = 8))

  if (return) {
    return(p)
  }
  plot(p)
}

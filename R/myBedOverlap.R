#' Venn plot show intersection of 2 Granges
#'
#' @param bed1 First Grange object
#' @param bed2 Second Grange object
#' @param name1 Name of the first Grange object
#' @param name2 Name of the second Grange object
#' @param extend Number of bases to extend in each Grange
#' @param ignore.strand Ignore strand or not
#'
#' @return Venn object of bed overlap
#' @export
#'
#' @import Vennerable GenomicRanges BiocGenerics
#'
#' @examples
#' myBedOverlap(GM12878.H3K4me3.rep1, GM12878.H3K4me3.rep2, extend = 50)
myBedOverlap <- function(bed1, bed2, name1 = "bed1", name2 = "bed2", extend = 0, ignore.strand = TRUE) {
  start(bed1) <- start(bed1) - extend
  end(bed1) <- end(bed1) + extend
  start(bed2) <- start(bed2) - extend
  end(bed2) <- end(bed2) + extend

  temp <- reduce(c(bed1, bed2), ignore.strand = ignore.strand)
  temp$name <- paste("v", 1:seq_along(temp))
  temp$bed1 <- 0
  temp[countOverlaps(temp, bed1, ignore.strand = ignore.strand) > 0]$bed1 <- 1
  temp$bed2 <- 0
  temp[countOverlaps(temp, bed2, ignore.strand = ignore.strand) > 0]$bed2 <- 1
  weight <- c(
    `01` = sum(temp$bed1 == 1 & temp$bed2 == 0),
    `11` = sum(temp$bed1 == 1 & temp$bed2 == 1),
    `10` = sum(temp$bed1 == 0 & temp$bed2 == 1)
  )
  p <- Venn(Weight = weight, SetNames = c(name1, name2))
  plot(p, show = list(
    FaceText = "signature", SetLabels = FALSE,
    Faces = FALSE, DarkMatter = FALSE
  ))
  return(p)
}

#' Reverse complement of DNA sequence
#'
#' @param text DNA sequence with ATCG
#' @param complementary Whether perform complement or not
#'
#' @return Reversed DNA sequence
#' @export
#'
#' @examples
#' myRevSequence("ATGATCGTTCA", complementary = TRUE)
myRevSequence <- function(text, complementary = TRUE) {
  out <- c()
  for (one in text) {
    out <- c(out, paste(rev(unlist(strsplit(one, NULL))), collapse = ""))
  }
  if (complementary) {
    out <- gsub("A", "t", out)
    out <- gsub("T", "a", out)
    out <- gsub("C", "g", out)
    out <- gsub("G", "c", out)
    out <- toupper(out)
  }
  return(out)
}

#' Arrange multiple ggplot in grid
#'
#' @param ... ggplot objects
#' @param plotlist List of ggplot objects
#' @param file Output file
#' @param cols Number of columns
#' @param layout Order by "row" or "col"
#'
#' @return plots
#' @export
#'
#' @import grid
#'
#' @examples
#' myMultiPlot(p1, p2, p3, p4, cols = 2)
#' myMultiPlot(plotlist = p.list, cols = 2)
myMultiPlot <- function(..., plotlist = NULL, file, cols = 1, layout = "row") {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots <- length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (layout == "col") {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
      ncol = cols, nrow = ceiling(numPlots / cols)
    )
  } else if (layout == "row") {
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
      byrow = TRUE,
      ncol = cols, nrow = ceiling(numPlots / cols)
    )
  }

  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(
        layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col
      ))
    }
  }
}

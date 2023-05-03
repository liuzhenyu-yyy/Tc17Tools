#' Pair correlation and linear regression
#'
#' @param x First variable
#' @param y Second variable
#' @param xlab Label for the first variable
#' @param ylab Label for the second variable
#' @param return Return ggplot or not
#'
#' @return ggplot object
#' @export
#'
#' @import  ggplot2
#' @importFrom stats sd median lm
#'
#' @examples
#' x <- 1:20
#' y <- 5:24 + rnorm(20)
#' myPair(x, y)
myPair <- function(x, y, xlab = "x", ylab = "y", return = TRUE) {
  plot.data <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = plot.data)
  res1 <- paste("y = ", signif(model$coefficients[2], 4), "x",
    ifelse(model$coefficients[1] > 0, " + ", " - "),
    signif(abs(model$coefficients[1]), 4),
    sep = ""
  )
  res2 <- paste("r^2 = ", signif(summary(model)$r.squared, 4))
  p <- ggplot(plot.data) +
    geom_point(aes(x = x, y = y)) +
    theme_classic() +
    geom_smooth(aes(x = x, y = y), method = "lm") +
    geom_text(aes(x = median(x), y = max(y)),
      label = paste(res1, res2, sep = "\n")
    ) +
    xlab(xlab) +
    ylab(ylab)
  if (return) {
    return(p)
  }
  plot(p)
}

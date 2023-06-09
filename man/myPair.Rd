% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myPair.R
\name{myPair}
\alias{myPair}
\title{Pair correlation and linear regression}
\usage{
myPair(x, y, xlab = "x", ylab = "y", return = TRUE)
}
\arguments{
\item{x}{First variable}

\item{y}{Second variable}

\item{xlab}{Label for the first variable}

\item{ylab}{Label for the second variable}

\item{return}{Return ggplot or not}
}
\value{
ggplot object
}
\description{
Pair correlation and linear regression
}
\examples{
x <- 1:20
y <- 5:24 + rnorm(20)
myPair(x, y)
}

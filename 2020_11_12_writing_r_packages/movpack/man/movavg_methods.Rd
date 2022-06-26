\name{print.movavg}
\alias{print.movavg}
\alias{plot.movavg}
\title{Print and Plot Methods for 'movavg' Objects}
\description{
   Methods for printing and plotting objects of class \code{"movavg"}.
}
\usage{
\method{print}{movavg}(x, \dots)
\method{plot}{movavg}(x, \dots)
}
\arguments{
   \item{x}{an object of class \code{"movavg"}.}
   \item{\dots}{other arguments.}
}
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\examples{
x <- 1:50
y <- 2 + log(x) + rnorm(50, 0, 0.2)
res <- movavg(y)
res
plot(res, pch=19)
}

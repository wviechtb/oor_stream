\name{movavg}
\alias{movavg}
\title{Compute Moving Average}
\description{Function that computes the moving average of a numeric vector.}
\usage{
movavg(x, window=1, na.rm=FALSE)
}
\arguments{
   \item{x}{a numeric vector.}
   \item{window}{number to specify the window size.}
   \item{na.rm}{logical indicating whether missing values should be removed before computing the average within a window (default is \code{FALSE}).}
}
\details{
   Let \eqn{x_i} denote the \eqn{i}th element of a numeric vector of length \eqn{n}. Then the average within a particular \sQuote{window} is simply the average of all values from \eqn{x_{i-window}} to \eqn{x_{i+window}}. The function computes this average for \eqn{i = 1, \ldots, n} (ignoring values below \eqn{i-window < 1} or above \eqn{i+window > n}).
}
\value{
   An object of class \code{"movavg"}. The object is a list containing the following components:
   \item{mx}{the moving averages.}
   \item{window}{the window size.}
   \item{n}{the length of the input vector.}
   }
\author{
   Wolfgang Viechtbauer (\email{wvb@wvbauer.com})
}
\seealso{
   \code{\link{print.movavg}}, \code{\link{plot.movavg}}
}
\examples{
# a simple example
movavg(c(2,4,3,3,4,6,5,6,8,7,8,9))

# a more interesting example
x <- 1:50
y <- 2 + log(x) + rnorm(50, 0, 0.2)
res <- movavg(y)
res
plot(x, y, pch=19)
lines(x, res$mx)

# adjust the window size
res <- movavg(y, window=3)
res
plot(x, y, pch=19)
lines(x, res$mx)

# introduce a missing value
y[5] <- NA
res <- movavg(y)
res

# remove missings within a window before computing the average
res <- movavg(y, na.rm=TRUE)
res
}

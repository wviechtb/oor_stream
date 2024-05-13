############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-02
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 8.1
#
# last updated: 2024-05-09

############################################################################

### 8.1: Least squares, maximum likelihood, and Bayesian inference

## Least squares

# the simple regression model is given by:
#
# y_i = a + b * x_i + e_i
#
# and we assume e_i ~ N(0, sigma^2)

# simulate some data based on this model
set.seed(1239)
n <- 50
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rnorm(n, mean=0, sd=1)

# plot the data
plot(x, y, pch=21, bg="gray", bty="l", panel.first=grid())

# add the true regression line to the plot
abline(a=2, b=0.5, lwd=3)

# since we know the true regression line here, we can compute the errors
y - (2 + 0.5 * x)

# but in practice, we have to estimate the intercept and slope of the model; as
# explained in the book, we can do so using the equations given

# create the X and y matrices
X <- cbind(1, x)
X
y <- cbind(y)
y

# the regression model can then be written in matrix notation as:
#
# y = X %*% beta + e
#
# where beta is a column vector with the intercept and slope

# now apply equation (8.2) to compute the estimated intercept and slope
betahat <- solve(t(X) %*% X) %*% t(X) %*% y
dimnames(betahat) <- list(c("a","b"), NULL)
betahat

# change y back to a vector
y <- c(y)

# double-check that (8.3) and (8.4) provide the same estimates
b <- sum((x - mean(x)) * y) / sum((x - mean(x))^2)
a <- mean(y) - b * mean(x)
rbind(a, b)

# add the estimated regression line to the plot as a dotted line
abline(a, b, lwd=3, lty="dotted")

# fit the model using lm()
res <- lm(y ~ x)

# extract the estimated regression coefficients
coef(res)

# obtain the full results (including the standard errors of the coefficients)
summary(res)

# aside from being tedious, it should be noted that the 'manual' computations
# above are not how the estimates of the intercept and slope should be computed;
# lm() internally uses equations that are numerically more stable

# extract the variance-covariance matrix of the parameter estimates
vcoef <- vcov(res)
vcoef

# the values along the diagonal of this matrix are the squared standard errors;
# so the square root of the diagonal elements are the standard errors
se <- sqrt(diag(vcoef))
se

# turn the variance-covariance matrix into a correlation matrix; we see that the
# estimate of the intercept and slope are negatively correlated
cov2cor(vcoef)

# compute the residuals
resid <- y - (a + b * x)
resid

# can also get the residuals from the fitted model object
resid(res)

# check that the mean of the residuals is zero (note: due to numerical
# imprecision, this value is not exactly equal to 0, but practically
# indistinguishable from zero)
mean(resid)

# compute the residual sum of squares (RSS)
sum(resid^2)

# you cannot make this any smaller with other values of 'a' and 'b'

# function to compute the RSS for given values of 'a' and 'b'
rssfun <- function(a, b, xvals, yvals) {
   resid <- yvals - (a + b*xvals)
   rss <- sum(resid^2)
   return(rss)
}

# double-check that we get the same RSS as above
rssfun(a, b, xvals=x, yvals=y)

# try a few other values for 'a' and 'b' (all of these are larger)
rssfun(a=2.3, b=0.41, xvals=x, yvals=y)
rssfun(a=2.1, b=0.41, xvals=x, yvals=y)
rssfun(a=2.1, b=0.40, xvals=x, yvals=y)

# compute the RSS for various combinations of intercept and slope values

as <- seq(1.4, 3.2, length=100)
bs <- seq(0.2, 0.7, length=100)
rss <- matrix(NA, nrow=length(as), ncol=length(bs))

for (i in 1:length(as)) {
   for (j in 1:length(bs)) {
      rss[i,j] <- rssfun(as[i], bs[j], xvals=x, yvals=y)
   }
}

# create a perspective plot of the RSS surface and indicate the lowest point as
# a red dot (at the estimated intercept and slope)
sav <- persp(as, bs, rss, ticktype="detailed", theta=45,
             xlab="Intercept", ylab="Slope", zlab="RSS")
points(trans3d(a, b, min(rss), pmat=sav), pch=19, cex=1.5, col="red")

# instead of a perspective plot, we can visualize the surface using a contour
# plot with colors indicating the height; we also indicate the estimates with a
# red dot and lines extending from that dot +- one standard error for each
# coefficient

filled.contour(as, bs, rss, color.palette=hcl.colors, nlevels=50,
               xlab="Intercept", ylab="Slope",
               plot.axes = {
                  axis(side=1)
                  axis(side=2)
                  segments(a-se[1], b, a+se[1], b, lwd=3, col="red")
                  segments(a, b-se[2], a, b+se[2], lwd=3, col="red")
                  points(a, b, pch=19, cex=1.5, col="red")
               })

## Estimation of residual standard deviation sigma

# we know that the true sigma is equal to 1 (since we simulated the data), but
# in practice, we would not know this; instead, we can estimate sigma based on
# the residuals; we can do this by computing the square root of 1/n * RSS
sqrt(1/n * sum(resid^2))

# but a better estimate is to divide the RSS by n-2, where the 2 is the number
# of regression coefficients of the model (note that this actually makes the
# estimate of sigma worse here -- it is further away from the true value -- but
# in general, this yields a better estimate of sigma)
sqrt(1/(n-2) * sum(resid^2))

# re-write the function so that it takes a vector with the intercept and slope
# values as input
rssfun <- function(par, xvals, yvals) {
   a <- par[1]
   b <- par[2]
   resid <- yvals - (a + b*xvals)
   rss <- sum(resid^2)
   cat("a =", formatC(a, format="f", flag=" ", digits=6),
       "b =", formatC(b, format="f", flag=" ", digits=6),
       "rss =", formatC(rss, format="f", digits=6), "\n")
   return(rss)
}

# use numerical optimization methods to iteratively find the intercept and slope
# values that minimize the RSS (note that this is not necessary for this kind of
# model since the estimates of the intercept and slope can be obtained via
# 'closed-form solutions', but this illustrates the principle that we can obtain
# the same estimates using numerical optimization methods
optim(c(0,0), rssfun, xvals=x, yvals=y)

# we essentially get the same estimates (although there are minor numerical
# differences that arise because of the use of an iterative procedure for
# finding the estimates)

## Maximum likelihood

# if we set the intercept, slope, and sigma to some chosen values, we can
# compute the density of the observed data under a normal distribution; say we
# assume that the intercept is 2.2, the slope is 0.4, and sigma is 1.2, then we
# get the following density values for the data
p <- dnorm(y, mean = 2.2 + 0.4 * x, sd = 1.2)
p

# we can multiply these values to get the joint density
prod(p)

# we call this the 'likelihood' of the parameters given the data; we want to
# find those parameter values (estimates) that are most likely given the data;
# those are the maximum likelihood estimates

# for numerical reasons, instead of maximizing the product of the density
# values, we will maximize the sum of the log-transformed values
sum(log(p))

# this is the log likelihood given the parameter estimates we assumed

# function that computes the log likelihood
mlefun <- function(par, xvals, yvals) {
   a <- par[1]
   b <- par[2]
   sigma <- par[3]
   logp <- dnorm(yvals, mean = a + b * xvals, sd = sigma, log=TRUE)
   ll <- sum(logp)
   cat("a =", formatC(a, format="f", flag=" ", digits=6),
       "b =", formatC(b, format="f", flag=" ", digits=6),
       "sigma =", formatC(sigma, format="f", flag=" ", digits=6),
       "ll =", formatC(ll, format="f", digits=6), "\n")
   return(ll)
}

# use numerical optimization methods to iteratively find the intercept, slope,
# and sigma values that maximize the log likelihood (note: optim() does
# minimization by default, so we have to tell it to maximize)
optim(c(0,0,2), mlefun, xvals=x, yvals=y, control=list(fnscale=-1))

# the MLEs are identical to the least squares estimates (they only differ again
# due to the use of an iterative procedure for finding the estimates), except
# for sigma, where the MLE can be shown to be identical to
sqrt(1/n * sum(resid^2))

## Where do the standard errors come from? Using the likelihood surface to
## assess uncertainty in the parameter estimates

# re-write the mlefun() function so that sigma is directly computed from the
# intercept and slope values and that it optionally either returns the
# likelihood or the log likelihood (with the former being the default)
mlefun <- function(par, xvals, yvals, log=FALSE) {
   a <- par[1]
   b <- par[2]
   n <- length(yvals)
   sigma <- sqrt(1/n * sum((yvals - (a + b*xvals))^2)) # MLE of sigma
   if (log) {
      logp <- dnorm(yvals, mean = a + b * xvals, sd = sigma, log=TRUE)
      ll <- sum(logp)
      return(ll)
   } else {
      p <- dnorm(yvals, mean = a + b * xvals, sd = sigma)
      l <- prod(p)
      return(l)
   }
}

# compute the likelihood for various combinations of intercept and slope values

as <- seq(1.4, 3.2, length=100)
bs <- seq(0.2, 0.7, length=100)
ls <- matrix(NA, nrow=length(as), ncol=length(bs))

for (i in 1:length(as)) {
   for (j in 1:length(bs)) {
      ls[i,j] <- mlefun(c(as[i], bs[j]), xvals=x, yvals=y)
   }
}

# create a perspective plot of the likelihood surface (like Figure 8.1(a),
# except that we are using the simulated data from above)
persp(as, bs, ls)

# install the ellipse package
#install.packages("ellipse")

# load the ellipse package
library(ellipse)

# instead of a perspective plot, we can again visualize the surface using a
# contour plot with colors indicating the height; we also indicate the peak with
# a red dot and lines extending from that dot +- one standard error for each
# coefficient (recall that this encompasses 68% of the distribution of a normal
# distribution); we also add the contour ellipse that encompasses 68% of the
# joint distribution of the two coefficients assuming bivariate normality
filled.contour(as, bs, ls, color.palette=hcl.colors,
               xlab="Intercept", ylab="Slope",
               plot.axes = {
                  axis(side=1)
                  axis(side=2)
                  xy <- ellipse(vcoef, centre=c(a,b), level=0.68)
                  lines(xy[,1], xy[,2], lwd=3, col="red")
                  segments(a-se[1], b, a+se[1], b, lwd=3, col="red")
                  segments(a, b-se[2], a, b+se[2], lwd=3, col="red")
                  points(a, b, pch=19, col="red")
               })

# if we move away from the peak (red dot), then the drop in the likelihood is
# not so severe if an increase in the intercept value is paired with a decrease
# in the slope value (and vice-versa); this is due to the negative correlation
# between these two estimates

# the steepness of the likelihood surface around the peak is an indicator of the
# precision with which we have estimated the regression coefficients; if the
# drop around the peak is very steep, then we should have higher confidence in
# the estimates we have obtained; the steepness around the peak can be measured
# based on the second derivative of the (log) likelihood function; a high second
# derivative indicates high acceleration (high precision) and hence the inverse
# of the second derivative indicates imprecision; since we are dealing with two
# parameters, there is a 2x2 matrix of second derivatives, which is called the
# 'Hessian' matrix; it turns out that the inverse of the negative Hessian matrix
# corresponds to the variance-covariance matrix of the estimates

# install the numDeriv package
#install.packages("numDeriv")

# load the numDeriv package
library(numDeriv)

# obtain the Hessian matrix
H <- hessian(mlefun, x=c(a,b), xvals=x, yvals=y, log=TRUE)

# take the inverse of the negative Hessian
solve(-H)

# compare this to the variance-covariance matrix we obtained earlier from
# least squares estimation
vcoef

# the discrepancy is due to the way sigma is estimated by least squares versus
# maximum likelihood estimation (dividing either by n-2 or n); if we correct the
# inverse of the negative Hessian by n / (n-2), we get the same var-cov matrix
n / (n-2) * solve(-H)

## Bayesian inference

# we will implement the ideas discussed here next time

## Point estimate, mode-based approximation, and posterior simulations

# as above

############################################################################

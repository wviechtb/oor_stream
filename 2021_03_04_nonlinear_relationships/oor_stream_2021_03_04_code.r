############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-03-04
#
# Topic(s):
# - modeling non-linear relationships
# - polynomial regression
# - spline models
#
# last updated: 2021-03-10

############################################################################

# just a little addendum to the previous stream where we talked about random
# number generation; rnorm() can generate random values based on a (standard)
# normal distribution, but these are 'pseudo random numbers' (they look like
# random numbers and can essentially be treated as such, but ultimately they
# are algorithmically generated and not truly random)
rnorm(100)

# the 'random' package can generate truly random numbers
#install.packages("random")
library(random)

# generate 100 random integer between 0 and 10000000
x <- c(randomNumbers(100, col=1, min=0, max=10000000))

# turn these integers into proportions between 0 and 1
p <- x / 10000000

# use the inverse of the cumulative distribution function of a standard normal
# distribution to turn these proportions into random standard normal values
z <- qnorm(p)
z

# check that mean =~ 0 and SD =~ 1
mean(z)
sd(z)

############################################################################

# illustrative dataset
dat <- structure(list(xi = c(3.33, 9.54, 0.46, 1.35, 2.08, 7.79, 9.28, 3.97, 8.04, 8.4, 1.36, 8.4, 1.91, 3.85, 5.09, 1.8, 7.84, 3.38, 3.82, 2.4, 4.88, 6.95, 9.36, 1.18, 1.45, 6.86, 8.19, 3.94, 8.33, 4.75, 9.62, 1.02, 5.24, 8.96, 2.79, 0.43, 3.9, 6.13, 1.18, 8.89, 7.43, 7.88, 5.51, 5.73, 4.58, 1.46, 1.61, 8.52, 6.23, 4.51, 5.62, 8.72, 5.07, 9.13, 0.74, 7.67, 9.19, 0.24, 6.36, 2.23, 9.46, 3.57, 7.09, 4.67, 7.97, 1.68, 6.91, 3.02, 7.83, 6.84, 9.11, 3.66, 1.04, 5.62, 6.38, 7.44, 4.66, 0.47, 5.54, 0.99, 6.7, 2.03, 6.94, 9.49, 9.16, 3.13, 2.2, 3.53, 6.58, 5.88, 5.98, 8.81, 4.74, 2.63, 6.78, 3.09, 6.72, 2.14, 0.27, 8.93, 3.59, 0.59, 1.91, 2.47, 8.53, 6.45, 2.44, 6.38, 6.76, 3.41, 9.46, 1.77, 0.91, 7.16, 5.95, 7.88, 5.33, 2.17, 6.33, 5.94, 6.85, 0.62, 4.92, 1.2, 0.76, 4.5, 5.73, 4.24, 0.99, 6.7, 4.27, 0.91, 6.43, 0.9, 1.21, 9.04, 2.8, 9.35, 8.06, 7.96, 0.36, 1.3, 2.46, 0.73, 1.68, 9.35, 5.91, 6.74, 0.32, 3.43, 1.78, 9.01, 8.23, 0.57, 8.22, 2.42, 7.35, 2.61, 7.12, 2.21, 8.23, 2.48, 5.04, 5.16, 5.29, 4.27, 1.17, 7.52, 9.16, 2.56, 2.74, 8.1, 1.55, 6.12, 8.12, 4.74, 6.39, 5.64, 8.9, 2.5, 7.91, 9.33, 8.29, 4.62, 5.33, 3.96, 1.41, 6.49, 0.39, 8.22, 3.85, 4.55, 7.07, 4.53, 6.29, 0.75, 1.38, 8.22, 6.1, 1.97, 3.23, 5.8, 5.16, 8.27, 8.73, 7.46, 6.76, 8.25, 3.16, 1.77, 8.98, 3.24, 6.58, 4.15, 2.11, 5.24, 5.45, 6.1, 8.77, 0.77, 6.48, 9.44, 9.58, 6.24, 2.66, 7.34, 5.88, 7.37, 6.26, 6.86, 4.2, 5.75, 1.77, 7.53, 3.32, 6.19, 5.41, 7.1, 4.58, 5.92, 4.8, 1.17, 3.5, 4.82, 5.85, 0.84, 1.15, 5.38, 2.94, 6.68), yi = c(2.28, 5.86, 2.47, 1.79, 2.16, 6.41, 6.19, 2.89, 5.76, 6.28, 2.37, 5.66, 2.17, 2.47, 4.31, 1.57, 5.62, 2.58, 3.12, 2.05, 4.38, 5.62, 5.83, 1.97, 2.44, 5.75, 5.45, 2.27, 5.9, 3.91, 5.86, 1.7, 4.27, 5.91, 2.36, 1.85, 2.54, 5.74, 2.4, 5.92, 5.53, 5.9, 4.47, 4.75, 3.34, 2.02, 2.1, 5.89, 5.01, 3.42, 4.86, 5.89, 3.9, 6.43, 2.36, 6.37, 5.54, 2.46, 4.87, 2.43, 5.97, 3.35, 5.51, 3.36, 5.96, 2.08, 5.95, 2.79, 5.73, 5.81, 6.15, 2.71, 1.94, 4.95, 4.63, 5.63, 4.06, 2.29, 4.21, 2.15, 5.35, 2.08, 5.86, 6.45, 5.95, 2.33, 2.35, 2.81, 5.46, 5.43, 4.67, 6.5, 3.66, 2.76, 5.38, 2.63, 5.6, 1.58, 2.11, 5.86, 2.97, 1.93, 1.75, 2.69, 6.38, 5.09, 2.19, 6.22, 5.45, 2.82, 5.56, 2.45, 2.07, 5.53, 5.13, 5.32, 4.06, 1.94, 4.85, 4.71, 5.55, 2.58, 3.95, 1.99, 1.59, 3.69, 4.74, 2.87, 2.24, 5.99, 3.14, 1.44, 5.42, 1.73, 2.01, 5.59, 2.26, 5.92, 5.71, 5.68, 2.52, 2.1, 2.24, 1.78, 1.86, 5.8, 5.06, 5.29, 1.62, 3.09, 2.69, 6.02, 5.85, 2.01, 6.21, 2.36, 5.32, 1.91, 5.74, 2.86, 5.51, 2.37, 4.19, 4.31, 4.35, 3.05, 1.85, 5.72, 6.4, 2.1, 2.13, 5.94, 2.21, 5.71, 6.12, 3.74, 4.78, 5.33, 5.75, 2.18, 5.66, 6.39, 6.2, 3.29, 4.25, 2.91, 1.86, 5.79, 2, 5.65, 3.15, 3.46, 5.18, 3.5, 5.58, 2.26, 1.93, 5.66, 5.19, 2.32, 2.44, 5.31, 3.93, 5.57, 5.77, 5.59, 5.84, 5.99, 2.12, 2.53, 6.57, 2.63, 5.23, 3.1, 2.6, 4.17, 4.53, 4.89, 5.89, 2.08, 5.37, 5.85, 5.67, 5.06, 2.3, 5.66, 4.95, 5.69, 5.43, 5.49, 3.3, 4.58, 1.58, 5.55, 2.88, 4.56, 4.86, 5.7, 3.13, 4.94, 4.19, 1.88, 2.38, 3.6, 5.2, 1.95, 1.98, 4.62, 2.23, 5.27 )), class = "data.frame", row.names = c(NA, -250L))

# plot xi versus yi
plot(dat$xi, dat$yi, pch=21, col="gray40", bg="gray80", xlab="Variable x", ylab="Variable y", xlim=c(0,10), ylim=c(1,7))

# fit linear regression model
res <- lm(yi ~ xi, data=dat)
summary(res)

# compute predicted values based on the model (and 95% CI for the predicted values)
xs <- seq(0, 10, length=1001)
sav <- predict(res, newdata = data.frame(xi=xs), interval="confidence")
sav <- data.frame(sav)
polygon(c(xs,rev(xs)), c(sav$lwr,rev(sav$upr)), col=rgb(0,0,1,.3), border=NA)
lines(xs, sav$fit, lwd=3)

# if you prefer the points on top of the line, run this line
#points(dat$xi, dat$yi, pch=21, col="gray40", bg="gray80")

# fitted versus residuals plot
plot(fitted(res), resid(res), pch=19, xlab="Fitted Values", ylab="Residuals")
abline(h=0, lty="dotted")

############################################################################

# https://en.wikipedia.org/wiki/Polynomial_regression

# quadratic polynomial regression model:
#
# yi = beta0 + beta1 * xi + beta2 * xi^2 + ei
#    = beta0 + (beta1 + beta2*xi) * xi   + ei
#              |----------------|
#               the slope of xi (changes as a function of xi)
#
# cubic polynomial regression model:
#
# yi = beta0 + beta1 * xi + beta2 * xi^2 + beta3 * xi^3 + ei

# fit cubic polynomial regression model
res <- lm(yi ~ xi + I(xi^2) + I(xi^3), data=dat)
summary(res)

# compare the fit of the linear and cubic models
res0 <- lm(yi ~ xi, data=dat)
res1 <- lm(yi ~ xi + I(xi^2) + I(xi^3), data=dat)
anova(res0, res1)

# plot xi versus yi
plot(dat$xi, dat$yi, pch=21, col="gray40", bg="gray80", xlab="Variable x", ylab="Variable y", xlim=c(0,10), ylim=c(1,7))

# compute predicted values based on the model (and 95% CI for the predicted values)
xs <- seq(0, 10, length=1001)
sav <- predict(res, newdata = data.frame(xi=xs), interval="confidence")
sav <- data.frame(sav)
polygon(c(xs,rev(xs)), c(sav$lwr,rev(sav$upr)), col=rgb(0,0,1,.3), border=NA)
lines(xs, sav$fit, lwd=3)

# if you prefer the points on top of the line, run this line
#points(dat$xi, dat$yi, pch=21, col="gray40", bg="gray80")

# add the prediction interval
sav <- predict(res, newdata = data.frame(xi=xs), interval="prediction")
sav <- data.frame(sav)
polygon(c(xs,rev(xs)), c(sav$lwr,rev(sav$upr)), col=rgb(0,0,0,.05), border=NA)

# fitted versus residuals plot
plot(fitted(res), resid(res), pch=19, xlab="Fitted Values", ylab="Residuals")
abline(h=0, lty="dotted")

############################################################################

# fit cubic polynomial regression model
res <- lm(yi ~ xi + I(xi^2) + I(xi^3), data=dat)
summary(res)

# use poly() function instead
res <- lm(yi ~ poly(xi, 3), data=dat)
summary(res)

# note: this constructs 'orthogonal' polynomial terms; if you
# want the 'standard' polynomials, use poly(..., raw=TRUE)

############################################################################

# rms is the package corresponding to the book "Regression Modeling Strategies"
# by Frank Harrell; for more details, see: https://hbiostat.org/rms/
#
# a paper that might provide a nice introduction to restricted cubic splines:
# https://www.nature.com/articles/s41409-019-0679-x

#install.packages("rms")
library(rms)

# fit restricted cubic spline model with 4 knots
res <- lm(yi ~ rcs(xi, 4), data=dat)
summary(res)

# plot xi versus yi
plot(dat$xi, dat$yi, pch=21, col="gray40", bg="gray80", xlab="Variable x", ylab="Variable y", xlim=c(0,10), ylim=c(1,7))

# get the knot positions that were used in the model
knots <- attr(rcs(model.matrix(res)[,2], 4), "parms")
knots

# show knots in the scatterplot
abline(v=knots, lty="dotted")

# compute predicted values based on the model and add line to the plot
xs <- seq(0, 10, length=1001)
Xnew <- cbind(1, rcspline.eval(xs, knots, inclx=TRUE))
pred <- Xnew %*% coef(res)
lines(xs, pred, lwd=3)

# add the confidence interval
se <- apply(Xnew, 1, function(xnew) sqrt(rbind(xnew) %*% vcov(res) %*% cbind(xnew)))
ci.lb <- pred - 1.96*se
ci.ub <- pred + 1.96*se
polygon(c(xs, rev(xs)), c(ci.lb, rev(ci.ub)), col=rgb(0,0,1,.3), border=NA)

# add the prediction interval
se <- apply(Xnew, 1, function(xnew) sqrt(rbind(xnew) %*% vcov(res) %*% cbind(xnew) + sigma(res)^2))
ci.lb <- pred - 1.96*se
ci.ub <- pred + 1.96*se
polygon(c(xs, rev(xs)), c(ci.lb, rev(ci.ub)), col=rgb(0,0,0,.05), border=NA)

############################################################################

# another illustrative dataset
dat <- structure(list(xi = c(5.97, 2.44, 3.22, 6.69, 4.12, 4.44, 9.49, 0.92, 8.95, 2.49, 2.41, 6.64, 3.77, 8.16, 7.49, 8.94, 8.51, 5.15, 1.51, 6.54, 9.38, 6.46, 2.08, 5.78, 1.73, 8.87, 1.41, 2.02, 5.99, 1.43, 7.89, 7.22, 3.9, 0.65, 6.79, 0.33, 3.99, 8.16, 4.8, 6.72, 6.07, 6.01, 0.6, 4.2, 2.11, 9.26, 0.61, 1.82, 8.84, 5.87, 6.95, 5.25, 8.42, 9.25, 7.05, 8.77, 3.49, 6.67, 7.26, 0.35, 7.41, 4.77, 9.18, 1.89, 2.87, 0.9, 9.3, 7.46, 4.36, 8.96, 6.45, 5.25, 1.72, 7.2, 3.8, 8.87, 9.37, 6.9, 8.21, 1.87, 3.92, 3.04, 1.34, 9.16, 1.75, 7.14, 0.79, 6.97, 9.34, 3.6, 4.34, 2.02, 0.79, 9.45, 9.51, 8.38, 5.18, 5.67, 0.87, 0.87, 3.32, 9.14, 3.03, 7.78, 6.57, 9.15, 5.77, 0.89, 4.23, 0.64, 6.15, 9.25, 2.03, 7.5, 7.91, 5.87, 6.33, 2.32, 4.37, 4.62, 7.97, 1.21, 1.5, 8.04, 0.91, 4.56, 7.18, 7.72, 6.72, 1.57, 2.6, 6.51, 4.43, 2.47, 4.65, 5.99, 5.26, 6.51, 4.23, 4.08, 9.61, 9.15, 3.62, 3.55, 7.56, 4.58, 5.25, 1.67, 4.6, 3.76, 9.62, 1.34, 8.59, 3.01, 9.05, 8.4, 6.93, 8.5, 5.75, 4.45, 9.25, 6.44, 1.8, 2.84, 2.97, 7.31, 6.74, 9.18, 3.18, 8.37, 6.47, 5.85, 8.39, 0.57, 8.25, 7.05, 7.27, 6.68, 5.67, 6.41, 2.99, 7.26, 4.03, 1.72, 5.7, 0.98, 8.5, 6.27, 4.77, 0.46, 6.84, 2.66, 5.88, 9.15, 3.28, 2.78, 7.89, 4.6, 2.7, 9.32, 1.47, 4.95, 8.05, 8.31, 4.93, 4.1, 6.13, 4.6, 8.64, 5.07, 2.86, 4.01, 6.65, 2.81, 2.86, 0.25, 5.29, 7.48, 7.23, 3.87, 6.06, 6.48, 9.1, 3.88, 1.97, 1.64, 4.84, 0.33, 3.09, 4.66, 9.51, 5.33, 8.63, 1.28, 5.04, 6.35, 7.33, 7.77, 7.97, 0.4, 2.41, 3.74, 5.71, 9.04, 4.72, 5.78, 1.36, 6.02, 0.24, 2.77), yi = c(5.09, 4.31, 5.26, 4.71, 5.23, 5.64, 4.23, 2.72, 4.34, 4.12, 4.85, 4.56, 6.7, 3.89, 4.53, 4.19, 3.67, 5.35, 3.59, 4.11, 4.78, 4.1, 3.74, 5.14, 3.19, 3.93, 3.81, 3.89, 4.45, 3.13, 4.17, 3.86, 5.98, 2.22, 4.53, 1.79, 5.43, 3.96, 5.62, 4.35, 5.42, 5.03, 1.86, 6.09, 4.39, 4.26, 1.58, 3.3, 3.79, 4.95, 3.98, 5.12, 3.56, 3.84, 3.89, 4.13, 6.17, 4.64, 4.44, 1.5, 4.15, 5.71, 4.37, 3.25, 5.67, 2.28, 3.69, 4.56, 5.09, 3.56, 4.31, 5.65, 3.56, 4.44, 5.92, 3.98, 4.16, 4.53, 4.11, 3.76, 5.73, 5.71, 2.85, 4.33, 3.35, 3.66, 2.75, 4.76, 4.47, 5.42, 6.36, 2.83, 2.48, 3.78, 3.78, 4.62, 5.79, 5.88, 3.17, 1.81, 5.8, 3.69, 5.18, 4.89, 5.01, 4.34, 5.8, 2.66, 6.33, 1.42, 4.98, 3.45, 4.01, 3.92, 4.15, 5.22, 4.5, 3.76, 6.21, 5.01, 4.6, 2.94, 2.9, 4.65, 2.79, 5.61, 4.23, 3.91, 4.5, 3.21, 4.89, 4.25, 6.03, 4.48, 6.44, 4.82, 6.21, 4.77, 6.28, 5.98, 4.25, 4.44, 5.85, 5.68, 3.48, 5.32, 5.6, 3.4, 6.1, 5.96, 4.2, 3.23, 4.8, 5.32, 4.09, 4.11, 4.08, 4.88, 5.35, 5.98, 4.41, 4.97, 3.91, 5.51, 5.3, 4.61, 3.81, 2.9, 4.8, 4.01, 4.03, 5.34, 4.23, 1.98, 4.25, 4.31, 4.66, 4.79, 5.11, 4.07, 4.89, 4.37, 6.22, 3.05, 4.69, 2.63, 4.36, 5.03, 6.04, 1.55, 4.26, 4.49, 4.84, 4.07, 5.54, 5.09, 3.68, 6.76, 4.73, 4.45, 3.58, 5.72, 3.47, 4.46, 6.15, 5.64, 5.16, 5.42, 4.87, 6.46, 4.81, 6.01, 4.18, 4.31, 4.34, 1.22, 5.7, 3.89, 3.82, 6.04, 4.8, 4.02, 3.92, 5.64, 4.31, 3.05, 5.9, 0.83, 5.05, 5.65, 3.5, 5.73, 4.23, 2.06, 6.1, 4.59, 3.88, 3.9, 4.89, 1.25, 3.97, 6.28, 5.17, 4.54, 6.17, 4.98, 3.12, 5.13, 1.19, 5.19 )), class = "data.frame", row.names = c(NA, -250L))

# plot xi versus yi
plot(dat$xi, dat$yi, pch=21, col="gray40", bg="gray80", xlab="Variable x", ylab="Variable y", xlim=c(0,10), ylim=c(1,7))

# fit restricted cubic spline model with 4 knots
res <- lm(yi ~ rcs(xi, 4), data=dat)
summary(res)

# get the knot positions that were used in the model
knots <- attr(rcs(model.matrix(res)[,2], 4), "parms")
knots

# show knots in the scatterplot
abline(v=knots, lty="dotted")

# compute predicted values based on the model and add line to the plot
xs <- seq(0, 10, length=1001)
Xnew <- cbind(1, rcspline.eval(xs, knots, inclx=TRUE))
pred <- Xnew %*% coef(res)
lines(xs, pred, lwd=3)

# add the confidence interval
se <- apply(Xnew, 1, function(xnew) sqrt(rbind(xnew) %*% vcov(res) %*% cbind(xnew)))
ci.lb <- pred - 1.96*se
ci.ub <- pred + 1.96*se
polygon(c(xs, rev(xs)), c(ci.lb, rev(ci.ub)), col=rgb(0,0,1,.3), border=NA)

# add the prediction interval
se <- apply(Xnew, 1, function(xnew) sqrt(rbind(xnew) %*% vcov(res) %*% cbind(xnew) + sigma(res)^2))
ci.lb <- pred - 1.96*se
ci.ub <- pred + 1.96*se
polygon(c(xs, rev(xs)), c(ci.lb, rev(ci.ub)), col=rgb(0,0,0,.05), border=NA)

############################################################################

# manually specify the knot positions
knots <- c(2, 4, 7, 9)
res <- lm(yi ~ rcs(xi, knots), data=dat)
summary(res)

# plot xi versus yi and show knot positions
plot(dat$xi, dat$yi, pch=21, col="gray40", bg="gray80", xlab="Variable x", ylab="Variable y", xlim=c(0,10), ylim=c(1,7))
abline(v=knots, lty="dotted")

# show knots in the scatterplot
abline(v=knots, lty="dotted")

# compute predicted values based on the model and add line to the plot
xs <- seq(0, 10, length=1001)
Xnew <- cbind(1, rcspline.eval(xs, knots, inclx=TRUE))
pred <- Xnew %*% coef(res)
lines(xs, pred, lwd=3)

# add the confidence interval
se <- apply(Xnew, 1, function(xnew) sqrt(rbind(xnew) %*% vcov(res) %*% cbind(xnew)))
ci.lb <- pred - 1.96*se
ci.ub <- pred + 1.96*se
polygon(c(xs, rev(xs)), c(ci.lb, rev(ci.ub)), col=rgb(0,0,1,.3), border=NA)

# add the prediction interval
se <- apply(Xnew, 1, function(xnew) sqrt(rbind(xnew) %*% vcov(res) %*% cbind(xnew) + sigma(res)^2))
ci.lb <- pred - 1.96*se
ci.ub <- pred + 1.96*se
polygon(c(xs, rev(xs)), c(ci.lb, rev(ci.ub)), col=rgb(0,0,0,.05), border=NA)

############################################################################

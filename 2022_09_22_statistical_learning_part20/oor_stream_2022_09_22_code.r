############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-09-22
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 7.6 - 7.8
#
# last updated: 2022-09-23

############################################################################

### brief recap of smoothing splines (section 7.5) and some additions

# install (if necessary) the ISLR2
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# load the splines package
library(splines)

# copy the Wage dataset to 'dat'
dat <- Wage

# plot the data
plot(wage ~ age, data=dat, pch=21, col="darkgray", bg="lightgray", cex=0.7,
     xlab="Age", ylab="Wage", bty="l")

############################################################################

# natural spline model with knots at every unique value of age

knots <- sort(unique(dat$age))
knots <- knots[-c(1, length(knots))]
res0 <- lm(wage ~ ns(age, knots=knots), data=dat)

# add the regression line/curve to the plot
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res0, newdata=newdat)
lines(newdat$age, pred, col="blue", lwd=8)

# fit smoothing spline, setting the effective degrees of freedom to 16 (which
# implies a certain value for lambda) and ensure that all unique values of age
# are used as the knot positions
res1 <- smooth.spline(dat$age, dat$wage, df=16, all.knots=TRUE)
res1

# add the regression line/curve to the plot
lines(res1, col="red", lwd=8)

# smoothing spline with lower effective degrees of freedom (so more smoothing)
res2 <- smooth.spline(dat$age, dat$wage, df=8, all.knots=TRUE)
res2

# add the regression line/curve to the plot
lines(res2, col="green4", lwd=8)

# try to manually fit the smoothing spline

X <- cbind(1, ns(dat$age, knots=knots))
y <- dat$wage
Xd <- cbind(1, ns(seq(min(dat$age), max(dat$age), length=10000), knots=knots))

fitfun <- function(par, X, y, Xd, lambda=0) {
   pred <- c(X %*% par)
   rss <- sum((y - pred)^2)
   pred <- c(Xd %*% par)
   penalty <- sum((diff(diff(pred)))^2)
   rss + lambda * penalty
}

# first try to replicate the natural spline model (like res0)
sav0 <- nlminb(c(mean(dat$wage), rep(0,ncol(X)-1)), fitfun, X=X, y=y, Xd=Xd)
pred0 <- c(cbind(1, ns(newdat$age, knots=knots)) %*% sav0$par)
lines(newdat$age, pred0, col="lightblue", lwd=4)

# now setting lambda to a value that gives essentially the same result as the
# smoothing spline with df=16 (like res1) (note: the computation of the
# penalty term in fitfun() is really simplistic and the scaling of lambda is
# very different, but with some trial and error, we can find a lambda value
# here that gives essentially the same level of smoothing)
sav1 <- nlminb(sav0$par, fitfun, X=X, y=y, Xd=Xd, lambda=12^8)
pred1 <- c(cbind(1, ns(newdat$age, knots=knots)) %*% sav1$par)
lines(newdat$age, pred1, col="orange", lwd=4)

# smoothing spline with a higher level of smoothing (like res2)
sav2 <- nlminb(sav1$par, fitfun, X=X, y=y, Xd=Xd, lambda=18^8)
pred2 <- c(cbind(1, ns(newdat$age, knots=knots)) %*% sav2$par)
lines(newdat$age, pred2, col="lightgreen", lwd=4)

############################################################################

### 7.6: Local Regression

# plot the data
plot(wage ~ age, data=dat, pch=21, col="darkgray", bg="lightgray", cex=0.7,
     xlab="Age", ylab="Wage", bty="l")

# fit local regression model using a span of 0.2 (note: by default, loess()
# uses a quadratic polynomial for the local regression models, while the book
# describes using a linear model in Algorithm 7.1)
res1 <- loess(wage ~ age, data=dat, span=0.2)
res1

# add the smoother line to the plot
newdat <- data.frame(age=seq(18,80,length=1000))
pred1 <- predict(res1, newdata=newdat)
lines(newdat$age, pred1, col="red", lwd=8)

# increase the span to 0.7
res2 <- loess(wage ~ age, data=dat, span=0.7)
res2

# add the smoother line to the plot
pred2 <- predict(res2, newdata=newdat)
lines(newdat$age, pred2, col="blue", lwd=8)

# add a legend
legend("topright", inset=.02, lty="solid", col=c("red","blue"), lwd=5,
       legend=c("Span is 0.2 (16.4 Degrees of Freedom)",
                "Span is 0.7 (5.3 Degrees of Freedom)"), bg="white")

# try to obtain the smoother manually without using loess()

span <- 0.2

xs <- seq(18,80,length=100)
pred <- rep(NA, length(xs))

for (i in 1:length(xs)) {

   dist    <- abs(dat$age - xs[i])
   incl    <- dist <= quantile(dist, prob=span)
   weights <- rep(0, nrow(dat))
   weights[incl] <- (1 - (dist[incl] / max(dist[incl]))^3)^3
   tmp <- lm(wage ~ age + I(age^2), data=dat, weights=weights)
   pred[i] <- predict(tmp, newdata=data.frame(age = xs[i]))

}

lines(xs, pred, lwd=4, col="orange")

# this gets us quite close to the red line; the orange line seems a bit more
# wiggly, so there might be some additional implementation details that are
# not described in the book / help page for loess()

############################################################################

# local regression using unit weights (so all nearby points are weighted
# equally) and using a simple regression model with just an intercept

# plot the data
plot(wage ~ age, data=dat, pch=21, col="darkgray", bg="lightgray", cex=0.7,
     xlab="Age", ylab="Wage", bty="l")

span <- 0.2

xs <- seq(18,80,length=1000)
pred <- rep(NA, length(xs))

for (i in 1:length(xs)) {

   dist    <- abs(dat$age - xs[i])
   incl    <- dist <= quantile(dist, prob=span)
   weights <- rep(0, nrow(dat))
   weights[incl] <- 1
   tmp <- lm(wage ~ 1, data=dat, weights=weights)
   pred[i] <- predict(tmp, newdata=data.frame(age = xs[i]))

}

lines(xs, pred, lwd=8, col="darkgreen")

# function to do k-nearest neighbor for a single predictor x
knn <- function(x, y, x0, k) {
   dist <- (x - x0)^2
   mean(y[order(dist)[1:k]])
}

for (i in 1:length(xs)) {
   pred[i] <- knn(dat$age, dat$wage, x0=xs[i], k=nrow(dat)*span)
}

lines(xs, pred, lwd=4, col="green")

# so doing local 'regression' where we use unit weights and the regression
# model is a very simple one with just an intercept is essentially the same as
# doing k-nearest neighbor, where k = n * span; note that the way this is
# implemented above isn't exactly the same, because when we do the local
# regression, we might end up with a fraction of the total sample size that is
# slightly different than the desired span and in knn() we just take the first
# k nearest neighbors in the dataset even if there are more neighbors with the
# same smallest distance, but in principle these two approaches are the same
# in this special case

############################################################################

# 10-fold cross validation to choose the span value

spans <- seq(0.2, 1, length=100)

set.seed(1234)

split <- sample(rep_len(1:10, nrow(dat)))
pred  <- rep(NA, nrow(dat))

mse  <- rep(NA, length(spans))

for (i in 1:length(spans)) {

   for (j in 1:10) {

      res <- loess(wage ~ age, data=dat, span=spans[i], subset=split!=j)
      pred[split == j] <- predict(res, newdata=dat[split==j,])

   }

   mse[i] <- mean((dat$wage - pred)^2)

}

plot(spans, mse, type="o", pch=19, lwd=2, xlab="Value for span", ylab="MSE")

# the value of 'spans' that gives the lowest mse
span <- spans[which.min(mse)]
span

abline(v=span, lty="dotted")

# fit the local regression model with this value of span to all data
res <- loess(wage ~ age, data=dat, span=span)

# plot the data
plot(wage ~ age, data=dat, pch=21, col="darkgray", bg="lightgray", cex=0.7,
     xlab="Age", ylab="Wage", bty="l")

# add the line based on this model
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat)
lines(newdat$age, pred, col="red", lwd=8)

############################################################################

### 7.7: Generalized Additive Models

# 7.7.1: GAMs for Regression Problems

# install the gam package (if necessary)
#install.packages("gam")

# load the gam package
library(gam)

# fit GAM with natural cubic splines for year and age (with 4 and 5 degrees of
# freedom, respectively) and education as a factor
res1 <- lm(wage ~ ns(year, df=4) + ns(age, df=5) + education, data=dat)
summary(res1)

# plot the marginal relationships
par(mfrow=c(1,3))
plot.Gam(res1, se=TRUE, col="red", lwd=6, scale=70)

############################################################################

# it is not 100% clear to me how plot.Gam() does this (especially the scaling
# of the y-axis is strange); but we can do the same thing manually using for
# example the emmeans package

# install the emmeans package (if necessary)
#install.packages("emmeans")

# load the emmeans package
library(emmeans)

# predict wage for different values of year, holding age constant at its mean
# and giving weights to the different educational levels in accordance with
# the frequencies as observed in our data (which we can think of as holding
# education constant at the mean educational level of the sample)

newdat <- seq(min(dat$year), max(dat$year), length=1000)
pred <- emmeans(res1, specs = ~ year, at=list(year=newdat), weights="proportional")
pred <- summary(pred)

par(mfrow=c(1,3), mar=c(5,4.5,4,2))

plot(newdat, pred$emmean, type="l", lwd=4, col="red", xlab="year",
     ylab=expression(f[1](year)), ylim=c(90,150))
lines(newdat, pred$lower.CL, lty="dotted")
lines(newdat, pred$upper.CL, lty="dotted")

# do the same thing with age (holding year constant at its mean and holding
# education constant as described above)

newdat <- seq(min(dat$age), max(dat$age), length=1000)
pred <- emmeans(res1, specs = ~ age, at=list(age=newdat), weights="proportional")
pred <- summary(pred)

plot(newdat, pred$emmean, type="l", lwd=4, col="red", xlab="age",
     ylab=expression(f[2](age)), ylim=c(70,130))
lines(newdat, pred$lower.CL, lty="dotted")
lines(newdat, pred$upper.CL, lty="dotted")

# do the same thing with education (holding age and year constant at their means)

newdat <- levels(dat$education)
pred <- emmeans(res1, specs = ~ education, at=list(education=newdat), weights="proportional")
pred <- summary(pred)

plot(1:5, pred$emmean, pch=19, xlab="education", ylab=expression(f[3](education)),
     xlim=c(0.5,5.5), ylim=c(80,170))
arrows(1:5, pred$lower.CL, 1:5, pred$upper.CL, angle=90, code=3, length=0.1)

############################################################################

# fit GAM with natural smoothing splines for year and age (with 4 and 5
# degrees of freedom, respectively) and education as a factor
res2 <- gam(wage ~ s(year, df=4) + s(age, df=5) + education, data=dat)
summary(res2)

# plot the marginal relationships
par(mfrow=c(1,3))
plot(res2, se=TRUE, col="blue", lwd=6, scale=70)

############################################################################

# 7.7.2: GAMs for Classification Problems

# dichotomize wage into high wage (more than 250k) versus not
dat$highwage <- ifelse(dat$wage > 250, 1, 0)

# logistic regression model with year as a linear term, age as a smoothing
# spline (with 5 degrees of freedom), and education as a factor
res3 <- gam(highwage ~ year + s(age, df=5) + education, data=dat, family=binomial)
summary(res3)

# plot the marginal relationships
par(mfrow=c(1,3))
plot(res3, se=TRUE, col="darkgreen", lwd=6, scale=10)

# exclude the people with the lowest education level
res4 <- gam(highwage ~ year + s(age, df=5) + education, data=dat,
            family=binomial, subset=education!="1. < HS Grad")
summary(res4)

# plot the marginal relationships
par(mfrow=c(1,3))
plot(res4, se=TRUE, col="darkgreen", lwd=6, scale=10)

############################################################################

### 7.8 Lab: Non-linear Modeling

# most things skipped, because we have already covered many things while
# working through the chapter (and at times at an even deeper level)

# but let's do a GAM with a smoothing spline for year and a local regression
# smoother for age (and education as before as a factor)
res5 <- gam(wage ~ s(year, df=4) + lo(age, span=0.7) + education, data=dat)
summary(res5)

# plot the marginal relationships
par(mfrow=c(1,3))
plot(res5, se=TRUE, col="darkgreen", lwd=6, scale=70)

# GAM with a local regression surface for year and age together
res6 <- gam(wage ~ lo(year, age, span=0.5) + education, data=dat)
summary(res6)

# plot the marginal relationships
par(mfrow=c(1,2))
plot(res6, se=TRUE, scale=70)

############################################################################

### Extra: Non-Linear Regression (not covered in the book)

# https://en.wikipedia.org/wiki/Nonlinear_regression

# simulate some data from a Michaelis–Menten model

set.seed(1234)

n <- 250

x <- runif(n, 0, 50)
y <- 5 * x / (3 + x) + rnorm(n, 0, 0.2)

dat <- data.frame(x=x, y=y)
rm(x,y)

# plot the data
plot(y ~ x, data=dat, pch=19, col="darkgray", ylim=c(0,6), bty="l")

# try to capture the relationship between x and y using a quadratic polynomial
res <- lm(y ~ x + I(x^2), data=dat)
summary(res)

newdat <- data.frame(x = seq(0, 50, length=1000))
pred <- predict(res, newdata=newdat)
lines(newdat$x, pred, lwd=6, col="blue")

# try a cubic model
res <- lm(y ~ x + I(x^2) + I(x^3), data=dat)
summary(res)

pred <- predict(res, newdata=newdat)
lines(newdat$x, pred, lwd=6, col="red")

# how about a smoothing spline?
res <- smooth.spline(dat$x, dat$y, df=8)
lines(res, col="darkgreen", lwd=6)

# add a legend
legend("topleft", inset=.02, lty="solid", col=c("red","blue","darkgreen"), lwd=5,
       legend=c("Quadratic Polynomial", "Cubic Polynomial", "Smoothing Spline"))

# now fit a non-linear regression model where the functional form of the model
# matches the underlying true model
res <- nls(y ~ beta1 * x / (beta2 + x), start = list(beta1=2, beta2=1), data=dat)
summary(res)

pred <- predict(res, newdata=newdat)
plot(y ~ x, data=dat, pch=19, col="darkgray", ylim=c(0,6), bty="l")
lines(newdat$x, pred, lwd=6, col="blue")

# beta1 is the upper asymptote of y as x goes to infinity
# beta2 is the value of x where y is half way between 0 and beta1
abline(h=coef(res)[1], lty="dotted")
abline(v=coef(res)[2], lty="dotted")
abline(h=coef(res)[1]/2, lty="dotted")

############################################################################

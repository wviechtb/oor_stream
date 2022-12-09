############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-12-08
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 10.6 - 10.8
#
# last updated: 2022-12-09

############################################################################

### 10.6: When to Use Deep Learning

# this section again uses the Hitters dataset, which we already looked at in
# connection with section 10.1 (not using all features, but the principle
# remains the same), so we are going to skip replicating these results

############################################################################

### 10.7: Fitting a Neural Network

# this section is just a conceptual overview of how neural networks are fitted
# so there is nothing to replicate here (one could implement backpropagation /
# (stochastic) gradient descent manually, but let's not ...)

############################################################################

### 10.8: Interpolation and Double Descent

# simulate data as described on page 439
set.seed(1234) # set the seed for reproducibility
n <- 20
x <- runif(n, -5, 5)
y <- sin(x) + rnorm(n, 0, 0.3)

# plot the data
plot(x, y, pch=19)

# add the true model line
xs <- seq(-5, 5, length=10000)
lines(xs, sin(xs), lwd=3)

# signal to noise ratio for this model (how much variance is there in the true
# values relative to the amount of error variance)
round(var(sin(xs)) / 0.3^2, digits=1)

# clean up
rm(x,y)

# data extracted from Figure 10.21 (using the juicr package)
dat <- structure(list(x = c(-4.89, -4.59, -4.09, -2.55, -1.36, -1.44, -1.11,
0.74, 0.93, 0.65, 0.91, 1.61, 1.8, 2.1, 2.64, 3.78, 3.91, 3.73, 3.66, 4.62), y
= c(0.93, 1.2, 0.26, -0.43, -0.93, -1.43, -1.3, 1.09, 0.99, 0.3, 0.19, 1.07,
0.75, 0.53, 0.5, -0.04, -0.58, -0.59, -0.81, -1.13)), row.names = c(NA, -20L),
class = "data.frame")

# plot the data
par(mfrow=c(2,2))
plot(y ~ x, data=dat, lwd=3, cex=1.4, xlim=c(-5,5), ylim=c(-3,3), xlab="", ylab="")

# add the true model line
xs <- seq(-5, 5, length=10000)
lines(xs, sin(xs), lwd=3)

# load the splines package
library(splines)

# fit the natural cubic spline model with 8 degrees of freedom (note: one
# degree of freedom is used by the intercept term)
res <- lm(y ~ ns(x, df=7), data=dat)
summary(res)

# this is the same as letting ns() take care of the intercept
res <- lm(y ~ 0 + ns(x, df=8, intercept=TRUE), data=dat)
summary(res)

# the regression coefficients are not directly comparable from these two
# parameterizations, but the fitted values are identical (and hence the
# residual standard error is the same)

# determine the knot positions chosen (including the boundary knots)
knots <- attributes(ns(dat$x, df=8, intercept=TRUE))$knots
knots <- sort(c(knots, attributes(ns(dat$x, df=8))$Boundary.knots))
knots

# add the knot positions to the plot
abline(v=knots, lty="dashed", col="gray")

# add the regression line to the plot
newdat <- data.frame(x=seq(-5,5,length=1000))
pred <- predict(res, newdata=newdat)
lines(newdat$x, pred, col="orange", lwd=3)

# add the points again (so they are on top)
points(y ~ x, data=dat, lwd=3, cex=1.4)

# add title
title("8 Degrees of Freedom")

# plot the data again and add the true model
plot(y ~ x, data=dat, lwd=3, cex=1.4, xlim=c(-5,5), ylim=c(-3,3), xlab="", ylab="")
lines(xs, sin(xs), lwd=3)

# fit the natural cubic spline model with 20 degrees of freedom
res <- lm(y ~ 0 + ns(x, df=20, intercept=TRUE), data=dat)
summary(res)

# add the regression line to the plot
newdat <- data.frame(x=seq(-5,5,length=1000))
pred <- predict(res, newdata=newdat)
lines(newdat$x, pred, col="orange", lwd=3)

# add the points again (so they are on top)
points(y ~ x, data=dat, lwd=3, cex=1.4)

# add title
title("20 Degrees of Freedom")

# plot the data again and add the true model
plot(y ~ x, data=dat, lwd=3, cex=1.4, xlim=c(-5,5), ylim=c(-3,3), xlab="", ylab="")
lines(xs, sin(xs), lwd=3)

# fit the natural cubic spline model with 42 degrees of freedom
res <- lm(y ~ 0 + ns(x, df=42, intercept=TRUE), data=dat)
summary(res)

# can't do this with lm() because it removes redundant coefficients

# can use the singular value decomposition (SVD) to obtain the 'minimum norm
# solution' for this model
X <- ns(dat$x, df=42, intercept=TRUE)
svdX <- svd(X)
u <- svdX$u
d <- diag(svdX$d)
v <- svdX$v
b <- v %*% diag(1/svdX$d) %*% t(u) %*% dat$y
round(b, 6)

# add the regression line to the plot
knots   <- attributes(X)$knots
b.knots <- attributes(X)$Boundary.knots
Xs <- ns(xs, knots=knots, Boundary.knots=b.knots, intercept=TRUE)
pred <- Xs %*% b
lines(xs, pred, col="orange", lwd=3)

# add the points again (so they are on top)
points(y ~ x, data=dat, lwd=3, cex=1.4)

# add title
title("42 Degrees of Freedom")

# plot the data again and add the true model
plot(y ~ x, data=dat, lwd=3, cex=1.4, xlim=c(-5,5), ylim=c(-3,3), xlab="", ylab="")
lines(xs, sin(xs), lwd=3)

# fit the natural cubic spline model with 80 degrees of freedom
X <- ns(dat$x, df=80, intercept=TRUE)
svdX <- svd(X)
u <- svdX$u
d <- diag(svdX$d)
v <- svdX$v
b <- v %*% diag(1/svdX$d) %*% t(u) %*% dat$y

# add the regression line to the plot
knots   <- attributes(X)$knots
b.knots <- attributes(X)$Boundary.knots
Xs <- ns(xs, knots=knots, Boundary.knots=b.knots, intercept=TRUE)
pred <- Xs %*% b
lines(xs, pred, col="orange", lwd=3)

# add the points again (so they are on top)
points(y ~ x, data=dat, lwd=3, cex=1.4)

# add title
title("80 Degrees of Freedom")

############################################################################

### a little detour on obtainin the 'minimum norm solution'

# plot the data again and add the true model
par(mfrow=c(1,1))
plot(y ~ x, data=dat, lwd=3, cex=1.4, xlim=c(-5,5), ylim=c(-3,3), xlab="", ylab="")
lines(xs, sin(xs), lwd=3)

# let's go back to the case where we have df=42
X <- ns(dat$x, df=42, intercept=TRUE)

# get the minimum normal solution via svd()
svdX <- svd(X)
u <- svdX$u
d <- diag(svdX$d)
v <- svdX$v
b <- v %*% diag(1/svdX$d) %*% t(u) %*% dat$y

# add the regression line to the plot
knots   <- attributes(X)$knots
b.knots <- attributes(X)$Boundary.knots
Xs <- ns(xs, knots=knots, Boundary.knots=b.knots, intercept=TRUE)
pred <- Xs %*% b
lines(xs, pred, col="orange", lwd=2)

# tackle this via optimization

# fit function (computes the mean squared error)
fitfun <- function(par, X, y)
   mean(c(X %*% par - y)^2)

# fine the values of par (regression coefficients) that minimize the MSE
res <- optim(par=rep(0,ncol(X)), fitfun, X=X, y=dat$y, control=list(maxit=50000))
res

# add the regression line to the plot
pred <- Xs %*% res$par
lines(xs, pred, col="dodgerblue", lwd=2)

# this also fits the data perfectly (i.e., it interpolates through the points)
# but it is not the minimum norm solution
sum(b^2)
sum(res$par^2) # way bigger

# this is not the minimum normal solution, because we have not added the
# additional goal of minimizing sum(b^2) in the optimization

# interestingly, if we use the BFGS algorithm or the quasi-Newton method that
# is implemented in nlminb(), we do get the minimum normal solution
res.bfgs <- optim(par=rep(0,ncol(X)), fitfun, X=X, y=dat$y, method="BFGS")
res.bfgs
c(sum(b^2), sum(res.bfgs$par^2)) # same!
res.nlminb <- nlminb(start=rep(0,ncol(X)), fitfun, X=X, y=dat$y)
res.nlminb
c(sum(b^2), sum(res.nlminb$par^2)) # same!

# it is not clear to me whether this will always be the case; if we want to be
# sure, we should add the minimization of sum(b^2) to the optimization problem

# we can do this by framing the problem as a convex minimization problem

# install (if necessary) the CVXR package
#install.packages("CVXR")

# load the CVXR package
library(CVXR)

# set up the data and the problem and solve it
X <- matrix(X, nrow=20, ncol=ncol(X)) # turn X into a regular matrix
y <- dat$y
beta <- Variable(42)
objective <- norm2(beta)
constraints <- list(y - (X %*% beta) == 0)
problem <- Problem(Minimize(objective), constraints)
result <- solve(problem)

# compare the solutions (essentially identical)
round(data.frame(cvxr=result$getValue(beta), svd=b,
                 bfgs=res.bfgs$par, nlminb=res.nlminb$par), digits=6)

# but can we get the minimum norm solution if we fit the same model using
# gradient descent as implemented in tensorflow?

# to supress info and warning messages when loading tensorflow
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "2")

# load the keras package
library(keras)

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model (note: the default learning rate is low, so fitting this
# takes a long time; we can speed things up by increasing the learning rate)
keras$optimizers$SGD <- keras$optimizers$legacy$SGD
model <- keras_model_sequential() |>
   layer_dense(input_shape=ncol(X), units=1, use_bias=FALSE) |>
   compile(optimizer=optimizer_sgd(learning_rate=1), loss="mse")

# fit the model
res <- fit(model, X, y, epochs=500, verbose=FALSE)
res

# note: the MSE is essentially 0, but do we get the minimum norm solution?

# compare the solutions (the keras results are close but quite identical)
round(data.frame(cvxr=result$getValue(beta), svd=b,
                 bfgs=res.bfgs$par, nlminb=res.nlminb$par,
                 keras=get_weights(model)[[1]]), digits=6)

# compare the sum of the squred coefficients
sum(b^2)
sum(get_weights(model)[[1]]^2) # slightly larger

# add the regression line to the plot
pred <- Xs %*% get_weights(model)[[1]]
lines(xs, pred, col="forestgreen", lwd=2)

# the line is almost identical to the one from the minimum norm solution

# add the points again (so they are on top)
points(y ~ x, data=dat, lwd=3, cex=1.4)

# compare the coefficients (close but quite identical)
plot(1:42, b, type="o", pch=19, col="orange", cex=2, lwd=3)
lines(1:42, get_weights(model)[[1]], type="o", pch=19, col="forestgreen", lwd=3)

# maybe there is a way to tweak some of the settings further to obtain the
# minimum norm solution using this approach, but what we get is close enough

############################################################################

# replicate Figure 10.20

# simulate a large test dataset
n.test <- 100000
x.test <- runif(n.test, -5, 5)
y.test <- sin(x.test) + rnorm(n.test, 0, 0.3)

# vector with the degrees of freedom values
dfs <- seq(2, 74, by=2)

# vectors to save the training and test MSE
mse.train <- rep(NA_real_, length(dfs))
mse.test  <- rep(NA_real_, length(dfs))

pbar <- txtProgressBar(min=0, max=length(dfs), style=3)

for (i in 1:length(dfs)) {

   setTxtProgressBar(pbar, i)

   # obtain the regression coefficients for a given df
   X <- ns(dat$x, df=dfs[i], intercept=TRUE)
   svdX <- svd(X)
   u <- svdX$u
   d <- diag(svdX$d)
   v <- svdX$v
   b <- v %*% diag(1/svdX$d) %*% t(u) %*% dat$y

   # get the training MSE
   pred <- X %*% b
   mse.train[i] <- mean((dat$y - pred)^2)

   # get the test MSE
   knots   <- attributes(X)$knots
   b.knots <- attributes(X)$Boundary.knots
   X.test <- ns(x.test, knots=knots, Boundary.knots=b.knots, intercept=TRUE)
   pred <- X.test %*% b
   mse.test[i] <- mean((y.test - pred)^2)

}

close(pbar)

# plot the training and test MSE
plot(dfs, mse.train, type="b", pch=19, col="orange", lwd=2, ylim=c(0,2),
     log="x", xlab="Degrees of Freedom", ylab="Error (MSE)")
lines(dfs, mse.test, type="b", pch=19, col="dodgerblue", lwd=2)
legend("topright", inset=.02, lty=1, pch=19, lwd=2,
       col=c("orange","dodgerblue"), legend=c("Training Error", "Test Error"))

############################################################################

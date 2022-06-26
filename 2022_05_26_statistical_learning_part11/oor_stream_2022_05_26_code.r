############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-05-26
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 5.1 - 5.1.4
#
# last updated: 2022-05-28

############################################################################

### 5.1.1: The Validation Set Approach

# (install) and load the ISLR2 package
#install.packages("ISLR2")
library(ISLR2)

# copy the Auto dataset to dat
dat <- Auto

# examine the first rows of the dataset
head(dat)

# fit linear regression model predicting mpg from horsepower
res <- lm(mpg ~ horsepower, data=dat)
summary(res)

# fit quadratic polynomial regression model
res <- lm(mpg ~ horsepower + I(horsepower^2), data=dat)
summary(res)

# residuals = deviations between the fitted/predicted and actually observed
# values on the dependent variable
fitted(res) - dat$mpg
predict(res) - dat$mpg
resid(res)

# the mean of the squared residuals is the training data error rate
mean(resid(res)^2)

# split data into a training and a test (validation) dataset
set.seed(5679)
id <- sample(nrow(dat), round(nrow(dat)/2))
dat.train <- dat[id,]
dat.test  <- dat[-id,]

# fit quadratic polynomial regression model in the training dataset
res <- lm(mpg ~ horsepower + I(horsepower^2), data=dat.train)
summary(res)

# compute the training error rate
mean(resid(res)^2)

# predict the dependent variable based on the model in the test dataset
pred <- predict(res, newdata=dat.test)

# compute the test error rate
mean((pred - dat.test$mpg)^2)

############################################################################

# typically, the test error rate will be higher than the training error rate;
# this is not guaranteed, but if one were to repeat this many times, with
# different splits, then it should come out that way; let's try this out

mse.train <- rep(NA, 1000)
mse.test  <- rep(NA, 1000)

for (i in 1:1000) {

   id <- sample(nrow(dat), round(nrow(dat)/2))
   dat.train <- dat[id,]
   dat.test  <- dat[-id,]

   res <- lm(mpg ~ horsepower + I(horsepower^2), data=dat.train)
   mse.train[i] <- mean(resid(res)^2)

   pred <- predict(res, newdata=dat.test)
   mse.test[i] <- mean((pred - dat.test$mpg)^2)

}

mean(mse.train)
mean(mse.test)

# indeed, the test error rate is on average higher (although not by much)

############################################################################

# fit polynomial models of increasing complexity and compute the test error rate

degrees <- 1:10
mse.test <- rep(NA, length(degrees))

for (d in degrees) {

   res <- lm(mpg ~ poly(horsepower, degree=d), data=dat.train)
   pred <- predict(res, newdata=dat.test)
   mse.test[d] <- mean((pred - dat.test$mpg)^2)

}

# plot the test error rate against the degree of the polynomial (Figure 5.2, left)
plot(degrees, mse.test, type="o", col="red", pch=19, lwd=5, ylim=c(16,28),
     xlab="Degree of Polynomial", ylab="Mean Squared Error")

# note: this doesn't look exactly the same as Figure 5.1, since the random
# split we did is different than the one used by the authors

# repeat the above 10 times with new splits and add the lines to the existing
# plot (like Figure 5.2, right)

invisible(replicate(10, {

   id <- sample(nrow(dat), round(nrow(dat)/2))
   dat.train <- dat[id,]
   dat.test  <- dat[-id,]

   for (d in degrees) {
      res <- lm(mpg ~ poly(horsepower, degree=d, raw=TRUE), data=dat.train)
      pred <- predict(res, newdata=dat.test)
      mse.test[d] <- mean((pred - dat.test$mpg)^2)
   }

   lines(degrees, mse.test, col="gray", pch=19, lwd=3)

}))

# for some splits, the higher degree polynomial models lead to really erratic
# test error rates; not sure why this never happened for the authors

############################################################################

### 5.1.2: Leave-One-Out Cross-Validation

degrees <- 1:10
mse.test <- rep(0, length(degrees))

for (d in degrees) {

   print(d) # print d so we can see how this is progressing

   for (i in 1:nrow(dat)) {

      res <- lm(mpg ~ poly(horsepower, degree=d), data=dat, subset=-i)
      pred <- predict(res, newdata=dat[i,])
      mse.test[d] <- mse.test[d] + (pred - dat$mpg[i])^2

   }

}

mse.test <- mse.test / nrow(dat)
mse.test

# plot the test error rate against the degree of the polynomial (Figure 5.4, left)
plot(degrees, mse.test, type="o", col="blue", pch=19, lwd=5, ylim=c(16,28),
     xlab="Degree of Polynomial", ylab="Mean Squared Error")

# use the shortcut equation to accomplish the same thing

mse.test <- rep(NA, length(degrees))

for (d in degrees) {
   res <- lm(mpg ~ poly(horsepower, degree=d), data=dat)
   mse.test[d] <- sum((resid(res) / (1 - hatvalues(res)))^2)
}

mse.test <- mse.test / nrow(dat)
mse.test

############################################################################

# use LOOCV (i.e., jackknifing) to estimate the variability and bias in a
# regression coefficient (this is not covered in the book); see:
# https://en.wikipedia.org/wiki/Jackknife_resampling

# degree of the polynomial (below, we will examine the coefficient of the
# highest degree term in the polynomial regression model)
d <- 6

bvals <- rep(NA, nrow(dat))

for (i in 1:nrow(dat)) {

   res <- lm(mpg ~ poly(horsepower, degree=d), data=dat, subset=-i)
   bvals[i] <- coef(res)[d+1]

}

# distribution of the jackknife replicates of the regression coefficient
hist(bvals, breaks=25)

# the mean thereof is the 'jackknife estimate'
mean(bvals)

# jackknife estimate of the variance of the regression coefficient
varj <- (nrow(dat) - 1) / nrow(dat) * sum((bvals - mean(bvals))^2)
varj

# this gives us an estimate of the standard error of the regression coefficient
sqrt(varj)

# the standard error we get from fitting the model to all data is computed
# based on an equation we can derive from statistical theory
res <- lm(mpg ~ poly(horsepower, degree=d), data=dat)
summary(res)

# estimate the bias in the regression coefficient
(nrow(dat) - 1) * (mean(bvals) - coef(res)[d+1])

# correct for this bias
nrow(dat) * coef(res)[d+1] - (nrow(dat) - 1) * mean(bvals)

############################################################################

### 5.1.3: k-Fold Cross-Validation

set.seed(1234)

degrees <- 1:10
k <- 10

# plot the test error rate against the degree of the polynomial (Figure 5.4, right)
plot(NA, xlim=c(1,10), ylim=c(16,28),
     xlab="Degree of Polynomial", ylab="Mean Squared Error")

invisible(replicate(10, {

   mse.test <- rep(0, length(degrees))
   grp <- sample(1:k, nrow(dat), replace=TRUE)

   for (d in degrees) {

      for (i in 1:k) {
         res <- lm(mpg ~ poly(horsepower, degree=d), data=dat, subset=grp!=i)
         pred <- predict(res, newdata=dat[grp==i,])
         mse.test[d] <- mse.test[d] + mean((pred - dat$mpg[grp==i])^2)
      }

      mse.test[d] <- mse.test[d] / k

   }

   lines(degrees, mse.test, col="gray", pch=19, lwd=5)

}))

############################################################################

# we cannot easily reproduce Figure 5.6, since we do not know what the true
# model is that underlies the simulated data; instead, we will simulate data
# from a quadratic polynomial regression model and then we can obtain the true
# test error rate (by simulating a large test dataset from the same model) and
# we can examine how well LOOCV and k-fold CV approximate this error rate

set.seed(1234)

# number of subjects in the training dataset
n <- 100

# simulate training data
x <- runif(n, 0, 10)
y <- 2 + 0.8 * x - 0.1 * x^2 + rnorm(n, 0, 0.5)

# plot the training data
plot(x, y, pch=19)

# add the line from the true model to this plot
xs <- seq(0, 10, length=1000)
ys <- 2 + 0.8 * xs - 0.1 * xs^2
lines(xs, ys, lwd=5, col="red")

# simulate a very large test dataset
xnew <- runif(10^6, 0, 10)
ynew <- 2 + 0.8 * xnew - 0.1 * xnew^2 + rnorm(10^6, 0, 0.5)
newdat <- data.frame(x = xnew)

# compute the test error rate for this very large test dataset for polynomial
# models of degree 1 to 10

degrees <- 1:10
mse.test.true <- rep(0, length(degrees))

for (d in degrees) {

   res <- lm(y ~ poly(x, degree=d))
   pred <- predict(res, newdata=newdat)
   mse.test.true[d] <- mean((pred - ynew)^2)

}

# plot the true test error rate against the degree of the polynomial
plot(degrees, mse.test.true, type="o", col="blue", pch=19, lwd=5, ylim=c(0.1,1),
     xlab="Degree of Polynomial", ylab="Mean Squared Error")

# now we will do LOOCV

mse.test.loocv <- rep(0, length(degrees))

for (d in degrees) {

   for (i in 1:n) {

      res <- lm(y ~ poly(x, degree=d), subset=-i)
      pred <- predict(res, newdata=data.frame(x=x[i]))
      mse.test.loocv[d] <- mse.test.loocv[d] + (pred - y[i])^2

   }

}

mse.test.loocv <- mse.test.loocv / n

# plot the test error rate from LOOCV against the degree of the polynomial
lines(degrees, mse.test.loocv, lty="dashed", lwd=5)

# now do 10-fold CV

k <- 10

mse.test.kfold <- rep(0, length(degrees))
grp <- sample(1:k, n, replace=TRUE)

for (d in degrees) {

   for (i in 1:k) {
      res <- lm(y ~ poly(x, degree=d), subset=grp!=i)
      pred <- predict(res, newdata=data.frame(x=x[grp==i]))
      mse.test.kfold[d] <- mse.test.kfold[d] + mean((pred - y[grp==i])^2)
   }

   mse.test.kfold[d] <- mse.test.kfold[d] / k

}

# plot the test error rate from 10-fold CV against the degree of the polynomial
lines(degrees, mse.test.kfold, col="orange", lwd=5)

# show for which degree the test error rate from LOOCV and 10-fold CV is minimized
points(which.min(mse.test.loocv), min(mse.test.loocv), pch=4, cex=4)
points(which.min(mse.test.kfold), min(mse.test.kfold), pch=4, col="orange", cex=4)

# so both LOOCV and 10-fold CV tend to slightly underestimate the true test
# error rate except for the high degree polynomial models, where they
# overestimate the true test error rate, at times quite severely; however, for
# the most part, they both yield similar estimates of the true test error rate
# and suggest that a cubic polynomial model yields the lowest test error rate
# (which is slightly too complex, since we know that the true model is
# actually a cubic polynomial)

# we can see a little better what is going on by 'zooming in' on the error
# rates around 0.22 to 0.28

plot(degrees, mse.test.true, type="o", col="blue", pch=19, lwd=5, ylim=c(0.22,0.28),
     xlab="Degree of Polynomial", ylab="Mean Squared Error")
lines(degrees, mse.test.loocv, lty="dashed", lwd=5)
lines(degrees, mse.test.kfold, col="orange", lwd=5)

############################################################################

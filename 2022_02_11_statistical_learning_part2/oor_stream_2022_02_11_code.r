############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-02-11
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 2.2.2 - 2.2.3
#
# last updated: 2022-02-12

############################################################################

# note: the code below is not based on examples in the book but was created to
# illustrate certain concepts that were introduced in the sections covered

############################################################################

# the following code is based on the concepts discussed in section 2.2.2; in
# particular, the code below illustrates equation 2.7

# see also: https://en.wikipedia.org/wiki/Bias%E2%80%93variance_tradeoff

# make results reproducible by setting the seed for the random number generator
set.seed(1234)

# number of iterations (number of training datsets we will simulate)
iters <- 10000

# simulate data based on the true model for a large number of subjects (these
# will the test data, which we will make use of further below)
n_new <- 1000
x0 <- rnorm(n_new)
y0 <- 2 + 1 * x0 + 0.3 * x0^2 + rnorm(n_new)
yt <- 2 + 1 * x0 + 0.3 * x0^2
newdat <- data.frame(x = x0)

# set up matrices to store the predicted values based on three different models
pred1 <- matrix(NA, nrow=iters, ncol=n_new)
pred2 <- matrix(NA, nrow=iters, ncol=n_new)
pred3 <- matrix(NA, nrow=iters, ncol=n_new)

# sample size for the train data
n <- 100

for (i in 1:iters) {

   # simulate a training dataset based on the true model
   # y = 2 + 1 * x + 0.3 * x^2 + e, where e ~ N(0,1)
   x <- rnorm(n)
   y <- 2 + 1 * x + 0.3 * x^2 + rnorm(n)

   # fit a simple linear regression model (this is f-hat)
   res1 <- lm(y ~ x)

   # compute the predicted values for the test data based on model res1
   pred1[i,] <- predict(res1, newdata=newdat)

   # fit a quadratic polynomial regression model (this is another f-hat)
   res2 <- lm(y ~ x + I(x^2))

   # compute the predicted values for the test data based on model res2
   pred2[i,] <- predict(res2, newdata=newdat)

   # fit a cubic polynomial regression model (this is another f-hat)
   res3 <- lm(y ~ x + I(x^2) + I(x^3))

   # compute the predicted values for the test data based on model res3
   pred3[i,] <- predict(res3, newdata=newdat)

}

# for a single column in pred1, we can compute the variance of the predicted
# values; this is what is denoted as Var(f-hat(x_0)) in equation 2.7 and
# indicates how much the predicted value for a single new subject in the test
# dataset would vary if we estimated it using different training datasets
var(pred1[,1])

# we can do this for *all* columns in pred1 and take the average of these
# variances; this then tells us how large this variance is on average across
# all test subjects
mean(apply(pred1, 2, var))

# let's do the same thing for the predicted values in pred2 and pred3
mean(apply(pred2, 2, var))
mean(apply(pred3, 2, var))

# note how the mean variance is lowest for pred1, which is the simplest model;
# on the other hand, the mean variance is highest for pred3, since it is based
# on the cubic model, which is unnecessary complex (the true model is actually
# a quadratic model)

# the second term in equation 2.7 is for the squared bias, that is the squared
# mean difference between the predicted value and the true value for a single
# subject in the test dataset
mean(pred1[,1] - yt[1])^2

# again, we can compute the squared bias for all subjects in the test dataset
# and take the average; and again, let's do this for all three models
mean((apply(pred1, 2, mean) - yt)^2)
mean((apply(pred2, 2, mean) - yt)^2)
mean((apply(pred3, 2, mean) - yt)^2)

# note that the simple linear regression model has on average the highest
# squared bias, since it is an overly simplistic model; on the other hand, the
# quadratic and cubic models have no bias and hence the squared bias is
# essentially 0 for both of these models

# the last term in equation 2.7 is the variance of the errors, which we know
# to be 1, since the errors were simulated from a standard normal distribution

# so, we can put all three terms together, giving the expected train MSE for a
# single new subject in the test dataset for all three models
var(pred1[,1]) + mean(pred1[,1] - yt[1])^2 + 1
var(pred2[,1]) + mean(pred2[,1] - yt[1])^2 + 1
var(pred3[,1]) + mean(pred3[,1] - yt[1])^2 + 1

# as noted in the book (below eq. 2.7), the "overall expected test MSE" would
# then be the average of such values across all subjects in the test dataset;
# so let's compute this for all three models
mean(apply(pred1, 2, var) + (apply(pred1, 2, mean) - yt)^2 + 1)
mean(apply(pred2, 2, var) + (apply(pred2, 2, mean) - yt)^2 + 1)
mean(apply(pred3, 2, var) + (apply(pred3, 2, mean) - yt)^2 + 1)

# note that the simple linear regression model, which has the lowest variance
# but the highest squared bias, has the highest value above; on the other
# hand, the cubic model is overly complex, so while it has essentially no
# bias, it has a higher variance

############################################################################

# the following code is based on the concepts discussed in section 2.2.3

# make results reproducible by setting the seed for the random number generator
set.seed(1234)

# sample size for simulated data
n <- 200

# simulate two predictors x1 and x2 from uniform distributions
x1 <- runif(n)
x2 <- runif(n)

# compute the conditional probability of falling into class = 1 as a function
# of x1 and x2 based on the true model
p <- plogis(-6 + 40 * x1*x2 - 70 * (x1*x2)^3)

# use random values from a binomial distribution to assign subjects to class =
# 1 or class = 0 with probabilities as computed above
y <- rbinom(n, 1, p)

# scatterplot of x1 versus x2 with points colored based on class membership
plot(x1, x2, col=ifelse(y == 1, "orange", "blue"), pch=19)

# the Bayes classifiers assigns subjects to classes according to whatever
# class has the highest probability; in essence, if the probability of being
# in class = 1 is higher than 0.5 for a particular subject, then it assigns
# the subject to class = 1 and otherwise to class = 0; since we know the true
# probabilities, we can apply the Bayes classifier to the observed data
pred.bayes <- ifelse(p > 0.5, 1, 0)

# cross-classification of the true (y) versus the assigned class (pred.bayes)
table(y, pred.bayes)

# proportion of subjects for which the Bayes classifier is wrong
mean(y != pred.bayes)

# since we know the true model, we can determine the group assignment based on
# the Bayes classifier for all values of x1 and x2 between 0 and 1 on a grid
len <- 101
x1s <- seq(0, 1, length=len)
x2s <- seq(0, 1, length=len)

pred.bayes <- matrix(NA, nrow=len, ncol=len)

for (i in 1:len) {
   for (j in 1:len) {
      pred.bayes[i,j] <- plogis(-6 + 40 * x1s[i]*x2s[j] - 70 * (x1s[i]*x2s[j])^3) > 0.5
   }
}

# show the assignment to classes based on the Bayes classifier (in light
# orange/blue) and superimpose the actual data
image(pred.bayes, col=c(rgb(0,0,1,.2), rgb(1,.65,0,.2)), xlab="x1", ylab="x2")
points(x1, x2, col=ifelse(y == 1, "orange", "blue"), pch=19)

# simulate a large number of new data based on the true model
n_new <- 10000
x01 <- runif(n_new)
x02 <- runif(n_new)
p0  <- plogis(-6 + 40 * x01*x02 - 70 * (x01*x02)^3)
y0  <- rbinom(n_new, 1, p0)

# apply the Bayes classifier and examine the cross-classification
pred.bayes <- ifelse(p0 > 0.5, 1, 0)
table(y0, pred.bayes)

# proportion of subjects for which the Bayes classifier is wrong
mean(y0 != pred.bayes)

# the above is essentially the Bayes error rate, but as described in the book,
# if we know the true probabilities, then we can compute the Bayes error rate
# with equation 2.11
1 - mean(pmax(p0, 1-p0))

# note that the two values for the Bayes error rate are not exactly identical,
# but they would be if n_new goes to infinity

# now let's see how well the k-nearest neighbor classifier does; we can write
# the knn classifier as a function as follows (x1, x2, and y are for passing
# the training data to the function, x01 and x02 for a single test datapoint)
knn <- function(x1, x2, y, x01, x02, k) {
   dist <- (x1 - x01)^2 + (x2 - x02)^2
   ifelse(mean(y[order(dist)[1:k]]) > 0.5, 1, 0)
}

# try out the classifier for some (x01,x02) values
knn(x1, x2, y, .2, .4, k=5)
knn(x1, x2, y, .6, .8, k=5)

# now let's try it out for all subjects in the test dataset
pred.knn <- rep(NA, n_new)

for (i in 1:n_new) {
   pred.knn[i] <- knn(x1, x2, y, x01[i], x02[i], k=5)
}

# cross-classification of the true (y0) versus the assigned class (pred.knn)
table(y0, pred.knn)

# proportion of subjects for which the knn classifier is wrong
mean(y0 != pred.knn)

# note that this is higher than the error rate for the Bayes classifier

# as above, we can determine the class assignment based on a fine grid of x1
# and x2 values based on the knn classifier
len <- 101
x1s <- seq(0, 1, length=len)
x2s <- seq(0, 1, length=len)

pred.knn <- matrix(NA, nrow=len, ncol=len)

for (i in 1:len) {
   for (j in 1:len) {
      pred.knn[i,j] <- knn(x1, x2, y, x1s[i], x2s[j], k=5)
   }
}

# show the assignment to classes based on the knn classifier (in light
# orange/blue) and superimpose the actual data
image(pred.knn, col=c(rgb(0,0,1,.2), rgb(1,.65,0,.2)), xlab="x1", ylab="x2")
points(x1, x2, col=ifelse(y == 1, "orange", "blue"), pch=19)

# looks similar to the Bayes classifier, but is less smooth

# now let's try out different values of k

ks <- c(1:15, seq(20, 100, by=5))

error.train <- rep(NA, length(ks))
error.test  <- rep(NA, length(ks))

for (j in 1:length(ks)) {

   pred.knn <- rep(NA, n)
   for (i in 1:n) {
      pred.knn[i] <- knn(x1, x2, y, x1[i], x2[i], k=ks[j])
   }
   error.train[j] <- mean(y != pred.knn)

   pred.knn <- rep(NA, n_new)
   for (i in 1:n_new) {
      pred.knn[i] <- knn(x1, x2, y, x01[i], x02[i], k=ks[j])
   }
   error.test[j] <- mean(y0 != pred.knn)

}

# plot the train and test errors as a function of 1 / k (so increasing values
# on the x-axis reflect increasing flexibility of the method)
plot(1/ks, error.train, lwd=3, type="l", col="blue",
     xlab="1 / k (flexibility)", ylab="Error Rate")
lines(1/ks, error.test, lwd=3, col="red")
legend("bottomleft", inset=.01, legend=c("train error", "test error"),
       col=c("blue", "red"), lty="solid", lwd=3)

# add the Bayes error rate as a dashed horizontal line
abline(h = 1 - mean(pmax(p0, 1-p0)), lty="dashed")

# again we see the typical pattern, namely a decreasing train error rate as we
# increase the flexibility of the method, but this does not hold for the test
# error rate, which initially decreases but then increases again

############################################################################

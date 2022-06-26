############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-02-03
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 1 - 2.2.1
#
# last updated: 2022-02-12

############################################################################

# note: the code below is not based on examples in the book but was created to
# illustrate certain concepts that were introduced in the sections covered

############################################################################

# illustrate the idea of training and test MSE and the concept of overfitting

# make results reproducible by setting the seed for the random number generator
set.seed(42)

# sample size
n <- 250

# simulate data for a simple regression model
# true model: y = 2 + 1*x + e, where e ~ N(0,1)
x <- rnorm(n)
y <- 2 + 1 * x + rnorm(n)

# plot the data
plot(x, y, pch=19)

# fit simple regression model
res1 <- lm(y ~ x)
summary(res1)

# note: the fitted model ('f-hat') has the right functional form, but is not
# exactly identical to the true model ('f'); note that the estimated intercept
# and slope differ somewhat from the true values (true intercept = 2, true
# slope = 1); this discrepancy is 'reducible error' because the larger n, the
# closer the estimated coefficients will tend to be to the true coefficients

# add regression line to the scatterplot
abline(res1, lwd=3)

# mean squared error (eq. 2.5) for the training data
1/n * sum((y - predict(res1))^2)

# simulate a 100 additional predictors that are in reality unrelated to y
Z <- replicate(100, rnorm(n))

# fit a regression model including x and the irrelevant predictors
res2 <- lm(y ~ x + Z)
summary(res2)

# mean squared error for this model
1/n * sum((y - predict(res2))^2)

# note that the MSE is quite a bit lower then the one for model res1 above;
# however, what we have computed is the 'train MSE'; to assess how well these
# models work with new data, we have to compute the 'test MSE'

# simulate a large set of new data based on the true model
n_new <- 100000
x_new <- rnorm(n_new)
y_new <- 2 + 1 * x_new + rnorm(n_new)
Z_new <- replicate(100, rnorm(n_new))

# compute predicted outcomes based on models res1 and res2
pred1 <- cbind(1, x_new) %*% coef(res1)
pred2 <- cbind(1, x_new, Z_new) %*% coef(res2)

# compute the 'test MSE' for the two models
1/n_new * sum((y_new - pred1)^2)
1/n_new * sum((y_new - pred2)^2)

# note that the 'test MSE' is lower for model res1; in essence, model res2
# overfits the data, because it includes a very large number of irrelevant
# predictors, so even though it gives a better fit for the training data, this
# won't be the case for new data

############################################################################

# another concept discussed in these sections is the difference between a
# parametric model and a nonparametric model; a regression model for example
# is a parametric model

# plot the data again
plot(x, y, pch=19)

# add regression line from model res1 to the scatterplot
abline(res1, lwd=3, col="green")

# methods such as smoothers can be considered to be nonparametric methods (one
# can debate this point, since certain types of smoothers may still be based
# on parametric models that are fitted in succession to various parts of the
# data, but let's not quibble about this point)

# fit a smoother to the data
res3 <- loess(y ~ x)

# compute predicted values based on the smoother and to add the corresponding
# prediction line to the plot
x_pred <- data.frame(x=seq(-3,3,length=1000))
pred_smooth <- predict(res3, newdata=x_pred)
lines(x_pred$x, pred_smooth, lwd=3, col="red")

# as discussed in the book, when we are dealing with nonparametric methods, we
# often have to be careful not to overfit; to illustrate this, we can allow
# the smoother to be much more flexible, but now we are overfitting the data
res4 <- loess(y ~ x, span=0.2)
pred_smooth <- predict(res4, newdata=x_pred)
lines(x_pred$x, pred_smooth, lwd=3, col="blue")

############################################################################

# another illustration of how we can make the 'train MSE' essentially
# arbitrarily small by allowing the smoother to be very flexible, but this
# will lead to very poor performance in new data (i.e., the test MSE will be
# very high); so the goal will be to find the right level of smoothing, so
# that the test MSE will be as low as possible

# make results reproducible by setting the seed for the random number generator
set.seed(1234)

# simulate data based on a more complicated model
n <- 100
x <- c(0, 5, runif(n-2, 0, 5))
y <- 2 - 1 * x + 0.5 * x^2 - 0.07 * x^3 + rnorm(n, 0, .1)
plot(x, y, pch=19)

# illustrate 3 different levels of smoothing
res1 <- loess(y ~ x, span=0.1)
res2 <- loess(y ~ x, span=0.5)
res3 <- loess(y ~ x, span=2)

x_pred <- data.frame(x=seq(0,5,length=1000))
pred1 <- predict(res1, newdata=x_pred)
pred2 <- predict(res2, newdata=x_pred)
pred3 <- predict(res3, newdata=x_pred)
lines(x_pred$x, pred1, lwd=3, col="red")
lines(x_pred$x, pred2, lwd=3, col="blue")
lines(x_pred$x, pred3, lwd=3, col="green")

# simulate a very large number of new data based on the same model
n_new <- 1000000
x_new <- runif(n_new, 0, 5)
y_new <- 2 - 1 * x_new + 0.5 * x_new^2 - 0.07 * x_new^3 + rnorm(n_new, 0, .1)
x_new <- data.frame(x=x_new)

# fit smoother with increasing values of span (i.e., increasing smoothness)
# and compute the train and test MSE for each value/model
spans <- seq(0.1, 2, length=100)
mse.train <- rep(NA, length(spans))
mse.test  <- rep(NA, length(spans))

for (i in 1:length(spans)) {
   tmp <- loess(y ~ x, span=spans[i])
   mse.train[i] <- 1/n * sum((y - predict(tmp))^2)
   mse.test[i]  <- 1/n_new * sum((y_new - predict(tmp, newdata=x_new))^2)
}

# plot the train and test MSEs as a function of 1 / span (so increasing values
# on the x-axis reflect increasing flexibility of the method)
plot(1/spans, mse.train, lwd=3, type="l", xlab="1 / Span (flexibility)",
     ylab="MSE", ylim=range(c(mse.train, mse.test)))
lines(1/spans, mse.test, lwd=3, type="l", col="red")
legend("topright", inset=.01, legend=c("test MSE", "train MSE"),
       col=c("red", "black"), lty="solid", lwd=3)

# note how the train MSE keeps decreasing for increasing values of 1 / span;
# however, such high flexibility will lead to a large test MSE; for span
# values around 0.5 (and hence 1 / span =~ 2), the test MSE is minimized; in
# practice, we do not know what the true model is and so we cannot compute the
# test MSE in this manner; later on, we will discuss how we can try to
# estimate the test MSE using techniques such as cross-validation

############################################################################

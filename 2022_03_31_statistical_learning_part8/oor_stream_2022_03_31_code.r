############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-03-31
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 4.3.5 - 4.4.2
#
# last updated: 2022-04-01

############################################################################

# some notes about the previous stream:
#
# 1) Last time, we compared linear regression with k-nearest neighbors (KNN).
#    When applying KNN to predict the outcome value for an observation, we
#    examine the (Euclidean) distance between the observation and those in the
#    training dataset and find the k nearest observations (whose mean is then
#    used as the predicted value for the new observation). When computing the
#    distances, we first might want to rescale the predictor variables to a
#    common scale (e.g., z-scores), so that a one-unit distance is comparable
#    across all predictors. In the example we used last time, the predictors
#    all used the same units (one point = $1000 in advertising budget), so
#    this might not have been so relevant in the example, but it is still
#    worth noting here in case we apply KNN in other examples.
#
# 2) When we discussed linear regression, we covered the omnibus test of the
#    null hypothesis that all slope coefficients are equal to 0 (i.e., that
#    none of the predictors are related to the outcome variable). The book did
#    not discuss such a test for logistic regression, but this is of course
#    also possible in this context. An example:

# load the ISLR2 package
library(ISLR2)

# copy the Default dataset to dat
dat <- Default

# include balance, income, and students as predictors in the model (Table 4.3)
res <- glm(default ~ balance + income + student, data=dat, family=binomial)
summary(res)

# fit the 'null model' that includes no predictors
res0 <- glm(default ~ 1, data=dat, family=binomial)

# omnibus test by comparing the two models against each other
anova(res0, res, test="Chisq")

# the p-value is tiny here, so we reject the null hypothesis:
#
# H0: beta_balance = beta_income = beta_studentYes = 0
#
# and hence conclude that there does appear to be some kind of relationship
# between one or more of these predictors and the outcome

############################################################################

### 4.3.5: Multinomial Logistic Regression

# the book does not provide a concrete example for multinomial logistic
# regression that we can replicate (since the focus in the book is on
# classification for two groups/classes); for those interested in an example,
# see here: https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/

############################################################################

### 4.4: Generative Models for Classification

# an illustration of how things can go wrong when there is 'perfect
# separation' in the data and we fit a logistic regression model

set.seed(1234)
n <- 100
x <- runif(n, 0, 1)
y <- ifelse(x > 0.7, 1, 0)
plot(x, y, pch=19)

# note: everybody with an x value > 0.7 is in group y = 1 and 0 otherwise;
# hence, we can perfectly predict y from x; let's fit a logistic regression
# model with these data
res <- glm(y ~ x, family=binomial)
summary(res)

# note the large coefficients for x and also the corresponding huge SE

# consider an even a simpler case where the predictor is dichotomous (x is
# either 0 or 1); then the logistic model is given by this:
#
# for x = 0: log(p(Y = 1 | X = 0) / p(Y = 0 | X = 0)) = beta0
# for x = 1: log(p(Y = 1 | X = 1) / p(Y = 0 | X = 1)) = beta0 + beta1
#
# now say the data are like this (again, perfect separation):
#
#        y = 1   y = 0
# x = 1  n11     0
# x = 0  0       n00
#
# then the model should give:
#
# for x = 0: log(0 / 1) = -Inf, so beta0 should be -Inf
# for x = 1: log(1 / 0) = +Inf, so beta0 + beta1 should be +Inf
# so beta1 must be +Inf, to 'offset' the -Inf of beta0
#
# when we fit a model to data with perfect separation, the estimates of beta0
# and beta1 are not -Inf and +Inf (since the model fitting algorithm doesn't
# allow this), but the coefficients get really large

# in practice, we typically do not have perfect separation, but we might have
# something quite close to it; say the data are like in the example above
# (where everybody with x > 0.7 has y = 1 and 0 otherwise), except for two
# individuals; let repeatedly simulate such data, fit the logistic regression
# model, and save the estimated slope for x

iters <- 1000
coefs <- rep(NA, iters)

for (i in 1:iters) {

   x <- runif(n, 0, 1)
   y <- ifelse(x > 0.7, 1, 0)
   y[1:2] <- 1 - y[1:2] # flip y for the first two individuals
   res <- glm(y ~ x, family=binomial)
   coefs[i] <- coef(res)[2]

}

# histogram of the estimated slopes
hist(coefs, breaks=50, xlab="Slope Coefficient", main="")

# note how unstable the estimated slopes are!

# now let's do the same thing, but we will flip y for 25 individuals (so that
# the relationship between x and y is not so strong anymore)

for (i in 1:iters) {

   x <- runif(n, 0, 1)
   y <- ifelse(x > 0.7, 1, 0)
   y[1:25] <- 1 - y[1:25]
   res <- glm(y ~ x, family=binomial)
   coefs[i] <- coef(res)[2]

}

hist(coefs, breaks=50, xlab="Slope Coefficient", main="")

# the estimated slopes still fluctuate, but not nearly as much

# so, somewhat surprisingly, the closer we are in being able to predict y
# perfectly from the predictor(s) in logistic regression, the more unstable
# the estimated slopes become (which would make the model perform poorly in
# new/test data)

############################################################################

# now let's consider the approach for classification described on page 142;
# say there are two classes (k = 0 and k = 1), with n0 and n1 individuals in
# these two groups and there is a single predictor, x, that we have measured
# in these individuals; we can simulate such data as follows

set.seed(1234)

n1 <- 30000
n0 <- 10000
x_y1 <- rnorm(n1, mean=5, sd=1)
x_y0 <- rnorm(n0, mean=4, sd=1)

# note: while x is simulated from a normal distribution within the two groups
# here, this is (for the purposes of this example) NOT an assumption

# priors for y=0 and y=1
pi1 <- n1 / (n1 + n0)
pi0 <- n0 / (n1 + n0)

# estimate the densities of x within the two classes using 'kernel density
# estimation': https://en.wikipedia.org/wiki/Kernel_density_estimation
f1x <- density(x_y1)
f0x <- density(x_y0)

# plot the two distributions
plot(f1x, lwd=5, col="blue", xlim=c(0,10), main="")
lines(f0x, lwd=5, col="red")

# say we want to know f1x and f0x when x = 6
abline(v=6, lwd=3, lty="dotted")

# so we need to determine the height of the blue and red densities when x = 6;
# we will do this by finding the value of f1x and f0x for the closest x to 6
# based on the estimated densities
f1x_equal_to_6 <- f1x$y[which.min(abs(f1x$x - 6))]
f0x_equal_to_6 <- f0x$y[which.min(abs(f0x$x - 6))]
f1x_equal_to_6
f0x_equal_to_6
segments(-1, f1x_equal_to_6, 6, f1x_equal_to_6, lwd=3, lty="dotted", col="blue")
segments(-1, f0x_equal_to_6, 6, f0x_equal_to_6, lwd=3, lty="dotted", col="red")

# posterior probabilities of a person belonging to the blue and red classes when x = 6
pi1 * f1x_equal_to_6 / (pi1 * f1x_equal_to_6 + pi0 * f0x_equal_to_6)
pi0 * f0x_equal_to_6 / (pi1 * f1x_equal_to_6 + pi0 * f0x_equal_to_6)

# this illustrates the use of Bayes' theorem (equation 4.15) for this example

# hence, the estimated probability that a person comes from the blue class
# when x = 6 is around 93% and around 7% for the red class; so according to
# the Bayes' classifier, we would assign an observation where x = 6 to the
# blue class, since this has the highest posterior probability

############################################################################

# 4.4.1: Linear Discriminant Analysis for p = 1

# we will replicate the example in the book (note that we cannot simulate the
# exact same data, but we can simulate data just like those in the book)

# so, there are again two classes, now with 20 people in each class

set.seed(1234)

n1 <- 20
n0 <- 20

x_y1 <- rnorm(n1, mean= 1.25, sd=1)
x_y0 <- rnorm(n0, mean=-1.25, sd=1)
x <- c(x_y1, x_y0)
y <- c(rep(1, n1), rep(0, n0))

# in practice, the dataset would look like this
data.frame(y, x)

# histogram of the data in the two groups (like Figure 4.4, right panel)
hist(x_y1, xlim=c(-4,4), ylim=c(0,8), breaks=seq(-5,5,by=0.5),
     xlab="", main="", col=rgb(.8,0,1,.2))
par(new=TRUE)
hist(x_y0, xlim=c(-4,4), ylim=c(0,8), breaks=seq(-5,5,by=0.5),
     xlab="", main="", col=rgb(0,.6,0,.2), axes=FALSE, ylab="")

# compute the means of x within the two groups and the pooled variance
m1 <- mean(x_y1)
m0 <- mean(x_y0)
s2p <- ((n1-1)*var(x_y1) + (n0-1)*var(x_y0)) / (n1 + n0 - 2)

# add the LDA decision boundary to the histogram
abline(v = (m1 + m0) / 2, lwd=5)

# add the Bayes decision boundary to the histogram
abline(v = 0, lty="dashed", lwd=5)

# LDA classifier
pred.lda <- ifelse(c(x_y1, x_y0) > (m1 + m0) / 2, 1, 0)

# Bayes classifier
pred.bayes <- ifelse(c(x_y1, x_y0) > 0, 1, 0)

# show data with these classifiers
data.frame(y, x, pred.lda, pred.bayes)

# error rates for LDA and the Bayes classifier
mean(y != pred.lda)
mean(y != pred.bayes)

############################################################################

# but the above gives us the performance of LDA and the Bayes classifier in
# the training data; more important is the performance in new/test data

# for this, we will repeat the above many times and in each iteration of the
# simulation, we will also simulate new/test data and compute the error rate
# for these test data

iters <- 10000

n1_test <- 100
n0_test <- 100

error.rate.lda   <- rep(NA, iters)
error.rate.bayes <- rep(NA, iters)

for (i in 1:iters) {

   x_y1 <- rnorm(n1, mean= 1.25, sd=1)
   x_y0 <- rnorm(n0, mean=-1.25, sd=1)

   m1 <- mean(x_y1)
   m0 <- mean(x_y0)

   y_test <- c(rep(1, n1_test), rep(0, n0_test))
   x_y1_test <- rnorm(n1_test, mean= 1.25, sd=1)
   x_y0_test <- rnorm(n0_test, mean=-1.25, sd=1)

   pred.lda   <- ifelse(c(x_y1_test, x_y0_test) > (m1 + m0) / 2, 1, 0)
   pred.bayes <- ifelse(c(x_y1_test, x_y0_test) > 0, 1, 0)

   error.rate.lda[i]   <- mean(y_test != pred.lda)
   error.rate.bayes[i] <- mean(y_test != pred.bayes)

}

# average test data error rate of LDA and the Bayes classifier
round(100 * mean(error.rate.lda), 1)
round(100 * mean(error.rate.bayes), 1)

############################################################################

### 4.4.2: Linear Discriminant Analysis for p > 1

# install the ISLR2 package
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# copy the Default dataset to dat
dat <- Default

# load MASS package
library(MASS)

# look at the documentation of the lda() function
help(lda)

# do LDA using balance and student as predictors for 'default' (yes/no)
res <- lda(default ~ balance + student, data=dat)
res

# get predictions for the training data
pred <- predict(res)

# confusion matrix (Table 4.4)
addmargins(table(pred$class, dat$default), margin=c(1,2))

# error rate of LDA in the training data
100 * mean(pred$class != dat$default)

# sensitivity
table((pred$class == dat$default)[dat$default == "Yes"])
round(100 * mean((pred$class == dat$default)[dat$default == "Yes"]), 1)

# specificity
table((pred$class == dat$default)[dat$default == "No"])
round(100 * mean((pred$class == dat$default)[dat$default == "No"]), 1)

# use 0.2 as the threshold for assigning people to the default = "yes" class
pred$class <- ifelse(pred$posterior[,"Yes"] > 0.2, "Yes", "No")

# confusion matrix (Table 4.5)
addmargins(table(pred$class, dat$default), margin=c(1,2))

# error rate of LDA in the training data
100 * mean(pred$class != dat$default)

# sensitivity and specificity
round(100 * mean((pred$class == dat$default)[dat$default == "Yes"]), 1)
round(100 * mean((pred$class == dat$default)[dat$default == "No"]), 1)

############################################################################

# compute the error rate, sensitivity, and specificity for 1000 different
# threshold values between 0 and 1

thresholds <- seq(0, 1, length=1000)

error.rate  <- rep(NA, length(thresholds))
sensitivity <- rep(NA, length(thresholds))
specificity <- rep(NA, length(thresholds))

for (i in 1:length(thresholds)) {

   pred$class <- ifelse(pred$posterior[,"Yes"] > thresholds[i], "Yes", "No")
   error.rate[i]  <- mean(pred$class != dat$default)
   sensitivity[i] <- mean((pred$class == dat$default)[dat$default == "Yes"])
   specificity[i] <- mean((pred$class == dat$default)[dat$default == "No"])

}

# plot the overall and the false negative and false positive error rates as a
# function of the threshold values (Figure 4.7)
plot(thresholds, error.rate, type="l", lwd=5, ylim=c(0,.8), xlim=c(.001,.55))
lines(thresholds, 1 - sensitivity, lwd=5, col="blue")
lines(thresholds, 1 - specificity, lwd=5, col="orange")
legend("right", inset=.02, lty="solid", col=c("blue", "black", "orange"), lwd=5,
       legend = c("false negative rate", "overall error rate", "false positive rate"))

# ROC curve (Figure 4.8)
plot(1 - specificity, sensitivity, type="l", lwd=3, col="blue", xlim=c(0,1),
     ylim=c(0,1), xlab="False positive rate", ylab="True positive rate")
abline(a = 0, b = 1, lty="dotted")

############################################################################

# typically we would not want to draw the ROC curve manually; we can use the
# pROC package to automate all of this

# install and load package pROC
#install.packages(pROC)
library(pROC)

# for completeness sake, apply LDA again and get the posterior probabilities
res <- lda(default ~ balance + student, data=dat)
pred <- predict(res)

# use the roc() function, draw the plot, and get the area under the curve
sav <- roc(default ~ pred$posterior[,"Yes"], data=dat)
plot(sav, lwd=3)
auc(sav)

# compare LDA with logistic regression
res <- glm(default ~ balance + student, data=dat, family=binomial)
pred <- predict(res, type="response")
sav <- roc(default ~ pred, data=dat)
plot(sav, lwd=3, add=TRUE, col="red")
auc(sav)

# performance of LDA and logistic regression is almost identical for these data

############################################################################

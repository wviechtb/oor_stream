############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-06-02
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 5.1.5 - 5.4
#
# last updated: 2022-06-06

############################################################################

### 5.1.5: Cross-Validation on Classification Problems

# the dataset used in this section is not available; therefore, we will use
# the 'Default' dataset to try out some cross-validation methods in the
# context of a classification setting

# (install) and load the ISLR2 package
#install.packages("ISLR2")
library(ISLR2)

# copy the Default dataset to dat
dat <- Default

# examine the first 6 rows of the dataset
head(dat)

# turn default into a logical variable
dat$default <- dat$default == "Yes"

# frequency table of the outcome variable (default)
table(dat$default)

# note: defaulting is a relatively rare outcome (only a little over 3% of the
# customers defaulted on their credit card debt)

# fit logistic regression model predicting defaulting from variables balance,
# income, and student
res <- glm(default ~ balance + income + student, data=dat, family=binomial)
summary(res)

# predict whether a customer will default based on the model (i.e., whether
# the estimated probability of defaulting is larger than 0.5) and examine the
# confusion matrix
pred.train <- predict(res, type="response") > 0.5
table(pred.train, dat$default)

# compute the training error rate
errorrate.train <- mean(pred.train != dat$default)
errorrate.train

# do leave-one-out cross validation (LOOCV) to estimate the test error rate
# (this will take a few minutes since 10,000 models need to be fitted, so use
# a progress bar to indicate how things are progressing)

pred.loocv <- rep(NA, nrow(dat))

pb <- txtProgressBar(max=nrow(dat), style=3)

for (i in 1:nrow(dat)) {

   setTxtProgressBar(pb, value=i)

   res <- glm(default ~ balance + income + student, data=dat, family=binomial, subset=-i)
   pred.loocv[i] <- predict(res, newdata=dat[i,], type="response") > 0.5

}

close(pb)

# confusion matrix and LOOCV test error rate
table(pred.loocv, dat$default)
mean(pred.loocv != dat$default)

# note: this is identical to the training error rate; since n=10,000, the
# models are fitted with 9,999 data points each and therefore are essentially
# all the same as the model fitted to all 10,000 data points; as a result, the
# training error rate and the LOOCV test error rate are in this case identical

# do k-fold cross-validation (CV) to estimate the test error rate (with k=10)

set.seed(1234)

k <- 10

grp <- sample(1:k, nrow(dat), replace=TRUE)

pred.kfoldcv <- rep(NA, nrow(dat))

for (i in 1:k) {

   res <- glm(default ~ balance + income + student, data=dat, family=binomial, subset=grp!=i)
   pred.kfoldcv[grp==i] <- predict(res, newdata=dat[grp==i,], type="response") > 0.5

}

# confusion matrix and k-fold CV test error rate
table(pred.kfoldcv, dat$default)
mean(pred.kfoldcv != dat$default)

# again, the test error rate is the same as the training error rate, although
# the confusion matrix is slightly different; here, the models are fitted with
# ~9000 data points each and therefore are not going to differ all that much
# from each other again

# now fit (polynomial) models with increasing complexity and estimate the test
# error rate using 10-fold cross-validation for each model

k <- 10
degrees <- 1:6

errorrates.poly <- rep(0, length(degrees))
grp <- sample(1:k, nrow(dat), replace=TRUE)

for (d in degrees) {

   for (i in 1:k) {
      res <- glm(default ~ poly(balance, degree=d) + poly(income, degree=d) + student, data=dat, family=binomial, subset=grp!=i)
      pred <- predict(res, newdata=dat[grp==i,], type="response") > 0.5
      errorrates.poly[d] <- errorrates.poly[d] + sum(pred != dat$default[grp==i])
   }

}

errorrates.poly <- errorrates.poly / nrow(dat)
errorrates.poly

# plot the test error rate as a function of the polynomial degree
plot(degrees, errorrates.poly, type="o", pch=19, ylim=c(.025,.035),
     xlab="Degree of Polynomial", ylab="Error Rate")

# add the line for the error rate we get if we predict that nobody defaults
# (since defaulting is rare, doing so gives us a pretty low error rate)
abline(h=mean(dat$default), lty="dotted")

# add the line for the training error rate
abline(h=errorrate.train, lty="dashed")

# conclusion: no matter how high of a degree polynomial model we use, the test
# error rate pretty much stays flat; so a model without any higher degree
# polynomial terms seems perfectly fine for these data

############################################################################

# (install) and load the class package (to do k-nearest neighbor)
#install.packages("class")
library(class)

# rescale the numeric variables into a 0-1 range and turn the student factor
# variable into a 0/1 variable (so that when we use knn below, all predictors
# have a comparable range)
dat$balance <- (dat$balance - min(dat$balance)) / (max(dat$balance) - min(dat$balance))
dat$income  <- (dat$income -  min(dat$income))  / (max(dat$income)  - min(dat$income))
dat$student <- ifelse(dat$student == "Yes", 1, 0)

# split the data into a training and test dataset
id <- sample(nrow(dat), round(nrow(dat)/2))
dat.train <- dat[ id,]
dat.test  <- dat[-id,]
predvars <- c("balance", "income", "student")

# knn with k=5 and compute the error rate in the test data
set.seed(51358)
pred <- knn(dat.train[predvars], dat.test[predvars], dat.train$default, k=5)
mean(pred != dat.test$default)

# knn with increasing values of k and compute the test error rate using k-fold
# CV with k=10 (careful with the different meanings of k here)
ks.for.knn <- c(1:15, seq(20, 50, by=5), seq(60, 100, by=10))
k.for.cv <- 10

errorrates.knn <- rep(0, length(ks.for.knn))
grp <- sample(1:k.for.cv, nrow(dat), replace=TRUE)

for (j in 1:length(ks.for.knn)) {

   for (i in 1:k.for.cv) {
      pred <- knn(dat[grp!=i,predvars], dat[grp==i,predvars], dat$default[grp!=i], k=ks.for.knn[j])
      errorrates.knn[j] <- errorrates.knn[j] + sum(pred != dat$default[grp==i])
   }

}

errorrates.knn <- errorrates.knn / nrow(dat)
errorrates.knn

# plot the test error rate as a function of 1/k (flexibility) (use a log scale
# for the x-axis to make it easier to distinguish points where 1/k is low)
plot(1/ks.for.knn, errorrates.knn, type="o", pch=19, ylim=c(.025,.045),
     xlab="1/k (flexibility)", ylab="Error Rate", log="x")

# add the line for the error rate we get if we predict that nobody defaults
abline(h=mean(dat$default), lty="dotted")

# add the line for the training error rate
abline(h=errorrate.train, lty="dashed")

# find the value of k (in knn) which minimizes the test error rate
ks.for.knn[which.min(errorrates.knn)]

# conclusion: now we see the 'classic' U-shape in the plot; we minimize the
# test error rate for a value of around k = 25 (1/k = 0.04) for knn, but the
# performance is slightly worse in these data when compared to the logistic
# regression models we examined earlier

############################################################################

# while the data used in this section isn't available via the ISLR2 package
# (or some other place as far as I know), we can extract the data with some
# effort from Figure 2.13 (using packages like digitize, metaDigitise, or
# juicr); I did this, yielding the following dataset

dat <- structure(list(x1 = c(7.59, 6.67, 5.72, 5.12, 2.96, 2.25, 2.6, 2.35,
2.23, 3.15, 3.54, 3.2, 3.1, 3.32, 6.03, 5.81, 5.02, 4.37, 3.54, 4.49, 4.5,
5.16, 5.37, 5.63, 5.74, 4.89, 5.33, 5.36, 5.76, 4.44, 4.87, 4.78, 5.24, 5.87,
5.5, 4.94, 4.8, 4.65, 4.75, 5.19, 5.24, 5.42, 5.47, 5.71, 5.62, 5.79, 6.09,
6.29, 5.62, 5.89, 6.53, 6.66, 6.78, 6.67, 6.4, 6.51, 6.76, 6.93, 6.54, 6.32,
6.42, 6.6, 6.76, 7.01, 7.63, 7.28, 8.21, 8.54, 9.32, 8.97, 8.13, 8.01, 8.72,
8.88, 5.05, 5.26, 5.38, 5.4, 5.91, 5.97, 6.13, 6.33, 6.39, 6.94, 6.97, 6.08,
5.97, 5.63, 5.67, 5.96, 6.27, 6.21, 6.13, 5.6, 5.59, 5.69, 5.67, 5.34, 6.14,
5.99, 3.9, 4.77, 4.91, 5.88, 4.1, 3.06, 2.73, 3.91, 3.96, 4.85, 2.25, 1.84,
3.3, 4.13, 3.52, 1.34, 1.17, 0.64, 1.84, 2.42, 3.74, 3.79, 3.73, 1.41, 1.23,
1.24, 2.11, 1.99, 2.24, 1.93, 1.37, 1.71, 1.61, 1.51, 1.86, 1.45, 2.27, 2.4,
2.46, 3.04, 3.66, 4.2, 4.22, 4.27, 2.78, 2.27, 2.44, 2.38, 2.39, 2.66, 2.93,
3.09, 3.62, 2.77, 3.09, 3.48, 4.25, 4.69, 4.9, 5.83, 6.11, 6.55, 6, 5.59,
5.52, 5.39, 3.04, 3.53, 3.5, 3.57, 3.35, 3.34, 6.69, 4.3, 3.95, 4, 3.55, 3.84,
3.9, 3.88, 4.08, 4.37, 4.34, 4.04, 4, 4.11, 4.32, 4.18, 4.04, 4.36, 4.95,
5.47, 6.14, 5.94, 5.53, 4.7, 4.96, 4.89, 5.5, 5.5), x2 = c(2.65, 2.37, 1.62,
1.75, 1.82, 2.23, 2.62, 2.84, 3.25, 2.83, 3.34, 3.57, 3.83, 3.8, 7.98, 7.66,
8.17, 8.06, 6.6, 7.54, 7.08, 7.36, 7.37, 7.41, 7.06, 6.81, 6.77, 6.57, 6.61,
6.15, 6.14, 6.06, 5.93, 6.14, 5.86, 5.54, 5.22, 5.01, 4.61, 5.41, 5.24, 5.47,
5.57, 5.57, 5.44, 5.4, 5.5, 6.07, 5.04, 4.92, 5.77, 5.75, 5.75, 5.63, 5.04,
4.93, 5.07, 4.83, 4.63, 4.42, 4.13, 4.18, 4.28, 4.07, 4.34, 6.02, 5.63, 6.26,
6.38, 6.92, 7.16, 7.28, 7.68, 7.63, 3.72, 3.75, 3.74, 3.83, 3.82, 3.71, 3.71,
3.5, 3.32, 3.58, 3.27, 3.44, 3.42, 3.52, 3.13, 3.18, 2.9, 2.73, 2.76, 2.94,
2.84, 2.83, 2.7, 2.75, 2.48, 2.41, 9.26, 9.21, 9.03, 8.79, 9.09, 9.06, 8.97,
8.84, 8.67, 8.51, 8.55, 8.14, 8.49, 8.56, 8.42, 7.75, 7.62, 7.26, 7.63, 7.85,
8.06, 7.93, 7.81, 6.88, 6.77, 6.66, 7.54, 7.33, 7.07, 6.66, 6.25, 6.28, 6.17,
5.93, 5.91, 5.38, 5.12, 5.12, 5.4, 4.45, 4.32, 4.21, 4.23, 4.55, 5, 6.03,
6.03, 5.83, 5.75, 5.66, 5.58, 5.79, 4.72, 7.21, 7.26, 7.6, 7.96, 8.09, 8.23,
8.3, 7.97, 7.8, 7.78, 7.83, 7.81, 7.64, 6.59, 6.87, 6.9, 6.97, 6.91, 7.12,
6.75, 7.16, 7.05, 6.65, 6.45, 6.3, 6.01, 5.89, 5.83, 5.92, 5.65, 5.57, 5.46,
5.23, 5.07, 4.98, 4.82, 4.75, 4.63, 4.48, 4.38, 4, 3.8, 3.89, 3.39, 3.27,
2.86, 3.26), grp = c("blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange", "orange", "orange", "orange",
"orange", "orange", "orange", "orange")), row.names = c(NA, -200L), class =
"data.frame")

# recreate Figure 2.13
symbols(dat$x1, dat$x2, circles=rep(.08,nrow(dat)), inches=FALSE, fg=dat$grp,
        lwd=4, xlim=c(0,10), ylim=c(0,10), xlab="x1", ylab="x2")

# since the process of extracting data from figures isn't 100% accurate, there
# might be some minor discrepancies, but that looks pretty much the same

# note: we still do not know what the true model is that generated the data,
# so we will not be able to compute the true test error rates as was done in
# the book, but we should be able to reproduce (at least quite closely) the
# estimated error rates using the given data

# turn grp into a logical variable
dat$grp <- dat$grp == "orange"

# fit polynomial models with increasing complexity and compute the training
# error rate

degrees <- 1:10
errorrates.training <- rep(0, length(degrees))

for (d in degrees) {

   res <- glm(grp ~ poly(x1, degree=d) + poly(x2, degree=d), data=dat, family=binomial)
   pred <- predict(res, type="response") > 0.5
   errorrates.training[d] <- mean(pred != dat$grp)

}

# plot the training error rate as a function of the polynomial degree
plot(degrees, errorrates.training, type="l", lwd=7, col="#30b5ff",
     ylim=c(.11,.21), xlab="Degree of Polynomial", ylab="Error Rate")

# note: the line doesn't look exactly like the one in Figure 5.8 (left panel)
# but it's quite close (the discrepancy must be due to slight deviations in
# the extracted data)

# fit polynomial models with increasing complexity and estimate the test error
# rate using k-fold cross-validation (with k=10)

set.seed(261)

k <- 10
degrees <- 1:10

errorrates.poly <- rep(0, length(degrees))
grp <- sample(rep(1:k, each=nrow(dat)/k))

for (d in degrees) {

   for (i in 1:k) {
      res <- glm(grp ~ poly(x1, degree=d) + poly(x2, degree=d), data=dat, family=binomial, subset=grp!=i)
      pred <- predict(res, newdata=dat[grp==i,], type="response") > 0.5
      errorrates.poly[d] <- errorrates.poly[d] + sum(pred != dat$grp[grp==i])
   }

}

errorrates.poly <- errorrates.poly / nrow(dat)
errorrates.poly

# add the test error rates to the plot
lines(degrees, errorrates.poly, lwd=7)

# note: I tried out many seeds, but couldn't find one where the black line
# shows the U-shape as in Figure 5.8, but at least we see how the error rates
# from CV are higher than the training error rates

# knn with increasing values of k and compute the training error rate
set.seed(544)
ks.for.knn <- 1:100
errorrates.training <- rep(0, length(ks.for.knn))
predvars <- c("x1", "x2")

for (j in 1:length(ks.for.knn)) {
   pred <- knn(dat[predvars], dat[predvars], dat$grp, k=ks.for.knn[j])
   errorrates.training[j] <- mean(pred != dat$grp)
}

# smooth the error rates a bit
errorrates.training <- predict(loess(errorrates.training ~ 1/ks.for.knn, span=0.35))

# plot the training error rate as a function of 1/k (flexibility) (use a log
# scale for the x-axis to make it easier to distinguish points)
plot(1/ks.for.knn, errorrates.training, type="l", lwd=7, col="#30b5ff",
     ylim=c(.11,.21), xlab="1/k (flexibility)", ylab="Error Rate", log="x")

# knn with increasing values of k and compute the test error rate using k-fold
# CV with k=10 (careful with the different meanings of k here)
k.for.cv <- 10

errorrates.knn <- rep(0, length(ks.for.knn))
grp <- sample(1:k.for.cv, nrow(dat), replace=TRUE)

for (j in 1:length(ks.for.knn)) {

   for (i in 1:k.for.cv) {
      pred <- knn(dat[grp!=i,predvars], dat[grp==i,predvars], dat$grp[grp!=i], k=ks.for.knn[j])
      errorrates.knn[j] <- errorrates.knn[j] + sum(pred != dat$grp[grp==i])
   }

}

errorrates.knn <- errorrates.knn / nrow(dat)

# smooth the error rates a bit
errorrates.knn <- predict(loess(errorrates.knn ~ 1/ks.for.knn, span=0.35))

# add the test error rates to the plot
lines(1/ks.for.knn, errorrates.knn, lwd=7)

# find the value of k (in knn) which minimizes the test error rate
ks.for.knn[which.min(errorrates.knn)]

# this looks similar to Figure 5.8 (right panel), although the error rates
# need to be smoothed a bit as otherwise they look much more jagged

############################################################################

### 5.2: The Bootstrap

# let's see if we can derive equation 5.6
#
# Var(aX + (1-a)Y) = a^2Var(X) + (1-a)^2Var(Y) + 2a(1-a)Cov(X,Y)
#                  = a^2 sigma^2_X + (1-a)^2 sigma^2_Y + 2a(1-a) sigma_XY = Var(return)
#
# take the derivative of this with respect to a, which yields:
#
# d Var(return) / d a = 2 a sigma^2_X - 2 (1-a) sigma^2_Y + (2-4a) sigma_XY
#                     = 2 a sigma^2_X - 2 sigma^2_Y + 2 a sigma^2_Y + 2 sigma_XY - 4 a sigma_XY
#                     = a (2 sigma^2_X + 2 sigma^2_Y - 4 sigma_XY) - 2 sigma^2_Y + 2 sigma_XY
#
# set the derivative equal to 0 and solve for a:
#
# a = (2 sigma^2_Y - 2 sigma_XY) / (2 sigma^2_X + 2 sigma^2_Y - 4 sigma_XY)
#   = (sigma^2_Y - sigma_XY) / (sigma^2_X + sigma^2_Y - 2 sigma_XY)

# simulate data like those in Figure 5.9

library(MASS)

set.seed(1234)

n <- 100
mu <- c(0,0)
Sigma <- matrix(c(1,0.5,0.5,1.25), nrow=2, ncol=2)

compalpha <- function(x, y)
   (var(y) - cov(x,y)) / (var(x) + var(y) - 2*cov(x,y))

# Figure 5.9

par(mfrow=c(2,2))

xy <- mvrnorm(n, mu=mu, Sigma=Sigma)
plot(xy[,1], xy[,2], pch=19, col="seagreen4", xlab="X", ylab="Y")
compalpha(xy[,1], xy[,2])

xy <- mvrnorm(n, mu=mu, Sigma=Sigma)
plot(xy[,1], xy[,2], pch=19, col="seagreen4", xlab="X", ylab="Y")
compalpha(xy[,1], xy[,2])

xy <- mvrnorm(n, mu=mu, Sigma=Sigma)
plot(xy[,1], xy[,2], pch=19, col="seagreen4", xlab="X", ylab="Y")
compalpha(xy[,1], xy[,2])

xy <- mvrnorm(n, mu=mu, Sigma=Sigma)
plot(xy[,1], xy[,2], pch=19, col="seagreen4", xlab="X", ylab="Y")
compalpha(xy[,1], xy[,2])

# estimate alpha for 1000 simulated datasets

alphas <- replicate(1000, {
   xy <- mvrnorm(n, mu=mu, Sigma=Sigma)
   compalpha(xy[,1], xy[,2])
})

par(mfrow=c(1,3))

# histogram of the estimated alpha values
hist(alphas, main="", col="#dba123")
abline(v=0.6, lwd=5)

# mean and SD of the alpha values
mean(alphas)
sd(alphas)

############################################################################

# generate a dataset that we will use for the bootstrapping
xy <- mvrnorm(n, mu=mu, Sigma=Sigma)

# compute alpha in the sample
compalpha(xy[,1], xy[,2])

# number of bootstrap samples
B <- 1000

alphas.boot <- double(B)

for (i in 1:B) {
   xy.boot <- xy[sample(n, replace=TRUE),]
   alphas.boot[i] <- compalpha(xy.boot[,1], xy.boot[,2])
}

# histogram of the bootstrap estimates of alpha
hist(alphas.boot, main="", col="#30b5ff")
abline(v=0.6, lwd=5)

# mean and SD of the bootstrap estimates
mean(alphas.boot)
sd(alphas.boot)

# boxplot
boxplot(list(True=alphas, Bootstrap=alphas.boot), col=c("#dba123","#30b5ff"))
abline(h=0.6, lwd=5)

############################################################################

# do bootstrapping using the boot package

# load the boot package
library(boot)

compalpha.boot <- function(dat, indices) {
   dat.boot <- dat[indices,]
   x <- dat.boot[,1]
   y <- dat.boot[,2]
   (var(y) - cov(x,y)) / (var(x) + var(y) - 2*cov(x,y))
}

res <- boot(xy, statistic=compalpha.boot, R=1000)
res

# get bootstrap CIs
boot.ci(res, type=c("norm","basic","perc","bca"))

# note: the percentile method is very similar to just taking the 2.5th and
# 97.5th quantiles (for a 95% CI) of the bootstrap estimates
quantile(res$t[,1], c(.025, .975))

############################################################################

### 5.3 Lab: Cross-Validation and the Bootstrap

# skipped, because we pretty much covered everything already

# in the lab, the cv.glm() function (from the boot package) is used to do
# LOOCV and k-fold CV based on the regression models fitted with glm(); this
# is of course very convenient, but this will not work in combination with
# knn() from the class package; using the caret package, one can apparently do
# CV in combination with knn:
#
# https://stats.stackexchange.com/questions/318968/knn-and-k-folding-in-r
#
# above, we simply programmed LOOCV and k-fold CV manually, which we can then
# easily apply to essentially any model/method for making predictions

# a note on page 218: the authors claim that "the standard formulas assume
# (somewhat unrealistically) that the xi are fixed" (when computing the
# standard errors of the regression coefficients); this isn't right; there are
# indeed two types of regression models, one where the predictors are fixed
# and one where the predictors are observed (i.e., random) variables; however,
# inferences are made in the same way under both models

############################################################################

### 5.4: Exercises

# skipped in the interest of time

############################################################################

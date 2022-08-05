############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-08-04
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 6.5.1 - 6.5.3
#
# last updated: 2022-08-05

############################################################################

### 6.5.1: Subset Selection Methods

# install (if needed) and load the ISLR2 package
#install.packages("ISLR2")
library(ISLR2)

# copy the Hitters dataset to dat
dat <- Hitters

# examine the dataset in a spreadsheet-like view
View(dat)

# get the names of the variables in the dataset
names(dat)

# get the dimensions of the dataset (number of rows and columns)
dim(dat)

# number of players with missing salary value
sum(is.na(dat$Salary))

# examine some summary statistics for all variables
summary(dat)

# remove all rows where at least one variable is missing
dat <- na.omit(dat)

# recheck dimensions and the number of players with missing salary value
dim(dat)
sum(is.na(dat$Salary))

############################################################################

# install (if needed) and load the leaps package
#install.packages("leaps")
library(leaps)

############################################################################

# Best Subset Selection

# best subset selection predicting salary from all other variables in the
# dataset (going up to the model that includes all 19 predictors)
res <- regsubsets(Salary ~ ., data=dat, nvmax=19)

# note: could also set nvmax=Inf to automatically go up to the model that
# includes all predictors, regardless of how many there are

# note: this considers all possible models of which there are
2^19

# examine the results
summary(res)

# the output shows what the 'best' model is (i.e., which predictors are
# included) for each model complexity (i.e., number of predictors included);
# 'best' here essentially means the model with the highest R^2 value for a
# given level of complexity

# examine the R^2 value for all 'best' models for each level of complexity (so
# the first value is the R^2 for the model that is best among all models with
# a single predictor, the second value is the R^2 for the model that is best
# among all models with two predictors, and so on)
summary(res)$rsq

# note: the R^2 values will always go up with model complexity, so these are
# not useful for selecting the overall 'best' model; instead, we can use
# statistics like adjusted R^2, Mallow's Cp, and the BIC to compare the
# various 'best' models to find the one that is best overall

# examine the adjusted R^2 values for these models
summary(res)$adjr2

# copy the RSS, adjusted R^2, Cp, and BIC values into vectors
res.rss   <- summary(res)$rss
res.adjr2 <- summary(res)$adjr2
res.cp    <- summary(res)$cp
res.bic   <- summary(res)$bic

# largest adjusted R^2 and its position
max(res.adjr2)
which.max(res.adjr2)

# so according to the adjusted R^2, the model with 11 predictors is the best
# overall model

# get the regression coefficients for this model
coef(res, id=11)

# set up a 2x2 plot and then plot the RSS, adjusted R^2, Cp, and BIC values
# for all 'best' models for the different levels of complexity

par(mfrow=c(2,2))

plot(res.rss,   xlab ="Number of Variables", ylab="RSS",          type="o", pch=19, xlim=c(0,20))
plot(res.adjr2, xlab ="Number of Variables", ylab="Adjusted RSq", type="o", pch=19, xlim=c(0,20))
points(which.max(res.adjr2), max(res.adjr2), col="red", cex=2, pch=19)
plot(res.cp,    xlab ="Number of Variables", ylab="Cp",           type="o", pch=19, xlim=c(0,20))
points(which.min(res.cp), min(res.cp), col="red", cex=2, pch=19)
plot(res.bic,   xlab ="Number of Variables", ylab="BIC",          type="o", pch=19, xlim=c(0,20))
points(which.min(res.bic), min(res.bic), col="red", cex=2, pch=19)

# note: RSS is also not useful for selecting the overall best model because it
# will always go down with increasing complexity

# plot which variables are included at each level of complexity and the
# corresponding R^2, adjusted R^2, Cp, and BIC values
par(mfrow=c(2,2))
plot(res, scale="r2")
plot(res, scale="adjr2")
plot(res, scale="Cp")
plot(res, scale="bic")
par(mfrow=c(1,1))

# we see that, according to the BIC, the model with 6 predictors is the
# overall best model; get the regression coefficients for this model
coef(res, id=6)

# get the full regression table for this model
X.best <- model.matrix(Salary ~ ., data=dat)[,names(coef(res, id=6))][,-1]
summary(lm(Salary ~ X.best, data=dat))

# note: the standard errors (and hence test statistics and p-values) given
# above do not properly reflect the fact that data were used in fitting this
# model that were also used in selecting this model from many other models

############################################################################

# Forward and Backward Stepwise Selection

# forward stepwise selection
res <- regsubsets(Salary ~ ., data=dat, nvmax=19, method="forward")
summary(res)

# which model complexity is best overall according to the BIC
which.min(summary(res)$bic)

# get the corresponding regression coefficients
coef(res, id=6)

# backward stepwise selection
res <- regsubsets(Salary ~ ., data=dat, nvmax=19, method="backward")
summary(res)

# which model complexity is best overall according to the BIC
which.min(summary(res)$bic)

# get the corresponding regression coefficients
coef(res, id=8)

# note: an exhaustive search, forward selection, and backward selection are
# not guaranteed to lead to the same overall best model

############################################################################

# Choosing Among Models Using the Validation-Set Approach and Cross-Validation

# for full reproducibility, set the seed of the random number generator
set.seed(1)

# create variables to indicate which players are in the training/test datasets
train <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE)
test  <- !train

# number of people in the training (TRUE) and the test (FALSE) group
table(train)

# best subset selection using only the training data
res <- regsubsets(Salary ~ ., data=dat[train,], nvmax=19)
summary(res)

# set up the model matrix (with all 19 predictors) for the test data
X.test <- model.matrix(Salary ~ ., data=dat[test,])

# for the 19 'best' models of increasing complexity, compute the test error rate

test.error <- rep(NA, 19)

for (i in 1:19) {
   coefi <- coef(res, id=i)
   pred <- X.test[,names(coefi)] %*% coefi
   test.error[i] <- mean((dat$Salary[test] - pred)^2)
}

# plot the test MSE for the models with increasing complexity
plot(test.error, xlab ="Number of Variables", ylab="Test MSE", type="o", pch=19, xlim=c(0,20))
points(which.min(test.error), min(test.error), col="red", cex=2, pch=19)

# for which model complexity is the test MSE minimized
which.min(test.error)

# coefficients / variables for the best overall model
coef(res, id=7)

# best subset selection using the full dataset going up to 7 predictors
res <- regsubsets(Salary ~ ., data=dat, nvmax=7)
summary(res)

# get the coefficients for the best 7 predictor model using the full dataset
coef(res, id=7)

# get the full regression table for this model
X.best <- model.matrix(Salary ~ ., data=dat)[,names(coef(res, id=7))][,-1]
summary(lm(Salary ~ X.best, data=dat))

# note: same issue with the standard errors as mentioned earlier

############################################################################

# choosing the overall best model using 10-fold cross-validation

k <- 10

set.seed(1)

folds <- sample(rep(1:k, length=nrow(dat)))

cv.errors <- matrix(NA, nrow=k, ncol=19)

for (j in 1:k) {

   res <- regsubsets(Salary ~ ., data=dat[folds != j,], nvmax=19)
   X.test <- model.matrix(Salary ~ ., data=dat[folds == j,])

   for (i in 1:19) {
      coefi <- coef(res, id=i)
      pred <- X.test[,names(coefi)] %*% coefi
      cv.errors[j,i] <- mean((dat$Salary[folds==j] - pred)^2)
   }

}

test.error.cv <- apply(cv.errors, 2, mean)

# plot the test MSE for the models with increasing complexity
plot(test.error.cv, xlab ="Number of Variables", ylab="Test MSE", type="o", pch=19, xlim=c(0,20))
points(which.min(test.error.cv), min(test.error.cv), col="red", cex=2, pch=19)

# for which model complexity is the test MSE minimized
which.min(test.error.cv)

# best subset selection using the full dataset going up to 10 predictors
res <- regsubsets(Salary ~ ., data=dat, nvmax=10)

# get the coefficients for the best 10 predictor model using the full dataset
coef(res, id=10)

# get the full regression table for this model
X.best <- model.matrix(Salary ~ ., data=dat)[,names(coef(res, id=10))][,-1]
summary(lm(Salary ~ X.best, data=dat))

# note: same issue with the standard errors as mentioned earlier

############################################################################

### 6.5.2: Ridge Regression and the Lasso

# install (if needed) and load the glmnet package
#install.packages("glmnet")
library(glmnet)

# create model matrix (leaving out the intercept)
x <- model.matrix(Salary ~ ., data=dat)[,-1]

# put the outcome variable into y
y <- dat$Salary

# check that we get the same as OLS when lambda=0 (note: have to use a smaller
# value for the convergence threshold for this to give the same coefficients)
res <- glmnet(x, y, alpha=0, lambda=0, thresh=1e-20)
round(data.frame(glmnet = coef(res)[,1],
                 lm     = coef(lm(Salary ~ ., data=dat))), digits=5)

# ridge regression for various values of lambda
lambdas <- 10^seq(10, -2, length=100)
res <- glmnet(x, y, alpha=0, lambda=lambdas)

# coef(res) gives a matrix with the ridge regression coefficients where each
# column corresponds to one of the lambda values considered

# consider the 50th lambda value; get the corresponding coefficients and the
# l2 norm value (= the square-root of the sum of the squared coefficients,
# leaving out the intercept)
res$lambda[50]
coef(res)[,50]
sqrt(sum(coef(res)[-1,50]^2))

# the same but for the 60th lambda value
res$lambda[60]
coef(res)[,60]
sqrt(sum(coef(res)[-1,60]^2))

# get the regression coefficients for lambda=50
pred50 <- predict(res, s=50, type="coefficients")[,1]
pred50

############################################################################

# a note about how predict() works here

# note: lambda=50 was not one the lambda values we specified above
options(scipen=100)
lambdas
options(scipen=0)

# predict() estimates the coefficients for lambda=50 by interpolating the
# values we actually considered when we called glmnet()

# coefficient profile plot as a function of log(lambda)
plot(res, xvar="lambda")

# indicate log(50) as a vertical line in the plot
abline(v=log(50))

# however, this interpolation is not entirely accurate
res50 <- glmnet(x, y, alpha=0, lambda=50)
round(data.frame(pred50 = pred50,
                 res50  = coef(res50)[,1]), digits=5)

# can use exact=TRUE to avoid the use of interpolation
pred50 <- predict(res, s=50, type="coefficients", exact=TRUE, x=x, y=y)[,1]
round(data.frame(pred50 = pred50,
                 res50  = coef(res50)[,1]), digits=5)

# we still do not get the exact same coefficients; the problem is again that
# 'thresh' needs to be lowered
res    <- glmnet(x, y, alpha=0, lambda=lambdas, thresh=1e-20)
res50  <- glmnet(x, y, alpha=0, lambda=50, thresh=1e-20)
pred50 <- predict(res, s=50, type="coefficients", exact=TRUE, x=x, y=y)[,1]
round(data.frame(pred50 = pred50,
                 res50  = coef(res50)[,1]), digits=5)

############################################################################

# validation-set approach for choosing lambda

set.seed(1)

train <- sample(1:nrow(x), nrow(x)/2)
test  <- -train
y.test <- y[test]

# ridge regression using only the training data
res <- glmnet(x[train,], y[train], alpha=0, lambda=lambdas, thresh=1e-12)

# predict the salary for the test data setting lambda=4
pred <- predict(res, s=4, newx=x[test,])

# compute the test MSE
mean((pred-y.test)^2)

# if lambda is very large, then essentially all coefficients are shrunken
# towards 0, in which case the predicted value is just the intercept in the
# training data, which is equal to the mean salary in the training data; then
# the test MSE would be equal to
mean((mean(y[train]) - y.test)^2)

# same if we set lambda=10^10 (i.e., very large)
pred <- predict(res, s=10^10, newx=x[test,])
mean((pred-y.test)^2)

# predict the salary for the test data setting lambda=0 (i.e., like OLS)
pred <- predict(res, s=0, newx=x[test,], exact=TRUE, x=x[train,], y=y[train])

# compute the test MSE
mean((pred-y.test)^2)

# compare the OLS coefficients from lm() with the ones we get from glmnet()
pred0 <- predict(res, s=0, exact=TRUE, type="coefficients", x=x[train,], y=y[train])
round(data.frame(lm     = coef(lm(y ~ x, subset=train)),
                 glmnet = pred0[,1]), digits=5)

# slightly different again; setting 'thresh' above to something even smaller
# (like thresh=1e-20) yields coefficients that are even closer

# compute the test MSE for various values of lambda (zooming into a range
# where we can find the minimum)

lambdas <- 10^seq(4, -2, length=100)
res <- glmnet(x[train,], y[train], alpha=0, lambda=lambdas)
test.error <- rep(NA, 100)

for (i in 1:100) {
   pred <- predict(res, s=lambdas[i], newx=x[test,])
   test.error[i] <- mean((pred-y.test)^2)
}

# plot the test MSE as a function of log(lambda)
plot(log(lambdas), test.error, type="o", pch=19, xlab="lambda", ylab="Test MSE")

# for which lambda value is test estimated test MSE minimized
bestlam <- lambdas[which.min(test.error)]
bestlam

# indicate that value in the plot above as a vertical line
abline(v=log(bestlam))

# ridge regression with this lambda value in the full dataset
res <- glmnet(x, y, alpha=0, lambda=bestlam)
coef(res)[,1]

############################################################################

# ridge regression using only the training data
lambdas <- 10^seq(10, -2, length=100)
res <- glmnet(x[train,], y[train], alpha=0, lambda=lambdas, thresh=1e-12)

# do 10-fold cross-validation using cv.glmnet() to find the lambda value that
# minimizes the out-of-sample MSE (but only using the training data!)

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)

# for which lambda value is the out-of-sample MSE minimized
bestlam <- cv.out$lambda.min
bestlam

# compute the test MSE using this lambda value
pred <- predict(res, s=bestlam, newx=x[test,])
mean((pred-y.test)^2)

# ridge regression with this lambda value in the full dataset
res <- glmnet(x, y, alpha=0, lambda=bestlam)
coef(res)[,1]

############################################################################

# The Lasso

# lasso regression using only the training data
lambdas <- 10^seq(10, -2, length=100)
res <- glmnet(x[train,], y[train], alpha=1, lambda=lambdas)

# coefficient profile plot as a function of log(lambda)
plot(res, xvar="lambda")

# note: the range of lambda values considered in the book isn't ideal, since
# there is a wide range of larger lambda values where all coefficients are
# shrunken to 0

# 10-fold cross-validation
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)

# for which lambda value is the out-of-sample MSE minimized
bestlam <- cv.out$lambda.min
bestlam

# compute the test MSE using this lambda value
pred <- predict(res, s=bestlam, newx=x[test,])
mean((pred-y.test)^2)

# use this lambda value in the full dataset
res <- glmnet(x, y, alpha=1, lambda=bestlam)
coef(res)[,1]

# in the book, the authors use the full sequence of lambdas and then predict
# the coefficients for the best lambda value, but this yields slightly
# different results
res <- glmnet(x, y, alpha=1, lambda=lambdas)
predict(res, s=bestlam, type="coefficients")[,1]

# again, this is due to the interpolation that predict() does by default

# contrast this again with using exact=TRUE
res <- glmnet(x, y, alpha=1, lambda=lambdas)
predict(res, s=bestlam, type="coefficients", exact=TRUE, x=x, y=y)[,1]

# but need to lower 'thresh' again to get essentially the same values
res <- glmnet(x, y, alpha=1, lambda=bestlam, thresh=1e-20)
predict(res, s=bestlam, type="coefficients")[,1]
res <- glmnet(x, y, alpha=1, lambda=lambdas, thresh=1e-20)
predict(res, s=bestlam, type="coefficients", exact=TRUE, x=x, y=y)[,1]

############################################################################

### 6.5.3: PCR and PLS Regression

# install (if needed) and load the pls package
#install.packages("pls")
library(pls)

# Principal Components Regression (PCR)

# do PCR with 10-fold cross-validation
set.seed(2)
res <- pcr(Salary ~ ., data=dat, scale=TRUE, validation="CV")
summary(res)

# plot the cross-validated MSE as a function of the number of components
validationplot(res, val.type="MSEP")

# do PCR with 10-fold cross-validation using only the training data
set.seed(1)
res <- pcr(Salary ~ ., data=dat, scale=TRUE, validation="CV", subset=train)
validationplot(res, val.type="MSEP")

# predict in the test data using 5 components
pred <- predict(res, x[test,], ncomp=5)

# compute the test MSE
mean((pred-y.test)^2)

# now fit PCR using 5 components in the full dataset
res <- pcr(Salary ~ ., data=dat, scale=TRUE, ncomp=5)
summary(res)

############################################################################

# Partial Least Squares Regression (PLSR)

# do PLSR with 10-fold cross-validation
set.seed(1)
res <- plsr(Salary ~ ., data=dat, scale=TRUE, validation="CV", subset=train)
summary(res)
validationplot(res, val.type="MSEP")

# predict in the test data using 1 component
pred <- predict(res, x[test,], ncomp=1)

# compute the test MSE
mean((pred-y.test)^2)

# now fit PLSR using 1 component in the full dataset
res <- plsr(Salary ~ ., data=dat, scale=TRUE, ncomp=1)
summary(res)

############################################################################

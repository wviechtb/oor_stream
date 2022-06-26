############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-06-16
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 6.2.1 - 6.2.3
#
# last updated: 2022-06-24

############################################################################

### 6.2.1: Ridge Regression

# install and load the ISLR2 package
#install.packages("ISLR2")
library(ISLR2)

# copy the Credit dataset to dat
dat <- Credit

# sample size
n <- nrow(dat)

# examine the first 6 rows of the dataset
head(dat)

# the goal is to predict 'Balance' based on the other variables in the dataset

# standard linear regression model (estimated using least squares)
res <- lm(Balance ~ Income + Limit + Rating + Student + Cards + Age +
          Education + Own + Married + Region, data=dat)
summary(res)

# dummy code the categorical predictors (and turn Region into two dummy variables)
dat$Student <- ifelse(dat$Student == "Yes",   1, 0)
dat$Own     <- ifelse(dat$Own     == "Yes",   1, 0)
dat$Married <- ifelse(dat$Married == "Yes",   1, 0)
dat$South   <- ifelse(dat$Region  == "South", 1, 0)
dat$West    <- ifelse(dat$Region  == "West",  1, 0)

# remove Region from 'dat' since we dummy coded it manually
dat$Region <- NULL

# standardize (z-score) each predictor
dat[names(dat) != "Balance"] <- data.frame(apply(dat[names(dat) != "Balance"], 2, scale))

# reorder variables in dat into the same order as we enter them in the model
dat <- dat[c("Income", "Limit", "Rating", "Student", "Cards", "Age",
             "Education", "Own", "Married", "South", "West", "Balance")]

# linear regression model with the standardized predictors
res <- lm(Balance ~ Income + Limit + Rating + Student + Cards + Age +
          Education + Own + Married + South + West, data=dat)
summary(res)

# note: while the coefficients change, the R^2 value and F-statistics are the
# same as before (rescaling the predictors doesn't change the fit of the model)

# equation 6.5 (objective function for ridge regression that we want to minimize)
fitridge <- function(betas, y, X, lambda)
   sum((y - X %*% betas)^2) + lambda * sum(betas[-1]^2)

# put all predictors into X, include a column of 1s (for the intercept), and
# make it a matrix
X <- dat[names(dat) != "Balance"]
X <- cbind(intercept=1, X)
X <- as.matrix(X)

# check that we get the same RSS value when we set lambda=0 and feed the
# coefficient estimates from least squares to the objective function
fitridge(coef(res), y=dat$Balance, X=X, lambda=0)
sum(resid(res)^2)

# therefore this should 'recover' the least squares coefficients
sav <- optim(rep(0,ncol(X)), fitridge, y=dat$Balance, X=X, lambda=0, method="BFGS")
round(cbind(lm = coef(res), fitridge = sav$par), digits=3)

# above, we used optim() with method="BFGS" for the optimization, which is a
# quasi-Newton method; see: https://en.wikipedia.org/wiki/Quasi-Newton_method)

# however, especially once we get to the lasso (see below), this method
# doesn't work so well anymore; I tried several other optimizers and one that
# seems to work quite well in this context is the Hooke-Jeeves algorithm
# implemented in the 'dfoptim' package; this is a 'pattern search' method;
# see: https://en.wikipedia.org/wiki/Pattern_search_(optimization)

# install and load the dfoptim package
#install.packages("dfoptim")
library(dfoptim)

# check that we can again recover the least squares coefficients with hjk()
sav <- hjk(rep(0,ncol(X)), fitridge, y=dat$Balance, X=X, lambda=0)
round(cbind(lm = coef(res), fitridge = sav$par), digits=3)

# do this for 100 different values of lambda between 0.01 and 50000

lambdas <- exp(seq(log(.01), log(50000), length=100))

bR <- matrix(NA, nrow=length(lambdas), ncol=ncol(X))
convs <- rep(NA, length(lambdas))

pb <- txtProgressBar(max=length(lambdas), style=3)

for (i in 1:length(lambdas)) {

   setTxtProgressBar(pb, value=i)

   sav <- hjk(coef(res), fitridge, y=dat$Balance, X=X, lambda=lambdas[i])
   bR[i,]   <- sav$par
   convs[i] <- sav$convergence

}

close(pb)

# check that all 'convergence' values indicate successful convergence
all(convs == 0)

# Figure 6.4, left panel

plot(NA, xlim=range(lambdas), ylim=c(-300,450), xlab=expression(lambda),
     ylab="Standardized Coefficients", log="x")
abline(h=0, lty="dotted")

apply(bR[,-c(1:5)], 2, function(b) lines(lambdas, b, lwd=2, col="gray"))

lines(lambdas, bR[,2], lwd=4)
lines(lambdas, bR[,3], lwd=4, col="red")
lines(lambdas, bR[,4], lwd=4, col="blue")
lines(lambdas, bR[,5], lwd=4, col="orange")

legend("topright", inset=.02, lty="solid", lwd=4,
       col=c("black","red","blue","orange","gray"),
       legend=c("Income","Limit","Rating","Student","Other Variables"))

# Figure 6.4, right panel

xvals <- apply(bR, 1, function(b) sqrt(sum(b[-1]^2))) / sqrt(sum(coef(res)[-1]^2))

plot(NA, xlim=c(0,1), ylim=c(-300,450),
     xlab=expression("||"*hat(beta)*phantom()[lambda]^R*"||"[2]/"||"*hat(beta)*"||"[2]),
     ylab="Standardized Coefficients")
abline(h=0, lty="dotted")

apply(bR[,-c(1:5)], 2, function(b) lines(xvals, b, lwd=2, col="gray"))

lines(xvals, bR[,2], lwd=4)
lines(xvals, bR[,3], lwd=4, col="red")
lines(xvals, bR[,4], lwd=4, col="blue")
lines(xvals, bR[,5], lwd=4, col="orange")

############################################################################

# in the book, the discussion on how to choose the value of lambda is deferred
# until section 6.2.3; we already do this here; in particular, we will use
# 10-fold cross-validation (CV) to select the lambda value that minimizes the
# test MSE

# note: we use a range of lambda values here that 'zooms in' on the area where
# the test MSE will be minimized (as in the book; see Figure 6.12)

lambdas <- exp(seq(log(.005), log(5), length=100))

bR <- matrix(NA, nrow=length(lambdas), ncol=ncol(X))

mse.cv <- rep(NA, length(lambdas))

set.seed(1234)

# randomly assign the n subjects to the 10 folds
fold <- sample(1:10, n, replace=TRUE)

pb <- txtProgressBar(max=length(lambdas), style=3)

for (i in 1:length(lambdas)) {

   setTxtProgressBar(pb, value=i)

   sav <- hjk(coef(res), fitridge, y=dat$Balance, X=X, lambda=lambdas[i])
   bR[i,] <- sav$par

   # vector to store the 'out-of-sample' predictions
   pred <- rep(NA, n)

   for (j in 1:10) {

      # ridge regression using all data except those from fold j
      sav <- hjk(coef(res), fitridge, y=dat$Balance[fold!=j], X=X[fold!=j,], lambda=lambdas[i])

      # using the estimated coefficients, predicted for those in fold j
      pred[fold==j] <- c(X[fold==j,] %*% sav$par)

   }

   # compute the test MSE
   mse.cv[i] <- mean((pred - dat$Balance)^2)

}

close(pb)

# plot the lambda values against the test MSE (like Figure 6.12, left panel)
plot(lambdas, mse.cv, type="l", log="x",
     xlab=expression(lambda), ylab="Cross-Validated MSE")

# find the lambda value for which the test MSE is lowest (lambda = 0.5)
lambda.cv <- lambdas[which.min(mse.cv)]
lambda.cv

# show this value in the plot
abline(v=lambda.cv, lty="dashed")

# using this value, obtain the ridge regression coefficients
hjk(coef(res), fitridge, y=dat$Balance, X=X, lambda=lambda.cv)

# note the coefficients are not much different for this lambda value when
# compared to lambda=0 (like Figure 6.12, right panel)

plot(NA, xlim=range(lambdas), ylim=c(-300,450), xlab=expression(lambda),
     ylab="Standardized Coefficients", log="x")
abline(h=0, lty="dotted")
abline(v=lambda.cv, lty="dashed")

apply(bR[,-c(1:5)], 2, function(b) lines(lambdas, b, lwd=2, col="gray"))

lines(lambdas, bR[,2], lwd=4)
lines(lambdas, bR[,3], lwd=4, col="red")
lines(lambdas, bR[,4], lwd=4, col="blue")
lines(lambdas, bR[,5], lwd=4, col="orange")

############################################################################

### 6.2.2: The Lasso

# equation 6.7 (objective function for the lasso that we want to minimize)
fitlasso <- function(betas, y, X, lambda)
   sum((y - X %*% betas)^2) + lambda * sum(abs(betas[-1]))

# check that we get the same RSS value when we set lambda=0 and feed the
# coefficient estimates from least squares to the objective function
fitlasso(coef(res), y=dat$Balance, X=X, lambda=0)
sum(resid(res)^2)

# therefore this should 'recover' the least squares coefficients
sav <- optim(rep(0,ncol(X)), fitlasso, y=dat$Balance, X=X, lambda=0, method="BFGS")
round(cbind(lm = coef(res), fitlasso = sav$par), digits=3)

# due to the use of absolute values in the objective function for the lasso,
# the optimization here is even more difficult; the objective function may be
# non-smooth, which can cause problems when numerically approximating the
# gradient (which is done in various optimization methods); the Hooke-Jeeves
# algorithm can also be used on functions that are not continuous or
# differentiable and hence might work better here

# check that we can again recover the least squares coefficients with hjk()
sav <- hjk(rep(0,ncol(X)), fitlasso, y=dat$Balance, X=X, lambda=0)
round(cbind(lm = coef(res), fitlasso = sav$par), digits=3)

# do this for 100 different values of lambda between 1000 and 400000

lambdas <- exp(seq(log(1000), log(400000), length=100))

bL <- matrix(NA, nrow=length(lambdas), ncol=ncol(X))
convs <- rep(NA, length(lambdas))

pb <- txtProgressBar(max=length(lambdas), style=3)

for (i in 1:length(lambdas)) {

   setTxtProgressBar(pb, value=i)

   sav <- hjk(coef(res), fitlasso, y=dat$Balance, X=X, lambda=lambdas[i])
   bL[i,]   <- sav$par
   convs[i] <- sav$convergence

}

close(pb)

# check that all 'convergence' values indicate successful convergence
all(convs == 0)

# Figure 6.6, left panel

plot(NA, xlim=range(lambdas), ylim=c(-300,450), xlab=expression(lambda),
     ylab="Standardized Coefficients", log="x")
abline(h=0, lty="dotted")

apply(bL[,-c(1:5)], 2, function(b) lines(lambdas, b, lwd=2, col="gray"))

lines(lambdas, bL[,2], lwd=4)
lines(lambdas, bL[,3], lwd=4, col="red")
lines(lambdas, bL[,4], lwd=4, col="blue")
lines(lambdas, bL[,5], lwd=4, col="orange")

legend("topright", inset=.02, lty="solid", lwd=4,
       col=c("black","red","blue","orange","gray"),
       legend=c("Income","Limit","Rating","Student","Other Variables"))

# note: the lambda values shown in Figure 6.6 in the book are quite different
# in magnitude than the ones we used above to get results that look similar to
# those in the book; this might be because the authors used the glmnet package
# (see below) to obtain their results and the definition of the objective
# function is slightly different in that package (but the pattern of results
# is the same, so this doesn't change anything fundamentally)

# Figure 6.6, right panel

xvals <- apply(bL, 1, function(b) sum(abs(b[-1]))) / sum(abs(coef(res)[-1]))

plot(NA, xlim=c(0,1), ylim=c(-300,450),
     xlab=expression("||"*hat(beta)*phantom()[lambda]^L*"||"[1]/"||"*hat(beta)*"||"[1]),
     ylab="Standardized Coefficients")
abline(h=0, lty="dotted")

apply(bL[,-c(1:5)], 2, function(b) lines(xvals, b, lwd=2, col="gray"))

lines(xvals, bL[,2], lwd=4)
lines(xvals, bL[,3], lwd=4, col="red")
lines(xvals, bL[,4], lwd=4, col="blue")
lines(xvals, bL[,5], lwd=4, col="orange")

############################################################################

# use 10-fold CV to select the lambda value that minimizes the test MSE

# note: we again use a range of lambda values that 'zooms in' on the area
# where the test MSE will be minimized

lambdas <- exp(seq(log(1), log(1000), length=100))

bL <- matrix(NA, nrow=length(lambdas), ncol=ncol(X))

mse.cv <- rep(NA, length(lambdas))

set.seed(1234)

# randomly assign the n subjects to the 10 folds
fold <- sample(1:10, n, replace=TRUE)

pb <- txtProgressBar(max=length(lambdas), style=3)

for (i in 1:length(lambdas)) {

   setTxtProgressBar(pb, value=i)

   sav <- hjk(coef(res), fitlasso, y=dat$Balance, X=X, lambda=lambdas[i])
   bL[i,] <- sav$par

   # vector to store the 'out-of-sample' predictions
   pred <- rep(NA, n)

   for (j in 1:10) {

      # lasso using all data except those from fold j
      sav <- hjk(coef(res), fitlasso, y=dat$Balance[fold!=j], X=X[fold!=j,], lambda=lambdas[i])

      # using the estimated coefficients, predicted for those in fold j
      pred[fold==j] <- c(X[fold==j,] %*% sav$par)

   }

   # compute the test MSE
   mse.cv[i] <- mean((pred - dat$Balance)^2)

}

close(pb)

# plot the lambda values against the test MSE
plot(lambdas, mse.cv, type="l", log="x",
     xlab=expression(lambda), ylab="Cross-Validated MSE")

# find the lambda value for which the test MSE is lowest (lambda = 174.75)
lambda.cv <- lambdas[which.min(mse.cv)]
lambda.cv

# show this value in the plot
abline(v=lambda.cv, lty="dashed")

# using this value, obtain the lasso coefficients
hjk(coef(res), fitlasso, y=dat$Balance, X=X, lambda=lambda.cv)

# note the coefficients are not much different for this lambda value when
# compared to lambda=0

plot(NA, xlim=range(lambdas), ylim=c(-300,450), xlab=expression(lambda),
     ylab="Standardized Coefficients", log="x")
abline(h=0, lty="dotted")
abline(v=lambda.cv, lty="dashed")

apply(bL[,-c(1:5)], 2, function(b) lines(lambdas, b, lwd=2, col="gray"))

lines(lambdas, bL[,2], lwd=4)
lines(lambdas, bL[,3], lwd=4, col="red")
lines(lambdas, bL[,4], lwd=4, col="blue")
lines(lambdas, bL[,5], lwd=4, col="orange")

############################################################################

# note: you shouldn't do ridge regression or the lasso in the manner shown
# above; there are more efficient methods for computing the shrunken (or
# 'regularized') coefficients under these methods, which are implemented in
# the glmnet package; the code above simply illustrates that we can in
# principle apply these methods by doing the optimization 'manually'

# install and load the glmnet package
#install.packages("glmnet")
library(glmnet)

# note: we specify a *decreasing* sequence of lambda values
lambdas <- exp(seq(log(50000), log(.01), length=100))

# use glmnet() to do ridge regression (alpha=1 for lasso, alpha=0 for ridge
# regression; we already standardized earlier, so we set standardize=FALSE)
res <- glmnet(X[,-1], dat$Balance, alpha=0, standardize=FALSE, lambda=lambdas)

# Figure 6.6, left panel

plot(NA, xlim=range(lambdas), ylim=c(-300,450), xlab=expression(lambda),
     ylab="Standardized Coefficients", log="x")
abline(h=0, lty="dotted")

apply(coef(res)[-c(1:5),], 1, function(b) lines(lambdas, b, lwd=2, col="gray"))

lines(lambdas, coef(res)[2,], lwd=4)
lines(lambdas, coef(res)[3,], lwd=4, col="red")
lines(lambdas, coef(res)[4,], lwd=4, col="blue")
lines(lambdas, coef(res)[5,], lwd=4, col="orange")

legend("topright", inset=.02, lty="solid", lwd=4,
       col=c("black","red","blue","orange","gray"),
       legend=c("Income","Limit","Rating","Student","Other Variables"))

# there is also the cv.glmnet() function in the package which does the
# cross-validation to select a lambda value

lambdas <- exp(seq(log(5), log(.005), length=100))

set.seed(1234)
res.cv <- cv.glmnet(X[,-1], dat$Balance, alpha=0, lambda=lambdas)

# plot the lambda values against the test MSE (like Figure 6.12, left panel)
plot(lambdas, res.cv$cvm, type="l", log="x",
     xlab=expression(lambda), ylab="Cross-Validated MSE")

# the lambda value for which the test MSE is lowest
res.cv$lambda.min

# show this value in the plot
abline(v=res.cv$lambda.min , lty="dashed")

# using this value, obtain the ridge regression coefficients
res <- glmnet(X[,-1], dat$Balance, alpha=0, standardize=FALSE,
              lambda=res.cv$lambda.min)
coef(res)

# I also tried using glmnet() and cv.glmnet() with the lasso (alpha=1), but
# the results seemed to suggest that lambda~=0 is best so this doesn't make
# for an interesting application (which may also be the reason why the authors
# do not do this in the book either)

###########################################################################

# A Simple Special Case for Ridge Regression and the Lasso (page 247)

# simulate a dataset like that used in this example

set.seed(1234)
n <- 20
X <- diag(n)
y <- sort(rnorm(n))
id <- 1:n
res <- lm(y ~ 0 + factor(id))
summary(res)

# need to change these objective functions slightly because the model doesn't
# have an intercept term
fitridge <- function(betas, y, X, lambda)
   sum((y - X %*% betas)^2) + lambda * sum(betas^2)
fitlasso <- function(betas, y, X, lambda)
   sum((y - X %*% betas)^2) + lambda * sum(abs(betas))

lambda <- 1

# compare the ridge coefficients with equation 6.14
res.ridge <- hjk(coef(res), fitridge, y=y, X=X, lambda=lambda)
round(cbind(fitridge = res.ridge$par, eq6.14 = y / (1 + lambda)), 3)

# compare the lasso coefficients with equation 6.15
res.lasso <- hjk(coef(res), fitlasso, y=y, X=X, lambda=lambda)
round(cbind(fitlasso = res.lasso$par, eq6.15 = ifelse(y > lambda/2, y-lambda/2, ifelse(y < -lambda/2, y+lambda/2, 0))), 3)

# like Figure 6.10 (both panels combined into a single figure)
plot(y, coef(res), type="l", lwd=3, lty="dashed", xlab=expression(y[j]),
     ylab="Coefficient Estimate")
abline(h=0, lty="dotted")
abline(v=0, lty="dotted")
lines(y, res.ridge$par, lwd=3, col="red")
lines(y, res.lasso$par, lwd=3, col="blue")
legend("topleft", inset=.01, lty=c("dashed","solid","solid"), lwd=3,
       col= c("black","red","blue"), legend=c("Least Squares","Ridge","Lasso"))

############################################################################

# finally consider the running example used by the authors where we have p=45
# predictors and a total sample size of n=50; let's simulate some data for
# this case where the first two predictors are really related to the outcome
# and the remaining 43 predictors are not

set.seed(59)

n <- 50
p <- 45

X <- replicate(p, c(scale(rnorm(n))))
y <- X[,1] * 1 + X[,2] * 1 + rnorm(n)

# fit the model including all predictors
res <- lm(y ~ X)
summary(res)

# neither the first nor the second predictor are significant, but some of the
# false predictors actually are; also note the very high R^2 of 0.99, which is
# a result of p being almost as large as n

# now do 10-fold CV with the lasso

fitlasso <- function(betas, y, X, lambda)
   sum((y - X %*% betas)^2) + lambda * sum(abs(betas[-1]))

# add vector of 1s for the intercept to X
X <- cbind(1,X)

fold <- rep(1:10, each=5)

lambdas <- seq(5,30,length=100)
mse.cv <- rep(NA, length(lambdas))
bL <- matrix(NA, nrow=length(lambdas), ncol=ncol(X))

pb <- txtProgressBar(max=length(lambdas), style=3)

for (i in 1:length(lambdas)) {

   setTxtProgressBar(pb, value=i)

   tmp <- hjk(rep(0,p+1), fitlasso, y=y, X=X, lambda=lambdas[i])
   bL[i,] <- tmp$par

   pred <- rep(NA, n)

   for (j in 1:10) {

      sav <- hjk(rep(0,p+1), fitlasso, y=y[fold!=j], X=X[fold!=j,], lambda=lambdas[i])
      pred[fold==j] <- c(X[fold==j,] %*% sav$par)

   }

   mse.cv[i] <- mean((pred - y)^2)

}

close(pb)

# plot the lambda values against the test MSE (like Figure 6.13, left panel)
plot(lambdas, mse.cv, type="l", log="x",
     xlab=expression(lambda), ylab="Cross-Validated MSE")

# find the lambda value for which the test MSE is lowest
lambda.cv <- lambdas[which.min(mse.cv)]
lambda.cv

# show this value in the plot
abline(v=lambda.cv, lty="dashed")

# using this value, obtain the lasso coefficients
sav <- hjk(rep(0,p+1), fitlasso, y=y, X=X, lambda=lambda.cv)

# compare the least squares and lasso coefficients
round(cbind(lm = coef(res), fitlasso = sav$par), digits=3)

# plot the lasso coefficients as a function of lambda (like Figure 6.13, right panel)
plot(NA, xlim=range(lambdas), ylim=range(bL), xlab=expression(lambda),
     ylab="Standardized Coefficients")
abline(h=0, lty="dotted")
apply(bL[,-c(1:3)], 2, function(b) lines(lambdas, b, col="gray", lwd=2))
lines(lambdas, bL[,2], col="red",  lwd=4)
lines(lambdas, bL[,3], col="blue", lwd=4)
abline(v=lambda.cv, lty="dashed")

# almost all coefficients are shrunken to 0 for the chosen value of lambda
# except for the first two, so we are actually very close to discovering what
# the true model is (i.e., that only predictors 1 and 2 are truly related to
# the outcome variable)

############################################################################

# how well does best subset selection do in this example?

# remove the first column from X
X <- X[,-1]

# install and load the leaps package
#install.packages("leaps")
library(leaps)

# we cannot do an exhaustive search over all 2^p models as this would take way
# too long, but we can do an exhaustive search for models up to a certain
# level of complexity, say 10
res <- regsubsets(x=X, y=y, nvmax=10, method="exhaustive")

# plot Mallow's Cp against the number of predictors
plot(1:10, summary(res)$cp, type="o", pch=19,
     xlab="number of predictors", ylab="Mallow's Cp")

# unfortunately, this does not reach a minimum within the range examined

# let's use forward selection where we can go up to complexity p=45
res <- regsubsets(x=X, y=y, nvmax=45, method="forward")
plot(1:45, summary(res)$cp, type="o", pch=19,
     xlab="number of predictors", ylab="Mallow's Cp")

# here we do find a minimum, namely for 14 predictors
which.min(summary(res)$cp)

# that is quite far from the truth, so this approach isn't working out so well
# here for finding what the true model is

############################################################################

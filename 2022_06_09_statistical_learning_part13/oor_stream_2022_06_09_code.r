############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-06-09
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 6.1 - 6.1.3
#
# last updated: 2022-06-12

############################################################################

### 6.1.1: Best Subset Selection

# install and load the ISLR2 package
#install.packages("ISLR2")
library(ISLR2)

# copy the Credit dataset to dat
dat <- Credit

# examine the first 6 rows of the dataset
head(dat)

# the goal is to predict 'Balance' based on the other variables in the dataset

# put all predictors into X and dummy code all categorical predictors (note:
# the three-level factor 'Region' is coded into a 'South' and a 'West' dummy
# variable)
X <- dat[-11]
X$Own     <- ifelse(X$Own     == "Yes",   1, 0)
X$Student <- ifelse(X$Student == "Yes",   1, 0)
X$Married <- ifelse(X$Married == "Yes",   1, 0)
X$South   <- ifelse(X$Region  == "South", 1, 0)
X$West    <- ifelse(X$Region  == "West",  1, 0)
X$Region  <- NULL

# turn X into a matrix
X <- as.matrix(X)

# sample size
n <- nrow(dat)

# number of predictors
p <- ncol(X)

# test that we can use X in lm()
res <- lm(Balance ~ X, data=dat)
summary(res)

# create an empty list called 'res' and then store the intercept-only model as
# the first element in this list
res <- list()
res[[1]] <- lm(Balance ~ 1, data=dat)

# now fit all models with k = 1, 2, ..., p predictors and store the models in
# res; we use j (initialized at 2) for the position counter

j <- 2

for (k in 1:p) {

   combs <- combn(p, k)

   for (i in 1:ncol(combs)) {
      res[[j]] <- lm(Balance ~ X[,combs[,i]], data=dat)
      j <- j + 1
   }

}

# for each model in res, extract the number of predictors in the model (note:
# we subtract 1 for the intercept)
ks <- sapply(res, function(x) length(coef(x))) - 1

# for each model in res, extract the R^2 value
r2s <- sapply(res, function(x) summary(x)$r.squared)

# for each model in res, extract the RSS value
rsss <- sapply(res, function(x) sum(resid(x)^2))

# examine the first 30 values from these vectors
cbind(ks, round(r2s,2), rsss)[1:30,]

# find the smallest RSS value within each level of k
minrss <- tapply(rsss, ks, min)
minrss

# find the largest R^2 value within each level of k
maxr2 <- tapply(r2s, ks, max)
maxr2

# Figure 6.1 (note: intercept-only model with k=0 also included)

par(mfrow=c(1,2))

plot(ks, rsss, pch=19, cex=0.5, col="skyblue", xlab="Number of Predictors",
     ylab="Residual Sum of Squares")

lines(as.numeric(names(minrss)), minrss, type="o", pch=19, col="red")

plot(ks, r2s, pch=19, cex=0.5, col="skyblue", xlab="Number of Predictors",
     ylab=expression(R^2))

lines(as.numeric(names(maxr2)), maxr2, type="o", pch=19, col="red")

dev.off()

# note that we haven't actually yet picked a single 'best' model; for this, we
# have to do step 3 from Algorithm 6.1, where we use something like Cp (AIC),
# BIC, or adjusted R^2 to pick a model; for example, we can compute the AIC
# for each model and find the model that has the lowest AIC

summary(res[[which.min(sapply(res, AIC))]])

# so the 'best' model according to the AIC is one with k=6 predictors, namely
# Income, Limit, Rating, Cards, Age, and Student

############################################################################

# we can automate the process above using the glmulti package

# install and load the glmulti package
#install.packages("glmulti")
library(glmulti)

Xy <- data.frame(cbind(X, Balance=dat$Balance))

res <- glmulti(Balance ~ Income + Limit + Rating + Cards + Age + Education + Own + Student + Married + South + West,
               data=Xy, level=1, confsetsize=2^p, fitfunction="lm", crit = "aic", report=FALSE, plotty=FALSE)

# note that this yields the same best model according to the AIC
print(res)

# can also plot the AIC for each model
plot(res)

# check which models have an AIC value that is no more than 2 points worse
# than that of the 'best' model
top <- weightable(res)
top <- top[top$aic <= min(top$aic) + 2,]
top

# get k, R^2, and RSS for each model
ks   <- sapply(res@objects, function(x) length(coef(x))) - 1
r2s  <- sapply(res@objects, function(x) summary(x)$r.squared)
rsss <- sapply(res@objects, function(x) sum(resid(x)^2))

# find the smallest RSS and R^2 value within each level of k
minrss <- tapply(rsss, ks, min)
maxr2  <- tapply(r2s, ks, max)

# again Figure 6.1

par(mfrow=c(1,2))

plot(ks, rsss, pch=19, cex=0.5, col="skyblue", xlab="Number of Predictors",
     ylab="Residual Sum of Squares")

lines(as.numeric(names(minrss)), minrss, type="o", pch=19, col="red")

plot(ks, r2s, pch=19, cex=0.5, col="skyblue", xlab="Number of Predictors",
     ylab=expression(R^2))

lines(as.numeric(names(maxr2)), maxr2, type="o", pch=19, col="red")

dev.off()

############################################################################

# sidenote: in the book, the 'Region' three-level factor is treated as two
# separate (dummy) variables that can be included/excluded from a model
# separately from each other; instead, we might want to treat 'Region' as a
# single variable that is either included or excluded (when it is included in
# a model, it will still be coded as two dummy variables); for this, we can
# just glmulti() as follows (in this case, there are only 1024 models to
# consider)

res <- glmulti(Balance ~ Income + Limit + Rating + Cards + Age + Education + Own + Student + Married + Region,
               data=dat, level=1, confsetsize=1024, report=FALSE, plotty=FALSE)
print(res)

# doing so, we still end up with the same 'best' model as we did earlier

############################################################################

# we can also do best subset selection using the leaps package

# install and load the leaps package
#install.packages("leaps")
library(leaps)

res <- regsubsets(x=X, y=dat$Balance, nvmax=p, method="exhaustive")
summary(res)

# this shows for each value of k the 'best' model according to R^2; within a
# value of k, we can simply select the best model by finding the one with the
# higher R^2 / lower RSS; however, we now still need to select the 'best'
# model overall, which we can do as above

# plot the Mallow's Cp value for the 'best' models as a function of k (note:
# selecting based on Cp is the same as selecting based on AIC for linear
# regression models)
plot(1:p, summary(res)$cp, type="o", pch=19)

# find the model with the lowest Cp value
best.cp <- which.min(summary(res)$cp)
best.cp

# mark that point on the plot
points(best.cp, summary(res)$cp[best.cp], pch=4, cex=3, lwd=3)

# which predictors are included in this model
colnames(X)[summary(res)$which[best.cp,][-1]]

# again, we find the same overall 'best' model according to Cp/AIC

############################################################################

### 6.1.2: Stepwise Selection

# forward stepwise selection

incl <- NULL

res <- list()

for (k in 1:p) {

   if (is.null(incl)) {
      to.try <- 1:p
   } else {
      to.try <- (1:p)[-incl]
   }

   tmp <- list()

   for (i in 1:length(to.try)) {

      if (is.null(incl)) {
         tmp[[i]] <- lm(Balance ~ X[,to.try[i]], data=dat)
      } else {
         tmp[[i]] <- lm(Balance ~ X[,incl] + X[,to.try[i]], data=dat)
      }

   }
   r2s  <- sapply(tmp, function(x) summary(x)$r.squared)
   best <- which.max(r2s)
   incl <- c(incl, to.try[best])

   res[[k]] <- tmp[[best]]

}

# the order of predictors included according to forward stepwise selection (so
# first Rating is included, then Income, then Student, and so on)
colnames(X)[incl]

############################################################################

# programming forward/backward stepwise selection (or even a 'hybrid'
# approach) is a bit of pain, but regsubsets() can do this for us

res <- regsubsets(x=X, y=dat$Balance, nvmax=p, method="forward")
summary(res)

# note that the order is the same as above

# backward stepwise selection

res <- regsubsets(x=X, y=dat$Balance, nvmax=p, method="backward")
summary(res)

# here, we have to read the table backwards; so first all predictors are
# included, then excluding 'Education' leads to the smallest drop in R^2, then
# excluding 'South' leads to the smallest drop, and so on

# again, we would still have to use something like Cp (AIC), BIC, or adjusted
# R^2 to then select the overall 'best' model from those found by
# forward/backward stepwise selection (or an exhaustive search)

############################################################################

### 6.1.3: Choosing the Optimal Model

## Cp, AIC, BIC, and Adjusted R2

# let's go back to an exhaustive search over all 2^p models and then see which
# is the overall best model according to Cp (AIC), BIC, and adjusted R^2

# Figure 6.2

res <- regsubsets(x=X, y=dat$Balance, nvmax=p, method="exhaustive")

par(mfrow=c(1,3))

plot(1:p, summary(res)$cp, type="o", pch=19, xlab="Number of Predictors",
     ylab=expression(C[p]))
best.cp <- which.min(summary(res)$cp)
points(best.cp, summary(res)$cp[best.cp], pch=4, cex=3, lwd=3)

plot(1:p, summary(res)$bic, type="o", pch=19, xlab="Number of Predictors",
     ylab="BIC")
best.bic <- which.min(summary(res)$bic)
points(best.bic, summary(res)$bic[best.bic], pch=4, cex=3, lwd=3)

plot(1:p, summary(res)$adjr2, type="o", pch=19, xlab="Number of Predictors",
     ylab=expression("Adjusted R"^2))
best.adjr2 <- which.max(summary(res)$adjr2)
points(best.adjr2, summary(res)$adjr2[best.adjr2], pch=4, cex=3, lwd=3)

dev.off()

# predictors included in these 'best' models
colnames(X)[summary(res)$which[best.cp,][-1]]
colnames(X)[summary(res)$which[best.bic,][-1]]
colnames(X)[summary(res)$which[best.adjr2,][-1]]

## Validation and Cross-Validation

# now we will use cross-validation (with k=10) to estimate the test MSE when
# doing an exhaustive search among models with increasing complexity (note:
# we'll skip the simpler validation set approach)

set.seed(1234)

k <- 10

fold <- sample(1:k, n, replace=TRUE)

mse.cv <- rep(NA, p)

for (j in 1:p) { # loop through the levels of complexity

   pred.kfoldcv <- rep(NA, n)

   for (i in 1:k) { # for a given complexity, do 10-fold CV

      res <- regsubsets(x=X[fold != i,], y=dat$Balance[fold != i], nvmax=j, method="exhaustive")
      Xpred <- cbind(1, X[fold == i, summary(res)$which[j,][-1], drop=FALSE])
      pred.kfoldcv[fold==i] <- c(Xpred %*% coef(res, id=j))

   }

   mse.cv[j] <- mean((dat$Balance - pred.kfoldcv)^2)

}

# Figure 6.3, right panel

plot(1:p, mse.cv, type="o", pch=19, xlab="Number of Predictors",
     ylab="Cross-Validation Error")

best <- which.min(mse.cv)
points(best, mse.cv[best], pch=4, cex=3, lwd=3)

# so according to this approach, a model with 6 predictors would be best to
# minimize the (estimated) test MSE; so we conduct an exhaustive search among
# all models with 6 predictors to find the best model

res <- regsubsets(x=X, y=dat$Balance, nvmax=best, method="exhaustive")
colnames(X)[summary(res)$which[best,][-1]]

# so the 'best' model according to this approach includes Income, Limit,
# Rating, Cards, Age, and Student as predictors

############################################################################

# on page 236, the authors describe a model selection approach based on the
# 'one-standard-error rule', that is, "we first calculate the standard error
# of the estimated test MSE for each model size, and then select the smallest
# model for which the estimated test error is within one standard error of the
# lowest point on the curve"; to estimate the test MSE, we can make use of
# cross-validation (as above) and to calculate the SE of an estimated test
# MSE, we can make use a bootstrapping

set.seed(1234)

# number of bootstrap samples
B <- 1000

# matrix to store the test MSE values for each level of complexity (columns)
# for every bootstrap iteration (rows)
mse.cv <- matrix(NA, nrow=B, ncol=p)

pb <- txtProgressBar(max=B, style=3)

for (b in 1:B) {

   setTxtProgressBar(pb, value=b)

   id <- sample(1:n, n, replace=TRUE)
   dat.boot <- dat[id,]
   X.boot <- X[id,]

   fold <- sample(1:k, n, replace=TRUE)

   for (j in 1:p) {

      pred.kfoldcv <- rep(NA, n)

      for (i in 1:k) {

         res <- regsubsets(x=X.boot[fold != i,], y=dat.boot$Balance[fold != i], nvmax=j, method="exhaustive")
         Xpred <- cbind(1, X.boot[fold == i, summary(res)$which[j,][-1], drop=FALSE])
         pred.kfoldcv[fold==i] <- c(Xpred %*% coef(res, id=j))

      }

      mse.cv[b,j] <- mean((dat.boot$Balance - pred.kfoldcv)^2)

   }

}

close(pb)

# compute the mean (across the bootstrap estimates) of the test MSE values for
# each level of complexity
mean.mse.cv <- apply(mse.cv, 2, mean)
mean.mse.cv

# compute the SE of the test MSE values
se.mse.cv <- apply(mse.cv, 2, sd)
se.mse.cv

# plot the mean test MSE values
plot(1:p, mean.mse.cv, type="o", pch=19,
     xlab="Number of Predictors", ylab="Cross-Validation Error")

# for which level of complexity is the mean test MSE the lowest
best <- which.min(mean.mse.cv)
best

# indicate that point on the plot
points(best, mean.mse.cv[best], pch=4, cex=3, lwd=3)

# add arrows showing plus/minus one SE from each mean
arrows(1:p, mean.mse.cv-se.mse.cv, 1:p, mean.mse.cv+se.mse.cv,
       code=3, angle=90, length=0.12, lwd=3)

# find the simplest model whose mean test MSE minus one SE still captures the
# lowest mean test MSE
minbest <- min(which(mean.mse.cv - se.mse.cv < mean.mse.cv[best]))
minbest

# it is not entirely clear to me if this is how the authors intend to use the
# 'one-standard-error rule'; one could also find the simplest model whose test
# MSE is captured by the lowest mean test MSE plus one SE
minbest <- min(which(mean.mse.cv < mean.mse.cv[best] + se.mse.cv[best]))
minbest

# either way, this suggests that a model with 4 predictors would be best

# do an exhaustive search and determine which model with 4 predictors is best
res <- regsubsets(x=X, y=dat$Balance, nvmax=minbest, method="exhaustive")
colnames(X)[summary(res)$which[minbest,][-1]]

# so, according to this approach, the 'best' model includes Income, Limit,
# Cards, and Student as predictors

############################################################################

# instead of bootstrapping the cross-validation approach, one could also
# bootstrap the methods discussed previously based on Cp (AIC), BIC, and
# adjusted R^2; let's try this with the BIC

# within each bootstrap iteration, we conduct an exhaustive search and find,
# for each level of complexity, the best model and store its BIC values in the
# 'bic' matrix

B <- 1000

bic <- matrix(NA, nrow=B, ncol=p)

pb <- txtProgressBar(max=B, style=3)

for (b in 1:B) {

   setTxtProgressBar(pb, value=b)

   id <- sample(1:n, n, replace=TRUE)
   dat.boot <- dat[id,]
   X.boot <- X[id,]

   res <- regsubsets(x=X.boot, y=dat.boot$Balance, nvmax=p, method="exhaustive")
   bic[b,] <- summary(res)$bic

}

close(pb)

# compute the mean BIC value for each level of complexity
mean.bic <- apply(bic, 2, mean)
mean.bic

# compute the SE of the BIC values for each level of complexity
se.bic <- apply(bic, 2, sd)
se.bic

# plot the mean BIC values
plot(1:p, mean.bic, type="o", pch=19, xlab="Number of Predictors", ylab="BIC")

# for which level of complexity is the mean BIC the lowest
best <- which.min(mean.bic)
best

# add arrows showing plus/minus one SE from each mean
arrows(1:p, mean.bic-se.bic, 1:p, mean.bic+se.bic,
       code=3, angle=90, length=0.12, lwd=3)

# find the simplest model whose mean BIC minus one SE still captures the
# lowest mean BIC
minbest <- min(which(mean.bic - se.bic < mean.bic[best]))
minbest

# find the simplest model whose mean BIC is captured by the lowest mean BIC
# plus one SE
minbest <- min(which(mean.bic < mean.bic[best] + se.bic[best]))
minbest

# either way, this suggests that a model with 3 predictors is best

# do an exhaustive search and determine which model with 3 predictors is best
res <- regsubsets(x=X, y=dat$Balance, nvmax=minbest, method="exhaustive")
colnames(X)[summary(res)$which[minbest,][-1]]

# so, according to this approach, the 'best' model includes Income, Rating,
# and Student as predictors

############################################################################

# the MASS package also provides a function for automated model selection; see
# help(stepAIC) for details

library(MASS)

res <- lm(Balance ~ ., data=Xy)
stepAIC(res)

# so the 'best' model according to this approach includes Income, Limit,
# Rating, Cards, Age, and Student as predictors

############################################################################

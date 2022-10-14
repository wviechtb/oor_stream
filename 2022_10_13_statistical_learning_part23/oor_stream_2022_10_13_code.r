############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-10-13
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 8.2.2 - 8.2.5
#
# last updated: 2022-10-14

############################################################################

### 8.2.1: Bagging (but now using the randomForest package)

# install (if necessary) the tree package
#install.packages("tree")

# load the tree package
library(tree)

# install (if necessary) the randomForest package
#install.packages("randomForest")

# load the randomForest package
library(randomForest)

# download the Heart dataset
download.file("https://www.statlearning.com/s/Heart.csv", destfile="Heart.csv")

# read in the dataset
dat <- read.csv("Heart.csv")

# inspect the first 6 rows
head(dat)

# remove subject ID variable X
dat$X <- NULL

# only keep the complete cases
dat <- dat[complete.cases(dat),]

# turn AHD into a factor (that is what tree() expects for the outcome)
dat$AHD <- factor(dat$AHD)

# turn the other string variables into factors (also necessary for tree())
dat$ChestPain <- factor(dat$ChestPain)
dat$Thal      <- factor(dat$Thal)

# create the training and the test dataset
set.seed(1235)
id.train  <- sample(nrow(dat), round(nrow(dat)/2))
dat.train <- dat[id.train,]
dat.test  <- dat[-id.train,]

# build tree in the training data
res1 <- tree(AHD ~ ., data=dat.train)
summary(res1)

# predicted group based on the tree in the test data
pred1 <- predict(res1, newdata=dat.test, type="class")

# test error rate of this single tree
error.single <- mean(dat.test$AHD != pred1)
error.single

# bagging using the randomForest() function
res2 <- randomForest(AHD ~ ., data=dat.train, mtry=ncol(dat.train)-1,
                     importance=TRUE, ntree=300)
res2

# predicted group based on bagging in the test data
pred2 <- predict(res2, newdata=dat.test)

# test error rate of bagging
error.bagging <- mean(dat.test$AHD != pred2)
error.bagging

# variable importance
importance(res2)
varImpPlot(res2)

############################################################################

# install (if necessary) the glmnet package
#install.packages("glmnet")

# load the glmnet package
library(glmnet)

# load the splines package
library(splines)

# repeat the validation set approach above 1000 times to get the mean error
# rate of a single tree, based on bagging (like we did last time, but now
# using the randomForest() function), and also based on random forests, a
# logistic regression model using all variables as predictors, the lasso, and
# a logistic model with spline terms for the quantitative predictors

set.seed(1234)

iters <- 1000

error.singlet.sim  <- rep(NA, iters)
error.bagging.sim <- rep(NA, iters)
error.randomf.sim <- rep(NA, iters)
error.logisti.sim <- rep(NA, iters)
error.lassooo.sim <- rep(NA, iters)
error.splines.sim <- rep(NA, iters)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (i in 1:iters) {

   setTxtProgressBar(pbar, i)

   id.train  <- sample(nrow(dat), round(nrow(dat)/2))
   dat.train <- dat[id.train,]
   dat.test  <- dat[-id.train,]

   res <- tree(AHD ~ ., data=dat.train)
   pred <- predict(res, newdata=dat.test, type="class")
   error.singlet.sim[i] <- mean(dat.test$AHD != pred)

   res <- randomForest(AHD ~ ., data=dat.train, mtry=ncol(dat.train)-1, ntree=300)
   pred <- predict(res, newdata=dat.test)
   error.bagging.sim[i] <- mean(dat.test$AHD != pred)

   res <- randomForest(AHD ~ ., data=dat.train, mtry=4, ntree=300)
   pred <- predict(res, newdata=dat.test)
   error.randomf.sim[i] <- mean(dat.test$AHD != pred)

   res <- glm(AHD ~ ., data=dat.train, family=binomial)
   pred <- ifelse(predict(res, newdata=dat.test, type="response") > 0.5, "Yes", "No")
   error.logisti.sim[i] <- mean(dat.test$AHD != pred)

   X <- model.matrix(res)
   cv.out <- cv.glmnet(X, dat.train$AHD, alpha=1, family="binomial")
   bestlam <- cv.out$lambda.min
   res <- glmnet(X, dat.train$AHD, alpha=1, family="binomial", lambda=bestlam)
   X <- model.matrix(AHD ~ ., data=dat.test)
   pred <- predict(res, newx=X, type="class")
   error.lassooo.sim[i] <- mean(dat.test$AHD != pred)

   res <- glm(AHD ~ ns(Age, df=3) + Sex + ChestPain + ns(RestBP, df=3) + ns(Chol, df=3) + Fbs + RestECG + ns(MaxHR, df=3) + ExAng + Oldpeak + Slope + Ca + Thal, data=dat.train, family=binomial)
   pred <- ifelse(predict(res, newdata=dat.test, type="response") > 0.5, "Yes", "No")
   error.splines.sim[i] <- mean(dat.test$AHD != pred)

}

close(pbar)

# mean test error rates of the different methods
mean(error.singlet.sim)
mean(error.bagging.sim)
mean(error.randomf.sim)
mean(error.logisti.sim)
mean(error.lassooo.sim)
mean(error.splines.sim)

# the mean error rate of bagging is quite similar to what we obtained last
# time doing the bagging manually; the slight difference can probably be
# explained based on the randomness of the bootstrapping, but also because
# randomForest() might build the trees in a slightly different way (e.g.,
# maybe a different depth) than what tree() does by default

# collect all of the test error rates in a data frame and create a boxplot for
# each method with the raw values (with jittering) superimposed
errors <- data.frame(error.singlet.sim, error.bagging.sim, error.randomf.sim,
                     error.logisti.sim, error.lassooo.sim, error.splines.sim)
par(mar=c(5,10,2,2))
boxplot(errors, horizontal=TRUE, las=1, range=0, xlab="Test Error Rate")
stripchart(lapply(errors, jitter, amount=.02), add=TRUE, pch=21, cex=0.5,
           method="jitter", jitter=0.1, col=rgb(0,0,0,.2), bg=rgb(0,0,0,.1))

# among the methods examined, plain logistic regression actually does best on
# average, followed by the lasso and random forests; the added flexibility of
# the logistic regression model with spline terms does not help due to
# overfitting in the training data

############################################################################

### 8.2.2: Random Forests

# instead of examining the performance of random forests in a single
# train/test split for an increasing number of trees (as in Figure 8.8), it is
# more interesting to ask how well the random forests classifier performs on
# average across many random train/test splits, which is what we are doing
# above (with the number of trees fixed to 300, which should be sufficiently
# large); we see that random forests on average performs better than bagging

############################################################################

### 8.2.3: Boosting

# install (if needed) and load the ISLR2 package
#install.packages("ISLR2")
library(ISLR2)

# copy the Hitters dataset to dat
dat <- Hitters

# keep rows where Salary is not missing
dat <- dat[!is.na(dat$Salary),]

# create log transformed Salary variable
dat$logSalary <- log(dat$Salary)

# keep only the variables we need below
dat <- dat[c("AtBat", "Hits", "HmRun", "Runs", "RBI", "Walks", "Years", "League",
             "Division", "PutOuts", "Assists", "Errors", "logSalary")]

# create a training and a test dataset
set.seed(1234)
id.train  <- sample(nrow(dat), round(nrow(dat)/2))
dat.train <- dat[id.train,]
dat.test  <- dat[-id.train,]

# fit regression model in the training data
res <- lm(logSalary ~ ., data=dat.train)
summary(res)

# MSE in the training data
mean((dat.train$logSalary - predict(res))^2)

# predict the outcome in the test data
pred <- predict(res, newdata=dat.test)

# MSE in the test data
mean((dat.test$logSalary - pred)^2)

# fit a single tree and get the MSE in the test data
res <- tree(logSalary ~ ., data=dat.train)
pred <- predict(res, newdata=dat.test)
mean((dat.test$logSalary - pred)^2)

# bagging and get the MSE in the test data
res <- randomForest(logSalary ~ ., data=dat.train, mtry=ncol(dat.train)-1)
pred <- predict(res, newdata=dat.test)
mean((dat.test$logSalary - pred)^2)

# random forests and get the MSE in the test data
res <- randomForest(logSalary ~ ., data=dat.train, mtry=round(sqrt(ncol(dat.train)-1)))
pred <- predict(res, newdata=dat.test)
mean((dat.test$logSalary - pred)^2)

# boosting (done manually)

B <- 5000
lambda <- .001

pred.train <- rep(0, nrow(dat.train))
pred.test  <- rep(0, nrow(dat.test))
ri <- dat.train$logSalary
mse.train <- rep(NA, B)
mse.test  <- rep(NA, B)

pbar <- txtProgressBar(min=0, max=B, style=3)

for (b in 1:B) {

   setTxtProgressBar(pbar, b)

   res <- tree(ri ~ . - logSalary, data=dat.train) # control=tree.control(nobs=nrow(dat.train), minsize=2, mindev=0))
   ptree <- prune.tree(res, best=2)
   fhatb.train <- predict(ptree)
   fhatb.test  <- predict(ptree, newdata=dat.test)
   pred.train <- pred.train + lambda * fhatb.train
   pred.test  <- pred.test  + lambda * fhatb.test
   ri <- ri - lambda * fhatb.train
   mse.train[b] <- mean((dat.train$logSalary - pred.train)^2)
   mse.test[b]  <- mean((dat.test$logSalary  - pred.test)^2)

}

close(pbar)

# plot the train/test data MSE as a function of trees used
par(mar=c(5,4,2,2))
plot(1:B, mse.train, type="l", lwd=3, log="y", ylab="MSE")
lines(1:B, mse.test, type="l", lwd=3, lty="dotted")
legend("topright", inset=.02, lwd=3, lty=c("dotted","solid"),
       legend=c("Test Data MSE","Training Data MSE"))

# final test MSE
tail(mse.test, 1)

# boosting (using the gbm package)

# install (if necessary) the gbm package
#install.packages("gbm")

# load the gbm package
library(gbm)

res <- gbm(logSalary ~ ., data=dat.train, distribution="gaussian",
           n.trees=5000, interaction.depth=1, shrinkage=.001)
summary(res)

pred <- predict(res, newdata=dat.test, n.trees=5000)
mean((dat.test$logSalary - pred)^2)

# boosting a linear regression model

B <- 5000
lambda <- .001

pred.train <- rep(0, nrow(dat.train))
pred.test  <- rep(0, nrow(dat.test))
ri <- dat.train$logSalary
mse.train <- rep(NA, B)
mse.test  <- rep(NA, B)

pbar <- txtProgressBar(min=0, max=B, style=3)

for (b in 1:B) {

   setTxtProgressBar(pbar, b)

   res <- lm(ri ~ . - logSalary, data=dat.train)
   fhatb.train <- predict(res)
   fhatb.test  <- predict(res, newdata=dat.test)
   pred.train <- pred.train + lambda * fhatb.train
   pred.test  <- pred.test  + lambda * fhatb.test
   ri <- ri - lambda * fhatb.train
   mse.train[b] <- mean((dat.train$logSalary - pred.train)^2)
   mse.test[b]  <- mean((dat.test$logSalary  - pred.test)^2)

}

close(pbar)

# plot the train/test data MSE as a function of models used
plot(1:B, mse.train, type="l", lwd=3, log="y", ylab="MSE")
lines(1:B, mse.test, type="l", lwd=3, lty="dotted")
legend("topright", inset=.02, lwd=3, lty=c("dotted","solid"),
       legend=c("Test Data MSE","Training Data MSE"))

# final test MSE
tail(mse.test, 1)

############################################################################

### 8.2.4: Bayesian Additive Regression Trees

# install (if needed) and load the BART package
#install.packages("BART")
library(BART)

X.train <- model.matrix(logSalary ~ ., data=dat.train)
X.test  <- model.matrix(logSalary ~ ., data=dat.test)
res <- gbart(X.train, dat.train$logSalary, x.test=X.test)

# test MSE
pred <- res$yhat.test.mean
mean((dat.test$logSalary - pred)^2)

############################################################################

# run a simulation where we try out the various methods above repeatedly with
# new train/test splits in each iteration and then check the mean MSE of each
# method (note that this takes quite a bit of time to run)

set.seed(1234)

iters <- 1000

mse.regress.sim <- rep(NA, iters)
mse.singlet.sim <- rep(NA, iters)
mse.bagging.sim <- rep(NA, iters)
mse.randomf.sim <- rep(NA, iters)
mse.boostin.sim <- rep(NA, iters)
mse.bartree.sim <- rep(NA, iters)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (i in 1:iters) {

   setTxtProgressBar(pbar, i)

   id.train  <- sample(nrow(dat), round(nrow(dat)/2))
   dat.train <- dat[id.train,]
   dat.test  <- dat[-id.train,]

   res <- lm(logSalary ~ ., data=dat.train)
   pred <- predict(res, newdata=dat.test)
   mse.regress.sim[i] <- mean((dat.test$logSalary - pred)^2)

   res <- tree(logSalary ~ ., data=dat.train)
   pred <- predict(res, newdata=dat.test)
   mse.singlet.sim[i] <- mean((dat.test$logSalary - pred)^2)

   res <- randomForest(logSalary ~ ., data=dat.train, mtry=ncol(dat.train)-1)
   pred <- predict(res, newdata=dat.test)
   mse.bagging.sim[i] <- mean((dat.test$logSalary - pred)^2)

   res <- randomForest(logSalary ~ ., data=dat.train, mtry=round(sqrt(ncol(dat.train)-1)))
   pred <- predict(res, newdata=dat.test)
   mse.randomf.sim[i] <- mean((dat.test$logSalary - pred)^2)

   res <- gbm(logSalary ~ ., data=dat.train, distribution="gaussian",
              n.trees=5000, interaction.depth=1, shrinkage=.001)
   pred <- predict(res, newdata=dat.test, n.trees=5000)
   mse.boostin.sim[i] <- mean((dat.test$logSalary - pred)^2)

   X.train <- model.matrix(logSalary ~ ., data=dat.train)
   X.test  <- model.matrix(logSalary ~ ., data=dat.test)
   tmp <- capture.output(res <- gbart(X.train, dat.train$logSalary, x.test=X.test))
   pred <- res$yhat.test.mean
   mse.bartree.sim[i] <- mean((dat.test$logSalary - pred)^2)

}

close(pbar)

# mean test MSEs of the different methods
mean(mse.regress.sim)
mean(mse.singlet.sim)
mean(mse.bagging.sim)
mean(mse.randomf.sim)
mean(mse.boostin.sim)
mean(mse.bartree.sim)

# collect all of the test MSEs in a data frame and create a boxplot for each
# method with the raw values (with jittering) superimposed
mses <- data.frame(mse.regress.sim, mse.singlet.sim, mse.bagging.sim,
                   mse.randomf.sim, mse.boostin.sim, mse.bartree.sim)
par(mar=c(5,10,2,2))
boxplot(mses, horizontal=TRUE, las=1, range=0, xlab="Test MSE")
stripchart(lapply(mses, jitter, amount=.02), add=TRUE, pch=21, cex=0.5,
           method="jitter", jitter=0.1, col=rgb(0,0,0,.2), bg=rgb(0,0,0,.1))

############################################################################

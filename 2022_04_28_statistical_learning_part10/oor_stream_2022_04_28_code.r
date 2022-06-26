############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-04-28
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 4.6.2 - 4.8
#
# last updated: 2022-04-29

############################################################################

### 4.6.2: Poisson Regression on the Bikeshare Data

# load the ISLR2 package
library(ISLR2)

# copy the Bikeshare dataset to dat
dat <- Bikeshare

# outcome: bikers
# categorical predictors: mnth, hr, weathersit
# dichotomous predictors: workingday
# quantitative predictor: temp

############################################################################

# illustrate the Poisson distribution

# probability of 0, 1, or 2 bikers when lambda = 5
round(dpois(0, lambda=5), digits=4)
round(dpois(1, lambda=5), digits=3)
round(dpois(2, lambda=5), digits=3)

# probability of 0, 1, ..., 20 bikers when lambda = 5
round(dpois(0:20, lambda=5), digits=3)

# make a barplot of these probabilities
barplot(dpois(0:20, lambda=5), names.arg = 0:20)

# examine the distributions when lambda = 1, 2, 5, or 10
par(mfrow=c(2,2))
barplot(dpois(0:20, lambda=1),  names.arg=0:20, main="lambda = 1")
barplot(dpois(0:20, lambda=2),  names.arg=0:20, main="lambda = 2")
barplot(dpois(0:20, lambda=5),  names.arg=0:20, main="lambda = 5")
barplot(dpois(0:20, lambda=10), names.arg=0:20, main="lambda = 10")
par(mfrow=c(1,1))

############################################################################

# a small example illustrating maximum likelihood estimation for a Poisson
# regression model with a single predictor (namely: temperature)

# start with some rough estimates for beta0 and beta1; when temp = 0 (approx.
# the coldest temperature), then let's say we expect on average 1 biker, and
# since log(1) = 0, we set beta0 = 0; and when temp = 1 (approx. the warmest
# temperature), then let's say we expect on average 600 bikers, and since
# log(600) =~ 6.4, we set beta1 = 6.4 (so that exp(0 + 6.4) =~ 600)
beta0 <- 0
beta1 <- 6.4

# based on these initial values, compute log(lambda) for each row in the dataset
loglambda <- beta0 + beta1 * dat$temp

# compute the probability of the observed number of bikers for each row using
# the lambda values as given by the initial estimates of beta0 and beta1; then
# take the product of all these probabilities - this is the likelihood value
prod(dpois(dat$bikers, lambda = exp(loglambda)))

# because all of the probabilities are between 0 and 1, the product ends up
# being indistinguishable from 0; to get around this problem, we compute the
# log likelihood; since log(a * b) = log(a) + log(b), this is equal to:
sum(dpois(dat$bikers, lambda = exp(loglambda), log = TRUE))

# we now start messing around with the beta0 and beta1 values until we find
# the two values for these parameters that maximize the log likelihood
beta0 <- 0
beta1 <- 5
loglambda <- beta0 + beta1 * dat$temp
sum(dpois(dat$bikers, lambda = exp(loglambda), log = TRUE))

beta0 <- 0
beta1 <- 7
loglambda <- beta0 + beta1 * dat$temp
sum(dpois(dat$bikers, lambda = exp(loglambda), log = TRUE))

beta0 <- 1
beta1 <- 7
loglambda <- beta0 + beta1 * dat$temp
sum(dpois(dat$bikers, lambda = exp(loglambda), log = TRUE))

# ... and so on; but we do not want to do this manually; software to fit
# Poisson regression models use clever algorithms to do this process quickly
res <- glm(bikers ~ temp, data=dat, family=poisson)
summary(res)
coef(res)

# so the values used above were pretty bad guesses; if we plug the estimates
# from the model into our computation of the log likelihood, we can see that
# this is identical to the maximized log likelihood that glm() finds
beta0 <- coef(res)[1]
beta1 <- coef(res)[2]
loglambda <- beta0 + beta1 * dat$temp
sum(dpois(dat$bikers, lambda = exp(loglambda), log = TRUE))
logLik(res)

# when temp = 0, we expect around 45 bikers on average (not 1)
unname(exp(coef(res)[1]))

# when temp = 1, we expect around 400 bikers on average (not 600)
unname(exp(coef(res)[1] + coef(res)[2] * 1))

############################################################################

# now let's fit the full model with all predictors (Table 4.11)
res <- glm(bikers ~ workingday + temp + weathersit + relevel(mnth, ref="Dec") + hr, data=dat, family=poisson)
round(coef(summary(res))[1:6,], digits=2)

# plot coefficients for mnth (Figure 4.15, left panel)
b <- coef(res)
b <- c(b[grep("mnth", names(b))], 0)
plot(b, type="o", pch=19, col="blue", xlab="Month", ylab="Coefficient", xaxt="n")
axis(side=1, at=1:12, label=substr(levels(dat$mnth), 1, 1))
abline(h=0, lty="dotted")

# plot coefficients for hr (Figure 4.15, right panel)
b <- coef(res)
b <- c(0, b[grep("hr", names(b))])
plot(b, type="o", pch=19, col="blue", xlab="Hour", ylab="Coefficient", xaxt="n")
axis(side=1, at=0:24)
abline(h=0, lty="dotted")

############################################################################

# let's briefly go back to the simple model with a single predictor
#
# log(lambda) when x = 0: beta0 + beta1 * 0
# log(lambda) when x = 1: beta0 + beta1 * 1
#
# now let's take the difference between these two log(lambda) values:
#
# log(lambda) when x = 1 - log(lambda) when x = 0
# = (beta0 + beta1 * 1) - (beta0 + beta1 * 0)
# =          beta1 * 1  -          beta1 * 0
# = beta1
#
# since log(a) - log(b) = log(a / b), then:
#
# log(lambda) when x = 1 - log(lambda) when x = 0
# = log(lambda when x = 1 / lambda when x = 0)
#
# and that is equal to beta1
#
# therefore, exp(beta1) = lambda when x = 1 / lambda when x = 0
#
# therefore exp(beta1) indicates how many times higher (when this is larger
# than 1) the mean count is when x increases by one unit
#
# for example, above we find an estimate of beta1 = 2.194164 and hence
# exp(2.194164) is around 9; so, when temp = 1, we expect 9 times as many
# biker on average compared to when temp = 0
#
# the same also works in models with multiple predictors

############################################################################

# check that we do not get any negative predicted counts

# when we use predict(), by default we get the predicted log(lambda) values
predict(res)[1:20]

# setting type="response" gives the predicted lambda values
predict(res, type="response")[1:20]

# and none of these can ever be negative
mean(predict(res, type="response") < 0)

############################################################################

# let's go back to the simpler model with a single predictor and plot the results

res <- glm(bikers ~ temp, data=dat, family=poisson)

xs <- seq(0, 1, length=1000)
pred <- predict(res, newdata=data.frame(temp = xs))
plot(xs, pred, type="l", lwd=5, xlab="Temperature", ylab="Predicted Log Average Count")

xs <- seq(0, 1, length=1000)
pred <- predict(res, newdata=data.frame(temp = xs), type="response")
plot(xs, pred, type="l", lwd=5, xlab="Temperature", ylab="Predicted Average Count")

############################################################################

### 4.7 Lab: Classification Methods

# 4.7.1: The Stock Market Data

help(Smarket)

dat <- Smarket

# 4.7.2: Logistic Regression

# check the levels of the 'Direction' variable
levels(dat$Direction)

# fit logistic regression model
res <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=dat, family=binomial)
summary(res)

# since the *second* level is 'Up', we are modeling the log(odds) of 'Up'

# fit the 'empty model' without any predictors
res0 <- glm(Direction ~ 1, data=dat, family=binomial)

# compare the empty model with the full model
anova(res0, res, test="Chisq")

# so the model with all the predictors does not lead to a significantly better
# fit than the model without any predictors :(

# predicted probability of an Up for the first 10 rows in the training data
predict(res, type="response")[1:10]

# convert predicted probabilities into predicted Ups and Downs (using a 0.5 cutoff)
dat$pred <- ifelse(predict(res, type="response") > 0.5, "Up", "Down")

# confusion matrix
table(dat$pred, dat$Direction)

# proportion of rows in the training data where the prediction is correct
mean(dat$pred == dat$Direction)

# hence the training error is just one minus that
mean(dat$pred != dat$Direction)

# create a training and test dataset by splitting up the dataset into two parts
dat.train <- dat[dat$Year < 2005,]
dat.test  <- dat[dat$Year >= 2005,]

# fit logistic regression model based on the training data
res <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=dat.train, family=binomial)

# confusion matrix and error rate for the test data
dat.test$pred <- ifelse(predict(res, newdata=dat.test, type="response") > 0.5, "Up", "Down")
table(dat.test$pred, dat.test$Direction)
mean(dat.test$pred != dat.test$Direction)

# fit a simpler model using the training data
res <- glm(Direction ~ Lag1 + Lag2, data=dat.train, family=binomial)

# confusion matrix and error rate for the test data
dat.test$pred <- ifelse(predict(res, newdata=dat.test, type="response") > 0.5, "Up", "Down")
table(dat.test$pred, dat.test$Direction)
mean(dat.test$pred != dat.test$Direction)

# predicted probability of an Up for some given Lag1/Lag2 values
predict(res , newdata=data.frame(Lag1=c(1.2,1.5), Lag2=c(1.1,-0.8)), type="response")

# 4.7.3: Linear Discriminant Analysis

# load the MASS package
library(MASS)

# LDA using the training data
res <- lda(Direction ~ Lag1 + Lag2, data=dat.train)
res

# predicted class/probabilities for the test data
pred <- predict(res, newdata=dat.test)
head(pred$class)
head(pred$posterior)

# confusion matrix and error rate
table(pred$class, dat.test$Direction)
mean(pred$class != dat.test$Direction)

# QDA using the training data
res <- qda(Direction ~ Lag1 + Lag2, data=dat.train)
res

# predicted class/probabilities for the test data
pred <- predict(res, newdata=dat.test)
head(pred$class)
head(pred$posterior)

# confusion matrix and error rate
table(pred$class, dat.test$Direction)
mean(pred$class != dat.test$Direction)

# 4.7.5: Naive Bayes

# install the e1071 package
install.packages("e1071")

# load the e1071 package
library(e1071)

# naive Bayes using the training data
res <- naiveBayes(Direction ~ Lag1 + Lag2, data=dat.train)
res

# predicted class for the test data
pred <- predict(res, newdata=dat.test)
head(pred)

# confusion matrix and error rate
table(pred, dat.test$Direction)
mean(pred != dat.test$Direction)

# predicted probabilities for the test data
pred <- predict(res, newdata=dat.test, type="raw")
head(pred)

# 4.7.6: K-Nearest Neighbors

# install the class package
install.packages("class")

# load the class package
library(class)

# knn using the training data with k=1
predvars <- c("Lag1", "Lag2")
pred <- knn(dat.train[predvars], dat.test[predvars], dat.train$Direction, k=1)
head(pred)

# confusion matrix and error rate
table(pred, dat.test$Direction)
mean(pred != dat.test$Direction)

# knn using the training data with k=3
pred <- knn(dat.train[predvars], dat.test[predvars], dat.train$Direction, k=3)

# confusion matrix and error rate
table(pred, dat.test$Direction)
mean(pred != dat.test$Direction)

# knn using the Caravan dataset

dat <- Caravan

# standardize all variables (except for the class variable)
which(names(dat) == "Purchase")
dat[-86] <- scale(dat[-86])

# create the training and the test dataset
dat.train <- dat[1001:nrow(dat),]
dat.test  <- dat[1:1000,]

# knn using the training data with k=1
set.seed(1)
pred <- knn(dat.train[-86], dat.test[-86], dat.train$Purchase, k=1)

# confusion matrix and error rate
table(pred, dat.test$Purchase)
mean(pred != dat.test$Purchase)

# knn using the training data with k=3
pred <- knn(dat.train[-86], dat.test[-86], dat.train$Purchase, k=3)

# confusion matrix and error rate
table(pred, dat.test$Purchase)
mean(pred != dat.test$Purchase)

# knn using the training data with k=5
pred <- knn(dat.train[-86], dat.test[-86], dat.train$Purchase, k=5)

# confusion matrix and error rate
table(pred, dat.test$Purchase)
mean(pred != dat.test$Purchase)

# logistic regression
res <- glm(Purchase ~ ., data=dat.train, family=binomial)

# confusion matrix and error rate for the test data
dat.test$pred <- ifelse(predict(res, newdata=dat.test, type="response") > 0.25, "Yes", "No")
table(dat.test$pred, dat.test$Purchase)
mean(dat.test$pred != dat.test$Purchase)

# 4.7.7 Poisson Regression

# see above!

############################################################################

### 4.8: Exercises

# skipped

############################################################################

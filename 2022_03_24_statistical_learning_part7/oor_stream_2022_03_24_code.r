############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-03-24
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 3.4 - 4.3.4
#
# last updated: 2022-03-25

############################################################################

### 3.4: The Marketing Plan

# the data used in this section: https://www.statlearning.com/s/Advertising.csv
#download.file("https://www.statlearning.com/s/Advertising.csv", destfile="Advertising.csv")

# read in the data and examine the first 6 rows
dat <- read.csv("Advertising.csv")

# 1. Is there a relationship between sales and advertising budget?

# fit the model with all three predictors
res <- lm(sales ~ TV + radio + newspaper, data=dat)
summary(res)

# 2. How strong is the relationship?

# R^2 value for the model
summary(res)$r.squared

# MSE value for the model
sigma(res)

# how to determine the contribution of each predictor

# one possibility is to examine the R^2 value from the model with versus the
# model without a particular predictor

# fit the model without the TV predictor
res0 <- lm(sales ~ radio + newspaper, data=dat)

# compare R^2 values from these two models
summary(res0)$r.squared
summary(res)$r.squared

# the R^2 value is 0.56 points higher
summary(res)$r.squared - summary(res0)$r.squared

# the R^2 value is ~1.70 times (or 70%) higher
(summary(res)$r.squared - summary(res0)$r.squared) / summary(res0)$r.squared

# can repeat this for each predictor; however, the R^2 values from the various
# sub-models will not add up to the R^2 from the full model (here, the sum is
# way too large and even larger than 1)
res01 <- lm(sales ~      radio + newspaper, data=dat)
res02 <- lm(sales ~ TV +         newspaper, data=dat)
res03 <- lm(sales ~ TV + radio,             data=dat)
summary(res01)$r.squared + summary(res02)$r.squared + summary(res03)$r.squared
summary(res)$r.squared

# another commonly used approach is to look at the squared semi-partial
# correlation between each predictor and the outcome

# install and load package ppcor
#install.packages("ppcor")
library(ppcor)

# compute the squared semi-partial correlation between each predictor and the outcome
spcor(dat[c("sales", "TV", "radio", "newspaper")])$estimate[1,-1]^2

# however, these also do not add up to the R^2 from the full model
sum(spcor(dat[c("sales", "TV", "radio", "newspaper")])$estimate[1,-1]^2)
summary(res)$r.squared

# there are several measures that do sum to the R^2 from the full model, which
# we can compute with the relaimpo package

# install and load package relaimpo
#install.packages("relaimpo")
library(relaimpo)

# the default is to compute the 'lmg' measure
calc.relimp(res)

# note that these relative importance values do add up to the full R^2
sum(calc.relimp(res)$lmg)
summary(res)$r.squared

# package dominanceanalysis also provides this

# install and load package dominanceanalysis
#install.packages("dominanceanalysis")
library(dominanceanalysis)
dominanceAnalysis(res)
sum(dominanceAnalysis(res)$contribution.average$r2)
summary(res)$r.squared

# 3. Which media are associated with sales?

# examine the individual tests
summary(res)

# 4. How large is the association between each medium and sales?

# get 95% CIs for the model coefficients
confint(res)

# get the variance inflation factors
#install.packages("car")
library(car)
vif(res)

# 5. How accurately can we predict future sales?

# visualize the marginal relationship between TV and sales
plot(sales ~ TV, data=dat, pch=19)

# add the marginal regression line (with 95% CI) to the plot
TVvals <- seq(0, 300, length=100)
pred <- predict(res, newdata=data.frame(TV = TVvals,
                                        radio = mean(dat$radio),
                                        newspaper = mean(dat$newspaper)),
                interval="confidence")
pred <- as.data.frame(pred)
lines(TVvals, pred$fit, lwd=5, col="red")
lines(TVvals, pred$lwr, lwd=3, col="red", lty="dotted")
lines(TVvals, pred$upr, lwd=3, col="red", lty="dotted")
pred <- predict(res, newdata=data.frame(TV = TVvals,
                                        radio = mean(dat$radio),
                                        newspaper = mean(dat$newspaper)),
                interval="prediction")
pred <- as.data.frame(pred)
lines(TVvals, pred$lwr, lwd=3, col="blue", lty="dotted")
lines(TVvals, pred$upr, lwd=3, col="blue", lty="dotted")

# 6. Is the relationship linear?

# look at various diagnostic plots for the model
par(mfrow=c(2,2))
plot(res)
par(mfrow=c(1,1))

# 7. Is there synergy among the advertising media?

# allow an interaction between TV and radio
res <- lm(sales ~ TV * radio + newspaper, data=dat)
summary(res)

############################################################################

### 3.5: Comparison of Linear Regression with K-Nearest Neighbors

# function to do a KNN fit

knn <- function(x0, X, y, k) {

   p0 <- length(x0)
   p  <- ncol(X)

   if (p0 != p)
      stop("Length of x0 (", p0, ") does not match number of columns in X (", p, ").")

   k <- round(k)
   n <- length(y)

   if (k <= 0 || k > n)
      stop("k must be >= 1 and <= n.")

   if (nrow(X) != n)
      stop("Length of y (", n, ") does not match number of rows in X (", nrow(X), ").")

   d <- apply(X, 1, function(xi) sqrt(sum((xi - x0)^2)))
   y.k <- y[order(d)[1:k]]
   pred <- mean(y.k)

   if (k == 1) {
      ci.lb <- NA
      ci.ub <- NA
   } else {
      # compute a confidence interval for the mean (only when k >= 2)
      # https://en.wikipedia.org/wiki/Confidence_interval#Example
      sd <- sd(y.k)
      crit <- qt(.975, df=k-1, lower.tail=TRUE)
      ci.lb <- pred - crit * sd / sqrt(k)
      ci.ub <- pred + crit * sd / sqrt(k)
   }

   #if (k == 2)
   #   warning("Confidence interval for k=2 is not trustworthy.")

   return(c(pred=pred, ci.lb=ci.lb, ci.ub=ci.ub))

}

# predicted sales when TV=70, radio=20, newspaper=30 based on KNN with k=5
knn(x0=c(70, 20, 30), X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=5)

# predicted sales when TV=70, radio=20, newspaper=30 based on KNN with k=2
knn(x0=c(70, 20, 30), X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=2)

# fit the model with all three predictors and get the predicted value
res <- lm(sales ~ TV + radio + newspaper, data=dat)
predict(res, newdata=data.frame(TV=70, radio=20, newspaper=30), interval="confidence")

# compare the performance of KNN (based on various values of k) with
# regression in the training data
res.knn.1 <- apply(dat[c("TV", "radio", "newspaper")], 1, function(xi) {
             knn(x0=xi, X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=1)})
res.knn.2 <- apply(dat[c("TV", "radio", "newspaper")], 1, function(xi) {
             knn(x0=xi, X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=2)})
res.knn.5 <- apply(dat[c("TV", "radio", "newspaper")], 1, function(xi) {
             knn(x0=xi, X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=5)})
res.knn.10 <- apply(dat[c("TV", "radio", "newspaper")], 1, function(xi) {
              knn(x0=xi, X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=10)})
res.knn.50 <- apply(dat[c("TV", "radio", "newspaper")], 1, function(xi) {
              knn(x0=xi, X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=50)})
res.knn.100 <- apply(dat[c("TV", "radio", "newspaper")], 1, function(xi) {
               knn(x0=xi, X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=100)})
res.knn.200 <- apply(dat[c("TV", "radio", "newspaper")], 1, function(xi) {
               knn(x0=xi, X=dat[c("TV", "radio", "newspaper")], y=dat$sales, k=200)})

# get the MSE for the KNN fits with varying k
mse.knn <- c(sd(dat$sales - res.knn.1[1,]),
             sd(dat$sales - res.knn.2[1,]),
             sd(dat$sales - res.knn.5[1,]),
             sd(dat$sales - res.knn.10[1,]),
             sd(dat$sales - res.knn.50[1,]),
             sd(dat$sales - res.knn.100[1,]),
             sd(dat$sales - res.knn.200[1,]))

# plot 1/k versus the MSE values from KNN
plot(1/c(1,2,5,10,50,100,200), mse.knn, pch=19, type="o", xlab="1/k (flexibility)")

# add the MSE from the regression model to the plot
abline(h = sd(resid(res)), lwd=3, lty="dotted")

# as we increase the flexibility of KNN, it eventually starts to outperform
# the regression model, but keep in mind that we are only looking at the
# performance in the training data; to figure out the performance in a new
# dataset (i.e., in a test dataset), we would either need to get new data or
# we could start doing things like cross-validation (which will be discussed
# in chapter 5)

############################################################################

### 3.6: Lab: Linear Regression

# skipped because we have essentially covered everything in the lab already

############################################################################

### 3.7: Exercises

# skipped because we have covered everything in chapter 3 very thoroughly

############################################################################

### 4.1: An Overview of Classification

# install the ISLR2 package
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# copy the Default dataset to dat
dat <- Default

# look at the documentation of the dataset
help(Default)

# set up the plotting device with one row, three columns, where the first
# column takes up 50% of the space and the next two columns 25% each
layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow=TRUE), widths=c(.5,.25,.25))
#layout.show(3)

# scatterplot of income versus balance with different colors and symbols for
# individuals that defaulted versus those that did not
plot(income ~ balance, data=dat,
     col = ifelse(default == "Yes", "darkorange4", "skyblue3"),
     pch = ifelse(default == "Yes", 3, 1))

# boxplots for balance and income versus default
boxplot(balance ~ default, data=dat, col = c("skyblue3", "darkorange3"))
boxplot(income ~ default, data=dat,  col = c("skyblue3", "darkorange3"))

# reset layout
par(mfrow=c(1,1))

# create a dummy variable which is 1 when default is "Yes" and 0 otherwise
dat$default01 <- ifelse(dat$default == "Yes", 1, 0)

# observed proportion of individuals that defaulted
mean(dat$default01)

# fit a simple linear regression model predicting default01 from balance
res <- lm(default01 ~ balance, data=dat)
summary(res)

# scatterplot of default01 versus balance and add the regression line based on
# the model (like Figure 4.2, left panel)
plot(default01 ~ balance, data=dat, col="orange2", pch="|",
     xlab="Balance", ylab="Probability of Default")
abline(h=c(0,1), lty="dashed")
abline(res, lwd=8, col="skyblue3")

# mean estimated proportion based on the model (which is equal to the observed
# proportion of individuals that defaulted)
mean(fitted(res))

############################################################################

### 4.3: Logistic Regression

# fit logistic regression model
res <- glm(default ~ balance, data=dat, family=binomial)
summary(res)

# scatterplot of default01 versus balance and add the regression line based on
# the model (like Figure 4.2, right panel)
plot(default01 ~ balance, data=dat, col="orange2", pch="|",
     xlab="Balance", ylab="Probability of Default")
abline(h=c(0,1), lty="dashed")
balvals <- seq(0, 2700, length=500)
pred <- predict(res, newdata=data.frame(balance = balvals), type="response")
lines(balvals, pred, lwd=8, col="skyblue3")

# mean estimated proportion based on the model (which is equal to the observed
# proportion of individuals that defaulted)
mean(fitted(res))

############################################################################

### 4.3.2: Estimating the Regression Coefficients

# compute the likelihood for some values of beta0 and beta1
b0 <- -10
b1 <- 0.005
pred <- exp(b0 + b1 * dat$balance) / (1 + exp(b0 + b1 * dat$balance))
prod(pred[dat$default == "Yes"]) * prod(1 - pred[dat$default == "No"])

# the problem with computing the likelihood this way is that it gets really
# really small and looks essentially like 0; instead, we can take the log of
# the likelihood (recall: log(a * b) = log(a) + log(b))
sum(log(pred[dat$default == "Yes"])) + sum(log(1 - pred[dat$default == "No"]))

# by manipulating the values of b0 and b1, we can try to manually find those
# values that maximize the log likelihood; glm() found the following estimates
coef(res)

# compute the log likelihood with these values
b0 <- coef(res)[1]
b1 <- coef(res)[2]
pred <- exp(b0 + b1 * dat$balance) / (1 + exp(b0 + b1 * dat$balance))
sum(log(pred[dat$default == "Yes"])) + sum(log(1 - pred[dat$default == "No"]))

# same as what glm() found
logLik(res)

# estimated coefficients from the logistic regression model, with standard
# errors, test statistics (z-values), and p-values (Table 4.1)
round(coef(summary(res)), 4)

############################################################################

### 4.3.3: Making Predictions

# predicted probability of defaulting when balance is 1000 or 2000
predict(res, newdata=data.frame(balance = 1000), type="response")
predict(res, newdata=data.frame(balance = 2000), type="response")

# can also include categorical predictors in a logistic regression model
contrasts(dat$student)
res <- glm(default ~ student, data=dat, family=binomial)
summary(res)

# predicted probability of defaulting for students and non-students
predict(res, newdata=data.frame(student = "Yes"), type="response")
predict(res, newdata=data.frame(student = "No"),  type="response")

############################################################################

### 4.3.4: Multiple Logistic Regression

# include balance, income, and students as predictors in the model (Table 4.3)
res <- glm(default ~ balance + income + student, data=dat, family=binomial)
summary(res)

# or use this to just get the table with rounded values
round(coef(summary(res)), 4)

# note: in the book, the authors re-scaled the income variable so that one
# unit corresponds to $1000 (this way, the coefficient for income is not so
# small; but note that this does not impact the z- and p-value)
res <- glm(default ~ balance + I(income/1000) + student, data=dat, family=binomial)
round(coef(summary(res)), 4)

# predicted probability of defaulting when balance = 1500 and income = 40000
# for students and non-students
predict(res, newdata=data.frame(balance = 1500, income=40000, student="Yes"), type="response")
predict(res, newdata=data.frame(balance = 1500, income=40000, student="No"), type="response")

############################################################################

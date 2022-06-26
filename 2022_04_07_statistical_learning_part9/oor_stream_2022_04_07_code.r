############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-04-07
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 4.4.3 - 4.6.1
#
# last updated: 2022-04-29

############################################################################

### 4.4.3: Quadratic Discriminant Analysis

# install the ISLR2 package
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# copy the Default dataset to dat
dat <- Default

# load MASS package
library(MASS)

# look at the documentation of the qda() function
help(qda)

# do LDA using balance and student as predictors for 'default' (yes/no)
res1 <- lda(default ~ balance + student, data=dat)
res1

# do QDA using balance and student as predictors for 'default' (yes/no)
res2 <- qda(default ~ balance + student, data=dat)
res2

# get predictions based on LDA and QDA for the training data
pred1 <- predict(res1)
pred2 <- predict(res2)

# confusion matrices for LDA and QDA
addmargins(table(pred1$class, dat$default), margin=c(1,2))
addmargins(table(pred2$class, dat$default), margin=c(1,2))

# error rate of LDA and QDA in the training data
100 * mean(pred1$class != dat$default)
100 * mean(pred2$class != dat$default)

# install and load package pROC
#install.packages(pROC)
library(pROC)

# draw the ROC curve based on LDA
sav1 <- roc(default ~ pred1$posterior[,"Yes"], data=dat)
plot(sav1, lwd=3, col="blue")

# add the curve for QDA to the plot
sav2 <- roc(default ~ pred2$posterior[,"Yes"], data=dat)
plot(sav2, lwd=3, add=TRUE, col="red")

# add legend
legend("top", inset=.06, lty="solid", col=c("blue","red"),
       lwd=3, legend=c("LDA","QDA"), bty="n")

# compute the area under the curve for both methods
auc(sav1)
auc(sav2)

############################################################################

### 4.4.4: Naive Bayes

# install and load the mvtnorm package
#install.packages("mvtnorm")
library(mvtnorm)

# compute the density for p=2 predictors where x1=1 and x2=2 for a bivariate
# normal distribution with true means equal to 0 for both variables, true
# variances equal to 1 for both variables, and true correlation equal to 0.7
dmvnorm(c(1,2), mean=c(0,0), sigma=matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))

# same, but now assume the true means are 0 and 2 for the two variables
dmvnorm(c(1,2), mean=c(0,2), sigma=matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))

# show for the case of a bivariate normal distribution that f(x1,x2) is equal
# to f(x1) * f(x2) when the two variables are independent (i.e., this is a
# demonstration of equation 4.29)
dmvnorm(c(1,2), mean=c(0,0), sigma=matrix(c(1,0,0,1), nrow=2, ncol=2))
dnorm(1, mean=0, sd=1) * dnorm(2, mean=0, sd=1)

############################################################################

# simulate data like in the example on page 156

set.seed(1234)

n1 <- 10000
n2 <- 10000

x11 <- rnorm(n1, mean=0, sd=1)
x12 <- rnorm(n1, mean=2, sd=0.5)
x13 <- sample(1:3, n1, replace=TRUE, prob=c(4/18, 6/18, 8/18))

x21 <- rnorm(n1, mean=-1.8, sd=0.75)
x22 <- rnorm(n1, mean=0, sd=1)
x23 <- sample(1:3, n2, replace=TRUE, prob=c(11/18, 1/18, 6/18))

# histograms of variables x1 and x2 and barplot of x3 in each class (Figure 4.10)

par(mfrow=c(2,3))

hist(x11, xlab="", xlim=c(-4,4), breaks=seq(-5,5,by=.5), col="seagreen3")
hist(x12, xlab="", xlim=c(-4,4), breaks=seq(-5,5,by=.5), col="skyblue2")
barplot(table(x13), col="darkorange2", main="Barplot of x13", space=2)

hist(x21, xlab="", xlim=c(-4,4), breaks=seq(-5,5,by=.5), col="seagreen3")
hist(x22, xlab="", xlim=c(-4,4), breaks=seq(-5,5,by=.5), col="skyblue2")
barplot(table(x23), col="darkorange2", main="Barplot of x23", space=2)

par(mfrow=c(1,1))

# compute pi_1 * f_1(x1=0.4) * f_1(x2=1.5) * f_1(x3=1) (for class 1)
f1 <- n1 / (n1 + n2) *
dnorm(0.4, mean=mean(x11), sd=sd(x11)) *
dnorm(1.5, mean=mean(x12), sd=sd(x12)) *
mean(x13 == 1)

# compute pi_2 * f_2(x1=0.4) * f_2(x2=1.5) * f_2(x3=1) (for class 2)
f2 <- n2 / (n1 + n2) *
dnorm(0.4, mean=mean(x21), sd=sd(x21)) *
dnorm(1.5, mean=mean(x22), sd=sd(x22)) *
mean(x23 == 1)

# posterior probability for class 1 and class 2
f1 / (f1 + f2)
f2 / (f1 + f2)

# note: cannot reproduce the values on page 156 exactly, since we not know the
# exact values of the distributions from which the authors simulated the data
# (and there would also be a discrepancy simply due to chance)

############################################################################

# naive Bayes for the 'Default' dataset

# copy the Default dataset to dat
dat <- Default

# do LDA using balance and student as predictors for 'default' (yes/no)
res <- lda(default ~ balance + student, data=dat)
res

# confusion matrices for LDA (Table 4.4)
pred <- predict(res)
addmargins(table(pred$class, dat$default), margin=c(1,2))

# function to do naive Bayes for this example

nb <- function(x1, x2, x3, outcome, balance, student, income) {

   f.y <- mean(outcome == "Yes") *
          dnorm(x1, mean=mean(balance[outcome == "Yes"]), sd=sd(balance[outcome == "Yes"])) *
          mean(student[outcome == "Yes"] == x2) *
          dnorm(x3, mean=mean(income[outcome == "Yes"]), sd=sd(income[outcome == "Yes"]))

   f.n <- mean(outcome == "No") *
          dnorm(x1, mean=mean(balance[outcome == "No"]), sd=sd(balance[outcome == "No"])) *
          mean(student[outcome == "No"] == x2) *
          dnorm(x3, mean=mean(income[outcome == "No"]), sd=sd(income[outcome == "No"]))

   post.y <- f.y / (f.y + f.n)
   post.n <- f.n / (f.y + f.n)

   return(c(post.y, post.n))

}

head(dat)

# posterior probabilities based on naive Bayes for the first subject
nb(x1=729.5265, x2="No", x3=44361.625, outcome=dat$default, balance=dat$balance, student=dat$student, income=dat$income)

# compute posterior probabilities for all subjects

dat$post.y <- NA
dat$post.n <- NA

for (i in 1:nrow(dat)) {
   sav <- nb(x1=dat$balance[i], x2=dat$student[i], x3=dat$income[i],
             outcome=dat$default, balance=dat$balance, student=dat$student, income=dat$income)
   dat$post.y[i] <- sav[1]
   dat$post.n[i] <- sav[2]
}

dat$pred <- ifelse(dat$post.y > 0.5, "Yes", "No")

head(dat)

# confusion matrix (Table 4.8)
addmargins(table(dat$pred, dat$default), margin=c(1,2))

# error rate
100 * mean(dat$pred != dat$default)

# note: in LDA (Table 4.4), the authors only used balance and student as
# predictors, but for applying naive Bayes, they also used income

# also add income for LDA
res <- lda(default ~ balance + student + income, data=dat)
pred <- predict(res)
addmargins(table(pred$class, dat$default), margin=c(1,2))
100 * mean(pred$class != dat$default)

# use a threshold of 0.2 for classifying when using naive Bayes (Table 4.9)
dat$pred <- ifelse(dat$post.y > 0.2, "Yes", "No")
addmargins(table(dat$pred, dat$default), margin=c(1,2))

############################################################################

### 4.5: A Comparison of Classification Methods

### 4.5.1: An Analytical Comparison

# this section is theoretical, so no code here

### 4.5.2: An Empirical Comparison

# let's try to replicate the results for scenario 1; note: we will leave out
# KNN-CV, since cross-validation has not been covered in the book yet

set.seed(1234)

# simulate the test data
n1.test <- 10000
n0.test <- 10000

x11.test <- rnorm(n1.test, 1, 1)
x12.test <- rnorm(n1.test, 1, 1)
x01.test <- rnorm(n0.test, 0, 1) # set sd to 2 for scenario 6
x02.test <- rnorm(n0.test, 0, 1) # set sd to 2 for scenario 6

y.test <- c(rep(1, n1.test), rep(0, n0.test))
X.test <- data.frame(x1 = c(x11.test, x01.test), x2 = c(x12.test, x02.test))

n1 <- 10 # set to 6 for scenario 6
n0 <- 10 # set to 6 for scenario 6

iter <- 100

errorrate.knn <- rep(NA, iter)
errorrate.lda <- rep(NA, iter)
errorrate.glm <- rep(NA, iter)
errorrate.nba <- rep(NA, iter)
errorrate.qda <- rep(NA, iter)

# function to do KNN-1

knn <- function(x, x1, x2, y) {
   d <- sqrt((x[1] - x1)^2 + (x[2] - x2)^2)
   y[which.min(d)]
}

# function for naive Bayes with two predictors

nb <- function(x1.test, x2.test, x1, x2, y) {

   f.0 <- mean(y == 0) *
          dnorm(x1.test, mean=mean(x1[y == 0]), sd=sd(x1[y == 0])) *
          dnorm(x2.test, mean=mean(x2[y == 0]), sd=sd(x2[y == 0]))

   f.1 <- mean(y == 1) *
          dnorm(x1.test, mean=mean(x1[y == 1]), sd=sd(x1[y == 1])) *
          dnorm(x2.test, mean=mean(x2[y == 1]), sd=sd(x2[y == 1]))

   post.0 <- f.0 / (f.1 + f.0)
   post.1 <- f.1 / (f.1 + f.0)

   return(cbind(post.0, post.1))

}

for (i in 1:iter) {

   # simulate the training data
   x11 <- rnorm(n1, 1, 1)
   x12 <- rnorm(n1, 1, 1)
   x01 <- rnorm(n0, 0, 1) # set sd to 2 for scenario 6
   x02 <- rnorm(n0, 0, 1) # set sd to 2 for scenario 6
   y <- c(rep(1, n1), rep(0, n0))
   x1 <- c(x11, x01)
   x2 <- c(x12, x02)

   # KNN-1
   pred <- apply(X.test, 1, function(x) knn(x, x1=x1, x2=x2, y=y))
   errorrate.knn[i] <- mean(pred != y.test)

   # LDA
   res <- lda(y ~ x1 + x2)
   pred <- predict(res, newdata=X.test)
   errorrate.lda[i] <- mean(pred$class != y.test)

   # logistic regression model
   res <- glm(y ~ x1 + x2, family=binomial)
   pred <- predict(res, newdata=X.test, type="response")
   errorrate.glm[i] <- mean(ifelse(pred > 0.5, 1, 0) != y.test)

   # naive Bayes
   pred <- nb(X.test[,1], X.test[,2], x1=x1, x2=x2, y=y)
   errorrate.nba[i] <- mean(ifelse(pred[,2] > 0.5, 1, 0) != y.test)

   # QDA
   res <- qda(y ~ x1 + x2)
   pred <- predict(res, newdata=X.test)
   errorrate.qda[i] <- mean(pred$class != y.test)

}

# show boxplots of the 100 error rates for each method (Figure 4.11, left panel)
boxplot(cbind(errorrate.knn, errorrate.lda, errorrate.glm, errorrate.nba, errorrate.qda),
        names=c("KNN-1", "LDA", "Logistic", "NBayes", "QDA"),
        col=c("skyblue", "seagreen", "seagreen", "pink", "darkorange"))

############################################################################

### 4.6: Generalized Linear Models

# look at help file for the Bikeshare dataset
help(Bikeshare)

# copy Bikeshare to dat
dat <- Bikeshare

# outcome: bikers
# categorical predictors: mnth, hr, weathersit
# dichotomous predictors: workingday
# quantitative predictor: temp

### 4.6.1: Linear Regression on the Bikeshare Data

# fit regression model (Table 4.10)
res <- lm(bikers ~ workingday + temp + weathersit + relevel(mnth, ref="Dec") + hr, data=dat)
round(coef(summary(res))[1:6,], digits=2)

# note: the authors do not indicate which level they made the reference level
# for the mnth and hr predictors; it seems like they used Dec(ember) for mnth,
# but for hr none of the levels yields the intercept as shown in Table 4.10

# plot coefficients for mnth (Figure 4.13, left panel)
b <- coef(res)
b <- c(b[grep("mnth", names(b))], 0)
plot(b, type="o", pch=19, col="blue", xlab="Month", ylab="Coefficient", xaxt="n")
axis(side=1, at=1:12, label=substr(levels(dat$mnth), 1, 1))
abline(h=0, lty="dotted")

# plot coefficients for hr (Figure 4.13, right panel)
b <- coef(res)
b <- c(0, b[grep("hr", names(b))])
plot(b, type="o", pch=19, col="blue", xlab="Hour", ylab="Coefficient", xaxt="n")
axis(side=1, at=0:24)
abline(h=0, lty="dotted")

# note: the reference level gets a coefficient of 0; since we do not know how
# the authors chose the reference level for hr, the y-axis values are not the
# same as in the figure, but the pattern is exactly the same

# addendum: the coding they used is explained in section 4.7.7

# percentage of fitted values below 0
round(100 * mean(predict(res) < 0), digits=1)

# plot of hr (jittered) versus bikers
plot(bikers ~ jitter(as.numeric(as.character(dat$hr))), data=dat, pch=19,
     cex=0.2, col="skyblue2", xlab="Hour", ylab="Number of Bikers")

# to diagnose potential problems with the regression model, we can use the
# built-in diagnostics plots that R provides
par(mfrow=c(2,2))
plot(res, cex=0.2, pch=19, col="skyblue2")
par(mfrow=c(1,1))

# plot of hr (jittered) versus log(bikers)
plot(log(bikers) ~ jitter(as.numeric(as.character(dat$hr))), data=dat, pch=19,
     cex=0.2, col="skyblue2", xlab="Hour", ylab="Log(Number of Bikers)")

# fit regression model with log(bikers) as outcome
res <- lm(log(bikers) ~ workingday + temp + weathersit + relevel(mnth, ref="Dec") + hr, data=dat)
round(coef(summary(res))[1:6,], digits=2)

# diagnostic plots
par(mfrow=c(2,2))
plot(res, cex=0.2, pch=19, col="skyblue2")
par(mfrow=c(1,1))

# by modeling log(bikers), the predicted (average) number of bikers is always
# going to be positive when we back-transform via exponentiation
100 * mean(exp(predict(res)) < 0)

############################################################################

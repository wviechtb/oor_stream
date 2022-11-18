############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-11-10
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 9.3 - 9.5
#
# last updated: 2022-11-18

############################################################################

### 9.3.1: Classification with Non-Linear Decision Boundaries

# data for Figure 9.8 (extracted with the juicr package)
dat <- structure(list(x1 = c(0.35, -0.31, -0.83, -0.63, -0.26, 0.77, 0.73,
0.48, 0.38, 0.57, 0.82, 1.59, 1.35, 1.12, 0.94, 0.91, 1.1, 1.51, 0.88, 0.69,
-0.17, -0.06, -0.12, -0.42, -0.7, -0.83, -0.71, -0.63, -0.49, 0.32, 0.38,
0.55, 0.41, 0.58, 0.61, 0.77, 0.76, -0.11, -0.07, -0.05, -0.02, 0.07, 0.17,
-0.16, -0.4, -0.06, -1.38, -1.47, -2, -2.22, -2.53, -2.82, -4.28, -4.23, -4.8,
-4.25, -3.94, -3.62, -3.55, -3.48, -3.31, -2.43, -2.44, -1.54, -1.42, -1.83,
-1.8, -2.64, -3.04, -2.71, -3.45, -3.37, -3.14, -2.42, 0.47, 1.42, 1.42, 0.86,
0.95, 1.25, 2.26, 3.42, 3.97, 3.15, 2.33, 2.33, 2.15, 1.29, 4.16, 4.39, 2.61,
2.39, 2.69, 2.68, 2.02, 2.07, 1.4, 1.85, 1.99, 3.05), x2 = c(2.09, 1.68, 1.77,
1.43, 1.17, 1.35, 0.91, 0.71, 0.53, 0.38, 0.5, 0.15, 0.05, -0.21, -0.27,
-0.51, -0.65, -0.64, -1.64, -1.67, -1.91, -1.53, -1.28, -1.53, -1.12, -0.91,
-0.76, -0.61, -0.68, -0.65, -0.46, -0.46, -0.33, -0.18, -0.1, -0.06, 0.02,
-0.59, -0.52, -0.39, -0.33, -0.22, 0.04, -0.08, -0.3, 0.71, 0.31, -0.04,
-0.17, -0.65, -0.69, -0.79, -1.56, -2.58, -3.25, -3.33, -3.07, -3.02, -3.18,
-3.38, -3.77, -4.38, -4.05, -4.42, -4.21, -2.78, -2.59, -3.43, -3.24, -3.03,
-2.22, -2, -2.02, -2.48, 1.53, 1.76, 1.99, 2.07, 2.37, 2.37, 2.07, 1.52, 1.92,
2.26, 2.68, 2.84, 2.86, 3.1, 3.21, 3.42, 3.46, 3.45, 3.83, 4.05, 3.89, 4.03,
4.2, 4.87, 5.07, 4.46), group = c("purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue",
"blue")), row.names = c(NA, -100L), class = "data.frame")

# create group factor variable (needed below by svm() function)
dat$fgroup <- factor(dat$group)

# draw figure 9.8
par(pty="s") # to generate a square plotting region
plot(dat$x1, dat$x2, col=ifelse(dat$group=="blue", "#30b5ff", "#c27ac0"),
     pch=19, cex=1.5, xlim=c(-5,5), ylim=c(-5,5), xlab="x1", ylab="x2")

# let's first try using a standard linear support vector classifier on these
# data (which is not going to work well at all!)

# install (if necessary) the e1071 package
#install.packges("e1071")

# load the e1071 package
library(e1071)

# fit linear support vector classifier
res.svm <- svm(fgroup ~ x1 + x2, data=dat, kernel="linear", scale=FALSE, cost=10)

# put circles around the support vectors
symbols(dat$x1[res.svm$index], dat$x2[res.svm$index], inches=FALSE,
        add=TRUE, circles=rep(.1,length(res.svm$index)), lwd=2)

# extract the rescaled coefficients
b0 <- coef(res.svm)[[1]] / sqrt(coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2)
b1 <- coef(res.svm)[[2]] / sqrt(coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2)
b2 <- coef(res.svm)[[3]] / sqrt(coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2)

# add the hyperplane for the support vector classifier to the plot
abline(a = -b0/b2, b = -b1/b2, lwd=3)

# determine which support vector is on the correct side of the hyperplane
# and that has the largest distance to the hyperplane and get its distance
# (based on this, we can determine the margin)
dist <- abs(b0 + b1*dat$x1 + b2*dat$x2)
maxdist <- dist == max(dist[1:nrow(dat) %in% res.svm$index & res.svm$fitted == dat$fgroup])
M <- dist[maxdist]

b0 <- coef(res.svm)[[1]]
b1 <- coef(res.svm)[[2]]
b2 <- coef(res.svm)[[3]]
M <- 1 / sqrt(b1^2 + b2^2)

# calculate value we need to add/subtract to the intercept for the margin
margin <- sqrt(1 + (b1/b2)^2) * M

# add the margin to the plot
abline(a = -b0/b2 - margin, b = -b1/b2, lwd=3, lty="dotted")
abline(a = -b0/b2 + margin, b = -b1/b2, lwd=3, lty="dotted")

# this is obviously a really bad classifier for these data

############################################################################

# now try adding x1^2 and x2^2 as additional predictors/features

# fit support vector classifier with these additional predictors
res.svm <- svm(fgroup ~ x1 + x2 + I(x1^2) + I(x2^2), data=dat, kernel="linear", scale=FALSE, cost=10)

# draw figure 9.8
par(pty="s") # to generate a square plotting region
plot(dat$x1, dat$x2, col=ifelse(dat$group=="blue", "#30b5ff", "#c27ac0"),
     pch=19, cex=1.5, xlim=c(-5,5), ylim=c(-5,5), xlab="x1", ylab="x2")

# extract the coefficients for the hyperplane
b0 <- coef(res.svm)[[1]]
b1 <- coef(res.svm)[[2]]
b2 <- coef(res.svm)[[3]]
b3 <- coef(res.svm)[[4]]
b4 <- coef(res.svm)[[5]]

# create grid of x1 and x2 points
x1x2grid <- expand.grid(x1=seq(-6,6,length=501), x2=seq(-6,6,length=501))

# compute the distance to the hyperplane for each row in x1x2grid
x1x2grid$dist <- apply(x1x2grid, 1, function(x) b0 + b1*x[1] + b2*x[2] + b3*x[1]^2 + b4*x[2]^2)

# superimpose the points of the grid with colors corresponding to the predictions
points(x1x2grid[,1], x1x2grid[,2], pch=19, cex=0.1,
       col=ifelse(x1x2grid$dist > 0, "#c27ac0", "#30b5ff"))

# redraw the points
points(dat$x1, dat$x2, col=ifelse(dat$group=="blue", "#30b5ff", "#c27ac0"), pch=19, cex=1.5)

# put circles around the support vectors
symbols(dat$x1[res.svm$index], dat$x2[res.svm$index], inches=FALSE,
        add=TRUE, circles=rep(.1,length(res.svm$index)), lwd=2)

# this works really well here!

############################################################################

# 9.3.2: The Support Vector Machine

# try to reproduce the SVMs as shown in Figure 9.9

# polynomial kernel with degree 3 (and set coef0 to 1 as in equation 9.22)
res.svm <- svm(fgroup ~ x2 + x1, data=dat, kernel="polynomial", degree=3, coef0=1)
plot(res.svm, dat[c("x1","x2","fgroup")], col=c("#30b5ff","#c27ac0"), grid=500)

# radial kernel (when using the default value of gamma, the decision boundary
# looked different than in Figure 9.9; it seems that using a larger value of
# gamma leads to something that look more like Figure 9.9)
res.svm <- svm(fgroup ~ x2 + x1, data=dat, kernel="radial", gamma=10)
plot(res.svm, dat[c("x1","x2","fgroup")], col=c("#30b5ff","#c27ac0"), grid=500)

############################################################################

# 9.3.3: An Application to the Heart Disease Data

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

# turn AHD into a factor
dat$AHD <- factor(dat$AHD)

# turn the other string variables into factors
dat$ChestPain <- factor(dat$ChestPain)
dat$Thal      <- factor(dat$Thal)

# create the training and the test dataset
set.seed(1)
id.train  <- sample(nrow(dat), 207)
dat.train <- dat[id.train,]
dat.test  <- dat[-id.train,]

# load MASS package (for LDA)
library(MASS)

# linear discriminant analysis (LDA) with the training data
res.lda <- lda(AHD ~ ., data=dat.train)

# get predictions for the training data
pred.lda <- predict(res.lda)$posterior[,"Yes"] # probability of 'Yes'
pred <- ifelse(pred.lda > 0.5, "Yes", "No")    # dichotomize at a 0.5 cutoff

# confusion matrix
table(dat.train$AHD, pred)

# sensitivity (probability that pred is Yes among those who really have AHD)
sum(dat.train$AHD == "Yes" & pred == "Yes") / sum(dat.train$AHD == "Yes")

# specificity (probability that pred is No among those who do not have AHD)
sum(dat.train$AHD == "No" & pred == "No") / sum(dat.train$AHD == "No")

# manually create the ROC curve
# https://en.wikipedia.org/wiki/Receiver_operating_characteristic

cutoffs <- seq(0, 1, length=1000)
sens <- rep(NA, length(cutoffs))
spec <- rep(NA, length(cutoffs))

for (i in 1:length(cutoffs)) {
   pred <- ifelse(pred.lda > cutoffs[i], "Yes", "No")
   sens[i] <- sum(dat.train$AHD == "Yes" & pred == "Yes") / sum(dat.train$AHD == "Yes")
   spec[i] <- sum(dat.train$AHD == "No" & pred == "No") / sum(dat.train$AHD == "No")
}

# Figure 9.10 (left panel)
plot(1-spec, sens, lwd=3, type="l", col="blue", xlab="False positive rate",
     ylab="True positive rate")
abline(a=0, b=1, lty="dotted")

# fit linear support vector classifier
res.svm <- svm(AHD ~ ., data=dat.train, kernel="linear")

# get predictions for the training data (here, pred.svm are the fitted values
# that indicate the distance of the points to the decision boundary)
pred.svm <- c(attributes(predict(res.svm, newdata=dat.train, decision.values=TRUE))$decision.values)
cutoffs <- seq(min(pred.svm), max(pred.svm), length=1000)

sens <- rep(NA, length(cutoffs))
spec <- rep(NA, length(cutoffs))

for (i in 1:length(cutoffs)) {
   pred <- ifelse(pred.svm > cutoffs[i], "Yes", "No")
   sens[i] <- sum(dat.train$AHD == "Yes" & pred == "Yes") / sum(dat.train$AHD == "Yes")
   spec[i] <- sum(dat.train$AHD == "No" & pred == "No") / sum(dat.train$AHD == "No")
}

# add the line for the ROC curve for the support vector classifier to the plot
lines(1-spec, sens, lwd=3, type="l", col="red")

############################################################################

# in the book (section 9.6.3), the authors suggest the ROCR package for
# creating ROC curves; I personally like the pROC package for that purpose)

# install (if necessary) the pROC package
#install.packges("pROC")

library(pROC)

# Figure 9.10 (left panel)

plot(roc(AHD ~ pred.lda, data=dat.train), lwd=3, col="blue",
     xlab="False positive rate", ylab="True positive rate")
plot(roc(AHD ~ pred.svm, data=dat.train), lwd=3, col="red", add=TRUE)

legend("bottomright", inset=.02, lty="solid", lwd=3, col=c("red", "blue"),
       legend=c("Support Vector Classifier", "Linear Discriminant Analysis"))

############################################################################

# Figure 9.10 (right panel)

plot(roc(AHD ~ pred.svm, data=dat.train), lwd=3, col="red",
     xlab="False positive rate", ylab="True positive rate")

# support vector machine (radial kernel with gamma=10^-3)
res.svm <- svm(AHD ~ ., data=dat.train, kernel="radial", gamma=10^-3)
pred.svm <- c(attributes(predict(res.svm, newdata=dat.train, decision.values=TRUE))$decision.values)
plot(roc(AHD ~ pred.svm, data=dat.train), lwd=3, col="black", add=TRUE)

# support vector machine (radial kernel with gamma=10^-2)
res.svm <- svm(AHD ~ ., data=dat.train, kernel="radial", gamma=10^-2)
pred.svm <- c(attributes(predict(res.svm, newdata=dat.train, decision.values=TRUE))$decision.values)
plot(roc(AHD ~ pred.svm, data=dat.train), lwd=3, col="green", add=TRUE)

# support vector machine (radial kernel with gamma=10^-1)
res.svm <- svm(AHD ~ ., data=dat.train, kernel="radial", gamma=10^-1)
pred.svm <- c(attributes(predict(res.svm, newdata=dat.train, decision.values=TRUE))$decision.values)
plot(roc(AHD ~ pred.svm, data=dat.train), lwd=3, col="blue", add=TRUE)

legend("bottomright", inset=.02, lty="solid", lwd=3, col=c("red", "black", "green", "blue"),
       legend=c("Support Vector Classifier", expression("SVM:" ~ gamma==10^-3),
                expression("SVM:" ~ gamma==10^-2), expression("SVM:" ~ gamma==10^-1)))

############################################################################

# above we predicted the training data; now let's predict the test data

# linear discriminant analysis (LDA) with the training data
res <- lda(AHD ~ ., data=dat.train)

# get predictions for the test data
pred.lda <- predict(res.lda, newdata=dat.test)$posterior[,"Yes"] # probability of 'Yes'

# fit linear support vector classifier with the training data
res.svm <- svm(AHD ~ ., data=dat.train, kernel="linear")

# get predictions for the test data
pred.svm <- c(attributes(predict(res.svm, newdata=dat.test, decision.values=TRUE))$decision.values)

# Figure 9.11 (left panel)

plot(roc(AHD ~ pred.lda, data=dat.test), lwd=3, col="blue",
     xlab="False positive rate", ylab="True positive rate")
plot(roc(AHD ~ pred.svm, data=dat.test), lwd=3, col="red", add=TRUE)

legend("bottomright", inset=.02, lty="solid", lwd=3, col=c("red", "blue"),
       legend=c("Support Vector Classifier", "Linear Discriminant Analysis"))

############################################################################

# Figure 9.11 (right panel)

plot(roc(AHD ~ pred.svm, data=dat.test), lwd=3, col="red",
     xlab="False positive rate", ylab="True positive rate")

# support vector machine (radial kernel with gamma=10^-3)
res.svm <- svm(AHD ~ ., data=dat.train, kernel="radial", gamma=10^-3)
pred.svm <- c(attributes(predict(res.svm, newdata=dat.test, decision.values=TRUE))$decision.values)
plot(roc(AHD ~ pred.svm, data=dat.test), lwd=3, col="black", add=TRUE)

# support vector machine (radial kernel with gamma=10^-2)
res.svm <- svm(AHD ~ ., data=dat.train, kernel="radial", gamma=10^-2)
pred.svm <- c(attributes(predict(res.svm, newdata=dat.test, decision.values=TRUE))$decision.values)
plot(roc(AHD ~ pred.svm, data=dat.test), lwd=3, col="green", add=TRUE)

# support vector machine (radial kernel with gamma=10^-1)
res.svm <- svm(AHD ~ ., data=dat.train, kernel="radial", gamma=10^-1)
pred.svm <- c(attributes(predict(res.svm, newdata=dat.test, decision.values=TRUE))$decision.values)
plot(roc(AHD ~ pred.svm, data=dat.test), lwd=3, col="blue", add=TRUE)

legend("bottomright", inset=.02, lty="solid", lwd=3, col=c("red", "black", "green", "blue"),
       legend=c("Support Vector Classifier", expression("SVM:" ~ gamma==10^-3),
                expression("SVM:" ~ gamma==10^-2), expression("SVM:" ~ gamma==10^-1)))

############################################################################

# 9.6 Lab: Support Vector Machines

# most of the things that are covered in the lab have already been done above;
# however, we haven't done the cross-validation to obtain good values for the
# 'tuning parameters' in svm() (i.e., the 'cost' and 'gamma' values); also, it
# would be nice to compare svm() with logistic regression (as discussed in
# section 9.5); to do so, we will *repeatedly* split the data into training
# and test data, fit a logistic regression model, a support vector classifier,
# and a support vector machine to the training data (and use cross-validation
# to tune the latter two), and get the misclassification rate for the test
# data; then we can see how these different methods fare against each other on
# average (not just for a particular split); note that the code below takes
# quite a while to run

set.seed(1234)

iters <- 1000

errorrate.glm <- rep(NA, iters)
errorrate.svc <- rep(NA, iters)
errorrate.svm <- rep(NA, iters)

bestcost.svc <- rep(NA, iters)
bestcost.svm <- rep(NA, iters)
bestgamma.svm <- rep(NA, iters)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (i in 1:iters) {

   setTxtProgressBar(pbar, i)

   # create the training and the test dataset
   id.train  <- sample(nrow(dat), 207)
   dat.train <- dat[id.train,]
   dat.test  <- dat[-id.train,]

   # logistic regression
   res <- glm(AHD ~ ., data=dat.train, family=binomial)
   pred <- predict(res, newdata=dat.test, type="response") > 0.5
   errorrate.glm[i] <- mean(pred & dat.test$AHD == "No")

   # support vector classifier (with CV to tune the cost value)
   res <- tune(svm, AHD ~ ., data=dat.train, kernel="linear",
               ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
   errorrate.svc[i] <- mean(predict(res$best.model, newdata=dat.test) == "Yes" & dat.test$AHD == "No")
   bestcost.svc[i] <- res$best.parameters[[1]]

   # support vector machine (with CV to tune the cost and gamma values)
   res <- tune(svm, AHD ~ ., data=dat.train, kernel="radial",
               ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                           gamma=c(0.0001, 0.001, 0.1, 0.5, 1, 2, 3, 4)))
   errorrate.svm[i] <- mean(predict(res$best.model, newdata=dat.test) == "Yes" & dat.test$AHD == "No")
   bestcost.svm[i] <- res$best.parameters[[1]]
   bestgamma.svm[i] <- res$best.parameters[[2]]

}

close(pbar)

# mean misclassification rate for the test data according to all three methods
mean(errorrate.glm) # 0.06847778
mean(errorrate.svc) # 0.06171111
mean(errorrate.svm) # 0.05951111

# create a boxplot to show these error rates

errors <- data.frame(errorrate.glm, errorrate.svc, errorrate.svm)

par(mar=c(5,11,2,2))
boxplot(errors, horizontal=TRUE, las=1, range=0, xlab="Test Error Rate",
        col=c("#1e90ffaa", "#b22222aa", "#228b22aa"), boxwex=0.6,
        names=c("Logistic Regression", "Support Vector Classifier", "Support Vector Machine"))
pts <- lapply(errors, jitter, amount=.02)
pts <- lapply(pts, pmax, 0)
stripchart(pts, add=TRUE, pch=21, cex=0.5, method="jitter", jitter=0.2,
           col=rgb(0,0,0,.2), bg=rgb(0,0,0,.1))

# also saved the cost and gamma values above; we can see that depending on the
# split, the 'best' values for these parameters can differ
bestcost.svc
bestcost.svm
bestgamma.svm

############################################################################

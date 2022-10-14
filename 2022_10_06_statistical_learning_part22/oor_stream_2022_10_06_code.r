############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-10-06
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 8.1.3 - 8.2.1
#
# last updated: 2022-10-13

############################################################################

# install (if necessary) the tree package
#install.packages("tree")

# load the tree package
library(tree)

### 8.1.3: Trees Versus Linear Models

# simulate some data like in Figure 8.7, top row

set.seed(1234)

n <- 500

x1 <- runif(n, -2, 2)
x2 <- runif(n, -2, 2)

beta0 <- 2
beta1 <- -2
beta2 <- 4
logodds <- beta0 + beta1 * x1 + beta2 * x2
probs <- plogis(logodds)
group <- rbinom(n, 1, probs)
col <- ifelse(group == 1, "green", "yellow2")

# plot the data
par(mfrow=c(2,2))
plot(x1, x2, pch=19, col=col, main="(a)")

# if logodds = beta0 + beta1*x1 + beta2*x2 = 0, then prob = 0.5, which defines
# the decision boundary; to draw the decision boundary, we rearrange this
# equation as follows:
#
# beta0 + beta1*x1 + beta2*x2 = 0
# beta2*x2 = -beta0 + -beta1*x1
# x2 = -beta0/beta2 + -beta1/beta2*x1
#
# so the line of the decision boundary has an intercept of -beta0/beta2 and a
# slope of -beta1/beta2
abline(a = -beta0/beta2, b = -beta1/beta2, lwd=3)

# fit logistic regression model
res <- glm(group ~ x1 + x2, family=binomial)
summary(res)

# predicted group based on predicted probability being larger than 0.5
pred <- ifelse(predict(res, type="response") > 0.5, 1, 0)

# draw misclassified points in red
points(x1[pred != group], x2[pred != group], pch=19, col="red")

# cross-classification table
table(group, pred)

# build tree
res <- tree(factor(group) ~ x1 + x2)
summary(res)

# prune to the tree to a size of 5
ptree <- prune.misclass(res, best=5)
summary(ptree)
ptree

# plot the data
plot(x1, x2, pch=19, col=col, main="(b)")

# draw in the segments
segments(-3, 0.247988, 3, 0.247988, lwd=3)
segments(0.245022, -3, 0.245022, 0.247988, lwd=3)
segments(-3, -0.64379, 0.245022, -0.64379, lwd=3)
segments(-1.47192, -3, -1.47192, -0.64379, lwd=3)

# predicted group based on the tree
pred <- predict(ptree, type="class")

# draw misclassified points in red
points(x1[pred != group], x2[pred != group], pch=19, col="red")

# cross-classification table
table(group, pred)

############################################################################

# simulate some data like in Figure 8.7, bottom row

probs <- rep(.95, n)
probs[x1 > -1 & x2 < 1] <- .05

group <- rbinom(n, 1, probs)
col <- ifelse(group == 1, "green", "yellow2")

# plot the data
plot(x1, x2, pch=19, col=col, main="(c)")

# fit logistic regression model
res <- glm(group ~ x1 + x2, family=binomial)
summary(res)

# decision boundary based on the logistic regression model
abline(a = -coef(res)[1]/coef(res)[3], b = -coef(res)[2]/coef(res)[3], lwd=3)

# predicted group based on predicted probability being larger than 0.5
pred <- ifelse(predict(res, type="response") > 0.5, 1, 0)

# draw misclassified points in red
points(x1[pred != group], x2[pred != group], pch=19, col="red")

# cross-classification table
table(group, pred)

# build tree
res <- tree(factor(group) ~ x1 + x2)
summary(res)
res

# no need to prune it

# plot the data
plot(x1, x2, pch=19, col=col, main="(d)")

# draw in the segments
segments(-3, 1.06707, 3, 1.06707, lwd=3)
segments(-1.0081, -3,  -1.0081, 1.06707, lwd=3)

# predicted group based on the tree
pred <- predict(res, type="class")

# draw misclassified points in red
points(x1[pred != group], x2[pred != group], pch=19, col="red")

# cross-classification table
table(group, pred)

# fit logistic regression model with an interaction between x1 and x2
res <- glm(group ~ x1 * x2, family=binomial)
summary(res)
pred <- ifelse(predict(res, type="response") > 0.5, 1, 0)
table(group, pred)

# the decision boundary based on this model is defined again by the
# combination of x1 and x2 values where
#
# beta0 + beta1*x1 + beta2*x2 + beta3*x1*x2 = 0
#
# we can rearrange this equation as follows:
#
# beta2*x2 + beta3*x1*x2 = -beta0 + -beta1*x1
# (beta2 + beta3*x1)*x2 = -beta0 + -beta1*x1
# x2 = -beta0/(beta2 + beta3*x1) + -beta1/(beta2 + beta3*x1)*x1

# redraw the points in figure (c)
par(mfg=c(2,1))
points(x1, x2, pch=19, col=col)

# draw decision boundary as a dotted line into figure (c)
x1s <- seq(-1.2, 2, length=1000)
x2s <- -coef(res)[1]/(coef(res)[3] + coef(res)[4]*x1s) + -coef(res)[2]/(coef(res)[3] + coef(res)[4]*x1s)*x1s
lines(x1s, x2s, lty="dotted", lwd=3)

# draw misclassified points in red
points(x1[pred != group], x2[pred != group], pch=19, col="red")

############################################################################

### 8.2: Bagging, Random Forests, Boosting, and Bayesian Additive Regression Trees

### 8.2.1: Bagging

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
res <- tree(AHD ~ ., data=dat.train)
summary(res)

# predicted group based on the tree in the test data
pred <- predict(res, newdata=dat.test, type="class")

# error rate of this single tree
error.single <- mean(dat.test$AHD != pred)
error.single

# do bagging based on an increasing number of bootstrap samples (note that
# this takes a bit of time; on my laptop, around 3 minutes)

Bs <- 10:300

error.bagging <- rep(NA, length(Bs))

pbar <- txtProgressBar(min=0, max=length(Bs), style=3)

for (i in 1:length(Bs)) {

   setTxtProgressBar(pbar, i)

   pred <- matrix(NA, nrow=Bs[i], ncol=nrow(dat.test))

   for (j in 1:Bs[i]) {

      # take bootstrap sample from the training data
      boot.train <- dat.train[sample(nrow(dat.train), replace=TRUE),]

      # build tree using the bootstrap sample
      tmp <- tree(AHD ~ ., data=boot.train)

      # predicted group based on the tree in the test data
      pred[j,] <- predict(tmp, newdata=dat.test, type="class") == "Yes"

   }

   # take majority vote on the predictions
   pred <- apply(pred, 2, mean) > 0.5

   # error rate
   error.bagging[i] <- mean(pred != (dat.test$AHD == "Yes"))

}

close(pbar)

# plot error rate from bagging against number of trees (Figure 8.8)
par(mfrow=c(1,1))
plot(Bs, error.bagging, xlab="Number of Trees", ylab="Error",
     ylim=c(.1,.3), type="l", col="gray70")

# add a smoother
lines(Bs, fitted(loess(error.bagging ~ Bs)), lwd=3)

# add the error rate from the single tree model to the figure
abline(h = error.single, lty="dashed", lwd=2)

############################################################################

# exercise 2 from 5.4

# determine the probability that a particular person will be included in a
# bootstrap sample through simulation
n <- 1000000
id <- 1:n
id.boot <- sample(n, replace=TRUE)
mean(id %in% id.boot)

# a) The probability that a particular person is chosen is 1/n. Hence, the
#    probability that a particular person is not chosen is 1 - 1/n.
# b) Since we are sampling with replacement, whatever happens for the first
#    bootstrap observation has no influence on what happens for the second
#    observations. Therefore, the probability is also 1 - 1/n.
# c) Since each draw is independent, we can just multiply these probabilities
#    n times, which yields (1 - 1/n)^n.
# d) 1 - (1 - 1/5)^5 =~ 0.6723
# e) 1 - (1 - 1/100)^100 =~ 0.6340
# f) 1 - (1 - 1/10000)^10000 =~ 0.6321
# g) We can draw the plot with:

plot(1:100000, 1 - (1 - 1/(1:100000))^(1:100000), type="l")

#    The plot quickly converges to a fixed value. We can derive this point
#    analytically: lim n->infinity 1 - (1 - 1/n)^n = 1 - 1 / exp(1)

1 - 1 / exp(1)

############################################################################

# Out-of-Bag Error Estimation

# below we again use bagging on the classification trees, but instead of using
# the 'validation set' approach above (where we split the data into a training
# and a test dataset and then use the trees built in the training data to
# predict group membership in the test data), we predict group membership for
# those subjects not included in a particular bootstrap sample to estimate the
# 'out-of-bag' error rate

pbar <- txtProgressBar(min=0, max=length(Bs), style=3)

error.oob <- rep(NA, length(Bs))

for (i in 1:length(Bs)) {

   setTxtProgressBar(pbar, i)

   pred <- matrix(NA, nrow=Bs[i], ncol=nrow(dat))

   for (j in 1:Bs[i]) {

      # subject ids included in the bootstrap sample
      id.boot <- sample(nrow(dat), replace=TRUE)

      # take bootstrap sample
      dat.boot <- dat[id.boot,]

      # build tree using the bootstrap sample
      tmp <- tree(AHD ~ ., data=dat.boot)

      # predicted group based on the tree for those not used in the bootstrap sample
      pred[j,-id.boot] <- predict(tmp, newdata=dat[-id.boot,], type="class") == "Yes"

   }

   # take majority vote on the predictions
   pred <- apply(pred, 2, mean, na.rm=TRUE) > 0.5

   # error rate
   error.oob[i] <- mean(pred != (dat$AHD == "Yes"), na.rm=TRUE)

}

close(pbar)

# add the line for the out-of-bag error rates against the number of trees
lines(Bs, error.oob, col="palegreen2", lwd=1)

# add a smoother
lines(Bs, fitted(loess(error.oob ~ Bs)), lwd=3, col="forestgreen")

# add a legend
legend("bottomright", inset=.02, col=c("black","black","forestgreen"),
       lwd=3, legend=c("Single Tree", "Test: Bagging", "OOB: Bagging"),
       lty=c("dashed", "solid", "solid"))

############################################################################

# in the validation set approach, the single tree might perform better or
# worse than what we get from bagging; to examine whether bagging on average
# performs better than a single tree, we can repeat the process (of splitting
# the data into a training and test dataset) many times and calculate the
# average test error rate based on the single tree and bagging; below we do
# this for a single number of bootstrap samples (B=300) for 1000 iterations;
# note that this takes quite some time

set.seed(1234)

iters <- 1000

error.single.sim  <- rep(NA, iters)
error.bagging.sim <- rep(NA, iters)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (i in 1:iters) {

   setTxtProgressBar(pbar, i)

   id.train  <- sample(nrow(dat), round(nrow(dat)/2))
   dat.train <- dat[id.train,]
   dat.test  <- dat[-id.train,]

   res <- tree(AHD ~ ., data=dat.train)
   pred <- predict(res, newdata=dat.test, type="class")
   error.single.sim[i] <- mean(dat.test$AHD != pred)

   pred <- matrix(NA, nrow=300, ncol=nrow(dat.test))

   for (j in 1:300) {
      boot.train <- dat.train[sample(nrow(dat.train), replace=TRUE),]
      tmp <- tree(AHD ~ ., data=boot.train)
      pred[j,] <- predict(tmp, newdata=dat.test, type="class") == "Yes"
   }

   pred <- apply(pred, 2, mean) > 0.5
   error.bagging.sim[i] <- mean(pred != (dat.test$AHD == "Yes"))

}

close(pbar)

# mean test error based on single trees and based on bagging
mean(error.single.sim)
mean(error.bagging.sim)

# add these means to the plot as points when B=300
points(300, mean(error.single.sim),  pch=21) # unfilled point
points(300, mean(error.bagging.sim), pch=19) # filled point

# add another legend
legend("topright", inset=.02, pch=c(21,19),
       legend=c("Mean Error Single Tree", "Mean Error Bagging"))

# so bagging does do better on average!

# but there are cases where a single tree does better; compute the difference
# of the two error rates and examine some summary statistics; note that the
# difference can be negative (which means that the single tree error rate was
# lower than the error rate from bagging)
summary(error.single.sim - error.bagging.sim)

############################################################################

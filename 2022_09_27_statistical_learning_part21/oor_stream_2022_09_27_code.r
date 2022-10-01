############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-09-27
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 8.1.1 - 8.1.2
#
# last updated: 2022-10-01

############################################################################

### 8.1: The Basics of Decision Trees

# 8.1.1: Regression Trees

# install (if necessary) the ISLR2
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# copy the Hitters dataset to dat
dat <- Hitters

# keep rows where Salary is not missing
dat <- dat[!is.na(dat$Salary),]

# histogram of the Salary variable
hist(dat$Salary, main="", xlab="Salary")

# create log transformed Salary variable
dat$logSalary <- log(dat$Salary)

# histogram of the log transformed Salary variable
hist(dat$logSalary, breaks=20, main="", xlab="Log(Salary)")

# first we just do a regression model with log(Salary) as outcome and
# variables Years and Hits as predictors
res <- lm(logSalary ~ Years + Hits, data=dat)
summary(res)

# residual sum of squares (RSS) of the regression model
RSS.model <- sum((dat$logSalary - predict(res))^2)
RSS.model

# we can compare this against the RSS we would get if we just use the mean of
# each player as the predicted value (which is identical to using a model that
# just includes an intercept but no predictors)
RSS.null <- sum((dat$logSalary - mean(dat$logSalary))^2)
RSS.null

# R^2 is one minus the ratio of these two RSS values
1 - RSS.model / RSS.null

# see https://en.wikipedia.org/wiki/Coefficient_of_determination (but using
# SS_res and SS_tot for RSS.model and RSS.null, respectively)

# manually recreate the values at the terminal nodes in the tree (Figure 8.1)
round(mean(subset(dat, Years <  4.5)$logSalary), digits=2)
round(mean(subset(dat, Years >= 4.5 & Hits <  117.5)$logSalary), digits=2)
round(mean(subset(dat, Years >= 4.5 & Hits >= 117.5)$logSalary), digits=2)

# to get the salary in the original units, we exponentiate these values
round(exp(mean(subset(dat, Years <  4.5)$logSalary)), digits=2)
round(exp(mean(subset(dat, Years >= 4.5 & Hits <  117.5)$logSalary)), digits=2)
round(exp(mean(subset(dat, Years >= 4.5 & Hits >= 117.5)$logSalary)), digits=2)

# color intensity values (for figure below)
colint <- with(dat, pmax(.05, 1 - (logSalary - min(logSalary)) /
                                  (max(logSalary) - min(logSalary))))

# Figure 8.2 (but using the color intensity values above so players with a
# higher salary get a darker shaded point)
par(mar=c(5,4,4,4))
plot(dat$Years, dat$Hits, pch=19, col=rgb(colint,colint,colint),
     xlab="Years", ylab="Hits", xaxt="n", yaxt="n")
abline(v=4.5, lwd=5, col="darkgreen")
segments(4.5, 117.5, max(dat$Years)+1, 117.5, lwd=5, col="darkgreen")
axis(side=1, at=c(min(dat$Years),   4.5, max(dat$Years)))
axis(side=4, at=c(min(dat$Hits),  117.5, max(dat$Hits)), las=1)
text( 2, 117.5, expression(R[1]), cex=1.5)
text(14,  60,   expression(R[2]), cex=1.5)
text(14, 180,   expression(R[3]), cex=1.5)

# computed predicted log(Salary) for each player based on the tree
pred <- with(dat,
             ifelse(Years < 4.5,  mean(logSalary[Years < 4.5]),
             ifelse(Hits < 117.5, mean(logSalary[Years >= 4.5 & Hits <  117.5]),
                                  mean(logSalary[Years >= 4.5 & Hits >= 117.5]))))

# residual sum of squares (RSS) of the tree
RSS.tree <- sum((dat$logSalary - pred)^2)
RSS.tree

# RSS before doing any splitting (i.e., use the mean of the entire group as
# the predicted value for each player)
RSS.null <- sum((dat$logSalary - mean(dat$logSalary))^2)
RSS.null

# again, R^2 is one minus the ratio of these two RSS values
1 - RSS.tree / RSS.null

# interestingly, the R^2 of the tree model is higher than of the regression
# model we fitted above

############################################################################

# Figure 8.2 but created with ggplot2

library(ggplot2)

ggplot(dat, aes(Years, Hits)) +
   geom_point(shape = 19, aes(color = logSalary)) +
   scale_colour_gradient(low="#dddddd", high="#000000") +
   geom_vline(xintercept = 4.5, color = "darkgreen") +
   annotate(geom = "segment", x = 4.5, xend = Inf, y = 117.5, yend = 117.5, color = "darkgreen") +
   annotate(geom = "text", x = c(2,14,14), y = c(117.5,60,178), label = c("R[1]", "R[2]", "R[3]"), parse=TRUE) +
   labs(x = "Years", y = "Hits") +
   scale_x_continuous(limits = c(0, max(dat$Years)), breaks = c(min(dat$Years), 4.5, max(dat$Years))) +
   scale_y_continuous(limits = c(1, max(dat$Hits)),  breaks = NULL, sec.axis = sec_axis(~., breaks = c(min(dat$Hits), 117.5, max(dat$Hits)))) +
   theme_bw() +
   theme(panel.grid = element_blank())

############################################################################

# building the tree manually for the first split

# try out all possible splits based on Years and compute the corresponding RSS

xs <- seq(min(dat$Years), max(dat$Years), length=1000)
RSSs.years <- rep(NA, length(xs))

for (i in 1:length(xs)) {

   pred <- ifelse(dat$Years < xs[i], mean(dat$logSalary[dat$Years <  xs[i]]),
                                     mean(dat$logSalary[dat$Years >= xs[i]]))
   RSSs.years[i] <- sum((dat$logSalary - pred)^2)

}

plot(xs, RSSs.years, type="l", lwd=3, xlab="Split Location for Years", ylab="RSS")

# split value at which the RSS is minimized (there are multiple values giving
# the same minimum RSS, because Years is measured in intergers, so it doesn't
# matter if we split at 4.02 or 4.98)
xs[which(RSSs.years == min(RSSs.years))]
min(RSSs.years)

# now do the same for Hits

xs <- seq(min(dat$Hits), max(dat$Hits), length=1000)
RSSs.hits <- rep(NA, length(xs))

for (i in 1:length(xs)) {

   pred <- ifelse(dat$Hits < xs[i], mean(dat$logSalary[dat$Hits <  xs[i]]),
                                    mean(dat$logSalary[dat$Hits >= xs[i]]))
   RSSs.hits[i] <- sum((dat$logSalary - pred)^2)

}

plot(xs, RSSs.hits, type="l", lwd=3, xlab="Split Location for Hits", ylab="RSS")

# split value at which the RSS is minimized (again, multiple values)
xs[which(RSSs.hits == min(RSSs.hits))]
min(RSSs.hits)

# since min(RSSs.years) is smaller than min(RSSs.hits), the first split in the
# tree will be done based on the Years variable at the location that yields
# min(RSSs.years) (which is essentially any value just above 4)

# now we could continue splitting within each of the two regions based on the
# two variables (Years and Hits), finding that split that yields the smallest
# RSS; since doing this manually gets tedious, let's not do that

############################################################################

# install (if necessary) the tree
#install.packages("tree")

# load the tree package
library(tree)

# build tree based on the Years and Hits variables
res <- tree(logSalary ~ Years + Hits, data=dat)
res

# note that the first split happens at Years < 4.5 versus Years >= 4.5 as we
# found manually above; then further splits are made based on the two variables

# plot the tree
plot(res)
text(res)

# get some summary information based on the tree
summary(res)

# prune the tree
ptree <- prune.tree(res)
ptree

# the output shows what values of alpha in eq. 8.4 (which is labeled 'k') lead
# to trees that are of decreasing complexity (i.e., with a decreased number of
# terminal nodes, as shown by 'size') and what the corresponding RSS value is
# (labeled 'dev'); when alpha (k) is large enough, then the tree is pruned
# back to having no splits (i.e., just one terminal node) and we get the RSS
# value we computed earlier for the 'null' model

# RSS before doing any splitting
RSS.null

# tree with higher complexity (more terminal nodes) have lower RSS
plot(ptree$size, ptree$dev, type="o", pch=19, xlab="Tree Size", ylab="RSS")

# this way, we cannot determine what an appropriate tree size should be (since
# we would always choose the largest tree size)

# note: there is also a plot method for objects returned by prune.tree()
plot(ptree)

# create a training and a test dataset
set.seed(19)
id.train  <- sample(nrow(dat), round(nrow(dat)/2))
dat.train <- dat[id.train,]
dat.test  <- dat[-id.train,]

# build tree in the training data
res <- tree(logSalary ~ AtBat + Hits + HmRun + Runs + RBI + Walks + Years + PutOuts + Assists, data=dat.train)
summary(res)

# plot the tree (like Figure 8.4)
plot(res)
text(res)

# note: we cannot re-create the exact same tree, since we do not know how the
# training and testing datasets were created for the example in the book

# prune the tree
ptree <- prune.tree(res)
ptree

# plot the mean squared error for the training data against the tree size
plot(ptree$size, ptree$dev/nrow(dat.train), type="o", pch=19, lwd=3,
     ylim=c(0,1), xlab="Tree Size", ylab="Mean Squared Error", xaxt="n")
axis(side=1, 1:10)

# cross-validation (6-fold) to determined an appropriate tree size

set.seed(1234)
split <- sample(rep_len(1:6, nrow(dat.train)))
MSEmat <- matrix(NA, nrow=6, ncol=10)

for (i in 1:6) {

   tmp <- tree(logSalary ~ AtBat + Hits + HmRun + Runs + RBI + Walks + Years + PutOuts + Assists,
               data=dat.train[split!=i,], control=tree.control(nobs=nrow(dat.train[split!=i,]), minsize=2))

   for (j in 1:10) {
      if (j == 1) {
         pred <- mean(dat.train$logSalary[split!=i])
      } else {
         ptree <- prune.tree(tmp, best=j)
         pred <- predict(ptree, newdata=dat.train[split==i,])
      }
      MSEmat[i,j] <- mean((dat.train$logSalary[split==i] - pred)^2)
   }

}

MSEmat
MSE.cv <- apply(MSEmat, 2, mean)
MSE.cv

# add the line for the cross-validated MSE to the plot
lines(1:10, MSE.cv, type="o", pch=19, lwd=3, col="#009f86")

# compute the MSE in the test dataset based on the model fitted using the training dataset

MSE.test <- rep(NA, 10)

for (j in 1:10) {
   if (j == 1) {
      pred <- mean(dat.train$logSalary)
   } else {
      ptree <- prune.tree(res, best=j)
      pred <- predict(ptree, newdata=dat.test)
   }
   MSE.test[j] <- mean((dat.test$logSalary - pred)^2)
}

# add the line for the test MSE to the plot
lines(1:10, MSE.test, type="o", pch=19, lwd=3, col="#ce6017")

# add a legend
legend("topright", inset=.02, lty="solid", col=c("black","#009f86","#ce6017"), lwd=3,
       legend=c("Training", "Cross-Validation", "Test"))

# according to the cross-validation, we should pick a tree with a size of 3;
# in the test data, the MSE also doesn't decrease much beyond this size

# the cv.tree() function also does cross-validation
cv.tree(res, K=6)

# since the splits that are created for the cross-validation are random, each
# time one runs this function one will get a different green line; let's do
# this 100 times and see what we get

for (i in 1:100) {
   tmp <- cv.tree(res, K=6)
   lines(tmp$size, tmp$dev/nrow(dat.train), col=rgb(.2,.8,.3,.2))
}

lines(1:10, MSE.cv, type="o", pch=19, lwd=3, col="#009f86")
lines(1:10, MSE.test, type="o", pch=19, lwd=3, col="#ce6017")

############################################################################

### 8.1.2: Classification Trees

# plot the Gini index and entropy for a single proportion

p1s <- seq(0.001,1,length=1000)
p2s <- 1-p1s
Gs <- rep(NA, length(p1s))
Ds <- rep(NA, length(p1s))
for (i in 1:length(p1s)) {
   Gs[i] <- p1s[i]*(1-p1s[i]) + p2s[i]*(1-p2s[i])
   Ds[i] <- - (p1s[i]*log(p1s[i]) + (p2s[i]*log(p2s[i])))
}

plot(p1s,  Gs, type="l", lwd=3, ylim=c(0,0.8), col="#009f86")
lines(p1s, Ds, lwd=3, col="#ce6017")

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
set.seed(1234)
id.train  <- sample(nrow(dat), round(nrow(dat)/2))
dat.train <- dat[id.train,]
dat.test  <- dat[-id.train,]

# build tree in the training data
res <- tree(AHD ~ ., data=dat.train)
res
summary(res)

# plot the tree (like Figure 8.6)
plot(res)
text(res)

# instead of using a, b, ... for the levels of categorical (factor) variables,
# use the original level names (but this makes the plot harder to read)
plot(res)
text(res, pretty=0)

# prune the tree
ptree <- prune.tree(res)
ptree

# deviance for the 'null model' with no splits (where size = 1)
-2 * sum(table(dat.train$AHD) * log(table(dat.train$AHD) / nrow(dat.train)))

# note: the equation for the deviance is given on page 353

# prune the tree but now based on
ptree <- prune.misclass(res)
ptree

# compute the training error rate as a function of tree complexity

error.train <- rep(NA, 13)

for (j in 1:13) {
   if (j == 1) {
      pred <- names(which.max(table(dat.train$AHD)))
   } else {
      ptree <- prune.misclass(res, best=j)
      pred <- predict(ptree, newdata=dat.train, type="class")
   }
   error.train[j] <- mean(dat.train$AHD != pred)
}

# plot the error rate as a function of tree size for the training data
plot(1:13, error.train, type="o", pch=19, lwd=3,
     ylim=c(0,0.6), xlab="Tree Size", ylab="Error")

# cross-validation (10-fold) to determine an appropriate tree size

set.seed(1236)
split <- sample(rep_len(1:10, nrow(dat.train)))
errormat <- matrix(NA, nrow=10, ncol=13)

for (i in 1:10) {

   tmp <- tree(AHD ~ ., data=dat.train[split!=i,], control=tree.control(nobs=nrow(dat.train[split!=i,]), minsize=2))

   for (j in 1:13) {
      if (j == 1) {
         pred <- names(which.max(table(dat.train$AHD[split!=i])))
      } else {
         ptree <- prune.misclass(tmp, best=j)
         pred <- predict(ptree, newdata=dat.train[split==i,], type="class")
      }
      errormat[i,j] <- mean(dat.train$AHD[split==i] != pred)
   }

}

errormat
error.cv <- apply(errormat, 2, mean)
error.cv

# add the line for the cross-validated error rate to the plot
lines(1:13, error.cv, type="o", pch=19, lwd=3, col="#009f86")

# compute the test error based on the model fitted using the training dataset

error.test <- rep(NA, 13)

for (j in 1:13) {
   if (j == 1) {
      pred <- names(which.max(table(dat.train$AHD)))
   } else {
      ptree <- prune.misclass(res, best=j)
      pred <- predict(ptree, newdata=dat.test, type="class")
   }
   error.test[j] <- mean(dat.test$AHD != pred)
}

# add the line for the test error rate to the plot
lines(1:13, error.test, type="o", pch=19, lwd=3, col="#ce6017")

# add a legend
legend("topright", inset=.02, lty="solid", col=c("black","#009f86","#ce6017"), lwd=3,
       legend=c("Training", "Cross-Validation", "Test"))

# note: in the book, the colors for the cross-validation and the test error
# rates are switched (compared to section 8.1.1)

# again use cv.tree() for the cross-validation and repeat this 100 times

for (i in 1:100) {
   tmp <- cv.tree(res, K=10, FUN=prune.misclass)
   lines(tmp$size, tmp$dev/nrow(dat.train), col=rgb(.2,.8,.3,.2))
}

lines(1:13, error.cv, type="o", pch=19, lwd=3, col="#009f86")
lines(1:13, error.test, type="o", pch=19, lwd=3, col="#ce6017")

# according to the cross-validation, I would pick a tree of fairly low size,
# so maybe even of just size 2
ptree <- prune.misclass(res, best=2)
ptree

# predict the class based on this tree in the test data
pred.test <- predict(ptree, newdata=dat.test, type="class")

# cross-classification table of the predicted and actual class
table(pred.test, dat.test$AHD)

# proportion of correct classifications
100 * sum(diag(table(pred.test, dat.test$AHD))) / nrow(dat.test)

# use a much larger tree
ptree <- prune.misclass(res, best=11)
pred.test <- predict(ptree, newdata=dat.test, type="class")
100 * sum(diag(table(pred.test, dat.test$AHD))) / nrow(dat.test)

# the proportion of correct classifications is higher, but just slightly

############################################################################

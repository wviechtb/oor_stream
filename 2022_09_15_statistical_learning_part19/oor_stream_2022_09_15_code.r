############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-09-15
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 7.4.4 - 7.5.2
#
# last updated: 2022-09-19

############################################################################

# 7.4.4: Choosing the Number and Locations of the Knots

# install (if necessary) the ISLR2
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# load the splines package
library(splines)

# copy the Wage dataset to 'dat'
dat <- Wage

# fit the natural cubic spline model with 4 degrees of freedom
res <- lm(wage ~ ns(age, df=4), data=dat)
summary(res)

# determine the knot positions chosen (including the boundary knots)
knots <- attributes(ns(dat$age, df=4))$knots
knots <- sort(c(knots, attributes(ns(dat$age, df=4))$Boundary.knots))
knots

# the knot positions are chosen based on the 25th, 50th, and 75th percentile
quantile(dat$age, c(.25, .50, .75))

# and the boundary knots based on the range of the predictor
range(dat$age)

# draw scatterplot (Figure 7.5, left panel)
plot(wage ~ age, data=dat, col="skyblue3", cex=0.7, xlab="Age", ylab="Wage")

# add the knot positions to the plot
abline(v=knots, lty="dashed")

# add the regression line to the plot
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="red", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="red", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="red", lty="dashed")

############################################################################

# 7.4.4: Choosing the Number and Locations of the Knots

# use ten-fold cross-validation to determine an appropriate number of knots

set.seed(1234)

dfs <- 1:10
mse <- rep(NA, length(dfs))

split <- sample(rep_len(1:10, nrow(dat)))
pred  <- rep(NA, nrow(dat))

for (i in 1:length(dfs)) {

   for (j in 1:10) {

      res <- lm(wage ~ ns(age, df=dfs[i]), data=dat, subset=split!=j)
      pred[split == j] <- predict(res, newdata=dat[split==j,])

   }

   mse[i] <- mean((dat$wage - pred)^2)

}

# Figure 7.6
plot(dfs, mse, type="b", pch=19, col="red", xlab="Degrees of Freedom",
     ylab="Mean Squared Error")

# now do the same for bs() (have to be a bit careful below; we have to
# distinguish the case where the dfs are below 3 versus above)

for (i in 1:length(dfs)) {

   for (j in 1:10) {

      if (dfs[i] < 3) {
         res <- lm(wage ~ bs(age, degree=dfs[i]), data=dat, subset=split!=j)
      } else {
         res <- lm(wage ~ bs(age, df=dfs[i]), data=dat, subset=split!=j)
      }
      pred[split == j] <- predict(res, newdata=dat[split==j,])

   }

   mse[i] <- mean((dat$wage - pred)^2)

}

# add the results to the plot above
lines(dfs, mse, type="b", pch=19, col="blue")

############################################################################

# 7.4.5: Comparison to Polynomial Regression

# draw scatterplot (Figure 7.7)
plot(wage ~ age, data=dat, col="darkgray", cex=0.7, xlab="Age", ylab="Wage")

# fit natural cubic spline with 15 degrees of freedom
res <- lm(wage ~ ns(age, df=15), data=dat)

# add the regression line to the plot
newdat <- data.frame(age=seq(17,81,length=1000))
pred <- predict(res, newdata=newdat)
lines(newdat$age, pred, col="red", lwd=5)

# fit degree-15 polynomial model
res <- lm(wage ~ poly(age, degree=15), data=dat)

# add the regression line to the plot
pred <- predict(res, newdata=newdat)
lines(newdat$age, pred, col="blue", lwd=5)

# add a legend
legend("topright", inset=.02, lty="solid", col=c("red","blue"), lwd=5,
       legend=c("Natural Cubic Spline", "Polynomial"), bg="white")

############################################################################

### 7.5: Smoothing Splines

# 7.5.1: An Overview of Smoothing Splines

# plot the data
plot(wage ~ age, data=dat, col="darkgray", cex=0.7, xlab="Age", ylab="Wage")

# compute the mean wage for each unique age value
mwageperage <- tapply(dat$wage, dat$age, mean)

# add this running mean to the scatterplot
lines(as.numeric(names(mwageperage)), mwageperage, lwd=5, col="blue")

# this is the same as fitting a regression model with age as a factor
res <- lm(wage ~ 0 + factor(age), data=dat)
summary(res)

# this yields the smallest possible RSS
sum(resid(res)^2)

# but the resulting regression 'line' is very non-smooth and wiggly

############################################################################

# now let's consider using a natural spline model with knots at every unique
# value of age (note: the boundary knots will be min(age) and max(age), so we
# remove those from 'knots', since they will get added back automatically)
knots <- sort(unique(dat$age))
knots <- knots[-c(1, length(knots))]
knots

# fit natural cubic spline model
res <- lm(wage ~ ns(age, knots=knots), data=dat)
summary(res)

# add the regression line/curve to the plot
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat)
lines(newdat$age, pred, col="red", lwd=5)

# note that the regression line is now smooth, but still very wiggly; in fact,
# the fit of this model is as good as for the model above where age was used
# as a factor (compare the residual standard error, R^2, etc.); hence, the RSS
# for this model is the same as the one above
sum(resid(res)^2)

# this natural cubic spline model is a smoothing spline as described in this
# book section where lambda (the 'tuning parameter') is set to 0 (so we get
# the maximum amount of wiggliness)

############################################################################

# 7.5.2: Choosing the Smoothing Parameter lambda

# the 'S' matrix discussed in the book is the same as the 'hat matrix' for the
# model above; the hat matrix is the matrix that 'projects' the observed
# values of y to the fitted/predicted values (which the book denotes g-hat)

# first 10 predicted values based on the model above
predict(res)[1:10]

# compute the hat matrix
X <- cbind(1, ns(dat$age, knots=knots))
H <- X %*% solve(t(X) %*% X) %*% t(X)
y <- cbind(dat$wage)

# now multiply H with y and check that we get the same predicted values
(H %*% y)[1:10]

# the 'effective degrees of freedom' for the model above are the sum of the
# diagonal elements of the H matrix
sum(diag(H))

# this is equal to the number of parameters in the model
length(coef(res))

# this is the highest number for the effective degrees of freedom (i.e., when
# lambda=0); can obtain the same fit with smooth.spline() if we set spar=-1.5
# and all.knot=TRUE
res <- smooth.spline(dat$age, dat$wage, spar=-1.5, all.knots=TRUE)
res

# if we allow lambda > 0, then the smoothing spline becomes less wiggly

# plot the data again
plot(wage ~ age, data=dat, col="darkgray", cex=0.7, xlab="Age", ylab="Wage")

# instead of setting lambda, the authors set the effective degrees of freedom
# to 16 (which implies a certain value for lambda)
res <- smooth.spline(dat$age, dat$wage, df=16)
res
lines(res, col="red", lwd=5)

# but which value for the effective degrees of freedom (and hence, lambda)
# should be used? can use leave-one-out cross-validation
res <- smooth.spline(dat$age, dat$wage, cv=TRUE)
res
lines(res, col="blue", lwd=5)

# add a legend
legend("topright", inset=.02, lty="solid", col=c("red","blue"), lwd=5,
       legend=c("16 Degrees of Freedom", "6.8 Degrees of Freedom"), bg="white")

############################################################################

# note: by default, smooth.spline() does not quite use all unique values of
# age as the knot positions; so if we want this, we have to set all.knots=TRUE

res <- smooth.spline(dat$age, dat$wage, all.knots=TRUE, cv=TRUE)
res
lines(res, col="green", lwd=5)

# this yields essentially the same fit as with all.knots=FALSE (the default)

############################################################################

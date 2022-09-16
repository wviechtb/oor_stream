############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-09-01
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 7.1 - 7.4.3
#
# last updated: 2022-09-16

############################################################################

### 7.1: Polynomial Regression

# start with a very simple example

# simulate some data
set.seed(1234)
n <- 100
x <- runif(n)
y <- 0.2 + 0.1 * x + 0.5 * x^2 + rnorm(n, 0, 0.02)

# draw the scatteplot
plot(x, y, pch=19)

# fit a simple regression model (assuming a linear relationship between x and y)
res <- lm(y ~ x)
summary(res)

# add the regression line to the scatterplot
abline(res, lwd=5, col="blue")

# fit a quadratic polynomial regression model
res <- lm(y ~ x + I(x^2))
summary(res)

# to add the regression 'line' (i.e., curve) to the plot, we can no longer use
# abline(); instead, we use predict() to compute the predicted values at the
# chosen values of 'x' (as given in the 'newdat' data frame)
newdat <- data.frame(x=seq(0,1,length=1000))
pred <- predict(res, newdata=newdat)
head(cbind(x=newdat$x, pred))

# add the regression line (curve) to the scatterplot
lines(newdat$x, pred, lwd=5, col="red")

# add legend
legend("topleft", inset=.02, lty="solid", col=c("blue","red"), lwd=5,
       legend=c("Linear Model", "Quadratic Model"))

# what do the coefficients in such a quadratic model mean? and how does such a
# model lead to such a curvilinear relationship between x and y?
#
# the model is given by:
#
# y = beta0 + beta1 * x + beta2 * x^2 + e
#
# now say x is equal to 0; then:
#
# y = beta0                           + e
#
# so beta0 is the estimated value when x = 0 (i.e., the intercept)
#
# now say x is just a smidge above 0, then x^2 is still more or less 0; so, we
# can, roughly speaking, simplify the model to:
#
# y = beta0 + beta1 * x               + e
#
# so around x ~= 0, beta1 is the slope of the 'line' (or rather: curve)
#
# now when x becomes larger than 0, we cannot ignore x^2 anymore, so then:
#
# y = beta0 + beta1 * x + beta2 * x^2 + e
#   = beta0 + (beta1 + beta2 * x) * x + e
#     |---|   |-----------------|
#    intrcpt      slope for x
#
# what is happening to the slope as x increases? the slope increases, because
# beta2 (in our example above) is positive; so the slope of x changes as a
# function of x, which is the reason why we get a curve instead of a line; and
# when x = 1, the slope is: beta1 + beta2

# install (if necessary) the ISLR2
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# read the help page for the 'Wage' dataset
help(Wage)

# copy the Wage dataset to 'dat'
dat <- Wage

# range of age variable
range(dat$age)

# fit polynomial regression model (4th degree)
res <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data=dat)
summary(res)

# Figure 7.1 (left panel)
plot(wage ~ age, data=dat, col="skyblue3", cex=0.7)

# add regression line/curve (with 95% CI) to the plot
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, lwd=5, col="blue")
lines(newdat$age, pred$lwr, lwd=3, col="blue", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="blue", lty="dashed")

# extract the regression coefficients (as a column vector)
b <- cbind(coef(res))
b

# extract the var-cov matrix of the regression coefficients (in the book, this
# is denoted as C with a hat)
vb <- vcov(res)
vb

# sidenote: the square-root of the diagonal of this matrix yields the standard
# errors of the regression coefficients (see summary(res) above)
sqrt(diag(vb))

# compute the predicted value when age = 25
l0 <- cbind(1, 25, 25^2, 25^3, 25^4)
pred <- c(l0 %*% b)
pred

# compare this with what we get from predict()
predict(res, newdata=data.frame(age=25))

# also get the standard error of the predicted value
predict(res, newdata=data.frame(age=25), se.fit=TRUE)

# compute the standard error manually
se.l0 <- c(sqrt(l0 %*% vb %*% t(l0)))
se.l0

# get the 95% CI for the predicted value
predict(res, newdata=data.frame(age=25), interval="confidence")

# compute the approximate 95% CI as described in the book
pred + c(-1,1) * 2 * c(se.l0)

# the 'exact' calculation of the CI by predict() uses a critical value from
# the t-distribution with n-p degrees of freedom, where n is the sample size
# and p the number of regression coefficients
pred + c(-1,1) * qt(.975, df=2995) * c(se.l0)

# with a sample size this large, this makes no practical difference

# instead of creating the polynomial terms manually, we can use poly()
res <- lm(wage ~ poly(age, 4), data=dat)
summary(res)

# by default, poly() creates 'orthogonal' polynomial terms (which are
# uncorrelated); if we want the same polynomial terms that we created manually
# earlier, we can use raw=TRUE
res <- lm(wage ~ poly(age, 4, raw=TRUE), data=dat)
summary(res)

############################################################################

# dichotomize wage into > 250 versus not
dat$wagehigh <- ifelse(dat$wage > 250, 1, 0)

# frequency table for the wagehigh variable
table(dat$wagehigh)

# logistic regression model predicting being a high earner
res <- glm(wagehigh ~ age + I(age^2) + I(age^3) + I(age^4), data=dat, family=binomial)
summary(res)

# Figure 7.1 (right panel)
plot(NA, xlim=c(18,80), ylim=c(0,.20), xlab="Age", ylab="Pr(Wage > 250 | Age)")
rug(jitter(dat$age[dat$wagehigh == 0]), side=1, col="skyblue3", lwd=1)
rug(jitter(dat$age[dat$wagehigh == 1]), side=3, col="skyblue3", lwd=1)

# add the regression line/curve (with 95% CI) to the plot (for the predicted
# probability of being in the 'high wage' group; note, the predicted values
# and CI bounds are calculated on the logit scale and are then transformed
# into probabilities with plogis())
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat, se.fit=TRUE)
pred$lwr <- pred$fit - qnorm(.975) * pred$se.fit
pred$upr <- pred$fit + qnorm(.975) * pred$se.fit
lines(newdat$age, plogis(pred$fit), lwd=5, col="blue")
lines(newdat$age, plogis(pred$lwr), lwd=3, col="blue", lty="dashed")
lines(newdat$age, plogis(pred$upr), lwd=3, col="blue", lty="dashed")

############################################################################

### 7.2: Step Functions

# cut up age into four groups
dat$agegrp <- cut(dat$age, breaks=c(0, 33, 49, 64, 100))
head(dat$agegrp, 10)

# [] means including the number; () means just above/below the number; so we
# have the group of the 0-33 year olds, the group of the 34-49, the group of
# the 50-64 year olds, and the group of the 65-100 year olds

# use the labels 1-4 for the groups
dat$agegrp <- cut(dat$age, breaks=c(0, 33, 49, 64, 100), labels=1:4)
head(dat$agegrp, 10)

# now use the age group factor in our regression model
res <- lm(wage ~ agegrp, data=dat)
summary(res)

# get the mean of age for the 4 groups
by(dat$wage, dat$agegrp, mean)

# these are identical to the intercept for the first group and the intercept
# plus the respective coefficient for the other groups
rbind(coef(res)[1], coef(res)[1]+coef(res)[2],
      coef(res)[1]+coef(res)[3], coef(res)[1]+coef(res)[4])

# Figure 7.2 (left panel)
plot(wage ~ age, data=dat, col="darkgray", cex=0.7)

# add the regression line (with 95% CI) to the scatterplot
newdat <- data.frame(age=18:80)
newdat$agegrp <- cut(newdat$age, breaks=c(0, 33, 49, 64, 100), labels=1:4)
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, lwd=5, col="green4")
lines(newdat$age, pred$lwr, lwd=3, col="green4", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="green4", lty="dashed")

# what if we would cut up age into many more groups?
dat$agegrp <- cut(dat$age, breaks=20, labels=1:20)
res <- lm(wage ~ agegrp, data=dat)
plot(wage ~ age, data=dat, col="darkgray", cex=0.7)
newdat$agegrp <- cut(newdat$age, breaks=20, labels=1:20)
pred <- predict(res, newdata=newdat)
lines(newdat$age, pred, lwd=5, col="green4")

# add the cutpoints as vertical lines to the plot
cp <- levels(cut(dat$age, breaks=20))
cp
cp <- as.numeric(gsub("]", "", sapply(strsplit(cp, split=","), tail, 1)))
cp
abline(v=cp, lty="dotted")

# this is really the other extreme of a model that imposes a global structure;
# here, the fit within an age category is only determined by the people
# falling within that group

############################################################################

# cut up age into four groups
dat$agegrp <- cut(dat$age, breaks=c(0, 33, 49, 64, 100), labels=1:4)

# dichotomize wage into > 250 versus not
dat$wagehigh <- ifelse(dat$wage > 250, 1, 0)

# now use the age group factor in our regression model
res <- glm(wagehigh ~ agegrp, data=dat, family="binomial")
summary(res)

# Figure 7.2 (right panel)
plot(NA, xlim=c(18,80), ylim=c(0,.20), xlab="Age", ylab="Pr(Wage > 250 | Age)")
rug(jitter(dat$age[dat$wagehigh == 0]), side=1, col="gray", lwd=1)
rug(jitter(dat$age[dat$wagehigh == 1]), side=3, col="gray", lwd=1)

# add the regression line/curve (with 95% CI) to the plot
newdat <- data.frame(age=18:80)
newdat$agegrp <- cut(newdat$age, breaks=c(0, 33, 49, 64, 100), labels=1:4)
pred <- predict(res, newdata=newdat, se.fit=TRUE)
pred$lwr <- pred$fit - qnorm(.975) * pred$se.fit
pred$upr <- pred$fit + qnorm(.975) * pred$se.fit
lines(newdat$age, plogis(pred$fit), lwd=5, col="green4")
lines(newdat$age, plogis(pred$lwr), lwd=3, col="green4", lty="dashed")
lines(newdat$age, plogis(pred$upr), lwd=3, col="green4", lty="dashed")

############################################################################

### 7.4: Regression Splines

# 7.4.1: Piecewise Polynomials

# tried to identify which points are shown in Figure 7.3 (could only identify
# 98 points in the figure; I suspect that the sub-sample included 100 data
# points, so the sub-sample used below isn't exactly the same as used in the
# book, but should be close enough)

pos <- c(13, 33, 40, 43, 56, 58, 69, 149, 163, 171, 179, 192, 196, 248, 263,
271, 279, 335, 394, 405, 431, 437, 442, 443, 491, 523, 552, 565, 572, 585,
592, 605, 607, 630, 643, 665, 671, 675, 705, 738, 752, 818, 830, 840, 880,
896, 899, 934, 937, 940, 954, 1091, 1095, 1122, 1130, 1303, 1377, 1385, 1392,
1445, 1509, 1513, 1520, 1534, 1536, 1570, 1575, 1582, 1712, 1769, 1772, 1782,
1794, 1796, 1799, 1839, 1840, 1871, 1948, 1966, 1977, 1997, 2091, 2096, 2114,
2147, 2208, 2246, 2299, 2471, 2533, 2598, 2600, 2625, 2708, 2850, 2891, 2907)
sub <- dat[pos,]

# fit piecewise cubic polynomials
res.lo <- lm(wage ~ poly(age, 3), data=sub, subset=age <= 50)
res.hi <- lm(wage ~ poly(age, 3), data=sub, subset=age >  50)

# Figure 7.3 (top left panel)
plot(wage ~ age, data=sub)
abline(v=50, lty="dotted")

# add the regression lines/curves to the plot
newdat <- data.frame(age=seq(18,50,length=1000))
pred <- predict(res.lo, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="blue", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="blue", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="blue", lty="dashed")
newdat <- data.frame(age=seq(50,80,length=1000))
pred <- predict(res.hi, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="blue", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="blue", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="blue", lty="dashed")

############################################################################

# 7.4.2: Constraints and Splines

# Figure 7.3 (top right panel)
plot(wage ~ age, data=sub)
abline(v=50, lty="dotted")

# fit the continuous piecewise cubic spline model (note: section 7.4.3
# explains how the 'truncated power basis' function needs to be computed)

tbpd <- function(x, knot, d) ifelse(x > knot, (x - knot)^d, 0)

res <- lm(wage ~ age + I(age^2) + I(age^3) +
          tbpd(age,50,1) + tbpd(age,50,2) + tbpd(age,50,3), data=sub)
summary(res)

newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="green4", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="green4", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="green4", lty="dashed")

# Figure 7.3 (bottom left panel)
plot(wage ~ age, data=sub)
abline(v=50, lty="dotted")

# fit the continuous piecewise cubic spline model that is also continuous for
# the second and third derivative
res <- lm(wage ~ age + I(age^2) + I(age^3) + tbpd(age,50,3), data=sub)
summary(res)

pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="red", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="red", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="red", lty="dashed")

# sidenote: instead of fitting the two piecewise cubic polynomials as we did
# for Figure 7.3, top left panel, we could fit a single model that provides
# the same fit
res <- lm(wage ~ age + I(age^2) + I(age^3) + tbpd(age,50,0) +
          tbpd(age,50,1) + tbpd(age,50,2) + tbpd(age,50,3), data=sub)
summary(res)

############################################################################

# 7.4.3: The Spline Basis Representation

# set the knot positions
knots <- c(30,45,60)

# Figure 7.4
plot(wage ~ age, data=sub, xlab="Age", ylab="Wage")
abline(v=knots, lty="dotted")

# fit the cubic spline model
res <- lm(wage ~ age + I(age^2) + I(age^3) + tbpd(age,knots[1],3) +
          tbpd(age,knots[2],3) + tbpd(age,knots[3],3), data=sub)
summary(res)

# add the regression line/curve to the plot
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="blue", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="blue", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="blue", lty="dashed")

# load the splines package
library(splines)

# we can use bs() from the splines package to fit the same model
res <- lm(wage ~ bs(age, knots=knots), data=sub)
summary(res)

# note: the coefficients are different (the way bs() computes the spline terms
# is somewhat different), but the fit of the model is the same (note the
# identical residual standard error, R^2, and so on); hence, the following
# will just add the same line/curve (and CI bounds) to the plot as above
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="blue", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="blue", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="blue", lty="dashed")

# fit the natural cubic spline model
res <- lm(wage ~ ns(age, knots=knots), data=sub)
summary(res)

# add the regression line/curve to the plot
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="red", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="red", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="red", lty="dashed")

# in the natural cubic spline model, the model assumes that the relationship
# between x and y is linear beyond the 'boundary knots', which are by default
# set at the min(x) and max(x) values
attributes(ns(dat$age, knots=knots))
range(dat$age)

# an even more restricted model would arise if we use knots[c(1,3)] as the
# boundary knots (note: this model was not considered in the book)
res <- lm(wage ~ ns(age, knots=knots[2], Boundary.knots=knots[c(1,3)]), data=sub)
summary(res)

# add the regression line/curve to the plot
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="green", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="green", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="green", lty="dashed")

# note that according to this model (the green line), the relationship between
# age and wage is linear for age values below 30 and for age values above 60

# add legend
legend("topright", inset=.02, lty="solid", col=c("red","blue","green"), lwd=5,
       legend=c("Natural Cubic Spline Model", "Cubic Spline Model",
                "Alternative Natural Cubic Spline"), bg="white")

############################################################################

# instead of using ns(), we can use the 'rms' package to fit such a model

# the 'rms' package is explained in detail in this book: Harrell, F. E., Jr.
# (2015). Regression modeling strategies: With applications to linear models,
# logistic and orginal regression, and survival analysis (2nd ed.). Springer.

# install the rms package
#install.packages("rms")

# load the rms package
library(rms)

# note: when using rcs(), the first and last knot values are the boundary
# knots, so to get the same natural cubic spline model as above (the red
# line), we have to add min(x) and max(x) to the knots
knots <- c(19,30,45,60,77)

# fit the natural cubic spline model (also called 'restricted cubic spline')
res <- lm(wage ~ rcs(age, parms=knots), data=sub)
summary(res)

# add the regression line/curve to the plot
pred <- predict(res, newdata=newdat, interval="confidence")
pred <- as.data.frame(pred)
lines(newdat$age, pred$fit, col="red", lwd=5)
lines(newdat$age, pred$lwr, lwd=3, col="red", lty="dashed")
lines(newdat$age, pred$upr, lwd=3, col="red", lty="dashed")

# this just adds the same red line (and CI) as already shown in the plot

############################################################################

# be careful when using rcs() in combination with lm(); the above works fine,
# since we manually specified the knot positions; however, instead of doing
# that, one can also just specify how many knots we want and rcs() will pick
# the knot positions automatically
res <- lm(wage ~ rcs(age, parms=5), data=sub)
summary(res)

# let's see which ones it picked
knots <- attributes(rcs(dat$age, parms=5))$parms
knots

# draw the scatterplot again and add the knot positions
plot(wage ~ age, data=sub, xlab="Age", ylab="Wage")
abline(v=knots, lty="dotted")

# note: the knot positions are based on certain quantiles of the x variable;
# for 5 knots, rcs() uses quantiles according to the following probabilities
quantile(dat$age, c(.05, .275, .50, .725, .95))

# if you now use the following code, then the predicted values are *wrong*
newdat <- data.frame(age=seq(18,80,length=1000))
pred <- predict(res, newdata=newdat)
lines(newdat$age, pred, col="red", lwd=5)

# this happens because the knot positions are now based on the data given to
# predict(), which are different than the ones used in the actual model
attributes(rcs(newdat$age, parms=5))$parms

# the rms package provides special functions for fitting regression models so
# that when predict() is used, the predicted values are correct
res <- ols(wage ~ rcs(age, parms=5), data=sub)
res
pred <- predict(res, newdata=newdat)
lines(newdat$age, pred, col="blue", lwd=5)

# add legend
legend("topright", inset=.02, lty="solid", col=c("red","blue"), lwd=5,
       legend=c("Wrong Predictions", "Correct Predictions"), bg="white")

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-03-10
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 3.2 - 3.3.1
#
# last updated: 2022-03-25

############################################################################

### 3.2: Multiple Linear Regression

# the data used in this section: https://www.statlearning.com/s/Advertising.csv
#download.file("https://www.statlearning.com/s/Advertising.csv", destfile="Advertising.csv")

# read in the data and examine the first 6 rows
dat <- read.csv("Advertising.csv")
head(dat)

# fit three simple regression models (Table 3.3)
res1 <- lm(sales ~ TV,        data=dat)
res2 <- lm(sales ~ radio,     data=dat)
res3 <- lm(sales ~ newspaper, data=dat)
summary(res1)
summary(res2)
summary(res3)

# look at the range of each predictor
range(dat$TV)
range(dat$radio)
range(dat$newspaper)

# fit the multiple regression model (Table 3.4)
res <- lm(sales ~ TV + radio + newspaper, data=dat)
summary(res)

# how are the coefficients actually estimated? it turns out that this involves
# some matrix algebra; we can do this manually as follows
X <- model.matrix(res)
head(X)
solve(t(X) %*% X) %*% t(X) %*% cbind(dat$sales)

# https://en.wikipedia.org/wiki/Linear_least_squares

# correlation matrix (Table 3.5)
round(cor(dat[c("TV", "radio", "newspaper", "sales")]), digits=4)

# F(3,196) = 570.3 (Table 3.6), p < .0001, so we reject the null hypothesis:
#
# H_0: beta_1 = beta_2 = beta_3 = 0
#
# (i.e., that all three slopes are equal to 0)

# we can do the same thing by comparing the full model with the reduced model
# that includes no predictors (except for the intercept term)
res1 <- lm(sales ~ TV + radio + newspaper, data=dat)
res0 <- lm(sales ~ 1,                      data=dat)
anova(res0, res1)

# we can also test other subsets of predictors; for example, test the null
# hypothesis H_0: beta_1 = beta_2 = 0
res1 <- lm(sales ~ TV + radio + newspaper, data=dat)
res0 <- lm(sales ~              newspaper, data=dat)
anova(res0, res1)

# tests of each individual slope are provided in the output
summary(res)

# in fact, these tests of individual predictors are identical to comparing two
# models as above, leaving out the predictor we want to test
res1 <- lm(sales ~ TV + radio + newspaper, data=dat)
res0 <- lm(sales ~ TV + radio,             data=dat)
anova(res0, res1)

# when we do a hypothesis test using the rule that we reject H_0 when p < .05,
# but the null hypothesis is actually true, then we have a .05 (5%) change of
# falsely rejecting it (this is called a 'Type I error'); this implies that we
# have 1 - .05 (95%) chance of NOT rejecting the null hypothesis; now if we do
# p = 100 such hypothesis tests, where the null hypothesis is true for every
# one of the tests (and assuming independence between the tests), then the
# probability of not rejecting *any* null hypothesis is (1 - .05)^100 =~ .006;
# this then implies that the probability of rejecting *at least one* null
# hypothesis in these 100 tests is 1 - (1 - .05)^100 =~ .994

# we can easily demonstrate this with a simulation
n <- 1000
X <- replicate(100, rnorm(n))
#y <- 5 + 0*X[,1] + 0*X[,2] + ... + 0*X[,100] + rnorm(n)
y <- 5 + rnorm(n)
res <- lm(y ~ X)
summary(res)

# the chance is very high that at least one of the predictors is significant;
# the F-test guards us against looking at all of these individual tests

# demonstration that a regression model will fit perfectly when the number of
# model coefficients (i.e., the intercept plus the slopes for the predictors)
# is equal to the sample size
n <- 100
X <- replicate(99, rnorm(n))
y <- 5 + rnorm(n)
res <- lm(y ~ X)
summary(res) # note: R^2 = 1
resid(res) # and all of the residuals are equal to 0

############################################################################

# we got a little side-tracked here on the issue how floating point numbers
# are represented in computers and how this can lead to some surprising things
sqrt(1/2) * sqrt(1/2)
sqrt(1/2) * sqrt(1/2) - 0.5
print(sqrt(1/2) * sqrt(1/2), digits=18)

# https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-doesn_0027t-R-think-these-numbers-are-equal_003f

############################################################################

# back to the advertisement dataset and let's just consider 2 predictors; so
# we can consider 4 possible models
res0 <- lm(sales ~ 1,          data=dat)
res1 <- lm(sales ~ TV,         data=dat)
res2 <- lm(sales ~ radio,      data=dat)
res3 <- lm(sales ~ TV + radio, data=dat)
summary(res0)
summary(res1)
summary(res2)
summary(res3)

# back to the 'full' model with all predictors
res4 <- lm(sales ~ TV + radio + newspaper, data=dat)
summary(res4)

# R^2 = .8972 (so about 90% of the variance in sales is accounted for by these
# three predictors); this happens to be identical to the squared correlation
# between Y and the predicted values (Y-hat)
summary(res4)$r.squared
cor(dat$sales, predict(res4))^2

# compare the R^2 from all models above
summary(res0)$r.squared
summary(res1)$r.squared
summary(res2)$r.squared
summary(res3)$r.squared
summary(res4)$r.squared

############################################################################

# draw a 3d scatterplot like Figure 3.5 but make it a bit easier to see the
# points that fall above the plane (the red dots; positive residuals) and
# those that fall below the plane (the blue dots; negative residuals)

#install.packages("scatterplot3d")
library(scatterplot3d)

s3d <- scatterplot3d(dat$TV, dat$radio, dat$sales, pch=19, color="white",
                     xlab="TV", ylab="radio", zlab="Sales")

orig <- s3d$xyz.convert(dat$TV, dat$radio, dat$sales)
fit  <- s3d$xyz.convert(dat$TV, dat$radio, fitted(res3))

posresid <- resid(res3) > 0
negresid <- resid(res3) < 0

segments(orig$x[negresid], orig$y[negresid], fit$x[negresid], fit$y[negresid], col="blue")
s3d$points3d(dat$TV[negresid], dat$radio[negresid], dat$sales[negresid], pch=19, col="blue")

s3d$plane3d(res3, lty="dotted", draw_polygon=TRUE, polygon_args=list(col=rgb(0,0,0,0.4)))

segments(orig$x[posresid], orig$y[posresid], fit$x[posresid], fit$y[posresid], col="red")
s3d$points3d(dat$TV[posresid], dat$radio[posresid], dat$sales[posresid], pch=19, col="red")

############################################################################

# plot TV versus sales
plot(sales ~ TV, data=dat, pch=19, xlab="TV Advertisement", ylab="Sales")

# add the 'marginal' regression line to the figure
TVvals <- seq(0, 300, length=100)
pred <- coef(res3)[1] + coef(res3)[2] * TVvals + coef(res3)[3] * mean(dat$radio)
lines(TVvals, pred, lwd=5, col="red")

# plot radio versus sales
plot(sales ~ radio, data=dat, pch=19, xlab="Radio Advertisement", ylab="Sales")

# add the 'marginal' regression line to the figure
radiovals <- seq(0, 50, length=100)
pred <- coef(res3)[1] + coef(res3)[2] * mean(dat$TV) + coef(res3)[3] * radiovals
lines(radiovals, pred, lwd=5, col="red")

# can do the same thing using the 'visreg' package
#install.packages("visreg")
#library(visreg)
#visreg(res3)

# partial regression plots (https://en.wikipedia.org/wiki/Partial_regression_plot)
plot(resid(lm(TV ~ radio, data=dat)), resid(lm(sales ~ radio, data=dat)), pch=19)
abline(a=0, b=coef(res3)[2], lwd=5, col="red")
plot(resid(lm(radio ~ TV, data=dat)), resid(lm(sales ~ TV, data=dat)), pch=19)
abline(a=0, b=coef(res3)[3], lwd=5, col="red")

# can compute predicted values manually using the coefficients
summary(res3)
coef(res3)
coef(res3)[[1]] + coef(res3)[[2]] * 100 + coef(res3)[[3]] * 20

# but the predict() function can do the work for us and in addition it can
# provide a confidence interval for the predicted average outcome and a
# prediction interval for the predicted sales
predict(res3, newdata=data.frame(TV = 100, radio = 20), interval = "confidence")
predict(res3, newdata=data.frame(TV = 100, radio = 20), interval = "prediction")

# using this, we can visualize again the marginal relationship between TV and
# sales (holding radio constant at its mean) and add the confidence and
# prediction intervals to the plot
plot(sales ~ TV, data=dat, pch=19, xlab="TV Advertisement", ylab="Sales")
TVvals <- seq(0, 300, length=100)
pred <- predict(res3, newdata=data.frame(TV = TVvals, radio = mean(dat$radio)), interval = "prediction")
pred <- data.frame(pred)
polygon(c(TVvals,rev(TVvals)), c(pred$lwr,rev(pred$upr)), col=rgb(0,0,0,0.2), border=NA)
pred <- predict(res3, newdata=data.frame(TV = TVvals, radio = mean(dat$radio)), interval = "confidence")
pred <- data.frame(pred)
polygon(c(TVvals,rev(TVvals)), c(pred$lwr,rev(pred$upr)), col=rgb(0,0,0,0.4), border=NA)
lines(TVvals, pred$fit, lwd=5, col="red")
points(sales ~ TV, data=dat, pch=19)

############################################################################

### 3.3.1 Qualitative Predictors

# install the ISLR2 package
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# copy the Credit dataset to dat
dat <- Credit

# scatterplot matrix of all quantitative variables (Figure 3.6)
pairs(dat[c("Balance", "Age", "Cards", "Education", "Income", "Limit", "Rating")],
      pch = 19, col = "blue", cex = 0.2)

# simple regression with a dummy variable (Table 3.7)
res <- lm(Balance ~ Own, data=dat)
summary(res)

# look at the model matrix to see how the encoding has been done
head(model.matrix(res))
head(data.frame(Own = dat$Own, model.matrix(res)))

# this regression model is in essence doing the same as a simple t-test
t.test(Balance ~ Own, data=dat, var.equal=TRUE)

# change the 'reference level' to 'Yes'
res <- lm(Balance ~ relevel(factor(Own), ref="Yes"), data=dat)
summary(res)

# since 'Own' is already a factor to begin with, don't need factor(); but you
# would need this if 'Own' was a pure string/character variable
res <- lm(Balance ~ relevel(Own, ref="Yes"), data=dat)
summary(res)

# change the encoding of the factor to +1/-1 encoding
res <- lm(Balance ~ C(Own, contr="contr.sum"), data=dat)
head(data.frame(Own = dat$Own, model.matrix(res)))
summary(res)

# these are all different 'parameterizations' of the same model

# in fact, there is another parameterization we could consider
res <- lm(Balance ~ 0 + Own, data=dat)
#res <- lm(Balance ~ -1 + Own, data=dat) # same
summary(res)

# here, the model is given by Y = beta_1 * OwnNo + beta_2 * OwnYes + error,
# where OwnNo = 1 for non-homeowners and 0 otherwise and OwnYes = 1 for
# homeowners and 0 otherwise
head(data.frame(Own = dat$Own, model.matrix(res)))

# so the model implies:
#
# Y = beta_1 + error for non-homeowners
# Y = beta_2 + error for     homeowners
#
# this is again the same model; have to be a bit careful though with the R^2
# and F-statistics, since these have different meanings now

# note: the estimated values for the two groups are the same as the group means
mean(dat$Balance[dat$Own == "No"])
mean(dat$Balance[dat$Own == "Yes"])

# regression with a categorical predictor with three levels (Table 3.8)
res <- lm(Balance ~ Region, data=dat)
summary(res)

# look at the model matrix to see how this encoding has been done
head(data.frame(Own = dat$Region, model.matrix(res)), 10)

# this is the same as doing a one-way ANOVA with three groups
summary(aov(Balance ~ Region, data=dat))

# to test the difference between regions South and West, we can do a linear
# contrast, comparing the RegionSouth and RegionWest coefficients against each
# other; the linearHypothesis() function from the 'car' package can do this
#install.packages("car")
library(car)
linearHypothesis(res, hypothesis.matrix=c(0,1,-1))

# we are testing: H_0: 0*beta_0 + 1*beta_1 + -1*beta_2 = beta_1 - beta_2 = 0

# we can again use the parameterization where we remove the intercept
res <- lm(Balance ~ 0 + Region, data=dat)
summary(res)

# again, the coefficients are just the means of the three regions
mean(dat$Balance[dat$Region == "East"])
mean(dat$Balance[dat$Region == "South"])
mean(dat$Balance[dat$Region == "West"])

# a model with a quantitative predictor (Income) and a categorical one (Student)
res <- lm(Balance ~ Income + Student, data=dat)
summary(res)

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-03-23
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 1.4 - 1.7
#
# last updated: 2023-03-24

############################################################################

### 1.4: Challenges in building, understanding, and interpreting regressions

## Regression to estimate a relationship of interest

# note: while we are using the same code as used in the book to simulate the
# data below, it turns out that the actual seed that must have been used is
# different, so we cannot exactly reproduce the same data and hence the
# results are slightly different

# simulate data as is used in Figure 1.5
set.seed(1151)
n <- 50
x <- runif(n, 1, 5)
g <- ifelse(x < 3, 0, 1)
y <- 10 + 3 * x + rnorm(n, 0, 3)

# plot the data as in Figure 1.5(a)
plot(g, y, pch=21, bg="gray", lwd=1.5, xlim=c(-0.1,1.1), ylim=c(9,31),
     xaxt="n", xlab="", ylab="Outcome Measurement", bty="l")
axis(side=1, at=c(0,1), label=c("Control", "Treatment"))
title("Regression with a binary treatment")

# fit regression model and add regression line to the plot
res <- lm(y ~ g)
summary(res)
abline(res, lwd=5)

# plot the data as in Figure 1.5(b)
plot(x, y, pch=21, bg="gray", lwd=1.5, xlim=c(1,5), ylim=c(9,31),
     xlab="Treatment Level", ylab="Outcome Measurement", bty="l")
title("Regression with continuous treatment")

# fit regression model and add regression line to the plot
res <- lm(y ~ x)
summary(res)
abline(res, lwd=5)

# simulate some data as in Figure 1.6
set.seed(1151)
y <- 5 + 30 * exp(-x) + rnorm(n, 0, 2)

# plot the data
plot(x, y, pch=21, bg="gray", lwd=1.5, xlim=c(1,5), ylim=c(0,15),
     xlab="Treatment Level", ylab="Outcome Measurement", bty="l")
title("Nonlinear treatment effect")

# fit non-linear regression model and add regression line to the plot
res <- nls(y ~ int + slope * exp(-x), start=list(int=1, slope=5))
summary(res)
xs <- seq(1,5,length=500)
pred <- predict(res, newdata=data.frame(x=xs))
lines(xs, pred, lwd=5)

# plot the data again for Figure 1.6(b)
plot(x, y, pch=21, bg="gray", lwd=1.5, xlim=c(1,5), ylim=c(0,15),
     xlab="Treatment Level", ylab="Outcome Measurement", bty="l")
title("Nonlinear effect, estimated with straight line fit")

# fit regression model and add regression line to the plot
res <- lm(y ~ x)
summary(res)
abline(res, lwd=5)

# a fitted-versus-residuals plot can reveal the non-linearity in the relationship
plot(fitted(res), residuals(res), pch=21, bg="gray", lwd=1.5,
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, lwd=3)

# same plot (this adds a smoother to the plot, which can help to identify if
# there might be a relationship between the fitted values and the residuals)
plot(res, which=1, pch=21, bg="gray", lwd=3, id.n=0)

# simulate some data where there is an interaction between a quantitative
# predictor and a dichotomous grouping variable
set.seed(1234)
g <- sample(c(0,1), n, replace=TRUE)
x <- runif(n, 1, 5)
y <- 10 + 5 * g + 1 * x + 4 * x*g + rnorm(n, 0, 5)

# plot the data
plot(x, y, pch=21, bg=ifelse(g==1, "dodgerblue", "firebrick"), cex=1.2,
     lwd=1.5, xlim=c(1,5), ylim=c(0,50), xlab="Treatment Level",
     ylab="Outcome Measurement", bty="l")

# fit regression model and add regression line to the plot
res <- lm(y ~ g + x + g:x)
summary(res)
xs <- seq(1,5,length=500)
pred <- predict(res, newdata=data.frame(x=xs, g=0))
lines(xs, pred, lwd=5, col="firebrick")
pred <- predict(res, newdata=data.frame(x=xs, g=1))
lines(xs, pred, lwd=5, col="dodgerblue")

## Regression to adjust for differences between treatment and control groups

# simulate data as in Figure 1.8
set.seed(1151)
n <- 100
g <- rep(0:1, n/2)
x <- ifelse(g==0, rnorm(n, 0, 1.2)^2, rnorm(n, 0, 0.8)^2)
y <- rnorm(n, 20 + 5*x + 10*g, 3)

# draw Figure 1.8
plot(x, y, pch=ifelse(g==0, 19, 21), cex=ifelse(g==0, 0.5, 1),
     xlab="Pre-treatment predictor", ylab="Outcome Measurement", bty="l")

# fit the regression model ignoring x
res <- lm(y ~ g)
summary(res)

# fit the regression model where we control for x
res <- lm(y ~ g + x)
summary(res)

# add the regression lines for the control and treatment groups
xs <- seq(0,10,length=500)
pred <- predict(res, newdata=data.frame(x=xs, g=0))
lines(xs, pred, lwd=3)
pred <- predict(res, newdata=data.frame(x=xs, g=1))
lines(xs, pred, lwd=3)

# add the arrow
pred5.0 <- predict(res, newdata=data.frame(x=5, g=0))
pred5.1 <- predict(res, newdata=data.frame(x=5, g=1))
arrows(5, pred5.0, 5, pred5.1, code=3, lwd=3)

# add text
text(5, pred5.0, "Control", pos=4)
text(5, pred5.1, "Treated", pos=2)

## Interpreting coefficients in a predictive model

# simulate some data like in the (somewhat weird/artificial) example
set.seed(1234)
n <- 1000
height <- runif(n, 60, 82) # about 152cm to 208cm in non-freedom units
earnings <- 11000 + 1500 * (height - 60) + rnorm(n, 0, 22000)
earnings[earnings < 0] <- 0 # cannot earn less than 0
plot(height, earnings, pch=21, bg="gray", cex=0.4, xlab="Height", ylab="Earnings")
res <- lm(earnings ~ height)
summary(res)
abline(res, lwd=5)

############################################################################

### 1.5: Classical and Bayesian inference

## Bayesian inference

# we can replicate the Bayesian inference as follows

# the effect is actually measured on a log scale, so we need to transform the
# estimate accordingly and we can then also reconstruct the standard error of
# the estimate from the confidence interval
estimate <- log(1.42)
stderr <- (log(1.98) - log(1.02)) / (2*1.96)

# double-check that we get the same CI as under classical inference
round(exp(estimate + c(-1,1) * 1.96 * stderr), digits=2)

# as noted in Exercise 9.6, for the prior we use a normal distribution with a
# mean of 0 and a standard deviation of 0.10

# we can then combine the estimate and the prior as described in equation 9.3
# and get the corresponding standard error as described in equation 9.4
estimate.post <- weighted.mean(c(0,estimate), 1/c(.10,stderr)^2)
stderr.post <- 1/sqrt(sum(1/c(.10,stderr)^2))

# based on these we can then construct a 95% posterior interval and then
# exponentiate the bounds to get the interval for the multiplicative effect
round(exp(estimate.post + c(-1,1) * 1.96 * stderr.post), digits=2)

# note: the upper bound (1.28) in the book is not quite correct

# this is identical to doing a meta-analysis where we treat the prior as if it
# was a study with an estimate equal to its mean and a standard error equal to
# its standard deviation; we can do this for example with the metafor package

# install.packages("metafor")
library(metafor)
res <- rma(c(0,estimate), sei=c(.10,stderr), method="EE")
predict(res, transf=exp, digits=2)

############################################################################

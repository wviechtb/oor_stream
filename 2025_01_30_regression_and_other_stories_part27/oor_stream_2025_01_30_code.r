############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-01-30
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.1 - 12.4
#
# last updated: 2025-01-31

############################################################################

# load the rstanarm package
library(rstanarm)

### 12.1: Linear transformations

## Scaling of predictors and regression coefficients

# download the dataset if it doesn't already exist
if (!file.exists("earnings.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the first six rows of the dataset
head(dat)

# fit a model predicting earnings from height
res <- stan_glm(earn ~ height, data=dat, refresh=0)
res

# Figure 12.1(a): plot of height versus earnings (with some jittering on the
# height values, to make the points more easily distinguishable and restricting
# the range of the y-axis so that one data point where earn=400000 does not
# squish together the rest of the data)
plot(earn ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", ylab="earnings", bty="l", ylim=c(0,200000))

# extract the sampled values for the posterior distributions
post <- as.data.frame(res)
head(post)

# add 10 regression lines based on these posterior samples
invisible(apply(post[1:10,], 1, function(b) abline(b[1], b[2])))

# Figure 12.1(b): same plot but the x-axis extended to 0
plot(earn ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", ylab="earnings", bty="l", xlim=c(0, max(dat$height)),
     ylim=c(-100000, 200000))

# add 10 regression lines based on these posterior samples
invisible(apply(post[1:10,], 1, function(b) abline(b[1], b[2])))

# fit a model predicting earnings from height in millimeters
dat$height_mm <- dat$height * 25.4
res <- stan_glm(earn ~ height_mm, data=dat, refresh=0)
res

# fit a model predicting earnings from height in miles
dat$height_miles <- dat$height / 63360
res <- stan_glm(earn ~ height_miles, data=dat, refresh=0)
res

# fit a model predicting earnings from standard deviation units of height
dat$height_sd <- dat$height / sd(dat$height)
res <- stan_glm(earn ~ height_sd, data=dat, refresh=0)
res

## Standardization using z-scores

# fit a model predicting earnings from standardized height
dat$height_z <- c(scale(dat$height))
# same as dat$height_z <- (dat$height - mean(dat$height)) / sd(dat$height)
res <- stan_glm(earn ~ height_z, data=dat, refresh=0)
res

############################################################################

### 12.2: Centering and standardizing for models with interactions

# download the dataset if it doesn't already exist
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# model with the interaction between mom_hs and mom_iq
res <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=dat, refresh=0)
res

## Centering by subtracting the mean of the data

# center each predictor at its mean
dat$c_mom_hs <- dat$mom_hs - mean(dat$mom_hs)
dat$c_mom_iq <- dat$mom_iq - mean(dat$mom_iq)

# refit the model with these centered predictors
res <- stan_glm(kid_score ~ c_mom_hs + c_mom_iq + c_mom_hs:c_mom_iq, data=dat, refresh=0)
res

## Using a conventional centering point

# center each predictor at a sensible reference point
dat$c2_mom_hs <- dat$mom_hs - 0.5
dat$c2_mom_iq <- dat$mom_iq - 100

# sidenote: mean(dat$mom_iq) is exactly 100 for this dataset, so the mom IQ
# scores were already rescaled to have exactly this mean (and sd(dat$mom_iq)
# is exactly 15)

# refit the model with these centered predictors
res <- stan_glm(kid_score ~ c2_mom_hs + c2_mom_iq + c2_mom_hs:c2_mom_iq, data=dat, refresh=0)
res

## Standardizing by subtracting the mean and dividing by 2 standard deviations

# standardize each predictor and divide by 2
dat$z_mom_hs <- (dat$mom_hs - mean(dat$mom_hs)) / (2*sd(dat$mom_hs))
dat$z_mom_iq <- (dat$mom_iq - mean(dat$mom_iq)) / (2*sd(dat$mom_iq))

# refit the model with these standardized predictors
res <- stan_glm(kid_score ~ z_mom_hs + z_mom_iq + z_mom_hs:z_mom_iq, data=dat, refresh=0)
res

############################################################################

### 12.3: Correlation and "regression to the mean"

# predict standardized y from standardized x (here, we use least squares
# estimation for simplicity and also because then what is stated in the book
# will be exactly true)
res <- lm(scale(kid_score) ~ scale(mom_iq), data=dat)
res

# note that the intercept is essentially 0 (it is not shown as exactly 0 because
# of the finite precision of the calculations, but we can treat it as 0 for all
# intents and purposes)

# force the intercept to be exactly 0 (i.e., remove the intercept term)
res <- lm(scale(kid_score) ~ 0 + scale(mom_iq), data=dat)
res

# the slope is exactly equal to the correlation between the two variables
coef(res)
cor(dat$kid_score, dat$mom_iq)

# if we do the same with stan_glm(), then the coefficient is very close to the
# correlation but not exactly identical due to the fact that we only have 4000
# sampled values from the posterior distribution of the slope (plus the priors
# have an influence on the posterior distributions, although given the size of
# the dataset, that influence will be quite small)
res <- stan_glm(scale(kid_score) ~ 0 + scale(mom_iq), data=dat, refresh=0)
coef(res)

## The principal component line and the regression line

# simulate some data from a bivariate normal distribution with a correlation of
# 0.5 and force the means to be 0 and the standard deviations to be 1
library(MASS)
set.seed(1234)
n <- 1000
dat <- mvrnorm(n, mu=c(0,0), Sigma=matrix(c(1,0.5,0.5,1), nrow=2), empirical=TRUE)
dat <- as.data.frame(dat)
names(dat) <- c("x", "y")
head(dat)

# plot the data and add the diagonal line (which is the line that minimizes the
# squared perpendicular distance between x and y)
plot(y ~ x, data=dat, pch=19, cex=0.5, xlim=range(dat), ylim=range(dat))
abline(0, 1, lwd=3)

# sidenote: this relates to a form of regression called Deming regression
# (https://en.wikipedia.org/wiki/Deming_regression)

# fit the model predicting y from x using least-squares estimation
res <- lm(y ~ x, data=dat)

# the intercept is then 0 and the slope is 0.5
round(coef(res), 4)

# add the regression line to the plot
abline(res, lwd=3, col="darkgray")

# compute the sum of the squared residuals based on the diagonal line
pred  <- 0 + 1*dat$x
resid <- dat$y - pred
sum(resid^2)

# compute the sum of the squared residuals based on the regression model
resid <- resid(res)
sum(resid^2)

## Regression to the mean

# see section 6.5 and the code from oor_stream_2024_04_11_code.r for a
# simulation of heights over many generations to demonstrate that the heights of
# daughters do not become more and more similar over time

############################################################################

### 12.4: Logarithmic transformations

## Earnings and height example

# read in the dataset
dat <- read.csv("earnings.csv")

# fit the model predicting earn from height
res1 <- stan_glm(earn ~ height, data=dat, refresh=0)
print(res1, digits=2)

# fit the model predicting log(earn) from height
res2 <- stan_glm(log(earn) ~ height, data=dat, refresh=0, subset=earn>0)
print(res2, digits=2)

# Figure 12.3(a): plot of height versus log(earnings)
plot(log(earn) ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", bty="l")

# extract the sampled values for the posterior distributions
post <- as.data.frame(res2)
head(post)

# add 10 regression lines based on these posterior samples
invisible(apply(post[1:10,], 1, function(b) abline(b[1], b[2])))

# Figure 12.3(b): plot of height versus earnings
plot(earn ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", bty="l", ylim=c(0,200000))

# add 10 regression lines based on these posterior samples
xs <- seq(min(dat$height), max(dat$height), length.out=1000)
apply(post[1:10,], 1, function(b) lines(xs, exp(b[1] + b[2]*xs)))

# exponentiate the coefficient for height
exp(coef(res2)[2])

# this is interpreted in the book as indicating a 6% 'expected positive
# difference' in earnings for people that differ in height by one inch
#
# let's look at this more carefully; the model we are using is this:
#
# E[log(y)|x] = beta0 + beta1 * x
#
# now suppose x is one unit higher, then:
#
# E[log(y)|x+1] = beta0 + beta1 * (x+1)
#
# hence beta1 = E[log(y)|x+1] - E[log(y)|x]
#
# so exp(beta1) = exp(E[log(y)|x+1] - E[log(y)|x])
#               = exp(E[log(y)|x+1]) / exp(E[log(y)|x])
#
# now, in general, for non-linear transformations, E[f(y)] != f(E[y]) due to
# Jensen's inequality (https://en.wikipedia.org/wiki/Jensen's_inequality)
#
# however, it is quite common to treat exp(beta1) as if it reflects
#
# exp(beta1) = E[y|x+1] / E[y|x]
#
# and the book essentially does the same thing; strictly speaking, this is not
# correct, although it often hold as a good approximation
#
# it would be 100% correct to say that exp(beta1) = Median(y|x+1) / Median(y|x)

# Figure 12.5(a): plot the kernel density estimate of the distribution of earnings
plot(density(dat$earn), lwd=5, bty="n", main="")

# obtain samples from the posterior distribution of the predicted earnings based
# on the model where we directly predict earnings
pred1 <- posterior_predict(res1)

# add 100 kernel density estimates from these samples to the plot
invisible(apply(pred1[sample(nrow(pred1), 100),], 1, function(x) lines(density(x), col="gray")))
lines(density(dat$earn), lwd=5)

# Figure 12.5(b): plot the kernel density estimate of the distribution of log(earnings)
plot(density(log(dat$earn[dat$earn > 0])), lwd=5, bty="n", main="")

# obtain samples from the posterior distribution of the predicted earnings based
# on the model where we predict log(earnings)
pred2 <- posterior_predict(res2)

# add 100 kernel density estimates from these samples to the plot
invisible(apply(pred2[sample(nrow(pred2), 100),], 1, function(x) lines(density(x), col="gray")))
lines(density(log(dat$earn[dat$earn > 0])), lwd=5)

## Why we use natural log rather than log base 10

# fit the model predicting log10(earn) from height
res3 <- stan_glm(log10(earn) ~ height, data=dat, refresh=0, subset=earn>0)
print(res3, digits=2)

# back-transform the coefficient for height so that it (approximately) reflects
# the ratio of earnings for people differing in height by one inch
10^(coef(res3)[2])

## Building a regression model on the log scale

# fit the model predicting log(earn) from height and sex
res <- stan_glm(log(earn) ~ height + male, data=dat, refresh=0, subset=earn>0)
print(res, digits=2)
exp(coef(res)[2:3])

# obtain samples from the posterior distribution of log(earn) for a 70-inch tall woman
pred70f <- posterior_predict(res, newdata=data.frame(height=70, male=0))

# obtain the mean and the interval mean +- sigma for log(earn)
mean(pred70f)
mean(pred70f) + c(-1,1) * sigma(res)

# we could also obtain the 16th and 84th quantile from the posterior
# distribution, which is essentially the same thing
quantile(pred70f, c(.16,.84))

# exponentiate the bounds to obtain the interval for the earnings
round(exp(mean(pred70f) + c(-1,1) * sigma(res)))

# sidenote: the interval given in the book is based on rounded values

# compute the (median) R^2
median(bayes_R2(res))

# include the interaction term
res <- stan_glm(log(earn) ~ height + male + height:male, data=dat, refresh=0, subset=earn>0)
print(res, digits=2)

# standardize height
dat$z_height <- (dat$height - mean(dat$height)) / sd(dat$height)

# use z_height in the model
res <- stan_glm(log(earn) ~ z_height + male + z_height:male, data=dat, refresh=0, subset=earn>0)
print(res, digits=2)

## Log-log model: transforming the input and outcome variables

# log-transform height
dat$log_height <- log(dat$height)

# model where we predict log(earn) from log(height)
res <- stan_glm(log(earn) ~ log_height + male, data=dat, refresh=0, subset=earn>0)
print(res, digits=2)

############################################################################

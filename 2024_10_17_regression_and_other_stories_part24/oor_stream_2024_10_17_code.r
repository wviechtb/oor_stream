############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-10-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 11.4 - 11.5
#
# last updated: 2024-10-25

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### addendum to the last session

# the book doesn't actually give examples of what a 'fitted versus residuals'
# plot might look like when some of the model assumptions are violated; let's
# consider a few examples

# simulate data with a non-linear relationship between x and y
set.seed(1234)
N <- 500
x <- runif(N, 0, 1)
y <- 1 + 4*x + -2*x^2 + rnorm(N, mean=0, sd=0.25)
dat <- data.frame(x=x, y=y)
head(dat)

# plot x versus y
plot(dat$x, dat$y, xlab="x", ylab="y", pch=21, bg="darkgray")

# fit the model that assumes a linear relationship between x and y
res <- stan_glm(y ~ x, data=dat, refresh=0)
res

# compute the predicted values and residuals
pred  <- predict(res)
resid <- dat$y - pred

# plot the fitted values versus the residuals
plot(pred, resid, pch=19, main="Residuals vs. predicted values",
     xlab="predicted value", ylab="residual",
     panel.first=abline(h=0, col="gray", lwd=5))

# we see an upside down U shape in the plot, which is indicative of missing
# the non-linear relationship between x and y

# simulate data with heteroscedasticity
N <- 500
x <- runif(N, 0, 1)
y <- 1 + 1*x + rnorm(N, mean=0, sd=(0.1 + 0.3*(1 - x)))
dat <- data.frame(x=x, y=y)
head(dat)

# plot x versus y
plot(dat$x, dat$y, xlab="x", ylab="y", pch=21, bg="darkgray")

# fit the model that assumes homoscedasticity
res <- stan_glm(y ~ x, data=dat, refresh=0)
res

# compute the predicted values and residuals
pred  <- predict(res)
resid <- dat$y - pred

# plot the fitted values versus the residuals
plot(pred, resid, pch=19, main="Residuals vs. predicted values",
     xlab="predicted value", ylab="residual",
     panel.first=abline(h=0, col="gray", lwd=5))

# we see a > shape in the plot (i.e., the residuals fluctuate more for low
# predicted values), which is indicative of heteroscedasticity

############################################################################

### 11.4: Comparing data to replications from a fitted model

# clean up the workspace
rm(list=ls())

## Example: simulation-based checking of a fitted normal distribution

# download the dataset (need to do this once)
if (!file.exists("newcomb.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Newcomb/data/newcomb.txt", destfile="newcomb.txt")

# read in the data and inspect the first 6 rows
dat <- read.table("newcomb.txt", header=TRUE)
head(dat)

# Figure 11.9: histogram of the y values in the dataset
hist(dat$y, main="", breaks=40)

# fit a linear regression model without predictors (an intercept-only model)
res <- stan_glm(y ~ 1, data=dat, refresh=0)
res

# extracted the sampled values from the posterior distribution of the
# intercept and error standard deviation
sims <- as.data.frame(res)
head(sims)

# simulate new data based on the sampled intercept and error SD values
set.seed(1234)
y_rep <- apply(sims, 1, function(x) rnorm(nrow(dat), mean=x[1], sd=x[2]))

# so we get 4000 simulated datasets
dim(y_rep)

# let's transpose y_rep, so each row corresponds to a dataset
y_rep <- t(y_rep)

# examine the first 6 data points of datasets 1 through 5
y_rep[1:5,1:6]

# we can also use posterior_predict() to accomplish the same thing
set.seed(1234)
y_rep <- posterior_predict(res)
y_rep[1:5,1:6]

# Visual comparison of actual and replicated datasets

# Figure 11.10: histogram of 20 randomly selected datasets from y_rep
par(mfrow=c(5,4), mar=c(3,3,2,2))
for (s in sample(nrow(y_rep), 20)){
   hist(y_rep[s,], main="", xlab="", ylab="", breaks=seq(-10,65,by=5))
}
par(mfrow=c(1,1), mar=c(5,4,4,2))

# Figure 11.11: plot the kernel density estimate of the distribution of y
plot(density(dat$y), lwd=5, main="", xlim=c(-50,55))

# add the kernel density estimate of each simulated dataset to the plot
apply(y_rep, 1, function(x) lines(density(x), col=rgb(0,0,0,.02)))

# Checking model fit using a numerical data summary

# compute the minimum value for each simulated dataset
test_rep <- apply(y_rep, 1, min)

# Figure 11.12: histogram of these minimum values with a vertical line at the
# minimum value observed in the actual data
hist(test_rep, xlim=c(-50,20), breaks=40, main="", xlab="")
abline(v=min(dat$y), lwd=5)

############################################################################

# before we get to the type of model discussed in the book, we will first do
# another illustration of the principle discussed at the end of section 11.4
# using the dataset from section 11.5

# download the dataset (need to do this once)
if (!file.exists("unemp.txt")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Unemployment/data/unemp.txt", destfile="unemp.txt")

# read in the data and inspect the first 6 rows
dat <- read.table("unemp.txt", header=TRUE)
head(dat)

# sample size
n <- nrow(dat)

# Figure 11:13: unemployment rate over time
plot(dat$year, dat$y, type="l", xlab="Year", ylab="Unemployment rate",
     ylim=c(0,10), bty="l", lwd=3)

# say we want to model the trend in the unemployment rate as a simple linear
# model with year as the predictor
res <- stan_glm(y ~ year, data=dat, refresh=0)
print(res, digits=3)

# extract the posterior samples
sims <- as.data.frame(res)
head(sims)
n_sims <- nrow(sims)

# based on the model, we can be certain that the slope is positive
mean(sims$year > 0)

# but maybe our model is wrong! let's put some of the ideas we have learned so
# far in this chapter into practice

# compute the predicted values and residuals
pred  <- predict(res)
resid <- dat$y - pred

# plot the fitted values versus the residuals
plot(pred, resid, pch=19, main="Residuals vs. predicted values",
     xlab="predicted value", ylab="residual")
abline(h=0, col="gray", lwd=5)

# maybe there is a hint of an upside down U shape in the plot, but let's not
# overinterpret this

# simulate new data based on the model using posterior_predict()
y_rep <- posterior_predict(res)

# plot the unemployment rate over time based on the actual data and based on
# 14 randomly chosen simulated datasets
par(mfrow=c(3,5), mar=c(3,2.5,2,1))
plot(dat$year, dat$y, type="l", xlab="", ylab="", ylim=c(0,12), bty="l", lwd=2, main="Actual data")
invisible(sapply(sample(n_sims, 14),
                 function(i) plot(dat$year, y_rep[i,], type="l", xlab="", ylab="",
                                  ylim=c(0,12), bty="l", lwd=2, main=paste("Simulation", i))))
par(mfrow=c(1,1), mar=c(5,4,4,2))

# the lines look quite different for the simulated datasets; the fluctuations
# are more rapid compared to the actual data; this is an indication that our
# model might be wrong

# let's try plotting a kernel density estimate of the observed residuals
plot(density(resid), lwd=5, main="", xlim=c(-6,6), ylim=c(0,0.35))

# add the kernel density estimate of the residuals from each simulated dataset
# to the plot (we'll use a for-loop for this)
for (i in 1:nrow(y_rep)) {
   lines(density(y_rep[i,] - (sims[i,1] + dat$year*sims[i,2])), col=rgb(0,0,0,.02))
}

# we see that the distribution of the observed residuals is somewhat unusual
# compared to the distributions based on the simulated data; but remember that
# the normality assumption is not one of the more critical assumptions, so
# maybe this is not something to worry about that much

# but an assumption that is more important to worry about is the 'independence
# of the errors' assumption; we will check this by computing the correlation
# between adjacent residuals (the lag-1 autocorrelation)
ar1 <- cor(resid[1:(n-1)], resid[2:n])
ar1

# compute the lag-1 autocorrelations based on the simulated datasets
ar1i <- rep(NA, nrow(y_rep))
for (i in 1:nrow(y_rep)) {
   r_rep <- y_rep[i,] - (sims[i,1] + dat$year*sims[i,2])
   ar1i[i] <- cor(r_rep[1:(n-1)], r_rep[2:n])
}

# plot the kernel density estimate of these autocorrelations and add a
# vertical line at the actually observed autocorrelation
plot(density(ar1i), lwd=2, xlim=c(-0.5,0.8), main="")
abline(v=ar1, lwd=5)

# we see that the actually observed autocorrelation is much higher than those
# observed in the simulated datasets; this shows that our model is wrong (as
# it assumes that the errors are independent, which is apparently not true for
# these data)

############################################################################

### 11.5: Example: predictive simulation to check the fit of a time-series model

## Fitting a first-order autoregression to the unemployment series

# add the lagged value of y to the dataset
dat$y_lag <- c(NA, dat$y[1:(n-1)])
head(dat)

# compute the lag-1 autocorrelation in y
cor(dat$y, dat$y_lag, use="complete.obs")

# fit a linear regression model predicting y from the value from the previous year
res <- stan_glm(y ~ y_lag, data=dat, refresh=0)
print(res, digits=2)

# note that the coefficient for y_lag from the model is actually an estimate
# of the autocorrelation in the y variable; earlier, we modeled the linear
# trend in y and computed the autocorrelation in the residuals (~0.74); if we
# add year as a predictor to the autocorrelation model, then we will find a
# very similar estimate for the y_lag coefficient
res <- stan_glm(y ~ y_lag + year, data=dat, refresh=0)
print(res, digits=2)

# remember that based on the model that ignored the autocorrelation, we were
# certain that the time trend in the unemployment rate is positive; let's see
# how certain we can be about this when we account for autocorrelation
sims <- as.data.frame(res)
mean(sims$year > 0)

# this is around 75%, which is still large, but considerably lower than what
# we obtained earlier; this shows that violation of the independence
# assumption can have a considerable impact on the findings

# but let's get back to the model that ignores any time trends
res <- stan_glm(y ~ y_lag, data=dat, refresh=0)
print(res, digits=2)

## Simulating replicated datasets

# extract the posterior samples
sims <- as.data.frame(res)
n_sims <- nrow(sims)

# simulate new datasets (note: we have to do this manually, that is, not using
# posterior_predict() as noted in the book)
y_rep <- matrix(NA, nrow=n_sims, ncol=n)
for (s in 1:n_sims) {
   y_rep[s,1] <- dat$y[1]
   for (t in 2:n) {
      y_rep[s,t] <- sims[s,"(Intercept)"] + sims[s,"y_lag"] * y_rep[s,t-1] + rnorm(1, mean=0, sd=sims[s,"sigma"])
   }
}

## Visual and numerical comparisons of replicated to actual data

# Figure 11.14: plot the unemployment rate over time based on the actual data
# and based on 14 randomly chosen simulated datasets
par(mfrow=c(3,5), mar=c(3,2.5,2,1))
plot(dat$year, dat$y, type="l", xlab="", ylab="", ylim=c(0,12), bty="l", lwd=2, main="Actual data")
invisible(sapply(sample(n_sims, 14),
                 function(i) plot(dat$year, y_rep[i,], type="l", xlab="", ylab="",
                                  ylim=c(0,12), bty="l", lwd=2, main=paste("Simulation", i))))
par(mfrow=c(1,1), mar=c(5,4,4,2))

# although the trends in the simulated data might be different from what we
# see in the simulated data, the fluctuations actually look somewhat similar;
# however, as noted in the book, the lines for the simulated data look more
# 'jagged' than the line for the actual data

# function that computes the number of switches in a time series (starting
# from the second year); this is different from the code in the book, but
# accomplishes the same thing in one line of code
test <- function(y)
   length(rle(sign(y[3:n] - y[2:(n-1)]))$lengths)

# now compute the number of switches in the observed and simulated data
test_y <- test(dat$y)
test_rep <- apply(y_rep, 1, test)

# number of switches in the observed data
test_y

# how often the number of switches in the simulated data is more extreme than
# the number of switches in the observed data
mean(test_rep > test_y)

# 80% of the number of switches in the simulated data fall in this interval
quantile(test_rep, c(.1,.9))

############################################################################

# just for fun, let's consider a model that also includes y lagged by two
# years as a predictor (and that also includes 'year' as predictor)
dat$y_lag2 <- c(NA, NA, dat$y[1:(n-2)])
res <- stan_glm(y ~ year + y_lag + y_lag2, data=dat, refresh=0)
print(res, digits=2)

# extract the sampled values
sims <- as.data.frame(res)
n_sims <- nrow(sims)

# simulate new datasets based on this model
y_rep <- matrix(NA, nrow=n_sims, ncol=n)
for (s in 1:n_sims) {
   y_rep[s,1] <- dat$y[1]
   y_rep[s,2] <- dat$y[2]
   for (t in 3:n) {
      y_rep[s,t] <- sims[s,"(Intercept)"] + sims[s,"year"] * dat$year[t] + sims[s,"y_lag"] * y_rep[s,t-1] + sims[s,"y_lag2"] * y_rep[s,t-2] + rnorm(1, mean=0, sd=sims[s,"sigma"])
   }
}

# plot the unemployment rate over time based on the actual data and based on
# 14 randomly chosen simulated datasets
par(mfrow=c(3,5), mar=c(3,2.5,2,1))
plot(dat$year, dat$y, type="l", xlab="", ylab="", ylim=c(0,12), bty="l", lwd=2, main="Actual data")
invisible(sapply(sample(n_sims, 14),
                 function(i) plot(dat$year, y_rep[i,], type="l", xlab="", ylab="",
                                  ylim=c(0,12), bty="l", lwd=2, main=paste("Simulation", i))))
par(mfrow=c(1,1), mar=c(5,4,4,2))

# compute the number of switches in the simulated data
test_rep <- apply(y_rep, 1, test)

# how often the number of switches in the simulated data is more extreme than
# the number of switches in the observed data
mean(test_rep > test_y)

# this is still quite extreme

############################################################################

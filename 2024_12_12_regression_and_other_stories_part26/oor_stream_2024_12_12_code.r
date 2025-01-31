############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-12-12
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 11.7 - 11.8
#
# last updated: 2025-01-30

############################################################################

### 11.7: External validation: checking fitted model on new data

# load the rstanarm package
library(rstanarm)

# download the dataset if it doesn't already exist
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# the dataset doesn't actually include information about the age of the
# children, so we cannot replicate exactly what is described in the book;
# however, let's pretend that the first 98 rows of the dataset contain the
# data of the older children, so we fit the model based on this subset
res1 <- stan_glm(kid_score ~ mom_iq + mom_hs, data=dat, refresh=0, subset=1:98)

# compute predicted values for the other children
dat.new <- dat[-(1:98),]
pred <- posterior_predict(res1, newdata=dat.new)

# note: pred contains 4000 predicted values for each child, based on the 4000
# sampled values of the intercept and slope from the posterior distributions

# compute the mean of the predicted values for each child
mean.pred <- colMeans(pred)
head(mean.pred)

# Figure 11.19(a): plot of the mean predicted value of each child against the
# corresponding actually observed value
plot(mean.pred, dat.new$kid_score, pch=21, bg="gray", bty="l",
     xlab="Predicted score", ylab="Actual score", panel.first=abline(0,1,lwd=3),
     xlim=c(20,140), ylim=c(20,140))

# same figure but using more natural ranges for the x- and y-axis
plot(mean.pred, dat.new$kid_score, pch=21, bg="gray", bty="l",
     xlab="Predicted score", ylab="Actual score")

# correlation between predicted and observed scores
cor(mean.pred, dat.new$kid_score)

# compute the residuals
resid <- dat.new$kid_score - mean.pred

# Figure 11.19(b): plot of the predicted values versus the residuals
plot(mean.pred, resid, pch=21, bg="gray", bty="l", xlab="Predicted score",
     ylab="Prediction error", panel.first={abline(h=0, lwd=3);
     abline(h=c(-1,1) * sd(resid), lty="dashed")})

# compute 50% predictive intervals for each child
predint <- apply(pred, 2, function(x) quantile(x, prob=c(.25, .75)))
predint[,1:5]

# compute how often the actual score is within the predictive interval
mean(dat.new$kid_score >= predint[1,] & dat.new$kid_score <= predint[2,])

# note: posterior_predict() makes predictions for single individuals, which is
# why the coverage is essentially 50%; if we had used posterior_linpred(),
# then this would make predictions for the *mean* of individuals and then the
# coverage would be way too low

############################################################################

### 11.8: Cross validation

## Leave-one-out cross validation

# simulate 20 data points based on a simple regression model
n <- 20
x <- 1:n
a <- 0.2
b <- 0.3
sigma <- 1
set.seed(2141)
y <- a + b*x + rnorm(n, mean=0, sd=sigma)
dat <- data.frame(x, y)

# Figure 11.20(a): plot of the data
plot(y ~ x, data=dat, pch=21, bg="gray", bty="l", xlim=c(0,20), ylim=c(0,8))

# fit the model to all data and to the data leaving out the 18th data point
res.all <- stan_glm(y ~ x, data=dat, refresh=0)
res.m18 <- stan_glm(y ~ x, data=dat[-18,], refresh=0)

# add the regression lines from these models to the plot
abline(res.all, lwd=3)
abline(res.m18, lwd=3, lty="dashed", col="gray")

# compute predicted values for the 18th data point based on the model with all
# data and the model where we left out the 18th data point
pred.all <- posterior_predict(res.all, newdata=dat[18,])
pred.m18 <- posterior_predict(res.m18, newdata=dat[18,])

# compute the posterior predictive distribution for the 18th data point based
# on equation (11.6)
sims.all <- as.data.frame(res.all)
condpred <- data.frame(y=seq(0,9,length.out=100))
condpred$density <- sapply(condpred$y, FUN=function(y) mean(dnorm(y, sims.all[,1] + sims.all[,2]*18, sims.all[,3])))

# histogram of the predicted values we obtain above
hist(pred.all[,1], freq=FALSE, breaks=50, main="", xlab="Score")

# superimpose a kernel density estimate of that distribution
lines(density(pred.all[,1]), lwd=3, col="dodgerblue")

# superimpose the posterior predictive distribution from equation (11.6)
lines(condpred$y, condpred$density, lwd=3, bty="l", col="firebrick")

# these match up quite closely

# Figure 11.20(a): plot of the data
plot(y ~ x, data=dat, pch=21, bg="gray", bty="l", xlim=c(0,20), ylim=c(0,8))
abline(res.all, lwd=3)
abline(res.m18, lwd=3, lty="dashed", col="gray")

# add the predictive distribution from the model using all data points
lines(condpred$density*6+18, condpred$y, lwd=3, bty="l")
abline(v=18)
segments(18, mean(pred.all), max(condpred$density*6+18), mean(pred.all))

# add the predictive distribution from the model leaving out the 18th data point
sims.m18 <- as.data.frame(res.m18)
condpred$density <- sapply(condpred$y, FUN=function(y) mean(dnorm(y, sims.m18[,1] + sims.m18[,2]*18, sims.m18[,3])))
lines(condpred$density*6+18, condpred$y, lwd=3, bty="l", lty="dashed", col="gray")
segments(18, mean(pred.m18), max(condpred$density*6+18), mean(pred.m18), lty="dashed", col="gray")

# Figure 11.20(b): show the residuals from the fitted model when using all
# data for model fitting (black points) and when doing leave-one-out model
# fitting (open points)
pred.all <- posterior_predict(res.all)
mean.pred.all <- colMeans(pred.all)
resid.all <- dat$y - mean.pred.all
plot(mean.pred.all, resid.all, pch=19, xlab="Predicted Score", ylab="Residual",
     bty="l", panel.first=abline(h=0, lty="dashed"), ylim=c(-2,2.5))
resid.loo <- dat$y - loo_predict(res.all)$value
segments(mean.pred.all, resid.all, mean.pred.all, resid.loo)
points(mean.pred.all, resid.loo, pch=21)

# standard deviation of the two types of residuals
round(sd(resid.all), digits=2)
round(sd(resid.loo), digits=2)

# note: we know that sigma is equal to 1, so we can see that the SD of the
# leave-one-out residuals is closer to sigma

# R^2 values based on the two types of residuals
round(1 - var(resid.all) / var(dat$y), digits=2)
round(1 - var(resid.loo) / var(dat$y), digits=2)

## Summarizing prediction error using the log score and deviance

# compute the log scores using log_lik() and show the first 6 values for the
# first observation
head(log_lik(res.all)[,1])

# this is the same as computing the log density of the first observation using
# the means from posterior_linpred() and the posterior samples of sigma
pred.all <- posterior_linpred(res.all)
head(dnorm(dat$y[1], pred.all[,1], sd=sims.all$sigma, log=TRUE))

# Figure 11.21: plot the expected log predictive density values from the model
# fitted to all data versus the leave-one-out values
elpd.all <- colMeans(log_lik(res.all))
elpd.loo <- loo(res.all)$pointwise[,"elpd_loo"]
plot(mean.pred.all, elpd.all, pch=19, bty="l",
     xlab="Predicted Score", ylab="log predictive density", ylim=c(-3.5,-1))
segments(mean.pred.all, elpd.all, mean.pred.all, elpd.loo)
points(mean.pred.all, elpd.loo, pch=21)

## Overfitting and AIC

# compute the AIC based on the model fit to all data (using k = number of
# regression coefficients fit in the model)
sum(-2 * elpd.all) + 2*2

# compare this to the deviance of the leave-one-out approach
sum(-2 * elpd.loo)

# these are quite similar to each other

## Demonstration of adding pure noise predictors to a model

# read in the data and fit the model
dat <- read.csv("kidiq.csv")
res1 <- stan_glm(kid_score ~ mom_hs + mom_iq, data=dat, refresh=0)
res1

# R^2 values based on the two types of residuals
pred.all <- posterior_predict(res1)
mean.pred.all <- colMeans(pred.all)
resid.all <- dat$kid_score - mean.pred.all
resid.loo <- dat$kid_score - loo_predict(res1)$value
round(1 - var(resid.all) / var(dat$kid_score), digits=2)
round(1 - var(resid.loo) / var(dat$kid_score), digits=2)

# compute the posterior predictive log scores
elpd.all <- colMeans(log_lik(res1))
elpd.loo <- loo(res1)$pointwise[,"elpd_loo"]
sum(elpd.all)
sum(elpd.loo)
loo1 <- loo(res1)
loo1

# simulate 5 noise predictors
noise <- replicate(5, rnorm(nrow(dat)))

# fit the model including these additional predictors
res2 <- stan_glm(kid_score ~ mom_hs + mom_iq + noise, data=dat, refresh=0)
res2

# R^2 values based on the two types of residuals
pred.all <- posterior_predict(res2)
mean.pred.all <- colMeans(pred.all)
resid.all <- dat$kid_score - mean.pred.all
resid.loo <- dat$kid_score - loo_predict(res2)$value
round(1 - var(resid.all) / var(dat$kid_score), digits=2)
round(1 - var(resid.loo) / var(dat$kid_score), digits=2)

# compute the posterior predictive log scores
elpd.all <- colMeans(log_lik(res2))
elpd.loo <- loo(res2)$pointwise[,"elpd_loo"]
sum(elpd.all)
sum(elpd.loo)
loo2 <- loo(res2)
loo2

# fit the model only including the maternal high-school indicator
res3 <- stan_glm(kid_score ~ mom_hs, data=dat, refresh=0)
res3
loo3 <- loo(res3)
loo3

# compare models 1 and 3 using loo_compare()
loo_compare(loo1, loo3)

# expand the first model with the interaction
res4 <- stan_glm(kid_score ~ mom_hs + mom_iq + mom_hs:mom_iq, data=dat, refresh=0)
loo4 <- loo(res4)
loo_compare(loo1, loo4)

# compute the leave-one-out R^2 values for the various models above
round(median(loo_R2(res1)), digits=2)
round(median(loo_R2(res2)), digits=2)
round(median(loo_R2(res3)), digits=2)
round(median(loo_R2(res4)), digits=2)

## Demonstration of K-fold cross validation using simulated data

# load the MASS package
library(MASS)

# simulate the data as described in the book
set.seed(1754)
n <- 60
k <- 30
rho <- 0.8
S <- matrix(0.8, nrow=k, ncol=k)
diag(S) <- 1
X <- mvrnorm(n, mu=rep(0,k), Sigma=S)
b <- c(-1,1,2, rep(0,k-3))
y <- X %*% b + rnorm(n, mean=0, sd=2)
dat <- data.frame(X, y)
head(dat)

# fit the model predicting y from all other variables in dat
res1 <- stan_glm(y ~ ., prior=normal(0, 10), data=dat, refresh=0)
res1

# try out leave-one-out estimation, but this generates a warning
loo1 <- loo(res1)

# do 10-fold cross-validation instead
kfold1 <- kfold(res1, K=10)
kfold1

# fit the model with regularized horseshoe priors on the coefficients
res2 <- update(res1, prior=hs())
kfold2 <- kfold(res2, K=10)
kfold2

# compare the two models
loo_compare(kfold1, kfold2)

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-05-30
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 9.4 - 9.6
#
# last updated: 2024-06-05

############################################################################

### 9.4: Example of Bayesian inference: beauty and sex ratio

# create the dataset for the example and examine it (the n values are based on
# guestimates of how many people fell into the various categories)
dat <- structure(list(x = c(-2, -1, 0, 1, 2),
                      y = c(50, 44, 50, 47, 56),
                      n = c(300, 600, 1200, 600, 300)),
                 row.names = c(NA, -5L), class = "data.frame")
dat

# percentage of girls of parents in the lower attractiveness categories (based
# on a weighted mean of the percentages) versus the highest category
p1 <- weighted.mean(dat$y[1:4], dat$n[1:4])
p2 <- dat$y[5]
p1
p2

# difference between the two percentages
p2 - p1

# 90% of the 3000 participants (i.e., 2700) of the survey fell into the first
# four attractiveness categories and 10% (i.e., 300) into the highest category
n1 <- sum(dat$n[1:4])
n2 <- dat$n[5]
n1
n2

# standard error of the difference between the two percentages
se <- sqrt(p1 * (100 - p1) / n1 + p2 * (100 - p2) / n2)
se

# mean and SE for the prior (for the difference between the two percentages)
theta_hat_prior <- 0
se_prior <- 0.25

# combine the prior with the data using equations (9.3) and (9.4)
theta_hat_data <- p2 - p1
se_data <- se
theta_hat_bayes <- (theta_hat_prior/se_prior^2 + theta_hat_data/se_data^2) / (1/se_prior^2 + 1/se_data^2)
se_bayes <- sqrt(1/(1/se_prior^2 + 1/se_data^2))
round(theta_hat_bayes, 2)
round(se_bayes, 2)

# plot the distributions for the prior, data, and posterior
xs <- seq(-2, 16, length=1000)
plot(xs, dnorm(xs, mean=theta_hat_prior, sd=se_prior), type="l", lty="dotted",
     bty="n", yaxt="n", yaxs="i", xlab=expression(theta), ylab="", ylim=c(0,1.6))
lines(xs, dnorm(xs, mean=theta_hat_data, sd=se_data), lty="dashed")
lines(xs, dnorm(xs, mean=theta_hat_bayes, sd=se_bayes))
legend("topright", lty=c("dotted", "dashed", "solid"),
       legend=c("Prior", "Data", "Posterior"))

# this plot demonstrates how little information there is in the actual data,
# relative to the prior information we have; hence, the posterior distribution
# is very much like the prior (just shifted slightly to the right)

# create a dataset with raw data like the survey dataset
dat <- lapply(split(dat, dat$x), function(x) cbind(attractiveness = rep(x$x, x$n),
                                                   girl = rep(c(0,1), times=round(c(1-x$y/100, x$y/100) * x$n))))
dat <- do.call(rbind, dat)
dat <- data.frame(dat)

# check that the counts are as expected
tab <- table(dat$attractiveness, dat$girl)
tab

# compute percentages of the counts across rows
prop.table(tab, margin=1) * 100

# construct a high versus low attractiveness dummy variable
dat$attracthigh <- ifelse(dat$attractiveness == 2, 1, 0)

# check that we get an estimated difference (between the proportions) of .08
# for the high versus not high attractiveness categories based on a standard
# linear regression model (using a 'linear probability model' to be precise)
res <- lm(girl ~ attracthigh, data=dat)
summary(res)

# load the rstanarm package
library(rstanarm)

# now we fit the same model using stan_glm(), specifying appropriate priors
# for the intercept and slope (using the default prior for the error SD)
set.seed(1241)
res <- stan_glm(girl ~ attracthigh, data=dat,
                prior_intercept=normal(location=0.49, scale=0.02),
                prior=normal(location=0, scale=0.0025), refresh=0)
res

# extract the median of the posterior for the intercept and slope and multiply
# the values by 100 to obtain percentages
coef(res) * 100

# compare the value for the slope to the value we calculated above manually
theta_hat_bayes

# extract the posterior samples of the intercept, slope, and sigma
post <- as.data.frame(res)

# plot the posterior distribution for the slope
plot(density(post[,2]), main="Posterior Distribution for the Slope", bty="l")

# draw all 4000 regression lines, using alpha blending (i.e., making the
# lines semi-transparent) to indicate what lines are more or less plausible
plot(NA, xlim=c(0,1), ylim=c(0.4,0.6), xaxt="n",
     xlab="Attractiveness Group", ylab="Proportion of Girls", bty="l")
axis(side=1, at=c(0,1), labels=c("Not High", "High"))
apply(post, 1, function(x) abline(a=x[1], b=x[2], col=rgb(0,0,0,.05)))
abline(a=median(post[,1]), b=median(post[,2]), lwd=6)

# if we use the default priors, then the median of the posterior of the slope
# is almost 8% points, so again we see how important it is to carefully think
# about the prior
res <- stan_glm(girl ~ attracthigh, data=dat, refresh=0)
coef(res) * 100

# extract the posterior samples of the intercept, slope, and sigma
post <- as.data.frame(res)

# draw all 4000 regression lines, using alpha blending (i.e., making the
# lines semi-transparent) to indicate what lines are more or less plausible
plot(NA, xlim=c(0,1), ylim=c(0.4,0.6), xaxt="n",
     xlab="Attractiveness Group", ylab="Proportion of Girls", bty="l")
axis(side=1, at=c(0,1), labels=c("Not High", "High"))
apply(post, 1, function(x) abline(a=x[1], b=x[2], col=rgb(0,0,0,.05)))
abline(a=median(post[,1]), b=median(post[,2]), lwd=6)

############################################################################

### 9.5: Uniform, weakly informative, and informative priors in regression

## Uniform prior distribution

# download the dataset for the example (run once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in and inspect the data
dat <- read.table("hibbs.dat", header=TRUE)
dat

# fit the model with lm() and inspect the results
res1 <- lm(vote ~ growth, data=dat)
summary(res1)

# fit a linear regression model using flat priors for all parameters
set.seed(1234)
res2 <- stan_glm(vote ~ growth, data=dat,
                 prior_intercept=NULL, prior=NULL, prior_aux=NULL, refresh=0)

# extract the samples from the posterior distributions for the three parameters
posterior <- as.data.frame(res2)
head(posterior)

# plot kernel density estimates of the three distributions
par(mfrow=c(3,1))
d1 <- density(posterior[,1])
d2 <- density(posterior[,2])
d3 <- density(posterior[,3])
plot(d1, main="Intercept")
plot(d2, main="Slope")
plot(d3, main="Sigma")
par(mfrow=c(1,1))

# get the mode of each of these distributions
d1$x[which.max(d1$y)]
d2$x[which.max(d2$y)]
d3$x[which.max(d3$y)]

# compare these to the least squares estimates
coef(res1)[[1]]
coef(res1)[[2]]
sigma(res1)

# for symmetric posterior distributions, the mean (or median) is a better
# summary, so for the intercept and slope, let's stick to these, but for a
# skewed distribution like for sigma, the mode should correspond more closely
# to the classical estimate

# compare the estimates
tab <- data.frame(lm = c(coef(res1), sigma(res1)),
                  stan_glm = c(mean(posterior[,1]), mean(posterior[,2]), d3$x[which.max(d3$y)]))
rownames(tab) <- c("Intercept", "Slope", "Sigma")
round(tab, 3)

############################################################################

## Default prior distribution

# refit the model using the default priors
set.seed(1237)
res <- stan_glm(vote ~ growth, data=dat, refresh=0)
coef(res)

# construct the default priors manually and check that we get the same results
set.seed(1237)
sd_x <- sd(dat$growth)
sd_y <- sd(dat$vote)
mean_y <- mean(dat$vote)
res <- stan_glm(vote ~ growth, data=dat,
                prior=normal(0, 2.5*sd_y/sd_x),
                prior_intercept=normal(mean_y, 2.5*sd_y),
                prior_aux=exponential(1/sd_y), refresh=0)
coef(res)

############################################################################

## Weakly informative prior distribution based on subject-matter knowledge

# fit the model using the priors as explained in this section
set.seed(1237)
res <- stan_glm(vote ~ growth, data=dat,
                prior=normal(5,5), prior_intercept=normal(50,10), refresh=0)
coef(res)

############################################################################

## Example where an informative prior makes a difference: Beauty and sex ratio

# in the book, the authors switch back to the beauty and sex ratio example,
# but we essentially already did the analysis of these data using more
# informative priors above (when we created the raw dataset and used
# stan_glm() to fit the model); here, we will stick to the election dataset

# fit the model with least squares regression
res1 <- lm(vote ~ growth, data=dat)
summary(res1)

# fit the Bayesian regression model with a prior for the slope that is much
# more narrowly centered around 0 (i.e., a much more informative prior)
set.seed(1237)
res2 <- stan_glm(vote ~ growth, data=dat,
                 prior=normal(0,1), refresh=0)
coef(res2)

# in 2022-2023, we discussed the book 'An Introduction to Statistical
# Learning', where we covered techniques such as ridge regression and the
# Lasso (see the code from the session on 2022-06-24); as discussed in that
# book, there is a direct connection between these methods and using a
# Bayesian model with informative priors on the regression coefficients (see
# the section 'Bayesian Interpretation for Ridge Regression and the Lasso' in
# chapter 6 of that book); we can demonstrate this as follows

# objective functions for ridge regression
fitridge <- function(beta, y, x, lambda)
   sum((y - beta[1] - beta[2]*x)^2) + lambda * beta[2]^2

# do ridge regression with lambda=17.5
res3 <- optim(c(50,0), fitridge, y=dat$vote, x=dat$growth, lambda=17.5)
res3$par

# put all results into a table
tab <- rbind(lm=coef(res1), stan_glm=coef(res2), fitridge=res3$par)
round(tab, digits=3)

# we see that if we do ridge regression with an appropriately chosen lambda
# (shrinkage penalty value), then we get essentially the same results as what
# we get from the Bayesian regression model (to be precise, the mode of the
# posteriors should match the results from ridge regression; above, coef(res2)
# gives us the median values, but the posterior distributions are fairly
# symmetric, so the equivalence still holds)

# it can be shown that the lambda value that makes these results essentially
# identical is given by sigma^2 / tau^2, where sigma^2 is the error variance
# of the regression model and tau^2 is the variance of the normal prior; in
# the case above, this is roughly equal to 17.5 and hence we get essentially
# equivalent results
sigma(res2)^2 / 1^2

# fit the Bayesian regression model with a Laplace (double exponential) prior
# (https://en.wikipedia.org/wiki/Laplace_distribution) for the slope that is
# much more narrowly centered around 0 (i.e., a much more informative prior)
set.seed(1237)
res2 <- stan_glm(vote ~ growth, data=dat,
                 prior=laplace(0,sqrt(1/2)), refresh=0)
coef(res2)

# objective functions for lasso regression
fitlasso <- function(beta, y, x, lambda)
   sum((y - beta[1] - beta[2]*x)^2) + lambda * abs(beta[2])

# do lasso regression with lambda=47
res3 <- optim(c(50,0), fitlasso, y=dat$vote, x=dat$growth, lambda=47)
res3$par

# sidenote: using optim() here works, but generally the optimization problem
# for lasso regression is a bit more tricky since the abs() term makes the
# objective function non-smooth

# put all results into a table
tab <- rbind(lm=coef(res1), stan_glm=coef(res2), fitridge=res3$par)
round(tab, digits=3)

# again, we see that if we do lasso regression with an appropriately chosen
# lambda (shrinkage penalty value), then we get essentially the same results
# as what we get from the Bayesian regression model

# it can be shown that the lambda value that makes these results essentially
# identical is given by 2 * sigma^2 / b, where sigma^2 is the error variance
# of the regression model and b is the scale parameter of the Laplace prior;
# in the case above, this is roughly equal to 47 and hence we get essentially
# equivalent results
2*sigma(res2)^2 / sqrt(1/2)

# for some further discussion of this connection (and corresponding proofs), see:
# https://ekamperi.github.io/mathematics/2020/08/02/bayesian-connection-to-lasso-and-ridge-regression.html

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-04-03
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.7 - 12.8
#
# last updated: 2025-04-08

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 12.7: Models for regression coefficients

# download the dataset if it doesn't already exist
if (!file.exists("student-merged.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Student/data/student-merged.csv", destfile="student-merged.csv")

# read in the dataset
dat <- read.csv("student-merged.csv")

# inspect the first six rows of the dataset
head(dat)

# set up a vector with the names of the predictor variables
predictors <- c("school","sex","age","address","famsize","Pstatus","Medu",
                "Fedu","traveltime","studytime","failures","schoolsup",
                "famsup","paid","activities","nursery","higher","internet",
                "romantic","famrel","freetime","goout","Dalc","Walc","health",
                "absences")

# select the subset of rows where the final-year mathematics grade (G3mat) is
# greater than 0 and only keep this variable plus the predictor variables
dat <- subset(dat, subset=G3mat>0, select=c("G3mat",predictors))
head(dat)

# predict G3mat from all other variables in the dataset (using default priors)
res0 <- stan_glm(G3mat ~ ., data=dat, refresh=0)
print(res0, digits=2)

# extract the posterior samples
post <- as.data.frame(res0)
post <- post[-c(1,ncol(post))]

# function that takes a matrix as input and plots kernel density estimates of
# the variables in the columns in the matrix with shading corresponding to the
# 25th and 75th percentiles of the distributions and a vertical line at the median

plotpost <- function(mat) {

   op <- par(mar=c(4,6,2,2), las=1)
   plot(NA, xlim=range(mat), ylim=c(1,ncol(mat)), yaxt="n", bty="l", xlab="", ylab="")
   abline(v=0, lty="dotted")

   for (i in 1:ncol(mat)) {
      tmp <- density(mat[,i], n=4096)
      tmp$y <- tmp$y / max(tmp$y) * 0.9
      q25 <- quantile(mat[,i], 0.25)
      q50 <- quantile(mat[,i], 0.50)
      q75 <- quantile(mat[,i], 0.75)
      pos25 <- which.min(abs(tmp$x - q25))
      pos50 <- which.min(abs(tmp$x - q50))
      pos75 <- which.min(abs(tmp$x - q75))
      y0 <- ncol(mat)+1-i
      polygon(c(tmp$x[pos25:pos75], rev(tmp$x[pos25:pos75])), c(y0+tmp$y[pos25:pos75], rep(y0, pos75-pos25+1)), col="gray90", border=NA)
      segments(tmp$x[pos50], y0, tmp$x[pos50], y0+tmp$y[pos50], lwd=3)
      cutoffs <- quantile(mat[,i], prob=c(0.005,0.995))
      tmp$y <- tmp$y[tmp$x > cutoffs[1] & tmp$x < cutoffs[2]]
      tmp$x <- tmp$x[tmp$x > cutoffs[1] & tmp$x < cutoffs[2]]
      lines(tmp$x, y0+tmp$y)
      segments(min(tmp$x), y0, max(tmp$x), y0)
   }

   axis(side=2, at=(ncol(mat)):1, label=colnames(mat))
   par(op)

}

# Figure 12.10a: Plot kernel density estimates of the posterior distributions
# of the coefficients for the predictor variables
plotpost(post)

# make a copy of the dataset and standardize all of the predictors
dat2 <- dat
dat2[,predictors] <- scale(dat2[,predictors])

# fit the model with all predictors standardized
res1 <- stan_glm(G3mat ~ ., data=dat2, refresh=0)
print(res1, digits=2)

# obtain information about the priors used in the model
prior_summary(res1)

# as the output shows, the coefficients actually have a prior with mean 0 and
# SD equal to 8.21 (which is 2.5 * sd(y)), because when using default priors,
# then autoscale=TRUE; see: https://mc-stan.org/rstanarm/articles/priors.html;
# so we need to set the prior for the coefficients ourselves if we want to
# give the regression coefficients normal priors with SD equal to 2.5

# refit the model with specified priors
res1 <- stan_glm(G3mat ~ ., data=dat2, refresh=0, prior=normal(location=0, scale=2.5))
print(res1, digits=2)

# check that the priors are now correct
prior_summary(res1)

# note: the prior for sigma is correct, since exponential(rate = 0.3) implies
# a mean of 1 / 0.3 =~ 3.3 as described in the book

# extract the posterior samples
post <- as.data.frame(res1)
post <- post[-c(1,ncol(post))]

# Figure 12.10b: Like the previous figure, but with standardized predictors
plotpost(post)

# compute the Bayesian R^2 (median of the posterior R^2 distribution)
postR2 <- bayes_R2(res1)
round(median(postR2), digits=2)

# compute the leave-one-out R^2
round(median(loo_R2(res1)), digits=2)

# compute LOO log score
loo1 <- loo(res1)
loo1

# the model is predicting the mean of the outcome variable
# E[y] = b1*z1 + b2*z2 + ... + bp*zp
#
# so the variance in E[y] is given by:
# Var(E[y]) = Var(b1*z1 + b2*z2 + ... + bp*zp)
#
# now assuming independence between the predictors and assuming independence
# between the priors for the regression coefficients, we can rewrite this as:
#           = Var(b1*z1) + Var(b2*z2) + ... + Var(bp*zp)
#
# since we assume the priors have a mean of 0 and the predictors are z-scored
# and hence also have a mean of 0, we can rewrite this as:
#           = Var(b1)*Var(z1) + ... + Var(bp)*Var(zp)
#
# and since the predictors have a variance of 1, we can rewrite this as:
#           = Var(b1) + ... + Var(bp)
#
# and since SD = 2.5 for the prior distributions, we can simplify this to:
#           = 2.5^2 * p
#
# and so SD(E[y])  = 2.5 * sqrt(p)

# so in the present case, the standard deviation of the predicted mean based
# on the prior distributions is approximately
musd <- 2.5 * sqrt(26)
round(musd, digits=2)

# the default prior for sigma is an exponential distribution, scaled to have a
# mean that is equal to the standard deviation of the outcome, which in this
# case is approximately 3.3
sdy <- sd(dat$G3mat)
round(sdy, digits=2)

# note: R^2 is roughly how much of the total variance (which consists of
# variance in the predicted means plus the error variance) is due to the
# variance in the predicted means
musd^2 / (musd^2 + sdy^2)

# to generate a whole prior distribution for R^2, we simulate sigma2 and beta
# values from their respective prior distributions many times and compute R^2
# for each of the simulated values
priorR2 <- replicate(4000, {
   sigma2 <- rexp(1, rate=0.3)^2
   beta   <- rnorm(26, mean=0, sd=2.5)
   muvar  <- var(as.matrix(dat2[,predictors]) %*% beta)
   muvar / (muvar + sigma2)
})

# Figure 12.11a (top): plot the prior distribution for R^2
hist(priorR2, breaks=seq(0,1,by=.01), main="Prior Distribution of R^2", xlab="")

# Figure 12.11a (bottom): plot the posterior distribution for R^2
hist(postR2, breaks=seq(0,1,by=.01), main="Posterior Distribution of R^2", xlab="")

# now say we assume a priori that the best we might be able to do is to
# account for about 30% of the variance (so R^2 =~ 0.30); then we can set the
# SD of the prior distributions for beta to sqrt(R^2 / p) * sd(y) and the mean
# of the exponential distribution for sigma to sqrt(1-R^2) * sd(y), in which
# case we will get the desired R^2 back
sdbeta <- sqrt(0.3 / 26) * sdy
musd   <- sdbeta * sqrt(26)
esd    <- sqrt(1-0.3) * sdy
musd^2 / (musd^2 + esd^2)

# generate a whole prior distribution for R^2 using the new priors
priorR2 <- replicate(4000, {
   sigma2 <- rexp(1, rate=1/esd)^2
   beta   <- rnorm(26, mean=0, sd=sdbeta)
   muvar  <- var(as.matrix(dat2[,predictors]) %*% beta)
   muvar / (muvar + sigma2)
})

# Figure 12.11b (top): plot the prior distribution for R^2
hist(priorR2, breaks=seq(0,1,by=.01), main="Prior Distribution of R^2", xlab="")

# fit the model with the new priors
res2 <- stan_glm(G3mat ~ ., data=dat2, refresh=0,
                 prior=normal(scale=sdbeta),
                 prior_aux=exponential(rate=1/esd))
print(res2, digits=2)

# note: since we are not using the default priors, then autoscale=FALSE by
# default, so we are using normal priors with SD equal to sdbeta as desired;
# also, we really should set the correct prior distribution for sigma (which
# is not done in the book, although because of the autoscaling that is done
# when using the default prior, the difference here is quite negligible)
prior_summary(res2)

# Figure 12.11b (bottom): plot the posterior distribution for R^2
postR2 <- bayes_R2(res2)
hist(postR2, breaks=seq(0,1,by=.01), main="Posterior Distribution of R^2", xlab="")

# extract the posterior samples
post <- as.data.frame(res2)
post <- post[-c(1,ncol(post))]

# compare the Bayesian and leave-one-out R^2
postR2 <- bayes_R2(res2)
round(median(postR2), digits=2)
round(median(loo_R2(res2)), digits=2)

# Figure 12.12a: Plot of the posterior distributions with the new prior
plotpost(post)

# generate a prior distribution for R^2 using the horseshoe prior (code from
# https://avehtari.github.io/ROS-Examples/Student/student.html with some
# adjustments / corrections)
p0 <- 6
p <- length(predictors)
n <- nrow(dat2)
slab_scale <- sd(dat2$G3mat) / sqrt(p0) * sqrt(0.3)
priorR2 <- replicate(4000, {
   sigma2 <- rexp(1, rate=1/(sqrt(1-0.3)*sdy))^2
   global_scale <- p0 / (p-p0) * sqrt(sigma2) / sqrt(n)
   lambda <- rcauchy(p)
   tau <- rcauchy(1, scale=global_scale)
   c2 <- 1 / rgamma(1, shape=0.5, rate=0.5)
   c <- slab_scale * sqrt(c2)
   lambda_tilde <- sqrt(c^2 * lambda^2 / (c^2 + tau^2*lambda^2))
   beta <- rnorm(p, mean = 0, sd = lambda_tilde * abs(tau))
   muvar <- var(as.matrix(dat2[,predictors]) %*% beta)
   muvar / (muvar + sigma2)
})

# note: the description in the book is not detailed enough to determine if the
# above fully matches up with what stan_glm() does when using such a prior as
# is done below (the paper by Piironen & Vehtari, 2017, gives further details,
# but requires very careful reading; https://doi.org/10.1214/17-EJS1337SI)

# Figure 12.11c (top): plot the prior distribution for R^2
hist(priorR2, breaks=seq(0,1,by=.01), main="Prior Distribution of R^2", xlab="")

# fit the model
global_scale <- (p0/(p-p0)) / sqrt(n) # without sigma, as the scaling by sigma is done inside stan_glm
res3 <- stan_glm(G3mat ~ ., data=dat2, prior=hs(global_scale=global_scale, slab_scale=slab_scale), refresh=0)
print(res3, digits=2)

# obtain information about the priors used in the model
prior_summary(res3)

# extract the posterior samples
post <- as.data.frame(res3)
post <- post[-c(1,ncol(post))]

# compare the Bayesian and leave-one-out R^2
postR2 <- bayes_R2(res3)
round(median(postR2), digits=2)
round(median(loo_R2(res3)), digits=2)

# Figure 12.12b: Plot of the posterior distributions with the new prior
plotpost(post)

# model that only includes selected predictors
res4 <- stan_glm(G3mat ~ failures + schoolsup + goout + absences, data=dat2, refresh=0)
print(res4, digits=2)

# compare the Bayesian and leave-one-out R^2
postR2 <- bayes_R2(res4)
round(median(postR2), digits=2)
round(median(loo_R2(res4)), digits=2)

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-10-31
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.3.3 - 4.3.6
#
# last updated: 2024-11-04

############################################################################

## 4.3.3: Grid approximation of the posterior distribution

# load the rethinking package
library(rethinking)

## 4.3.1: The data

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

# select only those who are 18 years or older
dat <- dat[dat$age >= 18,]

# create the grid for the grid approximation
mu.list <- seq(from=150, to=160, length.out=100)
sigma.list <- seq(from=7, to=9, length.out=100)
post <- expand.grid(mu=mu.list , sigma=sigma.list)
head(post, 10)

# select five people from the full dataset
sub <- dat[41:45,]

# compute the likelihood of the data for every combination of mu and sigma in
# the grid; that is, we compute the density of the observed height values
# under a normal distribution for given values of mu and sigma for each person
# in the dataset and then compute the product to get the joint density; since
# we consider mu and sigma as unknown (and compute the joint density for
# various values of mu and sigma), we call the resulting value a 'likelihood'
post$likelihood <- sapply(1:nrow(post), function(i) prod(dnorm(sub$height, mean=post$mu[i], sd=post$sigma[i])))
head(post)

# we then multiply the likelihood values by the prior plausibilities for mu
# and sigma (we use a normal distribution to reflect our prior knowledge about
# mu and a uniform distribution to reflect out prior knowledge about sigma)
post$probsub <- post$likelihood * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)
head(post)

# this then yields the posterior plausibility for a certain combination of mu
# and sigma in our grid (i.e., in essence, except for scaling, the posterior
# joint distribution of mu and sigma)

# determine which combination of mu and sigma in the grid is most plausible
which.max(post$probsub)
post[which.max(post$probsub),]

# note: what we are doing above is analogous to what we did for the globe
# tossing example in section 2.4.3

# draw the 3-dimensional surface for the posterior plausibilities for each
# combination of mu and sigma in the grid
tmp <- split(post, post$sigma)
tmp <- sapply(tmp, function(x) x$probsub)
rownames(tmp) <- mu.list
colnames(tmp) <- sigma.list
tmp[1:5,1:5]
persp(mu.list, sigma.list, tmp, theta=45, phi=25, shade=0.2, ticktype="detailed",
      xlab="mu", ylab="sigma", zlab="posterior plausibility")

# instead of drawing this 3d surface, we can draw a filled contour plot
filled.contour(mu.list, sigma.list, tmp, color.palette=hcl.colors, xlab="mu", ylab="sigma")

# why did we do all of the above based on only 5 people? because when we
# compute the product for the likelihood for all people in the dataset, then
# we run into numerical problems because the values we are multiplying are
# close to zero and the resulting value cannot be distinguished from 0
post$likelihood <- sapply(1:nrow(post), function(i) prod(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i])))
post$prod <- post$likelihood * dnorm(post$mu, mean=178, sd=20) * dunif(post$sigma, min=0, max=50)
head(post)

# to get around this problem, we will compute the log likelihood values with log(prod(x)) = sum(log(x))
post$likelihood <- NULL
post$prod <- NULL
post$ll <- sapply(1:nrow(post), function(i) sum(dnorm(dat$height, mean=post$mu[i], sd=post$sigma[i], log=TRUE)))
head(post)

# and now instead of computing likelihood * prior-mu * prior-sigma, we compute
# log(likelihood * prior-mu * prior-sigma) = log(likelihood) + log(prior-mu) + log(prior-sigma)
post$prod <- post$ll + dnorm(post$mu, mean=178, sd=20, log=TRUE) + dunif(post$sigma, min=0, max=50, log=TRUE)
head(post)

# in the last step, we exponentiate the prod values to get the posterior
# plausibilities (before doing so, we subtract the maximum value so that the
# values we are exponentiating are not quite so negative, again to avoid
# numerical issues)
post$prob <- exp(post$prod - max(post$prod))
head(post)

# determine which combination of mu and sigma in the grid is most plausible
which.max(post$prob)
post[which.max(post$prob),]

# draw the 3-dimensional surface for the posterior plausibilities
tmp <- split(post, post$sigma)
tmp <- sapply(tmp, function(x) x$prob)
rownames(tmp) <- mu.list
colnames(tmp) <- sigma.list
persp(mu.list, sigma.list, tmp, theta=45, phi=25, shade=0.2, ticktype="detailed",
      xlab="mu", ylab="sigma", zlab="posterior plausibility")

# instead of drawing this 3d surface, we can draw a filled contour plot
filled.contour(mu.list, sigma.list, tmp, color.palette=hcl.colors, xlab="mu", ylab="sigma")

# using the contour_xyz() and image_xyz() functions from the rethinking
# package, we can avoid having to do the restructuring of the post data frame
# as we did above for drawing the (filled) contour plot
contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob, col=hcl.colors(100))

## 4.3.4: Sampling from the posterior

# sample 10,000 values from the grid in accordance with the posterior
# plausibilities of the various combinations of mu and sigma
sample.rows <- sample(1:nrow(post), size=1e4, replace=TRUE, prob=post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

# Figure 4.4: scatterplot of the sampled values of mu and sigma (note that the
# figure in the book looks slightly different, maybe because a finer grid was
# used in the grid approximation)
plot(sample.mu, sample.sigma, cex=1, pch=16, col=col.alpha(rangi2,0.2))

# we could also draw the points with some jittering
plot(jitter(sample.mu, amount=0.1), jitter(sample.sigma, amount=0.1), cex=0.3, pch=16, col=rangi2)

# kernel density estimates of the marginal posterior distribution for mu and sigma
par(mfrow=c(1,2))
plot(density(sample.mu), lwd=4, col="#1e59ae", main="Posterior for mu")
plot(density(sample.sigma), lwd=4, col="#1e59ae", main="Posterior for sigma")
par(mfrow=c(1,1))

# 95% percentile interval for the sampled values of mu
PI(sample.mu, prob=0.95)
quantile(sample.mu, prob=c(0.025, 0.975))

# 95% percentile interval for the sampled values of sigma
PI(sample.sigma, prob=0.95)
quantile(sample.sigma, prob=c(0.025, 0.975))

# summarize the center and spread of the posterior distributions
mean(sample.mu)
mean(sample.sigma)
sd(sample.mu)
sd(sample.sigma)

# Overthinking: Sample size and the normality of sigma's posterior

# select a random sample of 20 people from the full dataset, do the grid
# approximation, sample from it, and draw the kernel density estimate of the
# posterior distribution for mu and sigma
sub <- dat[sample(nrow(dat), 20),]
mu.list <- seq(from=150, to=170, length.out=200)
sigma.list <- seq(from=4, to=20, length.out=200)
post2 <- expand.grid(mu=mu.list , sigma=sigma.list)
post2$ll <- sapply(1:nrow(post2), function(i) sum(dnorm(sub$height, mean=post2$mu[i], sd=post2$sigma[i], log=TRUE)))
post2$prod <- post2$ll + dnorm(post2$mu, mean=178, sd=20, log=TRUE) + dunif(post2$sigma, min=0, max=50, log=TRUE)
post2$prob <- exp(post2$prod - max(post2$prod))
sample2.rows <- sample(1:nrow(post2), size=1e4, replace=TRUE, prob=post2$prob)
sample2.mu <- post2$mu[sample2.rows]
sample2.sigma <- post2$sigma[sample2.rows]
par(mfrow=c(1,2))
plot(density(sample2.mu), lwd=4, col="#1e59ae", main="Posterior for mu")
curve(dnorm(x, mean=mean(sample2.mu), sd=sd(sample2.mu)), lwd=2, add=TRUE, col="gray")
plot(density(sample2.sigma), lwd=4, col="#1e59ae", main="Posterior for sigma")
curve(dnorm(x, mean=mean(sample2.sigma), sd=sd(sample2.sigma)), lwd=2, add=TRUE, col="gray")
par(mfrow=c(1,1))

## 4.3.5: Finding the posterior distribution with quap

flist <- alist(height ~ dnorm(mu, sigma),
               mu ~ dnorm(178, 20),
               sigma ~ dunif(0, 50))

res1 <- quap(flist, data=dat)
res1
precis(res1, prob=0.95)

# specify starting values

start <- list(mu=mean(dat$height), sigma=sd(dat$height))
res1 <- quap(flist, data=dat, start=start)
res1

# now we will re-implement what quap() is doing here

postfun <- function(x, h) {
   loglik <- sum(dnorm(h, mean=x[1], sd=x[2], log=TRUE))
   prior.mu <- dnorm(x[1], mean=178, sd=20, log=TRUE)
   prior.sigma <- dunif(x[2], min=0, max=50, log=TRUE)
   logposterior <- loglik + prior.mu + prior.sigma
   cat("mu =", formatC(x[1], digits=3, format="f"),
       "  sigma =", formatC(x[2], digits=3, format="f"),
       "  logposterior =", round(logposterior, 3), "\n")
   logposterior
}

# optimize postfun() over x=c(mu,sigma) (i.e., find the peak of the posterior)

res2 <- optim(par=c(mean(dat$height),sd(dat$height)), postfun, method="L-BFGS-B",
              lower=c(100,1), upper=c(300,100),
              control=list(fnscale=-1), h=dat$height, hessian=TRUE)
res2

# -1 * the inverse of the Hessian yields the estimated variance-covariance
# matrix of the estimates at the peak
-solve(res2$hessian)

# so the square-root of the diagonal elements of this matrix yields the SDs of
# the parameter estimates
sqrt(diag(-solve(res2$hessian)))

# this is identical to the SDs we get from quap()
precis(res1)$sd

# use a much more informative prior for mu

flist <- alist(height ~ dnorm(mu, sigma),
               mu ~ dnorm(178, 0.1),
               sigma ~ dunif(0, 50))

res3 <- quap(flist, data=dat)
res3

## 4.3.6: Sampling from a quap

# with vcov(), we can obtain the variance-covariance matrix of the estimates from quap()
vcov(res1)

# note how this is essentially the same to what we obtained above from the manual approach
-solve(res2$hessian)

# the diagonal elements are the variances
diag(vcov(res1))

# and we can transform the var-cov matrix into a correlation matrix
cov2cor(vcov(res1))

# extract 10,000 samples from the posterior distribution of mu and sigma
post <- extract.samples(res1, n=1e4)
head(post)

# get some summary statistics for these sampled values
precis(post, prob=0.95)

############################################################################

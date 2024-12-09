############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-12-05
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section: 4.4
#
# last updated: 2024-12-09

############################################################################

### 4.4: Linear prediction

# load the rethinking package
library(rethinking)

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

# select only those who are 18 years or older
dat <- dat[dat$age >= 18,]

# plot the height of the individuals versus their weight
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")

# correlation between height and weight
round(cor(dat$height, dat$weight), digits=2)

## 4.4.1: The linear model strategy

# 4.4.1.3: Priors

# simulate 100 alpha and beta values based on the prior distributions
set.seed(2971)
n <- 100
sim <- data.frame(a = rnorm(n, mean=178, sd=20),
                  b = rnorm(n, mean=0, sd=10))

# Figure 4.5 (left): plot the regression lines implied by these values
plot(NA, xlim=range(dat$weight), ylim=c(-100,400), xlab="weight", ylab="height", bty="l")
abline(h=0, lty=2)
abline(h=272, lty=1)
mtext("b ~ dnorm(0,10)")
xbar <- mean(dat$weight)
xs <- seq(min(dat$weight), max(dat$weight))
invisible(apply(sim, 1, function(par) lines(xs, par["a"] + par["b"] * (xs - xbar), col=rgb(0,0,0,0.2), lwd=2)))

# simulate 10^6 values from a log-normal distribution with a mean of 0 and an
# SD of 1 and create a kernel density estimate of the distribution
b <- rlnorm(1e6, mean=0, sd=1)
plot(density(b, from=-0.1, to=5), lwd=2, bty="l", main="")

# simulate 100 alpha and beta values based on the prior distributions where we
# use a log-normal distribution for the prior of beta
set.seed(2971)
sim <- data.frame(a = rnorm(n, mean=178, sd=20),
                  b = rlnorm(n, mean=0, sd=1))

# Figure 4.5 (right): plot the regression lines implied by these values
plot(NA, xlim=range(dat$weight), ylim=c(-100,400), xlab="weight", ylab="height", bty="l")
abline(h=0, lty=2)
abline(h=272, lty=1)
mtext("log(b) ~ dnorm(0,1)")
invisible(apply(sim, 1, function(par) lines(xs, par["a"] + par["b"] * (xs - xbar), col=rgb(0,0,0,0.2), lwd=2)))

## 4.4.2: Finding the posterior distribution

# use the quadratic approximation approach to find the MAP (maximum a
# posteriori) estimates of alpha and beta
model <- alist(height ~ dnorm(mu, sigma),
               mu <- a + b*(weight - xbar),
               a ~ dnorm(178, 20),
               b ~ dlnorm(0, 1),
               sigma ~ dunif(0, 50))
res1 <- quap(model, data=dat)
res1

# remember: based on the Hessian (matrix of second derivatives), we can
# estimate the variance-covariance matrix of the MAP estimates
vcov(res1)

# the square-root of the diagonal elements are the SDs of the estimates
round(sqrt(diag(vcov(res1))), digits=2)

# assuming normality, we can then construct 95% compatibility intervals
precis(res1, prob=0.95)

############################################################################

# in the Rethinking box on page 98, the book discusses that we can obtain the
# posterior distribution of mu quite easily from samples of the posterior
# distributions of alpha and beta

# take 10^5 samples from posterior distributions
post <- extract.samples(res1, n=1e5)
head(post)

# compute mu based on the sampled values of alpha and beta, using random
# weight values from the actual data and examine the distribution
post$mu <- post$a + post$b * (sample(dat$weight, 1e5, replace=TRUE) - xbar)
hist(post$mu, breaks=50, freq=FALSE, main="Posterior Distribution of mu", xlab="")

# note: this is *not* a normal distribution
curve(dnorm(x, mean=mean(post$mu), sd=sd(post$mu)), lwd=5, add=TRUE)

# for a fixed value of weight, the distribution of mu_i is normal; but since
# the weight values differ, the posterior distribution of mu is actually a
# mixture of many such normal distributions; let's see if we can obtain this
# distribution analytically (without the sampling approach above)

# compute the means and variances of the distributions of mu_i based on the
# MAP estimates of alpha and beta and their variance-covariance matrix
X <- cbind(1, dat$weight - xbar)
means <- c(X %*% coef(res1)[1:2])
vars <- diag(X %*% vcov(res1)[1:2,1:2] %*% t(X))
head(data.frame(means, vars))

# compute the density of height values for 1000 height values from min(height)
# to max(height) based on the different normal distributions implied by these
# means and variances
heights <- seq(min(dat$height), max(dat$height), length=1000)
dens <- matrix(NA, nrow=nrow(dat), ncol=1000)

for (i in 1:nrow(dat)) {
   dens[i,] <- dnorm(heights, mean=means[i], sd=sqrt(vars[i]))
}

# plot all these normal distributions
plot(NA, xlim=range(heights), ylim=c(0,max(dens)), bty="l", xlab="Height", ylab="Density")
apply(dens, 1, function(h) lines(heights, h))

# the marginal distribution of mu is really the sum over these distributions
# (rescaled, so the area under this mixture distribution is 1)
dens <- apply(dens, 2, sum)
trapezoid <- function(x,y) sum(diff(x)*(y[-1]+y[-length(y)]))/2
dens <- dens / trapezoid(heights, dens)

# draw the marginal distribution of mu based on the sampling approach again
# and add the distribution obtained analytically above
hist(post$mu, breaks=50, freq=FALSE, main="Posterior Distribution of mu", xlab="")
lines(heights, dens, lwd=5, col="#1e59ae")

# they match up nicely; note that the sampling approach was *much* easier

############################################################################

# same model as above, but parameterized in such a way that we get the
# posterior distribution of log(b)
res2 <- quap(alist(height ~ dnorm(mu, sigma),
                   mu <- a + exp(log_b)*(weight - xbar),
                   a ~ dnorm(178, 20),
                   log_b ~ dnorm(0, 1),
                   sigma ~ dunif(0, 50)), data=dat)
res2
precis(res2, prob=0.95)

# we can easily convert the log slope estimate from res2 back and get
# essentially the same MAP estimate
exp(coef(res2)["log_b"])
coef(res1)["b"]

# but how can we convert the SD of the log slope to the SD for the slope? we
# can do this using the delta method: https://en.wikipedia.org/wiki/Delta_method
#
# since the derivative of exp(x) is simply exp(x), we find:
se(res2)["log_b"] * exp(coef(res2)["log_b"])
se(res1)["b"]

# and these match up quite closely again

############################################################################

## 4.4.3: Interpreting the posterior distribution

# 4.4.3.1: Tables of marginal distributions

# table with the estimates, their SDs, and 95% compatibility (credible) intervals
precis(res1, prob=0.95)

# variance-covariance matrix of the estimates
round(vcov(res1), digits=3)

# corresponding correlation matrix
round(cov2cor(vcov(res1)), digits=3)

# plot of the marginal posterior distributions based on sampled values and
# scatterplots of these sampled values against each other
pairs(res1)

# 4.4.3.2: Plotting posterior inference against the data

# Figure 4.6: plot the height of the individuals versus their weight
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")

# extract 10^4 samples from the posterior distribution
post <- extract.samples(res1)
head(post)

# compute the mean of the intercept and slope samples
a_map <- mean(post$a)
b_map <- mean(post$b)

# add the regression line based on these means to the plot
curve(a_map + b_map*(x-xbar), lwd=5, add=TRUE)

# 4.4.3.3: Adding uncertainty around the mean

# select the first 10 people from the dataset
sub <- dat[1:10,]

# refit the model based on this subset (note: have to recompute xbar for the subset)
xbar <- mean(sub$weight)
res3 <- quap(model, data=sub)

# sample 20 values from the posterior
post <- extract.samples(res3, n=20)

# Figure 4.7 (upper left): plot the height of the individuals versus their
# weight for the subset with the 20 regression lines added based on the 20
# sampled values of the intercept and slope
plot(height ~ weight, data=sub, pch=21, bg="gray", bty="l")
invisible(apply(post, 1, function(par) curve(par["a"] + par["b"] * (x-xbar), col=rgb(0,0,0,0.2), lwd=2, add=TRUE)))

# Figure 4.7 (lower right): same plot but based on the full sample
xbar <- mean(dat$weight)
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")
post <- extract.samples(res1, n=20)
invisible(apply(post, 1, function(par) curve(par["a"] + par["b"] * (x-xbar), col=rgb(0,0,0,0.2), lwd=2, add=TRUE)))

# 4.4.3.4: Plotting regression intervals and contours

# generate 10^4 sampled values of mu for individuals with weight = 50
post <- extract.samples(res1)
mu_at_50 <- post$a + post$b * (50 - xbar)

# Figure 4.8: kernel density estimate of this distribution
plot(density(mu_at_50), col="#1e59ae", lwd=5, xlab="mu|weight=50", main="", bty="l")

# 95% compatibility interval
quantile(mu_at_50, prob=c(.025, .975))

# using link(), we can do the above automatically for many different weight values
weight.seq <- seq(from=25, to=70, by=1)
mu <- link(res1, data=data.frame(weight=weight.seq))
dim(mu)

# compute the mean and the compatibility interval for each value of weight in weight.seq
mu.mean <- apply(mu, 2, mean)
mu.pi   <- apply(mu, 2, function(x) quantile(x, prob=c(.025, .975)))

# Figure 4.9: plot of the data with the line based on mu.mean and the
# compatibility intervals added as a shaded region
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")
lines(weight.seq, mu.mean, lwd=5)
#shade(mu.pi, weight.seq)
polygon(c(weight.seq, rev(weight.seq)), c(mu.pi[1,], rev(mu.pi[2,])), col=rgb(0,0,0,0.2), border=NA)

# do the same analytically
X <- cbind(1, weight.seq - xbar)
means <- c(X %*% coef(res1)[1:2])
vars <- diag(X %*% vcov(res1)[1:2,1:2] %*% t(X))
lines(weight.seq, means, lwd=5, col="#1e59ae")
pi.lb <- means - 1.96*sqrt(vars)
pi.ub <- means + 1.96*sqrt(vars)
polygon(c(weight.seq, rev(weight.seq)), c(pi.lb, rev(pi.ub)), col=rgb(0,0,0.2,0.2), border=NA)

# do what link() does manually
post <- extract.samples(res1)
mu.link <- function(weight) post$a + post$b*(weight - xbar)
weight.seq <- seq(from=25, to=70, by=1)
mu <- sapply(weight.seq, mu.link)
mu.mean <- apply(mu, 2, mean)
mu.pi   <- apply(mu, 2, function(x) quantile(x, prob=c(.025, .975)))

# 4.4.3.5: Prediction intervals

# simulate height values of individuals for the various values of weight in weight.seq
sim.height <- sim(res1, data=list(weight=weight.seq))
dim(sim.height)

# compute 95% compatibility intervals for these height values
height.pi <- apply(sim.height, 2, function(x) quantile(x, prob=c(.025, .975)))
polygon(c(weight.seq, rev(weight.seq)), c(height.pi[1,], rev(height.pi[2,])), col=rgb(0,0,0,0.1), border=NA)

# this is Figure 4.10; as explained in the book, we could use sim(..., n=10^4)
# above to simulate more values, which will help to smooth out some of the
# roughness of the bounds of the prediction interval; another way to get rid
# of the roughness is to apply a smoother to the prediction interval bounds
height.pi[1,] <- supsmu(weight.seq, height.pi[1,])$y
height.pi[2,] <- supsmu(weight.seq, height.pi[2,])$y
lines(weight.seq, height.pi[1,], lty="dotted")
lines(weight.seq, height.pi[2,], lty="dotted")

# doing sim() manually
post <- extract.samples(res1)
hsimfun <- function(weight) rnorm(nrow(post), mean = post$a + post$b*(weight - xbar), sd = post$sigma)
weight.seq <- seq(from=25, to=70, by=1)
sim.height <- sapply(weight.seq, hsimfun)
height.pi <- apply(sim.height, 2, function(x) quantile(x, prob=c(.025, .975)))

############################################################################

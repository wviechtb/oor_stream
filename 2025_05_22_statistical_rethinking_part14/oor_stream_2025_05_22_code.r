############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-22
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 7.1 - 7.2
#
# last updated: 2025-05-23

############################################################################

# load the rethinking package
library(rethinking)

############################################################################

### 7.1: The problem with parameters

## 7.1.1: More parameters (almost) always improve fit

# create the small dataset
dat <- data.frame(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"),
                  pos     = c(4, 3, 3, 4, 4, 3, 1),
                  brain   = c(438, 452, 612, 521, 752, 871, 1350),
                  mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5))

# Figure 7.2: brain volume versus body size
plot(brain ~ mass, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
     ylab="brain volume (cc)", bty="l", xlim=c(30,70), ylim=c(200,1400))
text(dat$mass, dat$brain, dat$species, pos=dat$pos)

# standardize body bass and rescale brain volume so 0 still means zero brain
# volume and 1 for the maximum brain volume in the sample
dat <- transform(dat, mass_std  = (mass - mean(mass)) / sd(mass),
                      brain_std = brain / max(brain))

# fit the linear model using very vague priors for the intercept and slope and
# log normal prior for sigma (to force sigma to be positive)
res1 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b*mass_std,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat)
precis(res1, digits=3)

# compare the results to lm() (they are essentially the same)
res1.lm <- lm(brain_std ~ mass_std, data=dat)
summary(res1.lm)
sigma(res1.lm)

# sidenote: to compare sigma to the Bayesian model we should extract samples
# from the posterior distributions, including the one for log(sigma), then
# transform the latter to sigma, and then take the mean
post <- extract.samples(res1)
head(post)
mean(exp(post$log_sigma))

# can also take samples for the intercept and slope based on the lm() model (but not sigma)
post.lm <- extract.samples(res1.lm)
head(post.lm)

# note: these are just simulated from a multivariate normal distribution

# simulate posterior observations of brain_std for the 7 observed species
set.seed(12)
s <- sim(res1)
head(s)

# compute the residuals
r <- apply(s, 2, mean) - dat$brain_std

# compute R^2 (note: using variances computed with n in the denominator, not n-1)
resid_var <- var2(r)
outcome_var <- var2(dat$brain_std)
1 - resid_var/outcome_var

# function to compute R^2 in this manner
R2_is_bad <- function(quap_fit) {
   s <- sim(quap_fit, refresh=0)
   r <- apply(s, 2, mean) - dat$brain_std
   1 - var2(r) / var2(dat$brain_std)
}

# try this out (using the same seed as above to check that we get the same result)
set.seed(12)
R2_is_bad(res1)

# fit polynomial models of degree 2 to 6
res2 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,2)))
res3 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,3)))
res4 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,4)))
res5 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,5)))
res6 <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                   mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4 + b[5]*mass_std^5 + b[6]*mass_std^6,
                   a ~ dnorm(0.5, 1),
                   b ~ dnorm(0, 10),
                   log_sigma ~ dnorm(0, 1)), data=dat, start=list(b=rep(0,6)))

# note: model res6 also runs; we do not need to fix exp(log_sigma) to 0.001

# reminder:
# - sim() simulates posterior observations of y|x (i.e., for single individuals)
# - link() simulates posterior predictions of E[y|x]

# simulates posterior predictions for mass_std values between min(x) and max(x)
mass_seq <- seq(from=min(dat$mass_std), to=max(dat$mass_std), length.out=100)
l <- link(res1, data=list(mass_std=mass_seq))
head(l)

# compute the mean and 95% percentile interval for each column
mu <- apply(l, 2, mean)
ci <- apply(l, 2, PI, prob=0.95)

# plot the data and add the line based on the mean with the corresponding interval
plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="standardized body mass (kg)",
     ylab="standardized brain volume (cc)", bty="l")
lines(mass_seq, mu)
shade(ci, mass_seq)

# Figure 7.3: plots for all 6 models
par(mfrow=c(3,2))
res <- list(res1, res2, res3, res4, res5, res6)
mass_seq <- seq(from=-1.2, to=1.5, length.out=100)
xs <- seq(30, 65, by=5)
xs_std <- (xs-mean(dat$mass))/sd(dat$mass)
for (i in 1:6) {
   l <- link(res[[i]], data=list(mass_std=mass_seq))
   mu <- apply(l, 2, mean)
   ci <- apply(l, 2, PI)
   plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
        ylab="brain volume (cc)", bty="l", xlim=range(xs_std),
        ylim=c(min(ci,brain_std),max(ci,brain_std)), xaxt="n", yaxt="n")
   axis(side=1, at=xs_std, labels=xs)
   axis(side=2, at=ys_std, labels=ys)
   lines(mass_seq, mu, lwd=3)
   shade(ci, mass_seq)
   if (i == 6)
      abline(h = 0, lty="dashed")
   mtext(paste0("Polynomial: ", i, " (R^2 = ", round(R2_is_bad(res[[i]]), digits=2), ")"), line=2, cex=0.8)
}
par(mfrow=c(1,1))

## 7.1.2: Too few parameters hurts, too

# Figure 7.4 (left): regression lines based on the linear model leaving out
# one data point at a time
plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
     ylab="brain volume (cc)", bty="l", xlim=range(xs_std),
     ylim=c(0,1400)/max(dat$brain), xaxt="n", yaxt="n")
axis(side=1, at=xs_std, labels=xs)
axis(side=2, at=ys_std, labels=ys)

invisible(lapply(1:7, function(i) {
   tmp <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                      mu <- a + b*mass_std,
                      a ~ dnorm(0.5, 1),
                      b ~ dnorm(0, 10),
                      log_sigma ~ dnorm(0, 1)), data=dat[-i,])
   l <- link(tmp, data=list(mass_std=mass_seq))
   mu <- apply(l, 2, mean)
   lines(mass_seq, mu, lwd=3, col=adjustcolor("black", alpha.f=0.4))
}))

points(brain_std ~ mass_std, data=dat, pch=21, bg="gray")

# Figure 7.4 (right): regression lines based on the 4th degree polynomial
# model leaving out one data point at a time
plot(brain_std ~ mass_std, data=dat, pch=21, bg="gray", xlab="body mass (kg)",
     ylab="brain volume (cc)", bty="l", xlim=range(xs_std),
     ylim=c(-200,2500)/max(dat$brain), xaxt="n", yaxt="n")
axis(side=1, at=xs_std, labels=xs)
axis(side=2, at=ys_std, labels=ys)

invisible(lapply(1:7, function(i) {
   tmp <- quap(alist(brain_std ~ dnorm(mu, exp(log_sigma)),
                     mu <- a + b[1]*mass_std + b[2]*mass_std^2 + b[3]*mass_std^3 + b[4]*mass_std^4,
                     a ~ dnorm(0.5, 1),
                     b ~ dnorm(0, 10),
                     log_sigma ~ dnorm(0, 1)), data=dat[-i,], start=list(b=rep(0,4)))
   l <- link(tmp, data=list(mass_std=mass_seq))
   mu <- apply(l, 2, mean)
   lines(mass_seq, mu, lwd=3, col=adjustcolor("black", alpha.f=0.4))
}))

points(brain_std ~ mass_std, data=dat, pch=21, bg="gray")

############################################################################

### 7.2: Entropy and accuracy

## 7.2.3: From entropy to accuracy

# compute the Kullback-Leibler divergence for the example
p <- c(0.3, 0.7)
qis <- seq(0.01, 0.99, length=1000)
dkl <- sapply(qis, function(qi) {
   q <- c(qi, 1-qi)
   sum(p * (log(p) - log(q)))
})

# Figure 7.5: plot of the KL divergence as a fuction of q[1]
plot(qis, dkl, type="l", lwd=5, xlab="q[1]", ylab="Divergence of q from p")
abline(v=0.3, lty="dashed")

## 7.2.4: Estimating divergence

# compute the log pointwise predictive density of each individual observation
# for the linear model we fitted earlier
lppdi <- lppd(res1, n=1e4)
lppdi

# we can reproduce this manually by extracting posterior samples of the
# intercept, slope, and log(sigma) values; then we compute for an observation
# its density under a normal distribution with mean and SD based on a
# particular sample of these parameters, repeating this for all samples,
# taking the mean of the density values, and finally the log thereof
post <- extract.samples(res1)
sapply(1:7, function(i) log(mean(apply(post, 1, function(par) dnorm(dat$brain_std[i], mean=par[1] + par[2] * dat$mass_std[i], sd=exp(par[3]))))))

# note: the lppd() function does these computations in a numerically more
# stable way, but for the given example, we can reproduce the calculations in
# this way quite accurately

# compute the total log probability score for the model
sum(lppdi)

# we can think of this total log probability score as the Bayesian analog of
# the log likelihood under maximum likelihood estimation (does not give the
# same value, but it is similar in spirit)
logLik(res1.lm)
sigma2.mle <- sum(resid(res1.lm)^2) / 7
fitted <- fitted(res1.lm)
sum(dnorm(dat$brain_std, mean=fitted, sd=sqrt(sigma2.mle), log=TRUE))

## 7.2.5: Scoring the right data

# compute the log score for each of the models
set.seed(1)
res <- list(res1, res2, res3, res4, res5, res6)
sapply(res, function(m) sum(lppd(m)))

# compare against the log likelihood values from the corresponding lm() models
res1.lm <- lm(brain_std ~ poly(mass_std, 1, raw=TRUE), data=dat)
res2.lm <- lm(brain_std ~ poly(mass_std, 2, raw=TRUE), data=dat)
res3.lm <- lm(brain_std ~ poly(mass_std, 3, raw=TRUE), data=dat)
res4.lm <- lm(brain_std ~ poly(mass_std, 4, raw=TRUE), data=dat)
res5.lm <- lm(brain_std ~ poly(mass_std, 5, raw=TRUE), data=dat)
res6.lm <- lm(brain_std ~ poly(mass_std, 6, raw=TRUE), data=dat)
res.lm <- list(res1.lm, res2.lm, res3.lm, res4.lm, res5.lm, res6.lm)
sapply(res, logLik)

# function to simulate data based on the model given on page 212

simdata <- function(n) {
   X <- replicate(4, rnorm(n))
   mu <- 0.15 * X[,1] - 0.4 * X[,2]
   y <- rnorm(n, mean=mu, sd=1)
   return(data.frame(X, y))
}

# function to the fit the 5 models described

fitmodels <- function(dat) {

   res1 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a,
                      a ~ dnorm(0, 1)), data=dat)
   res2 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,1)))
   res3 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,2)))
   res4 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,3)))
   res5 <- quap(alist(y ~ dnorm(mu, 1),
                      mu <- a + b[1]*X1 + b[2]*X2 + b[3]*X3 + b[4]*X4,
                      a ~ dnorm(0, 1),
                      b ~ dnorm(0, 10)), data=dat, start=list(b=rep(0,4)))
   res <- list(res1, res2, res3, res4, res5)
   return(res)

}

# now we repeat the simulation described (note: this takes quite a bit of
# time, so the code prints the iteration number to keep track of progress;
# running this with 100 iterations is already sufficient to see the same
# pattern as shown in the book)

iters <- 100

dev.train <- matrix(NA_real_, nrow=iters, ncol=5)
dev.test  <- matrix(NA_real_, nrow=iters, ncol=5)

n <- 20

for (j in 1:iters) {

   print(j)
   dat <- simdata(n)
   res <- fitmodels(dat)
   lppd <- sapply(res, function(m) sum(lppd(m)))
   dev.train[j,] <- -2 * lppd
   dat <- simdata(n)
   lppd <- sapply(res, function(m) sum(lppd(m, data=dat)))
   dev.test[j,] <- -2 * lppd

}

dev.train.mean <- apply(dev.train, 2, mean)
dev.train.lo   <- dev.train.mean - apply(dev.train, 2, sd)
dev.train.hi   <- dev.train.mean + apply(dev.train, 2, sd)

dev.test.mean  <- apply(dev.test, 2, mean)
dev.test.lo    <- dev.test.mean - apply(dev.test, 2, sd)
dev.test.hi    <- dev.test.mean + apply(dev.test, 2, sd)

# Figure 7.6 (left): deviance in and out of sample for the 5 models for n=20
plot(NA, xlim=c(0.8,5.2), ylim=c(min(dev.train.lo, dev.test.lo), max(dev.train.hi, dev.test.hi)),
     xlab="number of parameters", ylab="deviance", main=paste("N =", n))
segments(1:5, dev.train.lo, 1:5, dev.train.hi, col="#1e59ae", lwd=2)
points(1:5, dev.train.mean, pch=19, col="#1e59ae")
segments(1:5 + 0.1, dev.test.lo, 1:5 + 0.1, dev.test.hi, lwd=2)
points(1:5 + 0.1, dev.test.mean, pch=21)

# now repeat the above with n=100

n <- 100

for (j in 1:iters) {

   print(j)
   dat <- simdata(n)
   res <- fitmodels(dat)
   lppd <- sapply(res, function(m) sum(lppd(m)))
   dev.train[j,] <- -2 * lppd
   dat <- simdata(n)
   lppd <- sapply(res, function(m) sum(lppd(m, data=dat)))
   dev.test[j,] <- -2 * lppd

}

dev.train.mean <- apply(dev.train, 2, mean)
dev.train.lo   <- dev.train.mean - apply(dev.train, 2, sd)
dev.train.hi   <- dev.train.mean + apply(dev.train, 2, sd)

dev.test.mean  <- apply(dev.test, 2, mean)
dev.test.lo    <- dev.test.mean - apply(dev.test, 2, sd)
dev.test.hi    <- dev.test.mean + apply(dev.test, 2, sd)

# Figure 7.6 (right): deviance in and out of sample for the 5 models for n=100
plot(NA, xlim=c(0.8,5.2), ylim=c(min(dev.train.lo, dev.test.lo), max(dev.train.hi, dev.test.hi)),
     xlab="number of parameters", ylab="deviance", main=paste("N =", n))
segments(1:5, dev.train.lo, 1:5, dev.train.hi, col="#1e59ae", lwd=2)
points(1:5, dev.train.mean, pch=19, col="#1e59ae")
segments(1:5 + 0.1, dev.test.lo, 1:5 + 0.1, dev.test.hi, lwd=2)
points(1:5 + 0.1, dev.test.mean, pch=21)

############################################################################

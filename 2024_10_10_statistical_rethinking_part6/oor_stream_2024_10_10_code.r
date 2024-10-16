############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-10-10
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.1 - 4.3.2
#
# last updated: 2024-10-16

############################################################################

### 4.1: Why normal distributions are normal

# simulate 16 flips of a fair coin 1000 times
tails <- rbinom(1000, size=16, prob=0.5)

# translate the number of tails observed into the position relative to the center line
pos <- tails * 1 + (16 - tails) * -1

# frequency table of the positions observed
table(pos)

# create a barplot of the frequencies
barplot(table(factor(pos, levels=seq(-16,16,by=2))))

# proportion of people standing on the center line (pos equal to 0)
mean(pos == 0)

# proportion of people standing 5 yards to the left/right of the center line
mean(pos == 5)
mean(pos == -5)

# note: for an even number of flips, pos must be a multiple of 2, so it is not
# possible to stand 5 yards away from the center line

## 4.1.1: Normal by addition

# simulate the sum of 16 random values from a uniform(0,1) distribution 1000 times
pos <- replicate(1000, sum(runif(16,-1,1)))

# histogram of the positions
hist(pos, main="", breaks=40)

# plot of a kernel density estimate of the distribution
plot(density(pos), lwd=4, main="", col="#1e59ae")

# superimpose a normal distribution with the observed mean and sd of the pos values
curve(dnorm(x, mean=mean(pos), sd=sd(pos)), lwd=2, add=TRUE, col="gray")

# repeat the simulation but now save the size of each step (instead of their total sum)
pos <- replicate(1000, runif(16,-1,1))

# examine the results for the first 5 people
pos[,1:5]

# take the cumulative sum across columns
pos <- apply(pos, 2, cumsum)

# examine the results for the first 5 people
pos[,1:5]

# Figure 4.2: plot of the position of the people over time
plot(NA, xlim=c(0,16), ylim=c(-1,1)*max(abs(pos)), xlab="step number", ylab="position")
apply(pos, 2, function(y) lines(0:16, c(0,y), col=rgb(30,89,174,50,maxColorValue=255)))
abline(v=c(4,8,16), lty="dashed", lwd=2)

# histogram of the positions after 4, 8, and 16 steps
par(mfrow=c(3,1))
plot(density(pos[4,], bw=0.2), xlim=c(-6,6), lwd=4, col="#1e59ae", xlab="position", ylab="density", main="4 Steps")
curve(dnorm(x, mean=mean(pos[4,]), sd=sd(pos[4,])), lwd=2, add=TRUE, col="gray")
plot(density(pos[8,], bw=0.2), xlim=c(-6,6), lwd=4, col="#1e59ae", xlab="position", ylab="density", main="8 Steps")
curve(dnorm(x, mean=mean(pos[8,]), sd=sd(pos[8,])), lwd=2, add=TRUE, col="gray")
plot(density(pos[16,], bw=0.2), xlim=c(-6,6), lwd=4, col="#1e59ae", xlab="position", ylab="density", main="16 Steps")
curve(dnorm(x, mean=mean(pos[16,]), sd=sd(pos[16,])), lwd=2, add=TRUE, col="gray")
par(mfrow=c(1,1))

# what the book discusses here is actually the central limit theorem:
# https://en.wikipedia.org/wiki/Central_limit_theorem

## 4.1.2: Normal by multiplication

# simulate the increase in growth for each of 12 loci (each value represents a
# multiplicative factor for growth, i.e., 1.05 means a 5% increase in growth)
growth <- 1 + runif(12,0,0.1)
growth

# take the product (the different loci interact, so the total growth is the
# product of these multiplicative factors)
prod(growth)

# now simulate 10,000 of these values
growth <- replicate(10000, prod(1 + runif(12,0,0.1)))

# plot of a kernel density estimate of the distribution
plot(density(growth), lwd=4, main="", col="#1e59ae")
curve(dnorm(x, mean=mean(growth), sd=sd(growth)), lwd=2, add=TRUE, col="gray")

# note that this looks again like a normal distribution for reasons explained
# in the book

# show that if the multiplicative factors are more substantial, then the
# resulting values do not look like a normal distribution
growth <- replicate(10000, prod(1 + runif(12,0,0.5)))
plot(density(growth), lwd=4, main="", col="#1e59ae")
curve(dnorm(x, mean=mean(growth), sd=sd(growth)), lwd=2, add=TRUE, col="gray")

# but if the multiplicative factors are much closer to 1, then it looks even
# more like a normal distribution
growth <- replicate(10000, prod(1 + runif(12,0,0.01)))
plot(density(growth), lwd=4, main="", col="#1e59ae")
curve(dnorm(x, mean=mean(growth), sd=sd(growth)), lwd=2, add=TRUE, col="gray")

# but note: the product is *not* going to converge to a normal distribution
# when the number of terms in the product increases; for example, take the
# product of a 100 terms
growth <- replicate(10000, prod(1 + runif(100,0,0.1)))
plot(density(growth), lwd=4, main="", col="#1e59ae")
curve(dnorm(x, mean=mean(growth), sd=sd(growth)), lwd=2, add=TRUE, col="gray", n=1001)

# and as we increase the number of terms, the distribution will look less and
# less than a normal distribution
growth <- replicate(10000, prod(1 + runif(500,0,0.1)))
plot(density(growth), lwd=4, main="", col="#1e59ae")
curve(dnorm(x, mean=mean(growth), sd=sd(growth)), lwd=2, add=TRUE, col="gray", n=1001)

# it turns out that the product converge to a log normal distribution
curve(dlnorm(x, meanlog=mean(log(growth)), sdlog=sd(log(growth))), lwd=2, add=TRUE, col="red", n=1001)

# see: https://en.wikipedia.org/wiki/Central_limit_theorem#Products_of_positive_random_variables
# this is also known as Gibrat's law: https://en.wikipedia.org/wiki/Gibrat's_law

## 4.1.3: Normal by log-multiplication

# as discussed above, we get a log normal distribution, so let's plot the log
# transformed values for one example
growth <- replicate(10000, prod(1 + runif(12,0,0.5)))
plot(density(log(growth)), lwd=4, main="", col="#1e59ae")
curve(dnorm(x, mean=mean(log(growth)), sd=sd(log(growth))), lwd=2, add=TRUE, col="gray")

## 4.1.4: Using Gaussian distributions

# nothing to replicate/code here

############################################################################

### 4.2: A language for describing models

# draw the posterior distribution based on a direct application of Bayes' theorem

postfun <- function(x, w, n)
   dbinom(w, size=n, prob=x) * dunif(x, 0, 1)

curve(postfun(x, w=6, n=9) / integrate(postfun, lower=0, upper=1, w=6, n=9)$value,
      from=0, to=1, xlab="p", ylab="density", lwd=6, col="#1e59ae")

# carry out the grid approximation (for a finer grid than what is described in
# the book) and add the rescaled values from the grid approximation to the
# figure; note that we get exactly the same posterior

p_grid <- seq(from=0, to=1, length.out=1000)
posterior <- dbinom(6, size=9, p_grid) * dunif(p_grid, 0, 1)
posterior <- posterior / sum(posterior) * 1000
lines(p_grid, posterior, lwd=2)

# note: in the stream on 2024-07-04, we discussed that one can actually write
# out the equation that Bayes' theorem yields analytically; this turns out to
# be a beta distribution with parameters alpha=1+w and beta=1+(n-w)

# add this distribution on top of the figure
curve(dbeta(x, 1+6, 1+3), add=TRUE, col="red")

############################################################################

### 4.3: Gaussian model of height

# load the rethinking package
library(rethinking)

## 4.3.1: The data

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

# examine the structure of the data frame
str(dat)

# get summary statistics and mini histograms for the variables
precis(dat)

############################################################################

# a little side discussion: why does precis() give the 5.5% and 94.5%
# quantiles, the interval that contains 89% of the values? this seems a bit
# arbitrary, but is essentially as arbitrary as the convention to report
# intervals that contain 95% of the values

# another argument is that quantiles closer to 50% are less bias and less
# variable; we can demonstrate this with a little simulation

# simulate n=40 random values from a standard normal distribution and compute
# the quantiles as given by 'probs'; repeat the above 10,000 times
probs <- c(.025, .975, .055, .945, .10, .90)
sav <- replicate(10000, quantile(rnorm(40), probs=probs))
sav[,1:5]

# the true quantiles
qnorm(probs)

# the average of the quantile values based on the simulated data
rowMeans(sav)

# the difference gives the bias in the estimated quantiles; we see that the
# bias is smaller for quantiles closer to 50%
qnorm(probs) - rowMeans(sav)

# we also see that the standard deviation of the estimates is smaller for
# quantiles closer to 50%
apply(sav, 1, sd)

# so this may be another argument for preferring not the usual 2.5% and 97.5%
# quantiles, but something less extreme (although why 5.5% and 94.% remains a
# bit of a mystery)

############################################################################

# back to the dataset; select from dat only those who are 18 years or older
dat <- dat[dat$age >= 18,]

## 4.3.2: The model

# plot the kernel density estimate of the height variable (note that dens()
# from the rethinking package defaults to using a lower bandwidth for the
# kernel density estimate, which results in a more wiggly distribution)
dens(dat$height, lwd=4, col="#1e59ae")

# note: when mixing together men and women in the sample, the distribution of
# height might actually be bi-modal, because we are mixing together two normal
# distributions; let's look at the distributions for men and women separately
dens(dat$height[dat$male == 1], lwd=4, col="#1e59ae", adj=1)
dens(dat$height[dat$male == 0], lwd=4, col="red", adj=1, add=TRUE)

# for now at least, we (and the book) will ignore this

# plot the prior for mu
curve(dnorm(x, mean=178, sd=20), from=100, to=250, lwd=4, col="#1e59ae")

# plot the prior for sigma
curve(dunif(x, 0, 50), from=-10, to=60, lwd=4, col="#1e59ae")

# generate the prior predictive distribution of heights
sample_mu    <- rnorm(10000, mean=178, sd=20)
sample_sigma <- runif(10000, 0, 50)
prior_h      <- rnorm(10000, sample_mu, sample_sigma)

# plot the distribution (and superimpose also a normal distribution on top)
dens(prior_h, lwd=4, col="#1e59ae", norm.comp=TRUE)

# now repeat the above but using a much larger SD for the prior of mu
sample_mu    <- rnorm(10000, mean=178, sd=100)
sample_sigma <- runif(10000, 0, 50)
prior_h      <- rnorm(10000, sample_mu, sample_sigma)
dens(prior_h, lwd=4, col="#1e59ae")

############################################################################

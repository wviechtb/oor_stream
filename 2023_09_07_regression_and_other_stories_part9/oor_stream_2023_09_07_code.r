############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-09-07
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.2
#
# last updated: 2023-09-14

############################################################################

### 4.2: Estimates, standard errors, and confidence intervals

# remember last time, we considered the (toy) example of a population
# consisting of N=10 individuals with heights equal to:
y <- c(178, 184, 165, 173, 196, 168, 171, 185, 180, 174)

# using combn(), we can generate all possible samples of size n=3
sampdist <- apply(combn(10, 3), 2, function(i) y[i])
sampdist

# say we are interested in the mean of the population (the parameter/estimand)
mean(y)

# take the mean of each dataset in the sampling distribution of the data
means <- apply(sampdist, 2, mean)
means

# note: we could also do the above with
colMeans(sampdist)

# the means vector is the sampling distribution of the mean (the statistic) in
# this particular example; let's create a histogram of this distribution
hist(means, main="Sampling Distribution of the Mean", xlab="Mean")

## Standard errors, inferential uncertainty, and confidence intervals

# the mean of the means in the sampling distribution is equal to the parameter
# we are estimating
mean(means)

# the standard deviation of the statistic in our sampling distribution is
# called the 'standard error' of the statistic; so this is the standard error
# of the mean in this example
sd(means)

# we could also be interested in the range (i.e., the difference between the
# maximum and minimum value in our sample) as a statistic
ranges <- apply(sampdist, 2, function(x) max(x) - min(x))
ranges

# histogram of the sampling distribution of the range
hist(ranges, main="Sampling Distribution of the Range", xlab="Range")

# standard error of the range
sd(ranges)

# note: for the range, the mean of the sampling distribution is way lower than
# the corresponding parameter in the population; this makes sense, since the
# observed ranges are either equal to the range in the population (if the
# sample happens to include both the smallest and the largest values in the
# population) or they are smaller
max(y) - min(y)
mean(ranges)

# sampling distribution of the variance
variances <- apply(sampdist, 2, function(x) var(x))
variances
hist(variances, main="Sampling Distribution of the Variance", xlab="Variance")
sd(variances)
mean(variances)
var(y)

# note: for the mean and variance, the mean of the values in the respective
# sampling distributions is equal to the corresponding parameter; so for these
# statistics, the statistic is an 'unbiased estimator' of the corresponding
# parameter (but not for the range, as we saw above)

# illustrate how the variation of the statistic (i.e., the standard error)
# gets smaller as we increase the sample size
par(mfrow=c(2,2))
ns <- c(3, 5, 7, 10)
for (n in ns) {
   sampdist <- apply(combn(10, n), 2, function(i) y[i])
   means <- apply(sampdist, 2, mean)
   hist(means, main=paste0("Sampling Distribution of the Mean (n=", n, ")"),
        xlab="Mean", breaks=seq(168, 190, by=2))
}
par(mfrow=c(1,1))

# when the standard error of the statistic goes to zero as we increase the
# sample size, then we say that the estimator is 'consistent'

# the example above is along the lines of the 'sampling model' introduced in
# section 4.1; now let's consider an example along the lines of the
# 'measurement model' where we cannot generate the entire sampling
# distribution of the data, but we can (if we know the properties of the
# generative model) generate a very large number of possible datasets easily;
# for example, say that the height of people in the population is normally
# distributed with a true mean of 175 and a true standard deviation of 10;
# then a random sample of 100 people from that population can be easily
# generated with rnorm()
x <- rnorm(100, mean=175, sd=10)
x

# then we can compute the mean height of our sample
mean(x)

# let's replicate this process 100000 times
means <- replicate(100000, mean(rnorm(100, mean=175, sd=10)))

# create a histogram of the sampling distribution of the mean in this scenario
hist(means, breaks=80, main="Sampling Distribution of the Mean", xlab="Mean")

# illustrate that the sampling distribution of the mean is still approximately
# normal even when the raw data come from a non-normal (in this case, a very
# right-skewed distribution) as long as n is sufficiently large (due to the CLT)
x <- ((rchisq(100, df=3) - 3) / sqrt(2*3)) * 10 + 175
x
hist(x)
means <- replicate(100000, mean(((rchisq(100, df=3) - 3) / sqrt(2*3)) * 10 + 175))
hist(means, breaks=80, main="Sampling Distribution of the Mean", xlab="Mean")

# the CLT also applies to other statistics, like the SD (but convergence to a
# normal distribution can be slower, so n needs to be larger)
sds <- replicate(100000, sd(rnorm(100, mean=175, sd=10)))
hist(sds, breaks=80, main="Sampling Distribution of the SD", xlab="SD")

# but the CLT does not kick in for a statistic like the range (because it is
# not the sum of many small independent random variables)
ranges <- replicate(100000, {x <- rnorm(100, mean=175, sd=10); max(x)-min(x)})
hist(ranges, breaks=80, main="Sampling Distribution of the Range", xlab="Range")

## Standard errors and confidence intervals for averages and proportions

# let's go back to the case where the raw data are actually normally
# distributed and we are looking at the sampling distribution of the mean
means <- replicate(100000, mean(rnorm(100, mean=175, sd=10)))

# compute the standard error of the mean in this example
sd(means)

# for a simple random sample, the standard error is equal to the SD of the
# data in the population divided by the square-root of the sample size
10 / sqrt(100)

# note that these two values are not exactly equal to each other because we
# are 'only' generating 100000 values of the statistic

# the equation also holds when the raw data are not normally distributed
means <- replicate(100000, mean(((rchisq(100, df=3) - 3) / sqrt(2*3)) * 10 + 175))
sd(means)
10 / sqrt(100)

# and it also holds when the sampling distribution is not normal
means <- replicate(100000, mean(((rchisq(5, df=3) - 3) / sqrt(2*3)) * 10 + 175))
hist(means, breaks=80, main="Sampling Distribution of the Mean", xlab="Mean")
sd(means)
10 / sqrt(5)

# let's go back to the case where the raw data are normally distributed
means <- replicate(100000, mean(rnorm(100, mean=175, sd=10)))
means
hist(means, breaks=80, main="Sampling Distribution of the Mean", xlab="Mean")

# add the true mean as a vertical line
abline(v=175, lwd=5)

# now when we actually run our experiment/study, we just see one sample and
# hence one observation (one draw) from that sampling distribution
obsmean <- mean(rnorm(100, mean=175, sd=10))
points(obsmean, 0, pch=19, cex=2)

# we know this one mean came from a normal sampling distribution based on the CLT

# if I would know sigma (the SD of the data in the population), then we would
# also know what the standard error of the mean is; in this case
se <- 10 / sqrt(100)
se

# so, if we take the observed mean +-2*se, then this has a ~95% chance of
# 'capturing' the true mean (to be precise, we should use +-1.96*se, but the
# difference is quite negligible)
arrows(obsmean - 2*se, 0, obsmean + 2*se, 0, angle=90, code=3, lwd=3)

# let's repeat this process 200 times
res <- replicate(200, {
   x <- rnorm(100, mean=175, sd=10)
   obsmean <- mean(x)
   ci.lb <- obsmean - 2*se
   ci.ub <- obsmean + 2*se
   c(obsmean=obsmean, ci.lb=ci.lb, ci.ub=ci.ub)
})
res

# draw the confidence intervals (in purple if it misses the true mean)
plot(NA, xlim=c(1,200), ylim=range(res), bty="l",
     xlab="Simulation", ylab="Estimate (95% CI)")
abline(h=175)
segments(1:200, res[2,], 1:200, res[3,], lwd=2,
         col=ifelse(res[2,] > 175 | res[3,] < 175, "#ff00cc", "#00ccff"))
points(1:200, res[1,], pch=19, cex=0.5)

# in practice, we do not know the SD of the data in the population; but we
# could use the SD of the sample as an indicator of what the SD in the
# population might be; say we draw this sample
x <- rnorm(100, mean=175, sd=10)
x

# then we can *estimate* the standard error with
sd(x) / sqrt(100)

# note: when people say 'standard error', they are typically referring to the
# estimate of the true standard error (the latter is essentially never known)

# now repeat what we did above, but using the estimated standard error
res <- replicate(200, {
   x <- rnorm(100, mean=175, sd=10)
   obsmean <- mean(x)
   ci.lb <- obsmean - 2*sd(x)/sqrt(100)
   ci.ub <- obsmean + 2*sd(x)/sqrt(100)
   c(obsmean=obsmean, ci.lb=ci.lb, ci.ub=ci.ub)
})
res

plot(NA, xlim=c(1,200), ylim=range(res), bty="l",
     xlab="Simulation", ylab="Estimate (95% CI)")
abline(h=175)
segments(1:200, res[2,], 1:200, res[3,], lwd=2,
         col=ifelse(res[2,] > 175 | res[3,] < 175, "#ff00cc", "#00ccff"))
points(1:200, res[1,], pch=19, cex=0.5)

# to account for the fact that we have replaced the unknown true standard
# error with the estimated one, we would have to use a t-distribution for
# constructing the CI; instead of +-2 (or more precisely, +-1.96), we would
# use the appropriate 'critical value' from a t-distribution with n-1 degrees
# of freedom
qt(.975, df=100-1)

# but when n is so large, the difference is negligible

# now let's look at proportions

# generate a sample of 100 0's and 1's where there is a 50% chance of a 0 and
# a 50% chance of a 1 and then compute the mean (i.e., the proportion)
x <- sample(c(0,1), 100, replace=TRUE)
x
mean(x)

# simulate 100000 of such proportions
props <- replicate(100000, mean(sample(c(0,1), 100, replace=TRUE)))
hist(props, breaks=40, main="Sampling Distribution of the Proportion", xlab="Proportion")

# the true standard error of a proportion is equal to sqrt(p*(1-p)/n) where p
# is the true proportion
sqrt(0.5*0.5/100)
sd(props)

# try this out when the chance of a 0 is 80% (and hence 20% for a 1)
props <- replicate(100000, mean(sample(c(0,1), 100, replace=TRUE, prob=c(.8,.2))))
hist(props, breaks=40, main="Sampling Distribution of the Proportion", xlab="Proportion")
sqrt(0.8*0.2/100)
sd(props)

# in practice, we do not know the true standard error, but we can estimate it
# by plugging in the observed proportion for the unknown true proportion in
# the equation for the standard error
x <- sample(c(0,1), 100, replace=TRUE)
x
obsprop <- mean(x)
obsprop
se <- sqrt(obsprop*(1-obsprop)/100)
se
obsprop - 2*se
obsprop + 2*se

## Standard error and confidence interval for a proportion when y = 0 or y = 𝑛

# when all of the observed values are 0 (or all are 1), then we cannot use the
# equation above, since this would yield an se equal to 0
x <- rep(0, 75)
x
obsprop <- mean(x)
obsprop
se <- sqrt(obsprop*(1-obsprop)/100)
se

# instead, we can then use
obsprop <- (sum(x) + 2) / (length(x) + 4)
obsprop
se <- sqrt(obsprop*(1-obsprop)/(length(x)+4))
se
round(obsprop - 2*se, 2)
round(obsprop + 2*se, 2)

# and since a proportion can never be negative
max(0, round(obsprop - 2*se, 2))

############################################################################

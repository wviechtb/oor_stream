############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-10-26
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.3
#
# last updated: 2024-02-15

############################################################################

### 4.3: Bias and unmodeled uncertainty

## Bias in estimation

# according to recent data, men in the US watch around 3 hours, women around
# 2.5 hours of television per day; hence, in the population, the mean number
# of hours watched by men and women combined (where there is an equal number
# of men and women) is 2.75; this is the population mean we want to estimate
pop.mean <- 0.5 * 3.0 + 0.5 * 2.5
pop.mean

# now let's simulate the situation where we take a sample of n=200 but women
# are more willing to participate and hence are over-represented in the sample
# (150 women, 50 men); let's assume that number of hours watched is normally
# distributed within each group with a standard deviation of 0.5 (technically,
# we could get a negative value for some participants, but the chance of this
# happening is very small, so we will ignore this issue); we then compute the
# mean number of hours for the 200 participants; we repeat this process 100000
# times, in essence generating the sampling distribution of the mean
set.seed(1234)
means <- replicate(100000, {
   hrs.m <- rnorm( 50, mean=3.0, sd=0.5)
   hrs.w <- rnorm(150, mean=2.5, sd=0.5)
   mean(c(hrs.m, hrs.w))
})

# look at the sampling distribution of the mean
hist(means, main="Sampling Distribution of the Mean", breaks=50)

# sidenote: since the sum of normally distributed variables is still normal
# (https://en.wikipedia.org/wiki/Sum_of_normally_distributed_random_variables)
# (which is also true if the variables have different means and/or standard
# deviations), the sampling distribution of the mean above is also exactly
# normal (and this will be true irrespective of the sample size, so we do not
# even need the CLT for the sampling distribution to become normal)

# add the population mean as a vertical line to the histogram
abline(v=pop.mean, lwd=5)

# show that the sample mean is a biased estimator of the population mean
mean(means)

# add the mean of the sample means as a vertical line to the histogram
abline(v=mean(means), lwd=5, lty="dotted")

# in fact, given the above, we know that the mean of the means in the sampling
# distribution is equal to the following weighted mean (the slight discrepany
# arises because we 'only' simulated 100000 means)
(50 * 3.0 + 150 * 2.5) / 200

# compute the standard error of the mean
sd(means)

# if we weight the values of the men and women appropriately (i.e., by the
# inverse of the sampling probabilities times the known proportions in the
# population), then the mean of these weighted values provides an unbiased
# estimate of the population mean
means <- replicate(100000, {
   hrs.m <- rnorm( 50, mean=3.0, sd=0.5)
   hrs.w <- rnorm(150, mean=2.5, sd=0.5)
   #0.5 * mean(hrs.m) + 0.5 * mean(hrs.w)
   mean(c(4/1*0.5*hrs.m, 4/3*0.5*hrs.w))
})

# look at the sampling distribution of the mean
hist(means, main="Sampling Distribution of the Mean", breaks=50)

# add the population mean as a vertical line to the histogram
abline(v=pop.mean, lwd=5)

# show that the sample mean is now an unbiased estimator of the population mean
mean(means)

# compute the standard error of the mean
sd(means)

# note that the mean of the weighted data has a larger standard error, so
# while it is now unbiased, it is a less precise estimator

############################################################################

## Adjusting inferences to account for bias and unmodeled uncertainty

# simulate the observed proportion of support for a candidate under a binomial
# model 10000 times, assuming that the true probability of support is 0.52
props <- replicate(10000, mean(rbinom(60000, 1, 0.52)))

# sidenote: the above is equivalent to rbinom(10000, 60000, 0.52) / 60000
# (which is much faster), but the above generalizes more easily to the
# scenario described further below

# look at the sampling distribution of the proportion
hist(props, main="Sampling Distribution of the Proportion", breaks=50)

# add the true proportion as a vertical line to the histogram
abline(v=0.52, lwd=5)

# as we can see, under this model, there is virtually no chance of ever seeing
# a proportion that is below 0.5, since the SE of the proportions is so low

# compute the mean and standard error of these proportions
mean(props)
sd(props)

# this is not a realistic model for such a poll, since it assumes a single
# fixed probability of support

# now we do 100 polls with n=600, where the true probability of support varies
# across polls; say these probabilities come from a beta distribution
# (https://en.wikipedia.org/wiki/Beta_distribution) with parameters alpha=1.56
# and beta=1.44, so the mean is still 0.52 (using a beta distribution to
# simulate the probabilities as these will automatically be between 0 and 1);
# we then compute an overall proportion across these 100 polls
props <- replicate(10000, {
   props100 <- replicate(100, mean(rbinom(600, 1, rbeta(1,1.56,1.44))))
   mean(props100)
})

# examine the sampling distribution
hist(props, main="Sampling Distribution of the Proportion", breaks=50)

# add the true proportion as a vertical line to the histogram
abline(v=0.52, lwd=5)

# compute the mean and standard error of these proportions
mean(props)
sd(props)

# now we see additional variability in the (overall) proportions, which is
# much more realistic

############################################################################

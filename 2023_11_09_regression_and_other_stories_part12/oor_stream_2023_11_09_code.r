############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-11-09
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 4.4 - 4.5
#
# last updated: 2023-11-22

############################################################################

### 4.4: Statistical significance, hypothesis testing, and statistical errors

## Statistical significance

# say we flip a coin 20 times and it is a fair coin (so 50% chance for heads
# and 50% chance for tails); then we can construct the sampling distribution
# of the proportion of heads observed in the 20 flips through simulation
set.seed(1234)
props <- rbinom(10000000, size=20, prob=0.5) / 20

# frequency table of the observed proportions
tab <- table(props)
tab

# examine the sampling distribution of the proportions
plot(tab, type="h", lwd=3, bty="l", xlab="Proportion", ylab="Frequency",
     main="Sampling Distribution of the Proportion")

# convert the frequency table of the observed proportions into the
# corresponding probabilities of observing the proportions
tab <- tab / 10000000
tab

# sidenote: we don't actually need to simulate proportions to construct the
# sampling distribution in this example, since we know based on statistical
# theory that the probabilities of observing these different proportions can
# be computed based on a binomial distribution
round(dbinom(0:20, size=20, prob=0.5), 6)

# but we will stick to the simulated values, since this illustrates the
# general principles without requiring additional statistical theory (and the
# difference between the simulated and actual probabilities is negligible)

# extract from 'tab' the actually observed proportions (as a numeric vector)
props <- as.numeric(names(tab))
props

# compute the probability of observing a proportion of 0.7
tab[props == 0.7]

# compute the probability of observing a proportion of 0.7 or higher
sum(tab[props >= 0.7])

# compute the probability of observing a proportion of 0.75 or higher
sum(tab[props >= 0.75])

# compute the probability of observing a proportion of 0.25
sum(tab[props == 0.25])

# compute the probability of observing a proportion of 0.25 or lower
sum(tab[props <= 0.25])

# now imagine you do the experiment (flipping the coin 20 times and observing
# the proportion of heads) once and you get the following result
heads <- c(T, F, F, F, F, T, F, F, T, T, F, F, T, F, T, T, F, F, T, F)
mean(heads)

# is this outcome unusual if the coin is fair?

# compute the probability of observing this result under the sampling
# distribution of a proportion for a fair coin
sum(tab[props == mean(heads)])

# compute the probability of observing this result or an even more extreme
# deviation from 0.5 under the sampling distribution of a proportion for a
# fair coin
sum(tab[props <= mean(heads)])

# this is not an unusual event to happen if the coin is really fair

# the probability computed above is the (one-sided) p-value of our observed
# result for testing the null hypothesis H0: the coin is fair

# but say we had observed the following result
heads <- c(T, F, F, F, F, T, F, F, T, T, F, F, F, F, F, T, F, F, F, F)
mean(heads)

# the probability of observing this result or an even more extreme one is very
# small (i.e., the p-value is very small)
sum(tab[props <= mean(heads)])

# this may make us question whether the coin is really fair; conventionally,
# we are going to reject the null hypothesis if the p-value is .05 or smaller

# compute the standard error of a proportion based on a sample size of 20 if
# the true proportion is 0.5 (see section 4.2)
se <- sqrt(0.5 * 0.5 / 20)
se

# compute the test statistic (how far away is the observed result from the
# value under the null hypothesis, relative to the standard error of the
# statistic); if this is large (say, +-2), then again we are going to reject
# the null hypothesis
z <- (mean(heads) - 0.5) / se
z

# where does the +-2 come from? under a normal sampling distribution, the
# probability of observing a statistic that is 2 or more standard errors to
# the right of the center is about 2.5% (strictly, it is 1.96 SEs); so, the
# probability of either observing a test statistic of +2 (or more positive) or
# -2 (or more negative) is 5% (due to the symmetry of a normal distribution);
# in a two-sided test, we don't care if the deviation is to the left or right
# of the center, so then we use the rule that the one-sided p-value must be
# 0.025 or smaller (or twice the one-sided p-value must be 0.05 or smaller)

# the sampling distribution of the proportion we saw above is not normal (it
# cannot really be, since it is a discrete distribution, while a normal
# distribution is continuous), but we can still use a normal distribution as
# an approximation; compute the probability of observing the test statistic we
# have observed or a more extreme one under a standard normal distribution
pnorm(z)

# this is not the same as what we computed earlier (0.0207125), since the
# normal approximation is not exact (it works better when we have a larger
# sample size, that is, we flip the coin more often)

# to get the p-value for our two-sided test, we have to multiple this
# probability by 2 (to get the two-sided p-value)
2 * pnorm(z)

# or more generally, since z might be negative or positive, we take the
# absolute value of the test statistic and then compute twice the probability
# of observing this result or an even more positive one
2 * pnorm(abs(z), lower.tail=FALSE)

############################################################################

## Hypothesis testing for simple comparisons

# simulate data for a study as described in this section, where we have 100
# people in each group, assuming that the true means are the same in the two
# groups (i.e., the null hypothesis is true that the treatment does not affect
# the mean cholesterol level of those in the treatment group)
set.seed(1234)
meandiff <- replicate(100000, {
   x.t <- rnorm(100, mean=225, sd=10)
   x.c <- rnorm(100, mean=225, sd=10)
   mean(x.t) - mean(x.c)
})

# sidenote: could speed this up by directly simulating the means, but the way
# above more directly corresponds to how the data would look like if such a
# study is run

# examine the sampling distribution of the mean difference
hist(meandiff, breaks=50, xlab="Mean Difference", bty="l",
     main="Sampling Distribution of the Mean Difference")

# now imagine we actually run the study and get these results
x.t <- round(rnorm(100, mean=222, sd=10))
x.c <- round(rnorm(100, mean=225, sd=10))
mean(x.t) - mean(x.c)

# add the observed mean difference as a vertical line to the histogram
abline(v = mean(x.t) - mean(x.c), lwd=3)

# compute the probability of the observed mean difference or a more extreme
# one under the sampling distribution
mean(meandiff <= mean(x.t) - mean(x.c))

# twice this probability is the two-sided p-value
2 * (mean(meandiff <= mean(x.t) - mean(x.c)))

# compute the test statistic (note: the null hypothesis here says that the
# true mean difference is 0, so let's add this part also to the numerator)
teststat <- ((mean(x.t) - mean(x.c)) - 0) / sd(meandiff)
teststat

# compute the two-sided p-value using the test statistic
2 * pnorm(abs(teststat), lower.tail=FALSE)

# these are not exactly the same because we only simulated 100000 values under
# the sampling distribution; also, strictly speaking, we should use a
# t-distribution to compute the p-value based on the test statistic
2 * pt(abs(teststat), df=100+100-2, lower.tail=FALSE)

# in practice, we do not have sd(meandiff), but we can estimate the standard
# error using the observed data
se <- sqrt(sd(x.t)^2 / 100 + sd(x.c)^2 / 100)
se

# so we then compute the test statistic using the estimated standard error and
# the corresponding two-sided p-value
teststat <- ((mean(x.t) - mean(x.c)) - 0) / se
2 * pt(abs(teststat), df=100+100-2, lower.tail=FALSE)

# since the two-sided p-value is below .05, we reject the null hypothesis and
# conclude that the treatment does affect the mean cholesterol value of the
# treatment group

############################################################################

### 4.5: Problems with the concept of statistical significance

# not really anything to put into code from this section

############################################################################

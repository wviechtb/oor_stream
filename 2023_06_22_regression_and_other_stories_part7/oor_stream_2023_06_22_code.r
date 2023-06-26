############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-06-22
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 3.5
#
# last updated: 2023-06-26

############################################################################

### 3.5: Probability distributions

## Normal distribution; mean and standard deviation
# https://en.wikipedia.org/wiki/Normal_distribution

# draw a normal distribution with a mean of 63.7 and a standard deviation of 2.7
xs <- seq(55, 74, length=1000)
ys <- dnorm(xs, mean=63.7, sd=2.7)
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3)
abline(v=63.7, lty="dotted")

# shade in the area of the distribution where height is 60 inches or below
xs.sub <- seq(55, 60, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,60,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="lightgray", border=NA)
lines(xs, ys, lwd=3)

# calculate the size of the shaded area
pnorm(60, mean=63.7, sd=2.7)

# this is the probability of sampling a woman from the distribution whose
# height is 60 inches or shorter (around 8.5%)

# generate a random sample of the height of one million women from this distribution
set.seed(1234)
height <- rnorm(1000000, mean=63.7, sd=2.7)

# draw a histogram of the values
hist(height)

# increase the number of break points
hist(height, breaks=100)

# the proportion of women in the sample who are 60 inches or shorter
mean(height <= 60)

# redraw the normal distribution and shade in more areas (for 1, 2, and 3
# standard deviations around the mean)
xs <- seq(55, 74, length=1000)
ys <- dnorm(xs, mean=63.7, sd=2.7)
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3)
xs.sub <- seq(63.7-3*2.7, 63.7+3*2.7, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,63.7+3*2.7,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="gray30", border=NA)
xs.sub <- seq(63.7-2*2.7, 63.7+2*2.7, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,63.7+2*2.7,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="gray60", border=NA)
xs.sub <- seq(63.7-1*2.7, 63.7+1*2.7, length=1000)
ys.sub <- dnorm(xs.sub, mean=63.7, sd=2.7)
polygon(c(xs.sub,63.7+1*2.7,rev(xs.sub)), c(ys.sub,0,rep(0,1000)), col="gray90", border=NA)
lines(xs, ys, lwd=3)
text(63.7, .01, "68%")
text(63.7-1.5*2.7, .01, "13.5%")
text(63.7+1.5*2.7, .01, "13.5%")

# size of the area mu-sigma to mu+sigma (one SD below to one SD above the mean)
pnorm(63.7+1*2.7, mean=63.7, sd=2.7) - pnorm(63.7-1*2.7, mean=63.7, sd=2.7)

# so this is the probability of sampling a woman from the distribution whose
# height is between one SD below the mean to one SD above the mean

# the proportion of women in the sample whose height falls within this range
mean(height >= 63.7-1*2.7 & height <= 63.7+1*2.7)

# size of the area mu-2*sigma to mu+2*sigma (two SDs below to two SDs above the mean)
pnorm(63.7+2*2.7, mean=63.7, sd=2.7) - pnorm(63.7-2*2.7, mean=63.7, sd=2.7)

# size of the area mu-3*sigma to mu+3*sigma (three SDs below to three SDs above the mean)
pnorm(63.7+3*2.7, mean=63.7, sd=2.7) - pnorm(63.7-3*2.7, mean=63.7, sd=2.7)

# draw the normal distributions for the women and men in the same plot
xs <- seq(55, 74, length=1000)
ys <- dnorm(xs, mean=63.7, sd=2.7)
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3,
     xlim=c(55,80), col="firebrick")
abline(v=63.7, lty="dotted")
xs <- seq(60, 80, length=1000)
ys <- dnorm(xs, mean=69.1, sd=2.9)
lines(xs, ys, lwd=3, col="dodgerblue")
abline(v=69.1, lty="dotted")
legend("topright", inset=.02, col=c("firebrick","dodgerblue"), lty=1,
       legend=c("women","men"), lwd=3)

# draw the density for the 50/50 mixture of the two distributions
xs <- seq(55, 80, length=1000)
ys.w <- dnorm(xs, mean=63.7, sd=2.7)
ys.m <- dnorm(xs, mean=69.1, sd=2.9)
ys <- 0.5 * ys.w + 0.5 * ys.m
plot(xs, ys, type="l", xlab="height (inches)", ylab="density", lwd=3)

# to illustrate the CLT, let's consider the case where the height of a person
# is determined by 350 variables that correspond to 'genetic factors' that can
# either be switched on (1) or off (0) and assume that there is a 50/50 chance
# that a factor is on versus off
x001 <- sample(c(0,1), 100000, replace=TRUE)
x002 <- sample(c(0,1), 100000, replace=TRUE)
# ...
x350 <- sample(c(0,1), 100000, replace=TRUE)

# instead of doing the above 350 times, we can use replicate()
set.seed(1234)
X <- replicate(350, sample(c(0,1), 100000, replace=TRUE))

# X is a matrix with 100000 rows and 350 columns; the columns are the 350
# variables representing the presence/absence of the genetics factors

# take the sum across these 350 variables (the row sums)
height <- rowSums(X)

# draw a histogram of the resulting values for the people
hist(height, breaks=100)

# the mean, variance, and SD of the height values
mean(height)
var(height)
sd(height)

# the mean of a variable that takes on with 50% chance the value 1 and with
# 50% chance the value 0 is 0.5; we summed up 350 of such variables, so the
# true mean of the height variable we generated above must be 350 times this
# mean
350 * 0.5

# the variance of each of the variables in X is equal to 0.25
((0-0.5)^2 + (1-0.5)^2) / 2

# the variance of the height variable then is 350 times this variance
350 * 0.25

# and hence the SD of the height variable is the square root of that
sqrt(350 * 0.25)

## Linear transformations

# transform the height in centimeters into the height in inches and draw the
# histogram of these values (still a normal distribution)
hist(height / 2.54)

# simulate the difference of the mean height of 100 men and the mean height of 100 women
mean(rnorm(100, mean=69.1, sd=2.9)) - mean(rnorm(100, mean=63.7, sd=2.7))

# replicate this process 100000 times
mdiff <- replicate(100000, mean(rnorm(100, mean=69.1, sd=2.9)) -
                           mean(rnorm(100, mean=63.7, sd=2.7)))

# histogram of the resulting values
hist(mdiff, breaks=100)

# the mean and SD of the resulting values
mean(mdiff)
sd(mdiff)

## Mean and standard deviation of the sum of correlated random variables

# say we measure people twice on the same variable, once before and once after
# some kind of treatment
set.seed(1234)
pretest  <- rnorm(100, mean=100, sd=15)
posttest <- pretest + rnorm(100, mean=12, sd=10)

# create a scatterplot of the two variables
plot(pretest, posttest, pch=21, bg="gray", xlab="Pre-test", ylab="Post-test")

# add the diagonal line for when pretest = posttest
abline(a=0, b=1)

# correlation between the two variables
cor(pretest, posttest)

# compute the sum of the two scores
sumscore <- posttest + pretest

# compute the sum of the means and the mean of the sum scores (same!)
mean(pretest) + mean(posttest)
mean(sumscore)

# compute the SD of the sum scores and compare this to the formula in the book (same!)
sd(sumscore)
sqrt(var(pretest) + var(posttest) + 2*cor(pretest, posttest)*sd(pretest)*sd(posttest))

# compute the change scores
change <- posttest - pretest

# compute the difference between the means and the mean of the change scores (same!)
mean(posttest) - mean(pretest)
mean(change)

# compute the SD of the change scores and compare this to the formula in the book (same!)
sd(change)
sqrt(var(pretest) + var(posttest) - 2*cor(pretest, posttest)*sd(pretest)*sd(posttest))

## Lognormal distribution
# https://en.wikipedia.org/wiki/Log-normal_distribution

# simulate the log weight of 1000000 men
logweight <- rnorm(1000000, mean=5.13, sd=0.17)

# histogram of these values
hist(logweight, breaks=100)

# exponentiate these values to get the weight (in pounds) of these people
weight <- exp(logweight)

# histogram of the weight values
hist(weight, breaks=100)

# exponentiate the mean and SD of the logweight values
exp(mean(logweight))
exp(sd(logweight))

# note: these are not the same as taking the mean and SD of the weight values
mean(weight)
sd(weight)

# we can estimate the mean weight from the mean and SD logweight values
exp(mean(logweight) + var(logweight)/2)

# and we can estimate the SD weight from the mean and SD logweight values
sqrt((exp(var(logweight)) - 1) * exp(2*mean(logweight) + var(logweight)))

# sidenote: the exponentiated median of the logweight values is the same as
# the median of the weight values
exp(median(logweight))
median(weight)

# since mean(logweight) and median(logweight) are estimates of the same thing
# (since the true mean and median under a normal distribution are identical),
# exp(mean(logweight)) is actually an estimate of the median weight

## Binomial distribution
# https://en.wikipedia.org/wiki/Binomial_distribution

# simulate 1000000 values from a binomial distribution with n=20 and p=0.3
baskets <- rbinom(1000000, size=20, prob=0.3)

# create a frequency table of these values
table(baskets)

# create a plot of the frequencies divided by 1000000
plot(table(baskets)/1000000, type="h", ylab="Probability", bty="l")

# mean and SD of the baskets values
mean(baskets)
sd(baskets)

# compare these to the formulas in the book
20*0.3
sqrt(20*0.3*(1-0.3))

## Poisson distribution
# https://en.wikipedia.org/wiki/Poisson_distribution

# simulate 100000 values from a Poisson distribution with rate equal to 380
hits <- rpois(100000, lambda=380)

# create a plot of the frequencies divided by 100000
plot(table(hits)/100000, type="h", ylab="Probability", bty="l")

# this looks very much like a normal distribution; this is not coincidence

# the process of generating a Poisson random variable with rate parameter
# equal to 380 is the same as generating 380 Poisson random variable with rate
# parameter equal to 1 and summing up their values
X <- replicate(380, rpois(100000, lambda=1))
hits <- rowSums(X)
plot(table(hits)/1000000, type="h", ylab="Probability", bty="l")

# and as discussed above, the CLT then tells us that this variable (since it
# is the sum of many independent random variables) should have a distribution
# that is approximated by a normal distribution

# the mean and variance of the hits variable
mean(hits)
var(hits)

# the mean and variance of a Poisson random variable are equal to the rate parameter

# simulate the number of cancer cases in a county of 100,000 people that are
# followed for one year when the rate of the cancer is 45.2 per 1,000,000
# person years and repeat this process 500,000 times
cases <- rpois(500000, lambda=4.52)

# create a plot of the frequencies divided by 500000
plot(table(cases)/500000, type="h", ylab="Probability", bty="l")

# the mean and variance of the cases variable
mean(cases)
var(cases)

# simulate data from a Poisson distribution where the rate is 7.5
michaels <- rpois(1000000, lambda=7.5)

# create a plot of the frequencies divided by 1000000
par(mfrow=c(2,1))
plot(table(michaels)/1000000, type="h", ylab="Probability", bty="l")

# we can also think of this example (and this may actually be more natural) as
# data coming from a binomial distribution
michaels <- rbinom(1000000, size=750, prob=0.01)

# create a plot of the frequencies divided by 1000000
plot(table(michaels)/1000000, type="h", ylab="Probability", bty="l")

# so we see that the Poisson distribution and the binomial distribution can
# look very similar to each other; this is not generally true, but will be
# true when 'size' is at least 20 and 'prob' is <= 0.05

## Comparing distributions

# in the study described in the book, 200 heart patients either received
# 'percutaneous coronary intervention' (stents) while the other half received
# a placebo and were measured in terms of how long (in seconds) they were able
# to exercise on a treadmill; the means in the treatment and control groups
# were 530 and 510 seconds, respectively, with a standard deviation of 190
# seconds and assume that the exercise times are normally distributed within
# the two groups; then the distribution of exercise times within the two
# groups look like this
xs <- seq(0, 1100, length=1000)
ys <- dnorm(xs, mean=510, sd=190)
par(mfrow=c(1,1))
plot(xs, ys, type="l", xlab="exercise time (seconds)", ylab="density", lwd=3, lty="dotted")
ys <- dnorm(xs, mean=530, sd=190)
lines(xs, ys, lwd=3)
legend("topright", inset=.02, lty=c("dotted","solid"), lwd=3, seg.len=4,
       legend=c("control","treatment"))

# therefore, 50% of the patients in the control group have an exercise time
# above 510; in contrast, the percentage of patients in the treatment group
# that have an exercise above 510 should be higher (since the mean exercise
# time in this group is higher); we can compute this as follows
100 * pnorm(510, mean=530, sd=190, lower.tail=FALSE)

# so, around 54% of patients in the treatment group have an exercise time
# above 510 (so this is a shift from the 50th to the 54th percentile)

############################################################################

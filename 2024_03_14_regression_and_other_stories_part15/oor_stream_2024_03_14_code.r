############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-03-14
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 5.4 - 6.4
#
# last updated: 2024-03-18

############################################################################

### 5.1: Bootstrapping to simulate a sampling distribution

# before considering the example given in the book, let's consider a simpler
# example where we know the actual distribution of the statistic that we are
# interested in

# for example, say we simulate data from a normal distribution with true mean
# equal to 100 and true SD equal to 15
set.seed(1234)
n <- 20
x <- rnorm(n, mean=100, sd=15)

# we can then compute the observed mean
mean(x)

# and we know based on theory that the standard error of the mean is given by
# the true SD divided by the square root of the sample size
15 / sqrt(n)

# let's confirm this via simulation

means <- replicate(100000, {
   x <- rnorm(n, mean=100, sd=15)
   mean(x)
})

hist(means, breaks=100)
abline(v=100, lwd=5)

# the SD of the simulate means is approximately equal to the true standard
# error; it only differs from the one we computed above because we only
# simulated 100,000 means
sd(means)

# but in practice, we only have a single sample for which we compute the
# statistic of interest (i.e., in this case the observed mean)
mean(x)

# since we do not know the true SD, we can still estimate the standard error
# of the mean by dividing the observed SD by the square root of the sample size
sd(x) / sqrt(n)

# now let's pretend we know nothing about statistical theory and we do not
# know this equation for computing (or more precisely: estimating) the
# standard error of the mean; we can use bootstrapping to obtain an estimate
# of the standard error of the mean as follows

# in a single bootstrap iteration, we take a sample from our own data of the
# same size as our data with replacement (so the a particular data point might
# appear multiple times in our bootstrap sample or not at all)
x.boot <- sample(x, size=n, replace=TRUE)
x.boot

# we then compute the statistic of interest in the bootstrap sample
mean(x.boot)

# in bootstrapping, we repeat this process many times

means <- replicate(100000, {
   x <- sample(x, size=n, replace=TRUE)
   mean(x)
})

# inspect the bootstrap distribution of our statistic of interest
hist(means, breaks=100)

# note that this distribution is *not* centered around the true mean, but it
# is centered around the observed mean; so we cannot use this distribution to
# magically recover what the true mean is; however, we can use the standard
# deviation of the bootstrap means to get an estimate of the standard error of
# our observed mean
sd(means)

# this is quite close to the standard error we computed above based on the
# theoretical equation

# now let's do a little simulation study to see how similar the standard error
# is when computed based on the theoretical equation versus bootstrapping
# (below, we only do 1000 bootstrap iterations, which should be sufficient)

iters <- 2000
se.thry <- numeric(iters)
se.boot <- numeric(iters)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (s in 1:iters) {

   setTxtProgressBar(pbar, s)

   x <- rnorm(n, mean=100, sd=15)
   se.thry[s] <- sd(x) / sqrt(n)

   means <- replicate(1000, {
      x <- sample(x, size=n, replace=TRUE)
      mean(x)
   })
   se.boot[s] <- sd(means)

}

# scatterplot of the SE computed based on the theoretical equation versus the
# SE computed based on bootstrapping (the diagonal line corresponds to the
# case where the two are identical)
plot(se.thry, se.boot, pch=21, bg="darkgray", cex=0.5,
     xlab="SE Based on Theory", ylab="SE Based on Bootstrapping")
abline(a=0, b=1, lwd=6, col="red")

# also add a horizontal and vertical line at the true SE
abline(h=15/sqrt(n), lty="dotted")
abline(v=15/sqrt(n), lty="dotted")

# so we can see that the SE based on bootstrapping is a close approximation to
# the SE based on theory (but if we look closely, we see that the bootstrap
# SEs tend to slightly underestimate the theoretical SEs)
mean(se.thry)
mean(se.boot)

# and we can compare the two above with the true SE
15 / sqrt(n)

# still, it is quite amazing that this trick (using the observed data to
# estimate the SD of the mean via resampling with replacement) actually works

# now let's go to the example from the book

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the dataset
head(dat)

# compute the ratio of the median earnings of women versus the median earnings of men
with(dat, median(earn[male==0]) / median(earn[male==1]))

# since we do not know the theoretical equation for computing the standard
# error of the ratio of two medians, we will use bootstrapping to estimate it

n <- nrow(dat)

ratios <- replicate(10000, {
   dat.boot <- dat[sample(n, replace=TRUE),]
   with(dat.boot, median(earn[male==0]) / median(earn[male==1]))
})

sd(ratios)

# we can also examine the bootstrap distribution of the statistic of interest
# (since the earnings only take on a relatively small number of unique values,
# the ratio of the two medians can also only take on a relatively small number
# of unique values; this looks a bit strange, but we can ignore this issue)
hist(ratios)

# let's do the bootstrapping using the boot package

# load the package
library(boot)

# function that takes the original data as input and a vector of indices that
# define the bootstrap sample and that then computes the statistic of interest
# based on the bootstrap sample
compratio <- function(x, idx) {
   dat.boot <- x[idx,]
   with(dat.boot, median(earn[male==0]) / median(earn[male==1]))
}

# run the bootstrapping with boot()
res <- boot(dat, statistic=compratio, R=10000)
res

# the 'std. error' values is the estimated standard error of the statistic

# note: element 't' of 'res' contains the bootstrap values of the statistic of
# interest (as a one column matrix), so we could also just compute the SD of
# these values ourselves
sd(res$t)

# note: using boot.ci(res), we can then obtain various types of confidence
# intervals for the true ratio, but this is now really beyond the discussion
# in the book, so let's skip this

############################################################################

### 6.2: Fitting a simple regression to fake data

# install the rstanarm package (need to do this once)
#install.packages("rstanarm")

# load the rstanarm package
library(rstanarm)

# simulate data as described in the book
x <- 1:20
n <- length(x)
a <- 0.2
b <- 0.3
sigma <- 0.5
set.seed(2141)
y <- a + b*x + rnorm(n, mean=0, sd=sigma)

# plot x versus y
plot(x, y, pch=21, bg="gray", main="Data and fitted regression line", bty="l")

# create a data frame with x and y
dat <- data.frame(x, y)

# remove the x and y vector objects from the workspace
rm(x, y)

# fit a simple linear regression model using stan_glm()
res <- stan_glm(y ~ x, data=dat)
print(res, digits=2)

# add the regression line to the plot
abline(res, lwd=3)

# use lm() to fit the same model using a more classical (frequentist) approach
res <- lm(y ~ x, data=dat)
summary(res)

# replicate the above 10000 times and check in each replication whether the
# 95% confidence interval for the slope actually captures the true slope

isin <- replicate(10000, {
   dat$y <- a + b*dat$x + rnorm(n, mean=0, sd=sigma)
   res <- lm(y ~ x, data=dat)
   ci <- confint(res)[2,]
   ci[1] <= b && ci[2] >= b
})

# check in what proportion of replicates the CI captures the true slope
mean(isin)

# we see that the CI does in fact capture the true slope is ~95% of the cases

############################################################################

### 6.3: Interpret coefficients as comparisons, not effects

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the dataset
head(dat)

# fit a regression model predicting earnings (in thousands of dollars) from
# the height of the individual and whether the person is male (1) or not (0)
set.seed(1234)
res <- stan_glm(earnk ~ height + male, data=dat)
res

# note: the results we obtain above may be slightly different than the ones
# given in the book, because the model fitting as done by stan_glm() involves
# some stochastic properties (to be discussed in more detail later on); to
# make the results obtained at least fully reproducible, we should always set
# the seed of the random number generator before the model fitting

# compute R^2, that is, how much of the variance in the earnings is accounted
# for based on the model; can also think of this as the proportional reduction
# in the variance when we explain some of the variation in the earnings based
# on the regression model
(sd(dat$earnk)^2 - sigma(res)^2) / sd(dat$earnk)^2
1 - sigma(res)^2 / sd(dat$earnk)^2

# to illustrate the point that is being made here about terminology, let's
# consider an even simpler model where we just use height as the predictor
res <- stan_glm(earnk ~ height, data=dat)
res

# so, based on this model, we would say that individuals that differ from each
# other by one inch, the taller person earns on average 1.6k more (or if
# people differ by x inches, the taller person earns on average x*1.6k more)

# suppose we randomly select two individuals from the dataset and compute the
# difference in their earnings and the difference their heights
sel <- dat[c(536,1258),]
sel$earnk[1] - sel$earnk[2]
sel$height[1] - sel$height[2]

# then for these two individuals, we can compute how big the difference in
# their earnings is per one-unit difference in inches
(sel$earnk[1] - sel$earnk[2]) / (sel$height[1] - sel$height[2])

# now let's repeat the above many times

n <- nrow(dat)
diffperinch <- replicate(10000, {
   sel <- dat[sample(n, 2),]
   (sel$earnk[1] - sel$earnk[2]) / (sel$height[1] - sel$height[2])
})

# when we now compute the mean of these values (and here we unfortunately have
# to filter out infinity and missing values that can arise due to division by
# zero), then we essentially get the same value as the slope
mean(diffperinch[!is.infinite(diffperinch) & !is.na(diffperinch)])

# this illustrates that the slope reflects a comparison between people (i.e.,
# on average, how much do people differ on y that differ one unit on x?)

############################################################################

### 6.4: Historical origins of regression

# download the dataset (only need to do this once)
#download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/PearsonLee/data/Heights.txt", destfile="heights.txt")

# read in the data
dat <- read.table("heights.txt", header=TRUE)

# inspect the dataset
head(dat)

# Figure 6.3a: mother's height versus daughter's height (with jittering to
# avoid points that overlap)
plot(jitter(daughter_height, amount=0.5) ~ jitter(mother_height, amount=0.5),
     data=dat, pch=19, cex=0.2, xlab="Mother's height (inches)",
     ylab="Adult daughter's height (inches)", bty="l")
grid()

# fit a simple linear regression model predicting the daughter's height based
# on the mother's height
res <- stan_glm(daughter_height ~ mother_height, data=dat)
res

# add the regression line to the plot
abline(res, lwd=5)

# also add a diagonal line
abline(a=0, b=1, lwd=5, lty="dotted")

# we can see that the regression line is shallower than the diagonal line

# add a point at the intersection of the two means
points(mean(dat$mother_height), mean(dat$daughter_height), pch=21, bg="red", cex=2)

# refit the model when recentering the mother's heights around 62.5 (which is
# approximately the mean of the the mother's heights in this sample)
res <- stan_glm(daughter_height ~ I(mother_height - 62.5), data=dat)
res

# this does not change the slope, but makes the intercept more interpretable
# (the intercept now reflects the mean height of daughters whose mother is
# 62.5 inches tall)

############################################################################

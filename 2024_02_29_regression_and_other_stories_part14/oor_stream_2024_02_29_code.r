############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-29
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 5.1 - 5.3
#
# last updated: 2024-03-06

############################################################################

### 5.1: Simulation of discrete probability models

## How many girls in 400 births?

# simulate a single draw from a binomial distribution with 400 'trials'
# (births) and a probability of 0.488 that the event of interest occurs on a
# single trial (i.e., that the baby is a girl)
n_girls <- rbinom(1, 400, 0.488)
n_girls

# repeat this process 1000 times and save the simulated values in a vector
n_sims <- 1000
n_girls <- rep(NA, n_sims)
for (s in 1:n_sims) {
   n_girls[s] <- rbinom(1, 400, 0.488)
}

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400 Births)")

# we don't really need a for-loop to do the above; we can directly simulate
# 1000 values from the binomial distribution; to make the simulated values
# reproducible, we also set the seed of the random number generator
set.seed(1234)
n_girls <- rbinom(n_sims, 400, 0.488)
n_girls

# create a histogram of the simulated values
hist(n_girls, main="", xlab="Number of Girls (out of 400 Births)")

## Accounting for twins

# simulate the process of 400 births where there is a certain chance of twins
# being born and save how many of the babies born on each birth are girls

birth_type <- sample(c("fraternal twin","identical twin","single birth"),
                     size=400, replace=TRUE, prob=c(1/125, 1/300, 1-1/125-1/300))

girls <- rep(NA, 400)

for (i in 1:400) {
   if (birth_type[i] == "single birth") {
      girls[i] <- rbinom(1, 1, 0.488)
   } else if (birth_type[i] == "identical twin") {
      girls[i] <- 2*rbinom(1, 1, 0.495)
   } else {
      girls[i] <- rbinom(1, 2, 0.495)
   }
}

n_girls <- sum(girls)
n_girls

# can also use a doubly-nested ifelse() construction to do the above

girls <- ifelse(birth_type=="single birth", rbinom(400, 1, 0.488),
                ifelse(birth_type=="identical twins", 2*rbinom(400, 1, 0.495),
                       rbinom(400, 2, 0.495)))
n_girls <- sum(girls)
n_girls

# simulate the process above 1000 times

n_girls <- rep(NA, n_sims)

for (s in 1:1000) {

   birth_type <- sample(c("fraternal twin","identical twin","single birth"),
                        size=400, replace=TRUE, prob=c(1/125, 1/300, 1-1/125-1/300))

   girls <- rep(NA, 400)

   for (i in 1:400) {
      if (birth_type[i] == "single birth") {
         girls[i] <- rbinom(1, 1, 0.488)
      } else if (birth_type[i] == "identical twin") {
         girls[i] <- 2*rbinom(1, 1, 0.495)
      } else {
         girls[i] <- rbinom(1, 2, 0.495)
      }
   }

   n_girls[s] <- sum(girls)

}

# Figure 5.1: histogram of 1000 simulated values for the number of girls born
# in a hospital from 400 births, as simulated from the model that includes the
# possibility of twins
hist(n_girls, main="", xlab="Number of Girls (out of 400 Births)")

############################################################################

### 5.2: Simulation of continuous and mixed discrete/continuous models

# https://en.wikipedia.org/wiki/Normal_distribution
# https://en.wikipedia.org/wiki/Log_normal_distribution
# https://en.wikipedia.org/wiki/Binomial_distribution
# https://en.wikipedia.org/wiki/Poisson_distribution

n_sims <- 1000
y1 <- rnorm(n_sims, mean=3, sd=0.5)
y2 <- rlnorm(n_sims, meanlog=3, sdlog=0.5)
y3 <- rbinom(n_sims, size=20, prob=0.6)
y4 <- rpois(n_sims, lambda=5)

# Figure 5.2

par(mfrow=c(2,2))
hist(y1, breaks=seq(floor(min(y1)), ceiling(max(y1)), 0.2), main="normal dist with mean 3 and sd 0.5")
hist(y2, breaks=seq(0, ceiling(max(y2)) + 5, 5), main="lognormal dist with logmean 3 and logsd 0.5")
hist(y3, breaks=seq(-0.5, 20.5, 1), main="binomial dist with 20 tries and probability 0.6")
hist(y4, breaks=seq(-0.5, max(y4) + 1, 1), main="Poisson dist with mean 5")
par(mfrow=c(1,1))

# generate the height of one randomly chosen adult (where there is a 0.48
# probability that the person is a male)

male <- rbinom(1, 1, 0.48) # 0 = female, 1 = male
male
height <- ifelse(male==1, rnorm(1, 69.1, 2.9), rnorm(1, 64.5, 2.7))
height

# generate the heights of 10 adults and take the mean of the 10 simulated values

N <- 10
male <- rbinom(N, 1, 0.48)
height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 64.5, 2.7))
avg_height <- mean(height)
avg_height

# repeat the above 100000 times to generate 100000 means (and also save the
# maximum height of the 10 adults in each iteration) and also use a progress
# bar to get an indication for how much longer we have to wait for the loop to
# finish

n_sims <- 100000
avg_height <- rep(NA, n_sims)
max_height <- rep(NA, n_sims)

pbar <- txtProgressBar(min=0, max=n_sims, style=3)

for (s in 1:n_sims) {

   setTxtProgressBar(pbar, s)

   male <- rbinom(N, 1, 0.48)
   height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 64.5, 2.7))
   avg_height[s] <- mean(height)
   max_height[s] <- max(height)
}

# create a histogram of the simulated means (using the density on the y-axis)
hist(avg_height, main="Dist of avg height of 10 adults", xlab="Average Height",
     breaks=100, freq=FALSE)

# superimpose a normal distribution on top of the histogram
curve(dnorm(x, mean=mean(avg_height), sd=sd(avg_height)), lwd=3, add=TRUE)

# create a histogram of the simulated maximums
hist(max_height, main="Dist of max height of 10 adults", xlab="Maximum Height",
     breaks=100, freq=FALSE)

# superimpose a normal distribution on top of the histogram
curve(dnorm(x, mean=mean(max_height), sd=sd(max_height)), lwd=3, add=TRUE)

## Simulation in R using custom-made functions

height_sim <- function(N) {
   male <- rbinom(N, 1, 0.48)
   height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 63.7, 2.7))
   mean(height)
}

avg_height <- replicate(100000, height_sim(N=10))
hist(avg_height, main="Dist of avg height of 10 adults", xlab="Average Height", breaks=100)

# return both the mean and the maximum

height_sim <- function(N) {
   male <- rbinom(N, 1, 0.48)
   height <- ifelse(male==1, rnorm(N, 69.1, 2.9), rnorm(N, 63.7, 2.7))
   c(mean = mean(height), max = max(height))
}

height_stats <- replicate(100000, height_sim(N=10))
height_stats[,1:5]

# the rows correspond to the mean and maximum and the columns to the replicates

############################################################################

### 5.3: Summarizing a set of simulations using median and median absolute deviation

# simulate 10000 values from a normal distribution and compute various summary
# statistics based on these values
N <- 10000
z <- rnorm(N, mean=5, sd=2)
cat("mean   =", mean(z), "\nmedian =", median(z),
    "\nsd     =", sd(z), "\nmad sd =", mad(z), "\n")

# install the pbapply package (only need to do this once) and load it
#install.packages("pbapply")
library(pbapply)

# repeat the above 5000 times and use pbreplicate() to get a progress bar

stats <- pbreplicate(5000, {
   z <- rnorm(N, mean=5, sd=2)
   c(mean = mean(z), median = median(z), sd = sd(z), madsd = mad(z))
})

stats[,1:5]

# the sample mean and median are both unbiased estimates of the true
# mean/median of the data (which is 5 here)
mean(stats["mean",])
mean(stats["median",])

# while the sample sd and madsd are not unbiased estimates of the true SD of
# the data, they are asymptotically unbiased, so these are both essentially 2
mean(stats["sd",])
mean(stats["madsd",])

# compute the standard deviation of the means (= standard error of the mean)
# and compare this to the theoretical value (they only differ from each other
# because we only simulated 5000 means)
sd(stats["mean",])
2 / sqrt(N)

# compute the standard deviation of the medians (= standard error of the
# median) and compare this to the theoretical value; note that the latter is
# only correct 'asymptotically' (i.e., when N is large), but with N=10000,
# this is surely sufficiently large for this equation to hold; for the
# distribution of the sample median, see:
# https://en.wikipedia.org/wiki/Median#Sampling_distribution
sd(stats["median",])
sqrt(1 / (4*N*dnorm(5, mean=5, sd=2)^2))
sqrt(pi/2) * 2 / sqrt(N) # simplified version of the previous line

# compute the standard deviation of the SD values (= standard error of the
# standard deviation) and compare this to the (asymptotic) theoretical value
sd(stats["sd",])
2 / sqrt(2*N)

# compute the standard deviation of the MAD SD values (= standard error of the
# median absolute deviation from the median, scaled by 1.483) and compare this
# to the (asymptotic) theoretical value
sd(stats["madsd",])
sqrt(2/pi) * 2 / sqrt(N) * 1.483

# we see above that the sample mean has a lower standard error than the sample
# median and that the sample SD has a lower standard error than the sample MAD
# SD; this is always true when the data are normally distributed even when N
# is small; so the statement in the book about the median and MAD SD being
# more 'stable' summaries for low sample sizes is not true in general;
# however, for other distributions, this is indeed the case; for example, for
# a t-distribution with low degrees of freedom, even though the mean and
# median are the same (i.e., 0) and both statistics are unbiased estimators,
# the sample median now has a lower standard error than the sample mean

N <- 100
stats <- pbreplicate(100000, {
   z <- rt(N, df=3)
   c(mean = mean(z), median = median(z), sd = sd(z), madsd = mad(z))
})

mean(stats["mean",])
mean(stats["median",])
sd(stats["mean",])
sd(stats["median",])

# note that we cannot easily compare the sd and madsd values, since they
# estimate different things (the madsd is scaled in such a way to estimate the
# SD of normally distributed data, but this is obviously not the case here)

############################################################################

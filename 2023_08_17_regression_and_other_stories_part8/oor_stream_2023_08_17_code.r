############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-08-17
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 3.6 - 4.1
#
# last updated: 2023-08-31

############################################################################

### 3.6: Probability modeling

## Using an empirical forecast

# switch off scientific notation for this example
options(scipen=100)

# simulate y (proportion of the votes received by the slightly less favored
# candidate) 10^7 times and then compute the probability of seeing exactly n/2
# votes for each candidate in n voters (this gives an empirical estimate of
# seeing an equal split of the votes using a simulation approach)
n <- 2000
y <- rnorm(10000000, mean=0.49, sd=0.04)
mean(round(y * n) == n/2)

# use the equation in the book
dnorm(0.5, mean=0.49, sd=0.04) / n

# we can confirm that when we change n (to say 20000 or 200000), this still
# works, but when n is very large, we need to increase how many values of y we
# simulate to get an accurate estimate with the simulation approach

# the equation given in the book is actually an approximation; to do this
# absolutely correctly, we need to figure out the probability of seeing a
# value of y that would give us an even split of the votes when we multiply y
# with n (and round); this will happen when the value of y is between 0.5 -
# 1/(2*n) and 0.5 + 1/(2*n); that is, when y is between:
0.5 - 1/(2*n)
0.5 + 1/(2*n)

# since then y multiplied by n (and rounded) gives us an even split
round((0.5 - 1/(2*n)) * n)
round((0.5 + 1/(2*n)) * n)

# we can compute the probability of seeing such a y value with pnorm()
pnorm(0.5+1/(2*n), mean=0.49, sd=0.04) - pnorm(0.5-1/(2*n), mean=0.49, sd=0.04)

# let's visualize this area
vals <- 10^5
xs <- seq(0.5 - 4*0.04, 0.5 + 4*0.04, length=vals)
ds <- dnorm(xs, mean=0.5, sd=0.04)
plot(xs, ds, type="l", xlab="Value of y", ylab="Density", bty="n")
abline(h=0)
xs <- seq(0.5 - 1/(2*n), 0.5 + 1/(2*n), length=vals)
ds <- dnorm(xs, mean=0.5, sd=0.04)
polygon(c(xs, rev(xs)), c(ds,rep(0,vals)), col="lightgray")

# since that area is so tiny, let's zoom in at the top
xs <- seq(0.5 - 5/(2*n), 0.5 + 5/(2*n), length=vals)
ds <- dnorm(xs, mean=0.5, sd=0.04)
plot(xs, ds, type="l", xlab="Value of y", ylab="Density", bty="n")
xs <- seq(0.5 - 1/(2*n), 0.5 + 1/(2*n), length=vals)
ds <- dnorm(xs, mean=0.5, sd=0.04)
polygon(c(xs, rev(xs)), c(ds,rep(0,vals)), col="lightgray")

# if we ignore the curvature at the top, then we can simplify the calculation
# of this area by just calculating the area of this rectangle
rect(0.5 - 1/(2*n), 0, 0.5 + 1/(2*n), max(ds), lty="dotted", col=rgb(0,0,0,0.2))

# and this is exactly what the formula in the book calculates
dnorm(0.5, mean=0.49, sd=0.04) / n

# the larger n is, the closer the equation from the book is to the correct
# value (and for 200000, there is essentially no difference)
n <- 200000
dnorm(0.5, mean=0.49, sd=0.04) / n
pnorm(0.5+1/(2*n), mean=0.49, sd=0.04) - pnorm(0.5-1/(2*n), mean=0.49, sd=0.04)

# calculate the probability that an additional 1000 votes for the candidate
# that is less favored is decisive (i.e., it will push the number of votes for
# the candidate from losing the election to winning the election); again, this
# is an approximation
1000 * dnorm(0.5, 0.49, 0.04) / n

# the exact calculation is this
pnorm(0.5+1/(2*n), mean=0.49, sd=0.04) - pnorm(0.5-1000/n+1/(2*n), mean=0.49, sd=0.04)

# these two are essentially the same and very close to 1/21
1/21

## Using an reasonable-seeming but inappropriate probability model

# probability of x=100000 when n=200000 and p=0.5
dbinom(100000, size=200000, prob=0.5)

# probability of x=100000 when n=200000 and p=0.49
dbinom(100000, size=200000, prob=0.49)

# simulate a very large number of values of y from the binomial model
y <- rbinom(10000000, size=200000, prob=0.49) / n

# histogram for these proportions
hist(y, breaks=250, freq=F)

# as we can see, under the binomial model, there is essentially no chance that
# the less favored candidate will win (the probability of y being larger than
# 0.5 is essentially zero); this does not seem like a reasonable model to
# capture the uncertainty of an election where one candidate is very slightly
# favored; in contrast, the normal model we used earlier allows for much more
# uncertainty in y; contrast the SD=0.04 we used earlier with the SD of the y
# values from the binomial model
sd(y)

# note: the true SD is sqrt(p*(1-p)/n)
sqrt(0.49*0.51/n)

# so the SD for y is much smaller under the binomial model

# set scipen back to the default
options(scipen=0)

############################################################################

### 4.1 Sampling distributions and generative models

## The sampling distribution

# say we are interested in the height of individuals and the population
# consists of N=10 individuals with heights equal to:
y <- c(178, 184, 165, 173, 196, 168, 171, 185, 180, 174)

# using combn(), we can generate all possible samples of size n=3
sampdist <- apply(combn(10, 3), 2, function(i) y[i])
sampdist

# so there are 120 different datasets in the sampling distribution

# in the measurement error example where the observations are generated from
# y_i = a + b*x_i + e_i (where e_i ~ N(0, sigma) for i = 1, ..., n), we cannot
# really generate all possible datasets, because there is an infinite number
# of them; for example, say n=10 and the x_i values are as follows
xi <- c(9.5, 2.9, 6.4, 9.0, 8.6, 2.5, 9.2, 3.0, 8.9, 5.8)

# and say a=2, b=1, and sigma=0.5, then 5 possible datasets would be
replicate(5, 2 + 1 * xi + rnorm(10, 0, 0.5))

# but one can generate an infinite number of such datasets

############################################################################

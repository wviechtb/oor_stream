############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-09-19
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 3.3 - 3.4
#
# last updated: 2024-09-24

############################################################################

### 3.3: Sampling to simulate prediction

## 3.3.1: Dummy data

# probabilities of seeing 0, 1, or 2 times water when the true probability of
# seeing water is 0.7 on a single toss based on a binomial distribution
data.frame(W=0:2, prob=dbinom(0:2, size=2, prob=0.7))

# simulate one value of W from this distribution
rbinom(1, size=2, prob=0.7)

# simulate 10 values of W from this distribution
rbinom(10, size=2, prob=0.7)

# simulate 100,000 values and create a frequency table of the observed values
dummy_w <- rbinom(1e5, size=2, prob=0.7)
tab <- table(dummy_w)
tab

# turn the frequencies into proportions
tab / sum(tab)

# simulate 100,000 values when there are 9 tosses
dummy_w <- rbinom(1e5, size=9, prob=0.7)
tab <- table(dummy_w)
tab

# Figure 3.5: plot of the frequencies
plot(tab, xlab="dummy water count", ylab="Frequency")

# it may happen that one of the values of W is never observed in the simulated
# values (although the chances of this happening with 100,000 values is very
# very small); to guarantee that all values show up in the table/plot (even if
# the frequency is 0), we can turn 'dummy_w' into a factor with the known
# values/levels in can take on before creating the table and plot
plot(table(factor(dummy_w, levels=0:9)), xlab="dummy water count", ylab="Frequency")

# load the rethinking package
library(rethinking)

# could also use the simplehist() function from the rethinking package
simplehist(dummy_w, xlab="dummy water count")

## 3.3.2: Model checking

# 3.3.2.1: Did the software work?

# not done since the software implementation is simple enough that it can be
# checked against analytic results, so this was skipped; maybe the book comes
# back to this idea in the context of a more complex example later on

# 3.3.2.2: Is the model adequate?

# recreate the grid approximation we did in chapter 2
p_grid <- seq(from=0, to=1, length.out=1000) # set up the grid
prob_p <- rep(1, 1000) # assumed prior (each value of p is equally likely)
prob_data <- dbinom(6, size=9, prob=p_grid) # compute the likelihoods
posterior <- prob_data * prob_p # compute the posterior values
posterior <- posterior / sum(posterior) # rescale them so they add up to 1
plot(p_grid, posterior, type="l", lwd=4) # plot the posterior distribution

# note that the peak of the posterior is at 6/9 (= W/N)
abline(v=6/9)

# for every value of p in the grid, construct the binomial distribution
mat <- sapply(p_grid, function(p) dbinom(0:9, size=9, prob=p))
mat[,1:5]

# multiply the probabilities of seeing 0:9 times water for a given value of p
# with the corresponding posterior probability of the value of p (note: since
# the multiplication is done column-wise, we need to transpose 'mat' first,
# then multiply, and then we transpose back)
mat <- t(posterior * t(mat))
mat[,1:5]

# now take the mean of the values across rows; this gives us the posterior
# predictive distribution for seeing 0:9 times water
ppd <- rowMeans(mat)
ppd <- ppd / sum(ppd)

# Figure 3.6: plot of the posterior predictive distribution
plot(0:9, ppd, type="h", lwd=5, xlab="number of water samples",
     ylab="probability", xaxt="n", ylim=c(0,0.3))
axis(side=1, 0:9)

# if we ignore the uncertainty as to what p is and just use the most probable
# value according to the posterior distribution for p (6/9), then we would be
# underestimating the uncertainty for new observations
lines(0:9 + 0.05, dbinom(0:9, size=9, prob=6/9), type="h", lwd=3, col="red")

# now suppose we just have 10,000 sampled values from the posterior distribution
samples <- sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

# now we are going to simulate 10,000 new data points where for each simulated
# value, we use the corresponding sampled value of p from the posterior
w <- rbinom(1e4, size=9, prob=samples)
head(w)

# create a frequency table of the simulated values (again, we turn w into a
# factor for this just to be certain that every value between 0 and 9, even
# one with 0 frequency, shows up in the table)
tab <- table(factor(w, levels=0:9))
tab

# rescale the frequencies to proportions
tab <- tab / sum(tab)
tab

# add these proportions to the figure
lines(0:9 - 0.05, tab, type="h", lwd=3, col="blue")

# add a legend
legend("topleft", inset=.01, lty=1, col=c("black","red","blue"), lwd=c(5,3,3),
       legend=c("posterior predictive distribution",
                "binomial(N=9, p=6/9)",
                "posterior predictive distribution simulations"))

# now we can use the simulated values from the posterior predictive
# distribution to compute for example a 80% percentile interval
quantile(w, probs=c(.10, .90))

# simulate values from the PPD of the maximum run lengths
sim.maxrun <- sapply(samples, function(p) {
   x <- rbinom(9, size=1, prob=p)
   maxrun <- max(rle(x)$lengths)
   factor(maxrun, levels=1:9)
})

# create a frequency table of the simulated values and rescale
tab <- table(sim.maxrun)
tab <- tab / sum(tab)
tab

# Figure 3.7 (left): plot the posterior predictive distribution
plot(1:9, c(tab), type="h", lwd=5, xlab="longest run length",
     ylab="probability", xaxt="n")
axis(side=1, 1:9)

# actual sequence observed
wobs <- c(1,0,1,1,1,0,1,0,1)

# maximum running length observed in the actual sequence
maxrun <- max(rle(wobs)$lengths)

# make the line in the plot blue
segments(maxrun, 0, maxrun, tab[which(maxrun == names(tab))], lwd=8, col="#1e59ae")

# simulate values from the PPD of the number of switches
sim.switches <- sapply(samples, function(p) {
   x <- rbinom(9, size=1, prob=p)
   switches <- length(rle(x)$lengths) - 1
   factor(switches, levels=0:8)
})

# create a frequency table of the simulated values and rescale
tab <- table(sim.switches)
tab <- tab / sum(tab)
tab

# Figure 3.7 (right): plot the posterior predictive distribution
plot(0:8, c(tab), type="h", lwd=5, xlab="number of switches",
     ylab="probability", xaxt="n")
axis(side=1, 0:8)

# number of switches observed in the actual sequence
switches <- length(rle(wobs)$lengths) - 1

# make the line in the plot blue
segments(switches, 0, switches, tab[which(switches == names(tab))], lwd=8, col="#1e59ae")

############################################################################

# on p. 67-68, the book states that even if there is correlation between the
# observed values on consecutive throws of the globe, we should still obtain
# the correct proportion in the long run; this is not entirely obvious, so we
# will conduct a little simulation to examine this; for this, let's assume
# that the person throwing the globe does not spin the globe much on each
# throw, so when the globe lands and we examine the presence of water at the
# location at the very top (or bottom) of the globe, we will tend to see the
# same observation as on the previous throw, since land and water are not
# distributed randomly across the globe, but are clustered; this will induce
# positive correlation between the throws

# let's draw a (flattened out) map of a fictitious earth where there is land
# in the upper right quadrant, so 3/4 of the planet is covered in water

plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="pos1", ylab="pos2", xaxs="i", yaxs="i")
rect(0, 0, 1, 1, col="#1e59ae")
rect(0.5, 0.5, 1, 1, col="brown4")

# now to simulate the process above, we are going to take 100,000 'steps'
# starting at a random location, where on each step we just move randomly a
# small amount horizontally and vertically; note that when we move beyond one
# of the borders of the map, we need to move our position to the opposite side
# (since we are technically moving around a globe); at each step, we record
# whether we are on water or not

steps <- 100000
water <- rep(NA, steps)
pos <- matrix(NA_real_, nrow=steps, ncol=2)
draw <- rep(1, steps)

# set a random starting position and record if we are on water

pos[1,] <- runif(2)
water[1] <- ifelse(pos[1,2] < 0.5, 1, ifelse(pos[1,1] < 0.5, 1, 0))

for (i in 2:steps) {

   # set the new location (add a random amount between -.05 and .05 to the
   # current pos1 and pos2 values)
   pos[i,] <- pos[i-1,] + runif(2, -.05, .05)

   # check if we have moved beyond one of the borders and, if so, record this
   # as a 0 in the 'draw' vector
   if (any(pos[i,] < 0 | pos[i,] > 1))
      draw[i] <- 0

   # if we have moved beyond a border, adjust the position to the opposite side
   if (pos[i,1] > 1)
      pos[i,1] <- pos[i,1] - 1
   if (pos[i,1] < 0)
      pos[i,1] <- 1 + pos[i,1]
   if (pos[i,2] > 1)
      pos[i,2] <- pos[i,2] - 1
   if (pos[i,2] < 0)
      pos[i,2] <- 1 + pos[i,2]

   # record if we are on water
   water[i] <- ifelse(pos[i,2] < 0.5, 1, ifelse(pos[i,1] < 0.5, 1, 0))

}

# check how often we have been on water; this is indeed approximately 0.75
mean(water)

# add our path on the map (note: when drawing the line segments, we need to
# skip cases where we moved beyond the border as otherwise we would get lines
# moving from one border to the opposite side)
pos[draw == 0,] <- NA
segments(pos[1:(steps-1),1], pos[1:(steps-1),2], pos[2:steps,1], pos[2:steps,2])

# the following will show an 'animation' of the position moving around the
# map, but this will take quite some time to finish
plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="pos1", ylab="pos2", xaxs="i", yaxs="i")
rect(0, 0, 1, 1, col="#1e59ae")
rect(0.5, 0.5, 1, 1, col="brown4")
for (i in 2:steps) {
   segments(pos[i-1,1], pos[i-1,2], pos[i,1], pos[i,2])
   title(list(paste("▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇▇", i), col="white"))
   title(paste("Step", i))
}

############################################################################

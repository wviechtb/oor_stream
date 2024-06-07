############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-06-06
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 2.1 - 2.3
#
# last updated: 2024-06-07

############################################################################

### 2.1: The garden of forking data

# the observed marbles
obs <- c("B","W","B")
obs

# list with the five possible conjectures (color of the marbles in the bag)
conjs <- lapply(4:0, function(x) rep(c("W","B"), times=c(x,4-x)))
conjs

# function that takes a conjecture and the observed draws as input and returns
# the number of paths that one can take through the garden of forking data
# according to the conjecture and the observed draws; the function does this
# by enumating all possible paths and then counting how many of them are
# consistent with the observed draws
count <- function(conj, obs) {
   n <- length(obs)
   conj <- expand.grid(replicate(n, conj, simplify=FALSE))
   sum(colSums(obs == t(conj)) == n)
}

# check that this returns the correct value (3) for conjecture 2
count(conjs[[2]], obs)

# now apply the function to all five conjectures
sapply(conjs, count, obs)

# now we draw another marble and it is blue
obs <- c("B","W","B","B")

# again compute the number of paths for each conjecture
sapply(conjs, count, obs)

# now make these counts the prior
prior <- sapply(conjs, count, obs)

# vector with the factory count data
factory <- c(0, 3, 2, 1, 0)

# the final count is the product of the two
prior * factory

# note that it does not matter what we consider the prior and the data here;
# we could just as well consider the information about the packaging of bags
# as prior information and the number of paths as the data

# the count() function above creates a large data frame (conj) that can get
# very large when 'obs' is a long sequence, to the point that your computer
# can easily run out of memory

# we can do the same calculation as explained on page 23 by computing for each
# element in 'obs' how many marbles match up in color for each element in a
# particular conjecture and then take the product term of these values; let's
# try this for the four observations above
sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))

# this will also work for longer observation sequences without things blowing
# up (although the counts themselves get very large)
obs <- sample(c("B","W"), 50, replace=TRUE)
sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))

# the absolute values are not relevant; we just care about the relative sizes
ways <- sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))
round(ways / sum(ways), 4)

# let's go back to the short sequence of three marbles
obs <- c("B","W","B")
ways <- sapply(conjs, function(conj) prod(sapply(obs, function(x) sum(x == conj))))
data.frame(p = 0:4 / 4, ways = ways, plausibility = ways / sum(ways))

# as will be discussed in the next section, the kind of process that is
# described by the marble example is actually the process underlying a
# binomial distribution; we can think of our observation as a count of the
# number of blue marbles in the three draws where, on each draw, there is a
# given probability of seeing a blue marble according to a certain conjecture;
# for example, say 1 out of the 4 marbles in the bag are blue, then there is a
# 1/4 = 0.25 probability of drawing a blue marble on a single draw and hence a
# 1-0.25 = 0.75 probability of drawing a white marble; if we see the sequence
# above (blue, white, blue), then the corresponding probabilities are 0.25,
# 0.75, and 0.25 and since the draws are independent, the probability of
# seeing this specific sequence is just the product of these probabilities

0.25 * 0.75 * 0.25

# however, there are other sequences that lead to two blue marbles in three
# draws, namely (white, blue, blue) and (blue, blue, white) and these
# sequences have the following probabilities

0.75 * 0.25 * 0.25
0.25 * 0.25 * 0.75

# there are 3 sequences with 2 blue marbles and so the probability of seeing
# any one of these three sequences is just the sum of their probabilities

0.25 * 0.75 * 0.25 + 0.75 * 0.25 * 0.25 + 0.25 * 0.25 * 0.75

# this is in fact what is computed by the equation on page 33

factorial(2 + 1) / (factorial(2)*factorial(1)) * 0.25^2 * (1-0.25)^1

# which is what dbinom() computes

dbinom(2, size=3, prob=1/4)

# now we can already introduce the concept of 'likelihood' (which formally
# appears in section 2.3); the data we have (namely seeing 2 blue marbles in
# the 3 draws) are now given, but we do not know the value of p (the true
# probability of drawing a blue marble on a single trial); let's compute the
# probability of seeing the data we have under the 5 possible conjectures

like <- dbinom(2, size=3, prob=c(0, 1/4, 2/4, 3/4, 4/4))
like

# when the data are given and we compute these probabilities under different
# values of the unknown parameter p, then these values are 'likelihoods'; they
# denote how likely the observed data are under the 5 different conjectures;
# we do not typically care about the absolute values, but only their relative
# sizes, which we can compute by dividing each likelihood value by their sum

like <- like / sum(like)
like

# note that these are exactly identical to the 'plausibilities' we computed
# earlier by going through the garden of forking data

# now the one piece still missing is the prior plausibilities of the different
# conjectures; we just multiply the likelihoods by these prior plausibilities
# to get the posterior plausibilities; for example, if we assume they are all
# equally likely a priori (i.e., 1/5th), then we get these values

like * c(1/5, 1/5, 1/5, 1/5, 1/5)

# on the other hand, if we know a priori the information that is given at the
# bottom of page 25, then we would get these values

like * c(0, 1, 2, 3, 0)

# note: the prior plausibilities do not have to add up to 1, since in the end,
# we just care about the relative sizes of the posterior plausibilities

# however, if we want to think of the posterior plausibilities as
# probabilities, then we need to rescale them in the end so they sum up to 1

post <- like * c(0, 1, 2, 3, 0)
post <- post / sum(post)
post

### 2.2: Building a model

# we will do what is shown in this section in the following part

### 2.3: Components of the model

# now let's do these computations in the context of the globe throwing
# example, where we see 6 W's in 9 trials; if the true probability of seeing a
# W on a single trial is 0.5, then the probability of seeing these data is

dbinom(6, size=9, prob=0.5)

# now again, since the true probability (p) is unknown, we can compute this
# probability for every possible value of p between 0 and 1 and then create a
# plot of this 'likelihood function'

ps <- seq(0, 1, length=1000)
ls <- dbinom(6, size=9, prob=ps)
plot(ps, ls, type="l", lwd=3, bty="l", xlab="True Probability", ylab="Likelihood")

# find the value of p that is most likely given the data

ps[which.max(ls)]

# sidenote: this is the maximum likelihood estimator, which is simply equal to
# the number of W's divided by the number of trials

6 / 9

# now we introduce the prior for p; we just need to multiply the likelihood
# values with the prior values, and *boom*, we have the posterior values

# for example, we could assume that every value of p is equally plausible to
# begin with; then the posterior values are essentially just the likelihood
# values, just rescaled

post <- ls * 1/1000
post

# again, to get posterior probabilities, we rescale them so they add up to 1

post <- post / sum(post)
post

# which we can plot again to see the posterior distribution

plot(ps, post, type="l", lwd=3, bty="l", xlab="True Probability",
     ylab="Posterior Probability")

# here, we have gone straight from the flat/uniform prior plus the data to the
# posterior as shown at the bottom right of Figure 2.5; but now let's recreate
# the entire figure, one data point at a time

obs <- c("W", "L", "W", "W", "W", "L", "W", "L", "W")
prior <- rep(1/1000, 1000)

par(mfrow=c(3,3))

for (i in 1:9) {

   ls <- dbinom(obs[i] == "W", 1, prob=ps)
   post <- ls * prior
   post <- post / sum(post)

   plot(ps, post, type="l", lwd=3, xlab="True Probability",
        ylab="Posterior Probability", ylim=c(0,.003))
   lines(ps, prior, lty="dashed")
   text(0, .0028, paste0("n = ", i), pos=4)

   prior <- post

}

############################################################################

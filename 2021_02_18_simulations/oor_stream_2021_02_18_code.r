############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-02-18
#
# Topic(s):
# - simulating data
# - simulation-based power analysis
# - other types of simulation studies
#
# last updated: 2021-02-18

############################################################################

# simulate 100 values from a normal distribution with true mean = 0 and true SD = 1
# https://en.wikipedia.org/wiki/Normal_distribution
# note: mean=0, sd=1 are the defaults, so could be left out
x <- rnorm(100, mean=0, sd=1)

# observed mean and SD
mean(x)
sd(x)

# histogram of the values
hist(x)

# simulate 100 values from a chi-square distribution with 5 degrees of freedom
# https://en.wikipedia.org/wiki/Chi-square_distribution
x <- rchisq(100, df=5)
hist(x)

# simulate 100 values from a Bernoulli distribution (e.g., flip a coin once)
# https://en.wikipedia.org/wiki/Bernoulli_distribution
x <- rbinom(100, size=1, prob=0.5)
x
table(x)

# simulate 100 values from a binomial distribution (e.g., flip a coin 5 times)
# https://en.wikipedia.org/wiki/Binomial_distribution
x <- rbinom(100, size=5, prob=0.5)
x
table(x)

# as 'size' gets large, a binomial distribution looks more and more like a normal distribution
x <- rbinom(10000, size=50, prob=0.5)
hist(x, breaks=30)

# gamma and beta distributions
# https://en.wikipedia.org/wiki/Gamma_distribution
# https://en.wikipedia.org/wiki/Beta_distribution
x <- rgamma(100, shape=1, scale=5)
hist(x)
x <- rbeta(100, shape1=3, shape2=3)
hist(x)

# look at help file for the distributions in the stats package
help(distributions)

# repeatedly generate 10 values from a standard normal distribution
rnorm(10)
rnorm(10)
rnorm(10)

# set seed of the random number generator; then things become reproducible
set.seed(1234)
rnorm(10)
rnorm(10)
rnorm(10)
set.seed(1234)
rnorm(10)
rnorm(10)
rnorm(10)

############################################################################

# an example of a two-independent samples t-test
# H0: mu1 = mu2 (the true means are the same in the two groups)
x1 <- c(2,4,2,3,4,5,2,3,4)
x2 <- c(3,2,4,5,6,5,4,6,5,3,4,7,6)
t.test(x1, x2, var.equal = TRUE)

# now let's do a simulation to examine the power of the t-test when
# n1 = n2 = 20
# mu1 = 15, mu2 = 20
# sigma = 5

iters <- 10000

pval <- rep(NA, iters)

for (i in 1:iters) {

   x1 <- rnorm(70, mean=0, sd=1)
   x2 <- rnorm(70, mean=0.5, sd=1)
   res <- t.test(x1, x2, var.equal = TRUE)
   pval[i] <- res$p.value

}

round(mean(pval <= .05), digits=2)

# this gives the 'empirical rejection rate'

# strictly speaking, don't need to run a simulation; for such simple cases, we
# have equations to figure the exact power; for example:
power.t.test(n=70, delta=0.5, sd=1)

# can also use this function to determine n for a desired power
power.t.test(delta=0.5, sd=1, power=.80)

# but this function cannot do power calculations for Welch's t-test

# with our simulation approach, we can!

iters <- 10000

pval <- rep(NA, iters)

for (i in 1:iters) {

   x1 <- rnorm(70, mean=0, sd=1)
   x2 <- rnorm(70, mean=0.5, sd=1)
   res <- t.test(x1, x2, var.equal = FALSE)
   pval[i] <- res$p.value

}

round(mean(pval <= .05), digits=2)

# apparently, we end up with essentially the same power for this scenario

############################################################################

# now, let's do a simulation study to examine how robust Student's t-test
# is to violations of the assumption of equal variances in the two groups

iters <- 10000

pval.s <- rep(NA, iters)
pval.w <- rep(NA, iters)

for (i in 1:iters) {

   x1 <- rnorm(30, mean=0, sd=5)
   x2 <- rnorm(70, mean=0, sd=1)
   res <- t.test(x1, x2, var.equal = TRUE)
   pval.s[i] <- res$p.value
   res <- t.test(x1, x2, var.equal = FALSE)
   pval.w[i] <- res$p.value

}

round(mean(pval.s <= .05), digits=2)
round(mean(pval.w <= .05), digits=2)

############################################################################

# a more complex case of a simulation to determine power

# n = 100 subjects, half in group 0, the other half in group 1, are
# measured tp = 3 times

n <- 100
tp <- 3

dat <- data.frame(id = rep(1:n, each=tp), time = 1:tp, grp = rep(c(0,1), each=n/2*tp))
dat
dat$ui <- rep(rnorm(n, mean=0, sd=1), each=tp)
dat
dat$eij <- rnorm(n*tp, mean=0, sd=1)
dat
dat$yij <- dat$grp * dat$time * 0.4 + dat$ui + dat$eij
dat

# in essence, we are assuming this for the average in the two groups over time

# |            -----  }
# |       -----       } group = 1
# |  -----            }
# |
# |  ---------------- } group = 0
# |
# +------------------
#   time1 time2 time3

# turn id, grp, and time into factors

dat$id   <- factor(dat$id)
dat$grp  <- factor(dat$grp)
dat$time <- factor(dat$time)

# analyze these data with a repeated-measures ANOVA
# one between-subjects factor (grp) and one within-subjects factor (time)

res <- aov(yij ~ time + grp + time:grp + Error(id/time), data=dat)
summary(res)

# analyze these data with a mixed-effects / multilevel model (using a
# relatively simle 'random intercepts' model)

library(nlme)

res <- lme(yij ~ time + grp + time:grp, random = ~ 1 | id, data=dat)
summary(res)
anova(res) # p-value for the interaction is exactly the same as with aov()

# now let's do a simulation to examine the power to detect the interaction

iters <- 1000

n <- 120
tp <- 3

dat <- data.frame(id = rep(1:n, each=tp), time = 1:tp, grp = rep(c(0,1), each=n/2*tp))

dat$id.fac   <- factor(dat$id)
dat$grp.fac  <- factor(dat$grp)
dat$time.fac <- factor(dat$time)

pval <- rep(NA, iters)

for (i in 1:iters) {

   dat$ui  <- rep(rnorm(n, mean=0, sd=1), each=tp)
   dat$eij <- rnorm(n*tp, mean=0, sd=1)
   dat$yij <- dat$grp * dat$time * 0.4 + dat$ui + dat$eij

   res <- aov(yij ~ time.fac + grp.fac + time.fac:grp.fac + Error(id.fac/time.fac), data=dat)
   pval[i] <- summary(res)[[2]][[1]][["Pr(>F)"]][2]

}

# empirical rejection rate for the grp:time interaction

round(mean(pval <= .05), digits=2)

############################################################################

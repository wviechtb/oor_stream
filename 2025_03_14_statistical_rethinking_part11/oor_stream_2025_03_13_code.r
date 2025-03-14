############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-13
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 5.2 - 5.3
#
# last updated: 2025-03-14

############################################################################

# load the rethinking package
library(rethinking)

### 5.2: Masked relationship

# get the milk dataset and put it into 'dat'
dat <- get(data(milk))

# inspect the first 6 rows
head(dat)

# standardize the three variables of interest
dat$K <- c(scale(dat$kcal.per.g))
dat$N <- c(scale(dat$neocortex.perc))
dat$M <- c(scale(log(dat$mass)))

# plot K (on the y-axis) versus N (on the x-axis)
plot(K ~ N, data=dat, pch=21, bg="gray", bty="l",
     xlab="Neocortext Percent (std)", ylab="Kilocal per g (std)")

# model predicting K from N
model1 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bN*N,
                a ~ dnorm(0, 1),
                bN ~ dnorm(0, 1),
                sigma ~ dexp(1))

# fit the model
res1 <- quap(model1, data=dat)

# get an error message, due to the missing values in neocortex.perc (and hence N)
dat$neocortex.perc

# keep rows where K, N, and M are complete (i.e., not missing)
dat <- dat[complete.cases(dat$K,dat$N,dat$M),]
dat

# fit the model using the complete data
res1 <- quap(model1, data=dat)

# sample 1000 values from the prior distributions
prior <- data.frame(extract.prior(res1))
head(prior)

# Figure 5.8 (left): plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Neocortext Percent (std)",
     ylab="Kilocal per g (std)", bty="l")
invisible(apply(prior[1:50,], 1, function(par) abline(par[1], par[2], lwd=1.5, col="gray30")))

# model predicting K from N, using tighter priors for the intercept and slope
model1 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bN*N,
                a ~ dnorm(0, 0.2),
                bN ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model
res1 <- quap(model1, data=dat)

# sample 1000 values from the prior distributions
prior <- data.frame(extract.prior(res1))
head(prior)

# Figure 5.8 (right): plot 50 of the regression lines based on the sampled values
plot(NA, xlim=c(-2,2), ylim=c(-2,2), xlab="Neocortext Percent (std)",
     ylab="Kilocal per g (std)", bty="l")
invisible(apply(prior[1:50,], 1, function(par) abline(par[1], par[2], lwd=1.5, col="gray30")))

# examine summary statistics for the posterior distributions
precis(res1, prob=0.95)

# Figure 5.9 (top left): plot the data and add the regression line (plus 95% CI)
plot(K ~ N, data=dat, pch=21, bg="gray", bty="l",
     xlab="Neocortext Percent (std)", ylab="Kilocal per g (std)")
xseq <- seq(from=min(dat$N)-0.15, to=max(dat$N)+0.15, length.out=30)
mu <- link(res1, data=list(N=xseq))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI)
lines(xseq, mu_mean, lwd=2)
shade(mu_pi, xseq)

# model predicting K from M
model2 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bM*M,
                a ~ dnorm(0, 0.2),
                bM ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model
res2 <- quap(model2, data=dat)
precis(res2, prob=0.95)

# Figure 5.9 (top right): plot the data and add the regression line (plus 95% CI)
plot(K ~ M, data=dat, pch=21, bg="gray", bty="l",
     xlab="Log Body Mass (std)", ylab="Kilocal per g (std)")
xseq <- seq(from=min(dat$M)-0.15, to=max(dat$M)+0.15, length.out=30)
mu <- link(res2, data=list(M=xseq))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI)
lines(xseq, mu_mean, lwd=2)
shade(mu_pi, xseq)

# model predicting K from N and M
model3 <- alist(K ~ dnorm(mu, sigma),
                mu <- a + bN*N + bM*M,
                a ~ dnorm(0, 0.2),
                bN ~ dnorm(0, 0.5),
                bM ~ dnorm(0, 0.5),
                sigma ~ dexp(1))

# fit the model
res3 <- quap(model3, data=dat)
precis(res3, prob=0.95)

# plot the slopes for all three models
plot(coeftab(res1, res2, res3), pars=c("bM","bN"))

# scatterplot matrix of the three variables
pairs(~ K + M + N, dat, pch=21, bg="gray")

# load the dagitty package
library(dagitty)

# define the DAG on the left on page 151 and draw it
dag <- dagitty("dag{M -> K <- N; M -> N}")
coordinates(dag) <- list(x=c(M=0, K=1, N=2), y=c(M=0.5, K=1, N=0.5))
drawdag(dag)

# get the Markov equivalence set for this DAG and draw them all
melist <- equivalentDAGs(dag)
drawdag(melist)

# close the plotting device (to use default settings for the next plot)
graphics.off()

# Figure 5.9 (bottom left): counterfactual plot for manipulated N where M = 0
xseq <- seq(from=min(dat$N)-0.25, to=max(dat$N)+0.25, length.out=30)
mu <- link(res3, data=data.frame(N=xseq, M=0))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI)
plot(NA, xlim=range(dat$M), ylim=range(dat$K), bty="l",
     xlab="Neocortext Percent (std)", ylab="Kilocal per g (std)")
lines(xseq, mu_mean, lwd=2)
shade(mu_pi, xseq)
title("Counterfactual plot holding M = 0")

# Figure 5.9 (bottom right): counterfactual plot for manipulated M where N = 0
xseq <- seq(from=min(dat$M)-0.25, to=max(dat$M)+0.25, length.out=30)
mu <- link(res3, data=data.frame(M=xseq, N=0))
mu_mean <- apply(mu, 2, mean)
mu_pi <- apply(mu, 2, PI)
plot(NA, xlim=range(dat$M), ylim=range(dat$K), bty="l",
     xlab="Log Body Mass (std)", ylab="Kilocal per g (std)")
lines(xseq, mu_mean, lwd=2)
shade(mu_pi, xseq)
title("Counterfactual plot holding N = 0")

############################################################################

### 5.3: Categorical variables

## 5.3.1: Binary categories

# get the Howell1 dataset and put it into 'dat'
dat <- get(data(Howell1))

# inspect the first 6 rows
head(dat)

# given the priors specified on page 154, simulate 10,000 mean height values
# of females and males
mu_female <- rnorm(1e4, mean=178, sd=20)
mu_male   <- rnorm(1e4, mean=178, sd=20) + rnorm(1e4, mean=0, sd=10)
precis(data.frame(mu_female, mu_male), prob=0.95)

# so we see more uncertainty in the prior means for the males compared to the
# females; to avoid this, we use an index variable for the two groups, which
# we can make use of when specifying the model
#dat$sex <- ifelse(dat$male==1, 2, 1)
dat$sex <- dat$male + 1 # simpler
head(dat)

# now specify the model where we use this index variable to denote two
# different model coefficients as the means for the two groups
model <- alist(height ~ dnorm(mu, sigma),
               mu <- a[sex],
               a[sex] ~ dnorm(178, 20),
               sigma ~ dunif(0, 50))
res <- quap(model, data=dat)
precis(res, depth=2, prob=0.95)

# extract samples from the posterior distributions
post <- data.frame(extract.samples(res))
head(post)

# compute the difference between the sampled values for the two model
# coefficients, add this to 'post', and obtain summary statatistics
post$diff_fm <- post$a.1 - post$a.2
precis(post, prob=0.95)

## 5.3.2: Many categories

# get the milk dataset and put it into 'dat'
dat <- get(data(milk))

# standardize the kilocal variable
dat$K <- c(scale(dat$kcal.per.g))

# examine the levels of the 'clade' factor
levels(dat$clade)

# create indices for the levels of the factor
dat$clade_id <- as.integer(dat$clade)
head(dat)

# specify the model where each clade level has its own mean (with the same
# prior), fit the model, and obtain summary statistics for the posterior
# distributions
model1 <- alist(K ~ dnorm(mu, sigma),
                mu <- a[clade_id],
                a[clade_id] ~ dnorm(0, 0.5),
                sigma ~ dexp(1))
res1 <- quap(model1, data=dat)
precis(res1, depth=2, prob=0.95)

# plot the coefficients for the means
labels <- paste0("a[" , 1:4 , "]: " , levels(dat$clade))
plot(precis(res1, depth=2, pars="a"), labels=labels, xlab="expected kcal (std)")

# create some random categorical variable
set.seed(63)
dat$house_id <- sample(1:4, replace=TRUE, size=nrow(dat))

# fit the model with two categorical predictors and obtain summary statistics
model2 <- alist(K ~ dnorm(mu, sigma),
                mu <- a[clade_id] + h[house_id],
                a[clade_id] ~ dnorm(0, 0.5),
                h[house_id] ~ dnorm(0, 0.5),
                sigma ~ dexp(1))
res2 <- quap(model2, data=dat)
precis(res2, depth=2, prob=0.95)

# note: since the code to simulate the house_id variable was slightly changed,
# we do not see h[4] with a larger coefficient compared to the rest (actually
# h[3] is relatively large), but this is not important here anyway, since none
# of the coefficients for h should have a large mean anyway

############################################################################

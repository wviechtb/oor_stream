############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-03-11
#
# Topic(s):
# - bootstrapping
# - permutation testing
#
# last updated: 2021-03-11

############################################################################

# read in data
dat <- read.delim("data_survey_edit.txt")

# look at first 6 rows of dataset
head(dat)

# number of rows
nrow(dat)

# to make things a bit easier below, keep only participants with complete data
# on the variables age, sex, pss, rses, and lotr
dat <- dat[complete.cases(dat[c("age", "sex", "pss", "rses", "lotr")]),]

# so we lose 5 participants
nrow(dat)

# mean stress level for female versus male participants
by(dat$pss, dat$sex, mean)

# mean difference
mean(dat$pss[dat$sex == "female"]) - mean(dat$pss[dat$sex == "male"])

# t-test of H0: true mean difference is 0
# https://en.wikipedia.org/wiki/T-test#Equal_or_unequal_sample_sizes,_similar_variances_(1/2_%3C_sX1/sX2_%3C_2)
t.test(pss ~ sex, data=dat, var.equal=TRUE)

############################################################################

# bootstrapping: https://en.wikipedia.org/wiki/Bootstrapping_(statistics)

# bootstrap the mean difference

set.seed(1234)

iters <- 10000
diffs <- rep(NA, iters)
n <- nrow(dat)

for (i in 1:iters) {

   ids <- sample(n, replace=TRUE)
   sub <- dat[ids,]
   diffs[i] <- mean(sub$pss[sub$sex == "female"]) - mean(sub$pss[sub$sex == "male"])

}

hist(diffs, breaks=50)
abline(v=mean(dat$pss[dat$sex == "female"]) - mean(dat$pss[dat$sex == "male"]), lwd=5)
mean(diffs)

ci <- quantile(diffs, c(.025, .975))
ci

abline(v=ci, lwd=3)

# histogram with kernel density estimate of bootstrap distribution superimposed

hist(diffs, breaks=50, freq=FALSE)
lines(density(diffs), lwd=5)

# let's use the boot package for this

library(boot)

bootfun <- function(data, ids) {

   sub <- data[ids,]
   mean(sub$pss[sub$sex == "female"]) - mean(sub$pss[sub$sex == "male"])

}

set.seed(1234)

sav <- boot(dat, statistic=bootfun, R=10000)
sav

hist(sav$t, breaks=50)
abline(v=sav$t0, lwd=5)

cis <- boot.ci(sav)
cis

abline(v=cis$bca[4:5], lwd=3)

# note: not exactly the same, but we can ignore the difference
cis$percent[4:5]
quantile(sav$t, c(.025, .975))

# or we can get the same percentile CI with this
quantile(sav$t, c(.025, .975), type=6)

############################################################################

# bootstrap CI for the standardized mean difference

bootfun <- function(data, ids) {

   sub <- data[ids,]
   pss.f <- sub$pss[sub$sex == "female"]
   pss.m <- sub$pss[sub$sex == "male"]
   varp <- ((length(pss.f)-1)*sd(pss.f)^2 + (length(pss.m)-1)*sd(pss.m)^2) / (nrow(sub) - 2)
   (mean(pss.f) - mean(pss.m)) / sqrt(varp)

}

set.seed(1234)

sav <- boot(dat, statistic=bootfun, R=10000)
sav

hist(sav$t, breaks=50)
abline(v=sav$t0, lwd=5)

cis <- boot.ci(sav)
cis

abline(v=cis$bca[4:5], lwd=3)

############################################################################

# bootstrap CI for cor(pss,rses) - cor(pss,lotr)

cor(dat$pss, dat$rses)
cor(dat$pss, dat$lotr)
cor(dat$pss, dat$rses) - cor(dat$pss, dat$lotr)

bootfun <- function(data, ids) {
   sub <- data[ids,]
   cor(sub$pss, sub$rses) - cor(sub$pss, sub$lotr)
}

set.seed(1234)

sav <- boot(dat, statistic=bootfun, R=10000)
sav

hist(sav$t, breaks=50)
abline(v=sav$t0, lwd=5)

cis <- boot.ci(sav)
cis

abline(v=cis$bca[4:5], lwd=3)

############################################################################

# permutation testing: https://en.wikipedia.org/wiki/Resampling_(statistics)#Permutation_tests

set.seed(1234)

iters <- 10000
diffs <- rep(NA, iters)

for (i in 1:iters) {

   sub <- dat
   sub$sex <- sample(sub$sex)
   diffs[i] <- mean(sub$pss[sub$sex == "female"]) - mean(sub$pss[sub$sex == "male"])

}

hist(diffs, breaks=50)
abline(v=0, lwd=5)
obsdiff <- mean(dat$pss[dat$sex == "female"]) - mean(dat$pss[dat$sex == "male"])
abline(v=obsdiff, lwd=3)

mean(diffs >= obsdiff)
mean(abs(diffs) >= abs(obsdiff))

# compare with t-test
t.test(pss ~ sex, data=dat, var.equal=TRUE)

############################################################################

# permutation testing in the context of a regression model

res <- lm(pss ~ age + sex + rses, data=dat)
summary(res)

set.seed(1234)

iters <- 10000
coefs <- matrix(NA, nrow=iters, ncol=4)

for (i in 1:iters) {

   sub <- dat
   sub$pss <- sample(sub$pss)
   tmp <- lm(pss ~ age + sex + rses, data=sub)
   coefs[i,] <- coef(tmp)

}

pairs(coefs, pch=16, cex=0.5)

pairs(coefs, pch=16, cex=0.5, col=rgb(0,0,0,.05))

# put histograms on the diagonal
panel.hist <- function(x, ...) {
   usr <- par("usr"); on.exit(par(usr))
   par(usr = c(usr[1:2], 0, 1.5))
   h <- hist(x, plot = FALSE, breaks = 50)
   breaks <- h$breaks; nB <- length(breaks)
   y <- h$counts; y <- y/max(y)
   rect(breaks[-nB], 0, breaks[-1], y, col = "lightgray")
}

pairs(coefs, pch=20, cex=0.5, col=rgb(0,0,0,.05), diag.panel=panel.hist)

mean(abs(coefs[,1]) >= abs(coef(res)[1]))
mean(abs(coefs[,2]) >= abs(coef(res)[2]))
mean(abs(coefs[,3]) >= abs(coef(res)[3]))
mean(abs(coefs[,4]) >= abs(coef(res)[4]))

# compare with summary(lm())
summary(res)

# to avoid a p-value of 0, can do a +1 correction
(sum(abs(coefs[,4]) >= abs(coef(res)[4])) + 1) / (iters + 1)

# a CI for R^2 using bootstrapping

bootfun <- function(data, ids) {

   sub <- data[ids,]
   tmp <- lm(pss ~ age + sex + rses, data=sub)
   summary(tmp)$r.squared

}

set.seed(1234)

sav <- boot(dat, statistic=bootfun, R=10000)
sav

hist(sav$t, breaks=50)
abline(v=sav$t0, lwd=5)

cis <- boot.ci(sav)
cis

abline(v=cis$bca[4:5], lwd=3)

# to see a skewed bootstrap distribution, rerun the above but remove rses from
# the model; then R^2 is quite small (~4%) and the bootstrap distribution is
# quite right-skewed

############################################################################

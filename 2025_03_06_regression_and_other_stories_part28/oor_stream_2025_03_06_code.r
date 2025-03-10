############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-06
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 12.5 - 12.6
#
# last updated: 2025-03-10

############################################################################

# load the rstanarm package
library(rstanarm)

############################################################################

### 12.5: Other transformations

## Square root transformations

# download the dataset if it doesn't already exist
if (!file.exists("earnings.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv", destfile="earnings.csv")

# read in the dataset
dat <- read.csv("earnings.csv")

# inspect the first six rows of the dataset
head(dat)

# fit a model predicting sqrt(earnings) from height
res <- stan_glm(sqrt(earn) ~ height, data=dat, refresh=0)
res

# plot of earnings versus height
plot(earn ~ jitter(height, amount=0.2), data=dat, pch=19, cex=0.3,
     xlab="height", ylab="earnings", bty="l", ylim=c(0,200000))

# extract the sampled values for the posterior distributions
post <- as.data.frame(res)
head(post)

# add 10 regression lines based on these posterior samples (squaring the
# predicted sqrt(earn) values, so they can be added to the plot above)
xs <- seq(min(dat$height), max(dat$height), length.out=1000)
apply(post[1:10,], 1, function(b) lines(xs, (b[1] + b[2]*xs)^2))

## Using discrete rather than continuous predictors

# download the dataset if it doesn't already exist
if (!file.exists("kidiq.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/KidIQ/data/kidiq.csv", destfile="kidiq.csv")

# read in the data and inspect the first 6 rows
dat <- read.csv("kidiq.csv")
head(dat)

# frequency table for the mom_work variable
table(dat$mom_work)

# model predicting the kids' test score from mom_work treated as a categorical variable
res <- stan_glm(kid_score ~ factor(mom_work), data=dat, refresh=0)
res

# - intercept: the estimated average test score for kids where mom_work = 1
#   (i.e., the reference level or baseline category)
# - factor(mom_work)j: the difference between the estimated average test score
#   for kids where mom_work = j versus where mom_work = 1

## Index and indicator variables

# download the dataset if it doesn't already exist
if (!file.exists("naes04.csv")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Gay/data/naes04.csv", destfile="naes04.csv")

# read in the dataset
dat <- read.csv("naes04.csv")

# inspect the first six rows of the dataset
head(dat)

# assign the age of 91 to all with age >= 91
dat$age[dat$age >= 91] <- 91

# create a frequency table of support versus age
tab <- table(dat$age, dat$gayFavorStateMarriage)
head(tab)

# turn the frequencies into percentages
tab <- prop.table(tab, margin=1) * 100
head(tab)

# add age as a proper variable to the table
tab <- cbind(tab, age=as.numeric(rownames(tab)))
head(tab)

# turn tab into a data frame
tab <- data.frame(tab)
head(tab)

# Figure 12.7 (left): plot of gayFavorStateMarriage versus age
plot(Yes ~ age, data=tab, pch=21, bg="gray", bty="l", ylim=c(0,60), las=1,
     xlab="Age", ylab="Support for same-sex marriage (%)")

# fit the model (using the aggregated data)
res1 <- stan_glm(Yes ~ age, data=tab, refresh=0)
res1

# add the regression line to the plot
abline(res1, lwd=3)

# compute the (median) R^2
median(bayes_R2(res1))

# discretize the age variable into 7 groups
breaks <- c(0, seq(29, 79, 10), 100)
tab$age_discrete <- cut(tab$age, breaks = breaks)
head(tab, 20)

# note (0,29] means age > 0 to age <= 29, so (29,39] is the group of people
# who are 30 to 39 years of age (since age is measured in integers)

# fit the model using the categorical age variable
res2 <- stan_glm(Yes ~ age_discrete, data=tab, refresh=0)
res2

# Figure 12.7 (right): plot of gayFavorStateMarriage versus age
plot(Yes ~ age, data=tab, pch=21, bg="gray", bty="l", ylim=c(0,60), las=1,
     xlab="Age", ylab="Support for same-sex marriage (%)")

# add the regression line segments based on the model to the plot
for (i in 2:length(breaks)) {
   pred <- coef(res2)[1] + ifelse(i >= 3, coef(res2)[i-1], 0)
   segments(breaks[i-1], pred, breaks[i], pred, lwd=3)
}

## Indicator variables, identifiability, and the baseline condition

# fit the same model but exclude the intercept / constant term
res3 <- stan_glm(Yes ~ 0 + age_discrete, data=tab, refresh=0)
res3

# in this case, each coefficient is the estimated mean (percentage of support)
# for a particular age category

############################################################################

### 12.6: Building and comparing regression models for prediction

# download the dataset if it doesn't already exist
if (!file.exists("mesquite.dat")) download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/refs/heads/master/Mesquite/data/mesquite.dat", destfile="mesquite.dat")

# read in the data and inspect the first 6 rows
dat <- read.table("mesquite.dat", header=TRUE)
head(dat)

# the data give the weight (plant biomass of the leaves) for 46 mesquite
# (https://en.wikipedia.org/wiki/Mesquite) bushes; the goal is to predict the
# weight from measurements taken before the harvesting took place

# fit the model where we use all variables as predictors of weight
res1 <- stan_glm(weight ~ diam1 + diam2 + canopy_height + total_height + density + group, data=dat, refresh=0)
res1

# do leave-one-out cross-validation for this model
loo1 <- loo(res1)
loo1

# get a warning message that the results cannot be trusted

# do 10-fold cross-validation for this model
kfold1 <- kfold(res1, K=10)
kfold1

# obtain summary statistics of the quantitative variables in the dataset
round(t(apply(dat[3:8], 2, function(x) c(summary(x), IQR=IQR(x)))), 1)

# fit the model where we use 5 random noise predictors
set.seed(1234)
dat$X <- replicate(5, rnorm(nrow(dat)))
res1b <- stan_glm(weight ~ X, data=dat, refresh=0)
res1b

# do 10-fold cross-validation for this model
kfold1b <- kfold(res1b, K=10)
kfold1b

# use loo_compare() to compare the fit of the two models above
loo_compare(kfold1, kfold1b)

# not surprisingly, the model with noise predictors is much worse in terms of
# its performance than the model with the actual predictors

# remove the noise predictors from the dataset
dat$X <- NULL

# fit the model where all variables are log transformed (except for the group dummy)
res2 <- stan_glm(log(weight) ~ log(diam1) + log(diam2) + log(canopy_height) +
                 log(total_height) + log(density) + group, data=dat, refresh=0)
res2

# do leave-one-out cross-validation for this model
loo2 <- loo(res2)

## Using the Jacobian to adjust the predictive comparison after a transformation

# the elpd values from models res1 and res2 cannot be directly compared
# because the outcome variable is different for the two models (one uses raw
# weight, the other uses log-transformed weight)
kfold1
loo2

# we can correct the loo2 results for the transformation that was done on the
# outcome variable as described in the book
loo2_with_jacobian <- loo2
loo2_with_jacobian$pointwise[,1] <- loo2_with_jacobian$pointwise[,1] - log(dat$weight)

# now we can compare the two models (note: loo_compare() still gives a warning
# about the y variable being different for the two models, but we can suppress
# this warning because we have manually fixed the issue)
suppressWarnings(loo_compare(kfold1, loo2_with_jacobian))

# this indicates that the model where predict log(weight) (and use log
# transformed predictors) is preferable

# draw 4000 samples from the posterior distribution of the data for the two models
yrep1 <- posterior_predict(res1)
yrep2 <- posterior_predict(res2)

# Figure 12.8 (left): kernel density plot of the observed weight values versus
# 100 random kernel density lines from the 4000 samples drawn above
plot(density(dat$weight), main="Model for weight", lwd=5, bty="l", xlim=c(-1000,5000))
subset <- sample(nrow(yrep1), 100)
invisible(apply(yrep1[subset,], 1, function(x) lines(density(x), col="gray80")))
lines(density(dat$weight), lwd=5)

# Figure 12.8 (right): kernel density plot of the observed log weight values
# versus 100 random kernel density lines from the 4000 samples drawn above
plot(density(log(dat$weight)), main="Model for log(weight)", lwd=5, bty="l", xlim=c(3,9), ylim=c(0,0.6))
subset <- sample(nrow(yrep2), 100)
invisible(apply(yrep2[subset,], 1, function(x) lines(density(x), col="gray80")))
lines(density(log(dat$weight)), lwd=5)

# extract the sampled values for the posterior distributions of the model parameters
post2 <- as.data.frame(res2)
head(post2)

# Figure 12.9 (left): kernel density estimates based on the samples values of
# the posterior distributions for the regression coefficients
library(bayesplot)
mcmc_areas(post2[2:7])

# Figure 12.9 (right)
plot(post2[["log(canopy_height)"]], post2[["log(total_height)"]], pch=21, cex=0.75,
     bg="gray", bty="l", xlab="coef of log(canopy_height)", ylab="coef of log(total_height)")
abline(h=0)
abline(v=0)

## Constructing a simpler model

# compute the (approximate) volume of the canopy of the bushes
dat$canopy_volume <- with(dat, diam1 * diam2 * canopy_height)

# use (log transformed) canopy volume as the single predictor
res3 <- stan_glm(log(weight) ~ log(canopy_volume), data=dat, refresh=0)
res3

# compare the performance of models res2 and res3
loo3 <- loo(res3)
loo_compare(loo2, loo3)

# while res2 is preferable, there is not much of a difference

# compare the leave-one-out R^2 values
round(median(loo_R2(res2)), 2)
round(median(loo_R2(res3)), 2)

# compute the canopy area and shape variables
dat$canopy_area  <- with(dat, diam1 * diam2)
dat$canopy_shape <- with(dat, diam1 / diam2)

# use volume, area, shape, and the other variables as predictors
res4 <- stan_glm(log(weight) ~ log(canopy_volume) + log(canopy_area) + log(canopy_shape) +
                 log(total_height) + log(density) + group, data=dat, refresh=0)
res4

# compare the performance of models res2 and res4
loo4 <- loo(res4)
loo_compare(loo2, loo4)

# here the difference between the two models is very small

# fit the simplified model
res5 <- stan_glm(log(weight) ~ log(canopy_volume) + log(canopy_shape) + group, data=dat, refresh=0)
res5

# compare the performance of models res4 and res5
loo5 <- loo(res5)
loo_compare(loo4, loo5)

# compute the (approximate) surface area of the triaxial elipsoid formed by
# the two diameter measurements and the canopy height variable; see:
# https://en.wikipedia.org/wiki/Ellipsoid#Approximate_formula
p <- 1.6075
dat$canopy_surface <- with(dat, 4 * pi * (((diam1/2)^p*(diam2/2)^p + (diam1/2)^p*(canopy_height/2)^p + (diam2/2)^p*(canopy_height/2)^p)/3)^(1/p))

# use the surface area instead of the volume of the canopy volume
res6 <- stan_glm(log(weight) ~ log(canopy_surface) + log(canopy_shape) + group, data=dat, refresh=0)
res6

# compare the performance of models res5 and res6
loo6 <- loo(res6)
loo_compare(loo5, loo6)

# this model does better than res5, but the difference isn't large

############################################################################

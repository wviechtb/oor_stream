############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-12-10
#
# Topic(s):
# - mixed-effects / multilevel models (part 1)
#
# last updated: 2020-12-10

############################################################################

# set working directory (in RStudio, 'Session' menu, 'Set Working Directory',
# 'To Source File Location'; then copy-paste setwd() command below)
#setwd("...")

# load the nlme package
library(nlme)

# load the lattice package
library(lattice)

############################################################################

# load the multilevel1.dat data
dat <- read.table("multilevel1.dat", header=TRUE)

# scatterplot of xij versus yij
plot(yij ~ xij, data=dat, pch=19, col="blue", xlim=c(0,10), ylim=c(0,10))

# fit regression model of yij on xij
res <- lm(yij ~ xij, data=dat)
summary(res)

# add regression line to scatterplot
abline(res, lwd=3)

# create grouped data object / data frame
dat <- groupedData(yij ~ xij | class, data=dat)

# create a figure with scatterplots for each class
plot(dat, layout=c(4,4), aspect=1, FUN = mean,
   panel = function(x, y) panel.xyplot(x, y, pch=19, cex=0.5))

# same as before but add regression line within each classroom
plot(dat, layout=c(4,4), aspect=1, FUN = mean,
   panel = function(x, y) {
      panel.xyplot(x, y, pch=19, cex=0.5)
      panel.abline(lm(y~x), lwd=2)})

# fit regression model within each classroom
res <- lmList(dat)
res

# average slope of the 16 classrooms
mean(coef(res)$xij)

# fit mixed-effects model with random intercepts and random slopes
res <- lme(yij ~ 1 + xij, random = ~ 1 + xij | class, data=dat)
summary(res)

# approximate 95% confidence interval for the mean slope
-0.000021 - 1.96*0.0340311
-0.000021 + 1.96*0.0340311

# we are (roughly!) 95% certain that the *mean slope* in the population of
# classrooms (from which these 16 classrooms have come) falls between -0.067
# and +0.067

# estimate an approximate (prediction) interval where 95% of the slopes are
-0.000021 - 1.96*0.01849544
-0.000021 + 1.96*0.01849544

# we estimate that (roughly!) 95% of the slopes in the population of
# classrooms (from which these 16 classrooms have come) fall within the
# interval -0.036 to +0.036

# the prediction interval above does not account for the uncertainty as to
# where the average slope is; we can improve on this by summing up the
# standard error of the average slope (which reflects the uncertainty as to
# where the average slope is) and the estimated standard deviation of the
# slopes (this needs to be done by adding the squared values and then taking
# the square root of that sum)
-0.000021 - 1.96*sqrt(0.0340311^2 + 0.01849544^2)
-0.000021 + 1.96*sqrt(0.0340311^2 + 0.01849544^2)

# we can actually get the CI for the mean intercept and the mean slope using
# the intervals() function
intervals(res, which="fixed")

# fit model with random intercepts (assuming no variance in slopes across
# classrooms; i.e., SD(slope) = 0)
res0 <- lme(yij ~ 1 + xij, random = ~ 1 | class, data=dat)
summary(res0)

# do a model comparison between the model with random slopes versus the model
# that assumes no variance in the slopes (likelihood ratio test)
anova(res, res0)

# this suggests that the model with random slopes does not fit significantly
# better than the model without random slopes

# get the predicted intercept and slope of the 16 classrooms
coef(res)

############################################################################

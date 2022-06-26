############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-12-17
#
# Topic(s):
# - mixed-effects / multilevel models (part 2)
#
# last updated: 2020-12-17

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
plot(yij ~ xij, data=dat, pch=19, col="blue", xlim=c(0,10), ylim=c(0,10), cex=0.5)

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

# load the multilevel2.dat data
dat <- read.table("multilevel2.dat", header=TRUE)

# let's repeat essentially the same things we did earlier

# plot the data
plot(yij ~ xij, data=dat, pch=19, col="blue", xlim=c(0,10), ylim=c(0,10), cex=0.5)

# plot the data including regression lines for each classroom
dat <- groupedData(yij ~ xij | class, data=dat)
plot(dat, layout=c(4,4), aspect=1, FUN = mean,
   panel = function(x, y) {
      panel.xyplot(x, y, pch=19, cex=0.5)
      panel.abline(lm(y~x), lwd=2)})

# fit mixed-effects model with random intercepts and random slopes
res <- lme(yij ~ xij, random = ~ xij | class, data=dat)
summary(res)

## fit mixed-effects model with random intercepts
res0 <- lme(yij ~ xij, random = ~ 1 | class, data=dat)
summary(res0)

# LRT to test whether there is significant variance in the slopes
anova(res, res0)

# get the predicted intercept and slope of the 16 classrooms
coef(res)

# plot the data with the predicted regression lines and regression line
# based on the average intercept and slope ("fixed")
pred <- augPred(res, level=0:1)
plot(pred, layout=c(4,4), aspect=1, FUN = mean, pch=19, cex=0.5, lwd=2)

############################################################################

# decompose the variance of yij into between- and within-group differences

# fit a regression model without any predictor
res <- lm(yij ~ 1, data=dat)
summary(res)

# get the error variance
sigma(res)^2

# this is the same as the variance of the DV
var(dat$yij)

# fit a model without any predictors but with random intercepts
res <- lme(yij ~ 1, random = ~ 1 | class, data=dat)
summary(res)
VarCorr(res)

# the intercept variance reflects variability between classrooms
# the residual  variance reflects variability within  classrooms

# so the total variance is just the sum of these two variances
sum(as.numeric(VarCorr(res)[,1])) # same as 0.3154995 + 1.1541149

# not exactly the same as var(dat$yij) but quite similar

# proportion of the total variance due to differences between classrooms;
# this is also called the 'intraclass correlation coefficient' (ICC)
as.numeric(VarCorr(res)[1,1]) / sum(as.numeric(VarCorr(res)[,1]))

# ICC = between-class variance / (between-class variance + within-class variance)

# proportion of the total variance due to differences within classrooms
as.numeric(VarCorr(res)[2,1]) / sum(as.numeric(VarCorr(res)[,1]))

############################################################################

# computing pseudo R^2 values

res  <- lme(yij ~ xij, random = ~ xij | class, data=dat)
res0 <- lme(yij ~ 1,   random = ~ 1 | class, data=dat)
VarCorr(res)
VarCorr(res0)

# proportional reduction in the estimated error variance when we include xij
# as a predictor in the model (and also allow the association between yij and
# xij to differ across classrooms)
(as.numeric(VarCorr(res0)[2,1]) - as.numeric(VarCorr(res)[3,1])) / as.numeric(VarCorr(res0)[2,1])

# this can be thought of as an R^2-like value (how much of the variance of yij
# within classrooms is accounted for by xij ?)

# but there are other types of R^2 that can be computed in such models

res  <- lme(yij ~ xij, random = ~ xij | class, data=dat)
res0 <- lme(yij ~ 1,   random = ~ xij | class, data=dat)
VarCorr(res)
VarCorr(res0)

# proportional reduction in the slope variance when we allow the average slope
# to be different from zero
(as.numeric(VarCorr(res0)[2,1]) - as.numeric(VarCorr(res)[2,1])) / as.numeric(VarCorr(res0)[2,1])

# this can also be thought of as an R^2-like value (for how much variance in
# slopes is accounted for by allowing the average slope to be non-zero)

############################################################################

# a model that disentangles the between- and within-classroom association
# between xij and yij

head(dat, 30)

# compute the classroom means of xij and add this to the dataset
dat$mi <- ave(dat$xij, dat$class, FUN=mean)

head(dat, 30)

# compute the within-classroom centered xij values
dat$dij <- dat$xij - dat$mi

head(dat, 30)

# fit model with the classroom means and the within-classroom centered xij
# values as predictors
res <- lme(yij ~ mi + dij, random = ~ dij | class, data=dat)
summary(res)

# if we do not differentiate, then the average slope for xij is a mix of the
# between- and within-classroom associations
res  <- lme(yij ~ xij, random = ~ xij | class, data=dat)
summary(res)

############################################################################

# a repeated-measures dataset
Orthodont

# plot data
plot(Orthodont, layout=c(6,5), aspect=1, FUN = mean, pch=19, cex=0.5, lwd=2)

# compute the ICC
res <- lme(distance ~ 1, random = ~ 1 | Subject, data=Orthodont)
as.numeric(VarCorr(res)[1,1]) / sum(as.numeric(VarCorr(res)[,1]))

# fit model with age as predictor (random intercepts and slopes)
res <- lme(distance ~ age, random = ~ age | Subject, data=Orthodont)
summary(res)

# show the predicted regression lines
pred <- augPred(res)
plot(pred, layout=c(6,5), aspect=1, FUN = mean, pch=19, cex=0.5, lwd=2)

# fit model with age, sex, and the interaction as predictors
res <- lme(distance ~ age * Sex, random = ~ age | Subject, data=Orthodont)
summary(res)

# test if the average slope of the female subjects is significantly different
# from zero (test a linear combination of the fixed effects)
anova(res, L=c(0,1,0,1))

# show the predicted regression lines and the line based on the fixed effects
# of the model (fixed)
pred <- augPred(res, level=0:1)
plot(pred, layout=c(6,5), aspect=1, FUN = mean, pch=19, cex=0.5, lwd=2)

############################################################################

# let's compare this to a repeated-measures ANOVA

tmp <- Orthodont
tmp$age <- factor(tmp$age)

res <- aov(distance ~ age * Sex + Error(Subject/age), data=tmp)
summary(res)

res <- lme(distance ~ age * Sex, random = ~ 1 | Subject, data=tmp)
summary(res)
anova(res)

############################################################################

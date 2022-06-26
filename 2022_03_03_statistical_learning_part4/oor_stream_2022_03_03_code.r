############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-03-03
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 2.4 - 3.1
#
# last updated: 2022-03-25

############################################################################

###### 2.4: Exercises

### Question 7

# read in the data
dat <- read.table(header=TRUE, text = "
obs x1 x2 x3 y
1 0 3 0 red
2 2 0 0 red
3 0 1 3 red
4 0 1 2 green
5 -1 0 1 green
6 1 1 1 red")

# new data point for which we do not know y but want to make a prediction for
new <- c(7, 0, 0, 0, NA)

# (a) Compute the Euclidean distance between each observation and the test
#     point, X1 = X2 = X3 = 0.

# we can combine the data with the new data point using rbind() (we only need
# columns 2 through 4)
rbind(dat, new)[2:4]

# get the matrix of all Euclidean distances; the last row of this matrix gives
# the distances of the original data and the new data point
dist(rbind(dat, new)[2:4])

# but we want this as a matrix
as.matrix(dist(rbind(dat, new)[2:4]))

# keep only the last row and remove the last value
d <- as.matrix(dist(rbind(dat, new)[2:4]))[7,-7]

# these are the Euclidian distances
d

# double-check the Euclidean distance for 3 and 7
sqrt((0-0)^2 + (1-0)^2 + (3-0)^2)

# (b) What is our prediction with K = 1? Why?

# the prediction when k = 1 is simply the value of y from the data point that
# has the lowest distance to the new data point

# the data point with the lowest distance is
which.min(d)

# so take the value of y from this
dat$y[which.min(d)]

# (c) What is our prediction with K = 3? Why?

# we need to find the 3 points with the lowest distance and see what value of y
# is most common among those 3 points; we can do this as follows

# order() gives us the ordering permutation; that is the 5th point has the
# lowest distance, the 6th point has the next lowest distance, and so on
order(d)

# so the first three of this as the points with the lowest distance
order(d)[1:3]

# get the value of y for those three points
dat$y[order(d)[1:3]]

# create a frequency table based on these 3 points
table(dat$y[order(d)[1:3]])

# so we see that 'red' is most common among those 3 points, so this is the
# predicted value for the new data point when k=3

# (d) If the Bayes decision boundary in this problem is highly non-linear,
#     then would we expect the best value for K to be large or small? Why?

# k should be low, since this means more flexibility for KNN

######################################

### Question 8

# note: the 'solution' below deviates slightly here and there from what is
# suggested in the book based on my own preferences

# need to get the data from here: https://www.statlearning.com/s/College.csv

# can also use R to download the file
#download.file("https://www.statlearning.com/s/College.csv", destfile="College.csv")

# read in the data and examine the first 6 rows
dat <- read.csv("College.csv")
head(dat)

# look at all rows where variable X includes 'Illinois'
dat[grep("Illinois", dat$X),]

# set the row names equal to variable X and remove the first variable (which is X)
rownames(dat) <- dat[,1]
dat <- dat[,-1]
head(dat)

# some summary statistics on the variables in the dataset
summary(dat)

# for character variables (i.e., Private), create a frequency table
table(dat$Private)

# scatterplot matrix of all variables except 'Private'
pairs(dat[2:11], pch=19, cex=0.1)

# boxplot of the 'Outstate' variable for each level of 'Private'
boxplot(Outstate ~ Private, data=dat)

# create a variable that is equal to 'Yes' when Top10perc > 50 and 'No' otherwise
dat$Elite <- ifelse(dat$Top10perc > 50, "Yes", "No")

# frequency table of this variable
table(dat$Elite)

# boxplot of the 'Outstate' variable for each level of 'Elite'
boxplot(Outstate ~ Elite, data=dat)

# histograms of the 4 variables below in a single figure
numb <- 20
op <- par(mfrow=c(2,2))
hist(dat$S.F.Ratio, main="Student to Faculty Ratio", xlab="", breaks=numb)
hist(dat$Outstate,  main="Out of State Tuition", xlab="", breaks=numb)
hist(dat$Top10perc, main="Top 10% HS Student Applicants", xlab="", breaks=numb)
hist(dat$PhD,       main="Percent of Faculty with PhDs", xlab="", breaks=numb)
par(op)

# calculate the acceptance percentage
dat$AcceptPerc <- 100 * dat$Accept / dat$Apps

# find the 10 universities/colleges with the lowest acceptance percentage
dat[order(dat$AcceptPerc),][1:10,]

# the usual suspects ...

######################################

### Question 9

# skipped this because it is essentially the same as the lab

######################################

### Question 10

# install the ISLR2 package (not 'library' -- in R, a 'library' is a location on
# your computer where packages are installed); note: by putting a # in front of
# the command, we do not accidentally reinstall the package every time we run
# this code (so remove the # once and run the command and then put the # back)
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# to look at the help file for this dataset (same idea with the # as above)
#?Boston

# copy the dataset to 'dat' (since I always name datasets 'dat')
dat <- Boston

# check the dimensions (number of rows and columns/variables)
dim(dat)

# examine the first 6 rows
head(dat)

# scatterplot of 'tax' versys 'ptration'
plot(dat$tax, dat$ptratio, pch=19)

# this doesn't look like 506 points; so there must be many cases where points
# are on top of each other, which we cannot see in this plot

# find all unique combinations of the two variables and how often they occurred
res <- xyTable(dat$tax, dat$ptratio)
res

# use this to create a scatterplot with the points proportional in their area to
# how often they occurred
plot(res$x, res$y, cex=sqrt(res$number/15), pch=19)

#pairs(dat, pch=19, cex=0.1)

# scatterplots of all variables versus 'crim'
op <- par(mfrow=c(4,3), mar=c(5,4,1,1))
for (i in 2:ncol(dat)) {
   plot(dat[[i]], dat$crim, pch=19, cex=0.5, xlab=names(dat)[i], ylab="crime rate")
}
par(op)

# the 10 census tracts with the highest crime rates
dat[order(dat$crim, decreasing=TRUE),][1:10,]

# compare this against summary statistics for the crime rate variable
summary(dat$crim)

# frequency table of the 'chas' variable (so 35 census tracts in this data set
# bound the Charles river)
table(dat$chas)

# median pupil-teacher ratio
median(dat$ptratio)

# the census tract with the lowest median value of owner-occupied homes
dat[order(dat$medv),][1,]

# 64 tracts average more than seven rooms per dwelling
table(dat$rm > 7)

# 13 tracts average more than seven rooms per dwelling
table(dat$rm > 8)

# all tracts that average more than eight rooms per dwelling
dat[dat$rm > 8,]

############################################################################

### 3.1: Simple Linear Regression

# the data used in this section: https://www.statlearning.com/s/Advertising.csv
#download.file("https://www.statlearning.com/s/Advertising.csv", destfile="Advertising.csv")

# read in the data and examine the first 6 rows
dat <- read.csv("Advertising.csv")
head(dat)

# plot the data
plot(sales ~ TV, data=dat, pch=19, col="red3", xlab="TV Advertisement Budget",
     ylab="Sales (in 1000 of Units)")

# fit the simple regression model
res <- lm(sales ~ TV, data=dat)

# add regression line to the plot
abline(res, lwd=6, col="blue")

# examine the model results
summary(res)

# the estimated intercept is 7.032594, the estimated slope is 0.047537;
# so the model says: Y-hat = 7.032594 + 0.047537 * TV

# if we just want the model coefficients, we can use coef()
coef(res)

# the estimates of intercept and slope are those that make the sum of the
# squared residuals (the 'residual sum of squares') as small as possible

# add the residuals to the plot
segments(dat$TV, predict(res), dat$TV, dat$sales, col="gray", lwd=2)
abline(res, lwd=6, col="blue")
points(sales ~ TV, data=dat, pch=19, col="red3")

# for example, the residual for the first data point is as follows
dat$TV[1]                                                    # value of TV
coef(res)[[1]] + coef(res)[[2]] * dat$TV[1]                  # predicted value
dat$sales[1] - (coef(res)[[1]] + coef(res)[[2]] * dat$TV[1]) # residual

# so the squared residual is
(dat$sales[1] - (coef(res)[[1]] + coef(res)[[2]] * dat$TV[1]))^2

# now let's do this for all data points
(dat$sales - (coef(res)[[1]] + coef(res)[[2]] * dat$TV))^2

# the residual sum of squares is just the sum of these squared residuals
sum((dat$sales - (coef(res)[[1]] + coef(res)[[2]] * dat$TV))^2)

# we can just get the residuals directly with resid(), so a more direct way to
# compute the RSS would have been
sum(resid(res)^2)

# the coef(res) values are those that make this as small as possible; for
# example, suppose we would set the estimated intercept to 7.05 and the
# estimated slope to 0.045; then the RSS would have been slightly larger
sum((dat$sales - (7.05 + 0.045 * dat$TV))^2)

# let's calculate the RSS value for different intercept values (between 5 and 9)
# and for different slope values (between .03 and .07)

b0s <- seq(5, 9, length=100)
b1s <- seq(.027, .068, length=100)

RSSmat <- matrix(NA, nrow=length(b0s), ncol=length(b1s))

for (i in 1:length(b0s)) {
   for (j in 1:length(b1s)) {
      RSSmat[i,j] <- sum((dat$sales - (b0s[i] + b1s[j] * dat$TV))^2)
   }
}

# contour plot of the RSS values (like Figure 3.2; apparently, the authors
# divided the RSS values by 1000, so we will also do the same)
RSSmat <- RSSmat / 1000
contour(b0s, b1s, RSSmat, levels=c(2.11,2.15,2.2,2.3,2.5,3), labcex=1, col="blue")
points(coef(res)[1], coef(res)[2], pch=19, col="red", cex=1.5)

# the red point corresponds to the actual estimated intercept and slope values
# that minimize the RSS

# we can also draw the three-dimensional plot
pmat <- persp(b0s, b1s, RSSmat, theta=30, phi=-10, border="blue", col="gray",
              xlab="beta_0", ylab="beta_1", zlab="RSS")
points(trans3d(coef(res)[1], coef(res)[2], min(RSSmat), pmat=pmat), pch=19, col="red", cex=1.5)

############################################################################

# let's simulate some data like the authors did for Figure 3.3
set.seed(1350)
n <- 100
x <- rnorm(n)
y <- 2 + 3 * x + rnorm(n, 0, 2.2)

# plot the data (like Figure 3.3, left panel)
plot(x, y)

# since we simulate the data, we know what the true intercept is (2) and what
# the true slope is (3); let's add the true regression line to the plot
abline(a=2, b=3, col="red", lwd=3)

# fit the regression model and add the estimated regression line to the plot
res <- lm(y ~ x)
summary(res)
abline(res, col="blue", lwd=3)

# the estimated regression line differs from the true regression line because of
# 'sampling error' (i.e., for each training dataset, we get a slightly different
# regression line); see Figure 3.3, right panel)

plot(NA, xlim=c(-2,2), ylim=c(-10,10), xlab="X", ylab="Y")

for (i in 1:10) {
   y <- 2 + 3 * x + rnorm(n, 0, 2.2)
   tmp <- lm(y ~ x)
   abline(tmp, col="lightblue", lwd=2)
}

abline(a=2, b=3, col="red", lwd=3)
abline(res, col="blue", lwd=3)

# let's simulate 10000 new training datasets and compute the estimated intercept
# and slope for each one of them

b0s <- rep(NA, 10000)
b1s <- rep(NA, 10000)

for (i in 1:10000) {
   y <- 2 + 3 * x + rnorm(n, 0, 2.2)
   tmp <- lm(y ~ x)
   b0s[i] <- coef(tmp)[1]
   b1s[i] <- coef(tmp)[2]
}

# draw histograms of the estimated intercepts and slopes; also add the true
# intercept and slope as vertical lines to the plots and the estimated intercept
# and slope based on our single training dataset above as dotted lines
op <- par(mfrow=c(1,2))
hist(b0s, breaks=50)
abline(v=2, lwd=6)
abline(v=coef(res)[1], lwd=6, lty="dotted")
hist(b1s, breaks=50)
abline(v=3, lwd=6)
abline(v=coef(res)[2], lwd=6, lty="dotted")
par(op)

# note that the estimated intercepts and slopes fluctuate randomly around the
# true intercept and slope; these are the 'sampling distributions' of the
# intercept and slope estimates for this particular example

# of course in practice, we do not know the true intercept and slope and we also
# only have a single training dataset; but we know that the estimated intercept
# and slope we observe in our data have come from such sampling distributions
# (as we can see above in the histograms)

# we would like to know how accurate our estimates are; for this, we want to
# know how large the variability in the sampling distributions is; if the
# sampling distributions are really 'wide', then we can't be very confident that
# the estimates we have are close to the true intercept and slope; on the other
# hand, if the sampling distributions are narrow, then the estimates all tend to
# fall close to the true intercept and slope and hence we can be more confident
# that our estimates are also close to the true values

# since we are simulating data, we can actually compute the standard deviation
# of the intercept and slope estimates across the 10000 datasets we simulated
sd(b0s)
sd(b1s)

# these are essentially the true 'standard errors' of the intercept and slope
# estimates (we would have to simulate an infinite number of training datasets
# to really get the true SEs, but this is close enough)

# again, in practice, we cannot compute the true SEs, since we only have a
# single dataset; but based on statistical theory, we can derive equations that
# allow us to estimate the true SEs; this is what you see in the output from our
# model under the 'Std. Error' column
summary(res)

# note that these values are not exactly the same as sd(b0s) and sd(b1s) but
# quite close (so strictly speaking, these values are estimates of the true SEs,
# but in practice, we simply call them 'standard errors' and do not draw the
# distinction between the true SEs and the estimated SEs)

# if n (the sample size) is larger, than the sampling distributions will be
# narrower and hence the SEs will be smaller (you can try this out by setting n
# above to a larger value, say 500, and rerunning the code above)

# using these SEs, we can compute 'confidence intervals' for the intercept and
# slope; as described in the book, a 95% confidence interval has the following
# property: "if we take repeated samples and construct the confidence interval
# for each sample, 95% of the intervals will contain the true unknown value of
# the parameter."

# let's check if this is true with our simulation study; we again repeatedly
# simulate new data, construct the confidence interval for the slope for each
# new sample and then check what proportion of these intervals captured the true
# slope (which we know to be 3)

lbs <- rep(NA, 10000)
ubs <- rep(NA, 10000)

for (i in 1:10000) {
   y <- 2 + 3 * x + rnorm(n, 0, 2.2)
   tmp <- confint(lm(y ~ x))
   lbs[i] <- tmp[2,1]
   ubs[i] <- tmp[2,2]
}

mean(lbs <= 3 & ubs >= 3)

# note that this is very close to .95 (or 95%); it deviates slightly from 95%
# because again, we would have to run this for an infinite number of iterations
# to get exactly 95%, but this is close enough to demonstrate that this really
# works

############################################################################

# let's go back to the advertising data

# fit the simple regression model
res <- lm(sales ~ TV, data=dat)
summary(res)

# so 0.457843 is the SE of the intercept and 0.002691 the SE of the slope

# how are the confidence intervals actually computed? roughly with this equation:
# estimate +- 2 * SE, so for example, for the slope, we would compute
0.047537 + c(-1,1) * 2 * 0.002691

# this isn't completely accurate (the value 2 in the equation above is an
# approximation), but we can use the confint() function to compute the exact
# confidence interval (for both the intercept and the slope)
confint(res)

# we can also think of a confidence interval (CI) as a range of values that are,
# in some sense, 'compatible' with our data; so if the 95% CI includes 0, then
# even no relationship between the predictor and outcome (which is what a slope
# of 0 implies) is also compatible with our data and we therefore shouldn't be
# too confident that there really is a relationship between the predictor and
# outcome; but in this dataset, the 95% CI for the slope excludes 0 (all values
# in the CI are positive), so a slope of 0 would not be compatible with our data

# another approach is to conduct a null hypothesis test; the idea is this:
# assume that there really is no relationship between the predictor and the
# outcome (i.e., the true slope is 0); this is our 'null hypothesis'
#
# H_0: beta_1 = 0
#
# and we want to test this against the alternative that there really is a
# relationship (i.e., the true slope is not 0); this is our 'alternative
# hypothesis' (often this is denoted H_1 instead of H_a as in the book)
#
# H_a: beta_1 != 0

# in the output, we see a 't value' for the slope (17.67); this is the 'test
# statistic for testing the null hypothesis; it is computed by dividing the
# estimate of the slope by its SE, so we can think of the test statistic as a
# 'signal to noise ratio', so the test statistic will be large when the
# estimated slope is far from 0 and/or when the SE is small (i.e., when we have
# a precise estimate of the true slope)
summary(res)

# next to the test statistic is the 'p-value' (Pr(>|t|) in the output); in this
# case, the p-value is very very small (< 2 * 10^-16)

# the p-value is the probability of seeing a test statistic as large as we see
# in our data, or an even larger one, assuming the null hypothesis is true (and
# under the 'assumptions of the model', but the book hasn't really discussed
# these assumptions so far; I am just noting this here already for preciseness)

# so a small p-value indicates that it is very unlikely that we would see such a
# large slope (or an even larger one) when we would assume that there really is
# no relationship between the predictor and the outcome

# in this case, we 'reject the null hypothesis'; how small does the p-value have
# to be before we do so? by convention, we reject H_0 when the p-value is <= .05
# although it should be emphasized that this is purely an arbitrary convention

# so in this example, we reject the null hypothesis that there is no
# relationship between TV advertising budgets and sales (and therefore conclude
# that there is a relationship)

# of course that conclusion could be right or wrong; since we do not know the
# truth (i.e., the true slope), we don't know if we have drawn the right
# conclusion

# a very nice article that discusses statistical tests, p-values, and confidence
# intervals is:

# Greenland, S., Senn, S. J., Rothman, K. J., Carlin, J. B., Poole, C., Goodman,
# S. N. & Altman, D. G. (2016). Statistical tests, P values, confidence
# intervals, and power: A guide to misinterpretations. European Journal of
# Epidemiology, 31(4), 337-350. https://doi.org/10.1007/s10654-016-0149-3

############################################################################

# concluding that there is a relationship between X and Y does not tell us how
# accurate our model is in predicting Y from X; to assess this, we can compute
# the 'residual standard error', which is in essence the standard deviation of
# the residuals
sd(resid(res))

# in the output, we see that the residual standard error is slightly different
summary(res)

# the actual equation (3.15) divides the RSS by n-2, while sd() above divides
# the RSS by n-1; we can correct this to get the exact same value
n <- 200
sd(resid(res)) * sqrt((n-1) / (n-2))

# another way to assess the accuracy of the model is to compute R^2 (we also see
# this value in the output above); R^2 indicates how much of the variance in the
# outcome variable is 'accounted for' (or 'explained') based on the predictor

# to understand this better, let's again fit our model and compute the variance
# of the residuals
res1 <- lm(sales ~ TV, data=dat)
var(resid(res1))

# what if we have not used any predictor in our regression model? then our model
# would have been Y = beta0 + error, which we fit as follows
res0 <- lm(sales ~ 1, data=dat)
summary(res0)

# the estimated intercept is identical to the mean of the outcome variable
mean(dat$sales)

# so if we only have an intercept and no predictor in the model, then the best
# we can do in predicting Y is to just use the mean

# how accurate is this model? let's again compute the variance of the residuals
var(resid(res0))

# this is actually identical to the variance of the outcome itself
var(dat$sales)

# and note that this is quite a bit larger than var(resid(res1)) above

# so var(resid(res0)) is the amount of variance in the outcome to begin with and
# var(resid(res1)) is the variance that is *unaccounted* for based on the model
# that includes the predictor; so the following will tell us how much of the
# variance has been accounted for
var(resid(res0)) - var(resid(res1))

# now take this value and divide it by how much we started out with; that way we
# get a proportion between 0 and 1
(var(resid(res0)) - var(resid(res1))) / var(resid(res0))

# so about 61% of the variance in Y has been accounted for by X; this is R^2

# as explained in the book, this is also identical to the square of the
# correlation between X and Y
cor(dat$sales, dat$TV)^2

############################################################################

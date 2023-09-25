############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-09-14
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 8.3 - 11.1
#
# last updated: 2023-09-25

############################################################################

### 8.3: One- and two-sample tests

# create vectors A and B with the data shown (could also use scan())
A <- c(79.98, 80.04, 80.02, 80.04, 80.03, 80.03, 80.04, 79.97, 80.05, 80.03, 80.02, 80.00, 80.02)
B <- c(80.02, 79.94, 79.98, 79.97, 79.97, 80.03, 79.95, 79.97)
A
B

# side-by-side boxplots of the two variables
boxplot(A, B)

# make it look a bit nicer
par(bty="l")
boxplot(list(A=A, B=B), xlab="Method", ylab="Latent Heat (cal/gm)")

# independent samples t-test
t.test(A, B)

# note: this runs Welch's t-test (which does not assume equality of variances
# in the two groups / for the two variables)

# for more details, see:
# https://en.wikipedia.org/wiki/Welch%27s_t-test
# https://en.wikipedia.org/wiki/Student%27s_t-test

# look at the observed variances of the two variables
var(A)
var(B)

# test the equality of the two variances
var.test(A, B)

# run the classical Student's t-test (assuming equal variances)
t.test(A, B, var.equal=TRUE)

# two-sample Wilcoxon test (or Mann-Whitney U test)
# https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test
wilcox.test(A, B)

# look at the empirical cumulative distribution function of the two variables
plot(ecdf(A), do.points=FALSE, verticals=TRUE, xlim=range(A, B), col="red", lwd=5, main="")
plot(ecdf(B), do.points=FALSE, verticals=TRUE, add=TRUE, col="blue", lwd=5)
text(80.00, 0.77, "Method B", cex=1.5, col="blue")
text(80.01, 0.25, "Method A", cex=1.5, col="red")

# Kolmogorov-Smirnov test (of the maximal vertical distance between the two ecdfs)
ks.test(A, B)

# note: these results are slightly different from what it shows in the manual,
# because now the test is based on the exact p-value, not the approximate one;
# to get the same result as in the manual, we can use
ks.test(A, B, exact=FALSE)

# put the data from the example into a data frame
dat <- data.frame(method=c(rep("A",length(A)),rep("B",length(B))), heat=c(A,B))
dat

# rerun everything above but with this data layout
boxplot(heat ~ method, data=dat, xlab="Method", ylab="Latent Heat (cal/gm)")
t.test(heat ~ method, data=dat)
by(dat$heat, dat$method, var)
var.test(heat ~ method, data=dat)
t.test(heat ~ method, data=dat, var.equal=TRUE)

plot(ecdf(dat$heat[dat$method=="A"]), do.points=FALSE, verticals=TRUE, xlim=range(A, B), col="red", lwd=5, main="")
plot(ecdf(dat$heat[dat$method=="B"]), do.points=FALSE, verticals=TRUE, add=TRUE, col="blue", lwd=5)
text(80.00, 0.77, "Method B", cex=1.5, col="blue")
text(80.01, 0.25, "Method A", cex=1.5, col="red")

ks.test(heat ~ method, data=dat)

############################################################################

### 9.2: Control statements

# illustrate if (expr_1) expr_2 else expr_3

x <- 5

if (x == 5) print("x is five!") else print("x is not five :(")

# note: the expression between () (the 'condition') must be a single TRUE or
# FALSE, so the following does not work and generates an error
if (dat$heat > 80) print("heat is above 80") else print("heat is 80 or below")

## the difference between && and & (and similarly || and |)

# which values of 'heat' are larger than 80?
dat$heat > 80

# which values of 'method' are equal to A?
dat$method == "A"

# which values of 'heat' are larger than 80 and are from method A?
dat$heat > 80 & dat$method == "A"

# the double && is only for a single logical
y <- 7
x == 5 && y < 8

# this generates an error
dat$heat > 80 && dat$method == "A"

# why would you ever want to use &&?

# the && is evaluted left-to-right, so if the first logical is FALSE, then
# none of the following expressions are evaluated; for example, running
# mean(rnorm(10^10)) > 0 would not only take a huge amount of time, but would
# require more memory than my computer has; but running the following is no
# problem, because the second expression (mean(rnorm(10^10)) > 0) is never
# actually run
x == 4 && mean(rnorm(10^10)) > 0

# but do not run 'x == 4 & mean(rnorm(10^10)) > 0' because then both sides of
# the & are run/evaluated and this would crash my R session!

# same thing with || (for 'or' comparisons); if the first expression is TRUE,
# then there is no need to evaluate the second and it isn't even run
x == 5 || mean(rnorm(10^10)) > 0

# the ifelse() function is a 'vectorized' version of if-else
ifelse(dat$heat > 80, "red", "blue")

# this is useful in all kinds of circumstances, for example to distinguish
# groups or values in plots; here is a silly example
plot(dat$heat, xlab="Observation Number", ylab="Heat", pch=19, cex=1.2,
     col=ifelse(dat$heat > 80, "red", "blue"))

## for-loops

# a very simple example
for (i in 1:20) {
   print(paste("i is equal to:", i))
}

# a few more examples

for (i in c(2,9,5)) {
   print(paste("i is equal to:", i))
}

for (i in c("chicken","cow","pig")) {
   print(paste("i is equal to:", i))
}

# as a sort-of not entirely silly application of this, let's consider the
# mtcars dataset and suppose we want to run simple regression models where we
# predict the mpg (mile per gallon) variable from each other variable in the
# dataset and we want to find out for which variable R^2 is the largest
mtcars

# we could do this manually as follows (note: we haven't actually gotten to
# fitting regression models - this comes in section 11, but let's already do
# this here to make this example a bit more interesting)
summary(lm(mpg ~ cyl,  data=mtcars))
summary(lm(mpg ~ disp, data=mtcars))
summary(lm(mpg ~ hp,   data=mtcars))
# ...

# but this is tedious (especially if the number of variables was even larger);

# note that we are fitting regression models where the predictor variables are
# in the following columns of the dataset
2:ncol(mtcars)

# we can refer to a particular variable from the dataset using column indices
mtcars[[2]]
mtcars[[3]]
# and so on

# so we can do the following
for (i in 2:ncol(mtcars)) {
   print(summary(lm(mpg ~ mtcars[[i]], data=mtcars)))
}

# but we would still have to look manually for the highest R^2 value which is
# tedious; without getting into how one can figure this out for now, it turns
# out that the R^2 value from a regression model can be extracted as follows
summary(lm(mpg ~ cyl,  data=mtcars))$r.squared

# so we can do the following
for (i in 2:ncol(mtcars)) {
   print(summary(lm(mpg ~ mtcars[[i]], data=mtcars))$r.squared)
}

# better, but still not ideal; let's actually *store* the R^2 values somewhere

r2 <- rep(NA, ncol(mtcars))
r2

for (i in 2:ncol(mtcars)) {
   r2[i] <- summary(lm(mpg ~ mtcars[[i]], data=mtcars))$r.squared
}

r2

# what is the largest R^2 value?
max(r2, na.rm=TRUE)

# which value of R^2 is the largest (the maximum)
which.max(r2)

# this corresponds to which variable in the dataset
names(mtcars)[which.max(r2)]

# so predicting mpg (miles per gallon) from wt (the weight of the car) yields
# the largest R^2 value (of about 0.75)

# often, one can avoid writing explicit loops and make the code more concise;
# for example, suppose we want the mean of every variable in the dataset; we
# could do this with a for-loop as follows
means <- rep(NA, ncol(mtcars))
for (i in 1:ncol(mtcars)) {
   means[i] <- mean(mtcars[[i]])
}
means

# but there are often specialized ('vectorized') functions that can do this
colMeans(mtcars)

############################################################################

### 10: Writing your own functions

# we will skip this (will come back to this topic in some future stream)

############################################################################

### 11.1: Defining statistical models; formulae

# let's do some regression modeling using formula syntax with the mtcars dataset

# scatterplot of mpg (miles per gallon) on the y-axis and wt (weight) on the x-axis
plot(mpg ~ wt, data=mtcars, pch=21, bg="lightgray", cex=1.5,
     xlab="Weight (in 1000lbs)", ylab="Mile per Gallon")

# simple regression model predicting mpg from wt
res <- lm(mpg ~ wt, data=mtcars)
res

# use summary() to get the full output from the regression model
summary(res)

# add the regression line from the simple regression model above to the plot
abline(res, lwd=3)

# more generally, we can get predicted values using predict()
wtvals <- seq(1, 6, length=100)
pred <- predict(res, newdata=data.frame(wt=wtvals))
pred

# add the regression line based on these predicted values to the plot
lines(wtvals, pred, lwd=3, col="blue")

# predict() can also compute confidence (and prediction) intervals
pred <- predict(res, newdata=data.frame(wt=wtvals), interval="confidence")
pred <- data.frame(pred)
pred
lines(wtvals, pred$lwr, lty="dotted")
lines(wtvals, pred$upr, lty="dotted")

# draw the points again (so they are on top of the lines)
points(mpg ~ wt, data=mtcars, pch=21, bg="lightgray", cex=1.5)

# in the model above, the intercept refers to the predicted average gas
# mileage for cars whose weight is 0 pounds (which is obviously meaningless);
# we can make the intercept meaningful by centering the predictor at a more
# meaningful value, for example at the mean
res <- lm(mpg ~ I(wt-mean(wt)), data=mtcars)
summary(res)

# so now the intercept refers to the predicted average gas mileage of cars
# whose weight is equal to the mean weight of the cars in this dataset

# one can also directly set the value at which to center
res <- lm(mpg ~ I(wt-3), data=mtcars)
summary(res)

# now the intercept is the predicted average gas mileage of cars whose weight
# is 3000 pounds (note: wt is given per 1000lbs)

# a regression model with multiple predictors (multiple regression)
res <- lm(mpg ~ wt + hp, data=mtcars)
summary(res)

# say we want to predict the gas mileage of a car that weighs 3000 pounds and
# has a horsepower of 150
predict(res, newdata=data.frame(wt=3, hp=150))

# predict the gas mileage for cars that weigh between 1000 and 6000 pounds
# holding horsepower constant at 150
predict(res, newdata=data.frame(wt=wtvals, hp=150))

# we saw earlier in the scatterplot that the relationship between mpg and wt
# may not be linear (redraw the scatterplot)
plot(mpg ~ wt, data=mtcars, pch=21, bg="lightgray", cex=1.5,
     xlab="Weight (in 1000lbs)", ylab="Mile per Gallon")

# again add the line from the linear regression model to the plot
res <- lm(mpg ~ wt, data=mtcars)
abline(res)

# fit a polynomial regression model to the second degree (to model a quadratic
# relationship between the two variables)
res <- lm(mpg ~ wt + I(wt^2), data=mtcars)
summary(res)

# compute predicted values for cars between 1000 and 6000 pounds
pred <- predict(res, newdata=data.frame(wt=wtvals))
pred

# note: we only need to specify the wt values for this model; the predict()
# function automatically constructs the squared term for making predictions

# add the regression line (or rather, curve) to the plot
lines(wtvals, pred, lwd=3, col="blue")

# instead of I(wt^2), we can use poly()
res <- lm(mpg ~ poly(wt, degree=2), data=mtcars)
summary(res)

# note: poly() constructs 'orthogonal polynomial' terms (to minimize the
# correlation between the terms), so the coefficients are different, but the
# underlying model is still going to give the same predictions

# predicted values are computed in the exact same way as before
pred <- predict(res, newdata=data.frame(wt=wtvals))
pred
lines(wtvals, pred, lwd=3, col="red")

# we can easily fit higher polynomial models (this is totally overfitting here)
res <- lm(mpg ~ poly(wt, degree=5), data=mtcars)
pred <- predict(res, newdata=data.frame(wt=wtvals))
lines(wtvals, pred, lwd=3, col="green")

############################################################################

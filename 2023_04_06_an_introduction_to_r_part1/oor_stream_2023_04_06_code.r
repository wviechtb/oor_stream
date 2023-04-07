############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-04-06
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): Appendix A
#
# last updated: 2023-04-07

############################################################################

# A few general notes about this manual: Based on my recollection of reading
# this manual 20+ years ago, there are parts that are overly technical and/or
# that illustrate ways of using R that I would not recommend (an example we
# will see below is the use of attach(), which I think is bad practice).
# Therefore, I will adjust the code from the manual for didactic purposes as
# deemed necessary.

### Appendix A: A sample session

# start R (or RStudio or whatever IDE / programming editor you are using)

# open the general help page
help.start()

# generate 50 random values from a standard normal distribution and put them
# into an object called 'x'
x <- rnorm(50)

# x is a 'numeric vector'
x

# again generate 50 random values from a standard normal distribution and put
# them into 'y' (note: the length of 'x' is used to determine here how many
# values should be generated)
y <- rnorm(x)

# scatterplot of x versus y
plot(x, y)

# list the objects in the 'workspace' (also called the 'global environment')
ls()

# remove objects 'x' and 'y' from the workspace
rm(x, y)

# again list the objects in the workspace
ls()

# character(0) means 'a character vector with no elements'

# create a vector of the numbers from 1 to 20
x <- 1:20
x

# take the square root of each element in 'x', divide this by 2, and add 1,
# and put the resulting vector of numbers into 'w'
w <- 1 + sqrt(x)/2

# create a 'data frame' where the first variable called 'x' contains the
# values from the 'x' vector, the second variable 'y' contains 20 random
# values from a standard normal distribution multiplied by w and x added,
# and the third variable contains the 'w' values
dummy <- data.frame(x = x, y = x + rnorm(x)*w, w = w)
dummy

# remove objects 'x' and 'w' from the workspace
rm(x, w)

# fit a simple regression model where 'y' is the outcome variable and 'x' is
# the predictor (these variables can be found in the 'dummy' data frame),
# store the results in 'fm', and then use the summary() function on this
# object to inspect the results
fm <- lm(y ~ x, data=dummy)
summary(fm)

# if we just want the coefficients, we can use coef()
coef(fm)

# fit a simple regression model with 1/w^2 as weights (so this does 'weighted
# least squares regression')
fm1 <- lm(y ~ x, data=dummy, weight=1/w^2)
summary(fm1)

# if we want to access variable 'y', this will not work
y

# variable 'y' is part of the 'dummy' data frame; if we want to access these
# values, we can use the following 'dollar' notation
dummy$y

# make the variables inside of the 'dummy' data frame visible by 'attaching'
# the data frame to the 'search path'
attach(dummy)

# and now this will work
y

# fit a nonparametric local regression function
lrf <- lowess(x, y)

# scatterplot of x versus y
plot(x, y)

# add the fitted line from the local regression model
lines(x, lrf$y)

# add a line to the plot with an intercept of 0 and a slope of 1 (lty=3 means
# that a dotted line should be drawn)
abline(0, 1, lty=3)

# add the regression line from the simple regression model we fitted above
abline(coef(fm), col="blue")

# add the regression line from the weighted regression model
abline(coef(fm1), col="red")

# detach the dummy data frame from the search path
detach(dummy)

# general note: using attach() is *highly* inadvisable (it can easily lead to
# confusion as to where R is finding variables/objects); instead, either make
# use of the 'data' argument as we did above with lm() (if the function allows
# this) or explicitly specify from which data frame you are taking a certain
# variable from

# unfortunately, the lowess() function does not have a 'data' argument that
# can be used as we did with lm()
lrf <- lowess(x, y, data=dummy)

# so instead, we can use the dollar notation to specify which variables to use
lrf <- lowess(dummy$x, dummy$y)

# instead of using attach() or repeatedly using 'dummy$...', we can also use
# the with() function
lrf <- with(dummy, lowess(x, y))

# create a scatterplot of the fitted values versus the residuals from the
# simple regression model we fitted above
plot(fitted(fm), resid(fm), xlab="Fitted values",
     ylab="Residuals", main="Residuals vs Fitted")

# create a Q-Q plot of the residuals (to check for normality of the residuals)
# https://en.wikipedia.org/wiki/Q-Q_plot
qqnorm(resid(fm), main="Residuals Rankit Plot")

# remove all objects in the workspace
rm(list=ls())

# figure out where the 'morley.tab' file is located on your computer
filepath <- system.file("data", "morley.tab", package="datasets")
filepath

# read in the Michelson data as a data frame and look at it
mm <- read.table(filepath)
mm

# there are five experiments (column Expt) and each has 20 runs (column Run)
# and (column Speed) is the recorded speed of light (given as km/sec, with
# 299000 subtracted)

# scatterplot of the measurements of the speed of light for the different
# experiments
with(mm, plot(Expt, Speed, main="Speed of Light Data", xlab="Experiment No."))

# change Expt and Run into factors
mm$Expt <- factor(mm$Expt)
mm$Run  <- factor(mm$Run)

# if the variable on the x-axis is a factor, then the plot() function
# automatically draws boxplots for the variable on the y-axis
with(mm, plot(Expt, Speed, main="Speed of Light Data", xlab="Experiment No."))

# so depending on the type of variables used as input, plot() actually creates
# a different type of visualization of the data

# fit a two-way ANOVA model (with 'main effects' only)
fm <- aov(Speed ~ Run + Expt, data=mm)
summary(fm)

# fit the model after removing 'Run'
fm0 <- update(fm, . ~ . - Run)

# do a full versus reduced model comparison
anova(fm0, fm)

# clean up
rm(list=ls())

# create a sequence of numbers between -pi and pi of length 50, put these
# values into 'x' and then make a copy of this vector and call this 'y'
x <- seq(-pi, pi, len=50)
y <- x

# for every combination of x and y, compute cos(y)/(1 + x^2)) and store the
# resulting values in a matrix
f <- outer(x, y, function(x,y) cos(y)/(1 + x^2))

# note that this creates a 50x50 matrix
dim(f)

# create a contour plot
contour(x, y, f)

# create a contour plot with more detail (more contour lines)
contour(x, y, f, nlevels=15)

# create a corresponding 3-d perspective plot
persp(x, y, f, theta=45, phi=40, col="lightgray")

# do some further manipulation of f and store the resulting matrix in fa
fa <- (f-t(f))/2

# create a contour plot of the fa matrix
contour(x, y, fa, nlevels=15)

# create a corresponding 3-d perspective plot
persp(x, y, fa, theta=45, phi=40, col="lightgray")

# filled contour plots of f and fa
filled.contour(x, y, f)
filled.contour(x, y, fa)

# clean up
rm(list=ls())

# R can do complex arithmetic, but who the hell cares about this if one is
# starting out with R?!? so we will skip this part ...

# quit R
q()

# if asked about 'Saving the workspace' when quitting R, always answer No; in
# general, it is best never to make use of this functionality as it can lead
# to a cluttered workspace and confusion (we can look at ways of saving
# objects or workspace states manually later on)

############################################################################

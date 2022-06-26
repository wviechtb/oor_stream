############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-03-17
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 3.3.2 - 3.3.3
#
# last updated: 2022-03-25

############################################################################

### 3.3.2: Extensions of the Linear Model - Removing the Additive Assumption

# the data used in this section: https://www.statlearning.com/s/Advertising.csv
#download.file("https://www.statlearning.com/s/Advertising.csv", destfile="Advertising.csv")

# read in the data and examine the first 6 rows
dat <- read.csv("Advertising.csv")
head(dat)

# fit the 'additive model' (model 3.20)
res <- lm(sales ~ TV + radio, data=dat)
summary(res)

# fit the 'interaction model' (model 3.33; Table 3.9)
res <- lm(sales ~ TV + radio + TV:radio, data=dat)
summary(res)

# switch off the use of scientific notation
options(scipen=100)

# again examine the results
summary(res)

# instead of doing this, we could also do the following
options(scipen=0) # 0 is the default for scipen

# get the regression table and round values to 4 digits
round(coef(summary(res)), digits=4)

# create the product term manually (same results)
dat$TVxratio <- dat$TV * dat$radio
res <- lm(sales ~ TV + radio + TVxratio, data=dat)
summary(res)

# we can also use * in the 'formula' of our regression model; this is a
# shortcut for adding both 'main effects' and the 'interaction term'
res <- lm(sales ~ TV * radio, data=dat)
summary(res)

# let's visualize this interaction

# plot TV versus sales
plot(sales ~ TV, data=dat, pch=19, xlab="TV Advertisement", ylab="Sales")

# manually compute predicted average sales as a function of increased TV
# advertising holding radio advertising constant at its mean
TVvals <- seq(0, 300, length=50)
pred <- coef(res)[1] + coef(res)[2]*TVvals + coef(res)[3]*mean(dat$radio) + coef(res)[4]*TVvals*mean(dat$radio)
pred
lines(TVvals, pred, lwd=8, col="#fdc086")

# we can accomplish the same using the predict() function; note that in the
# data frame given to the 'newdata' argument, we include the values for the
# predictor variables (for which we want to compute the predicted values) but
# we do not have to (or even should) include the product term, since predict()
# will automatically create this term when computing the predicted values
pred <- predict(res, newdata=data.frame(TV = TVvals, radio = mean(dat$radio)))
pred
lines(TVvals, pred, lwd=8, col="#fdc086")

# compute predicted average sales when radio advertising is held constant at
# one standard deviation below its mean
pred <- predict(res, newdata=data.frame(TV = TVvals, radio = mean(dat$radio)-sd(dat$radio)))
lines(TVvals, pred, lwd=8, col="#7fc97f")

# compute predicted average sales when radio advertising is held constant at
# one standard deviation above its mean
pred <- predict(res, newdata=data.frame(TV = TVvals, radio = mean(dat$radio)+sd(dat$radio)))
lines(TVvals, pred, lwd=8, col="#beaed4")

# add a legend
legend("topleft", inset=.02, lty="solid", lwd=5, col=c("#beaed4", "#fdc086", "#7fc97f"),
       legend = c("mean radio + sd radio", "mean radio", "mean radio - sd radio"))

############################################################################

# let's draw a 3d scatterplot like Figure 3.5, but now based on the
# interaction model; this is a bit more tricky, since the 'surface' that we
# are fitting through the data is no longer a flat plane; points that fall
# above the plane will be indicated with red dots (positive residuals) and
# points below the plane with blue dots (negative residuals); the three lines
# we added to the scatterplot above are also shown

#install.packages("scatterplot3d")
library(scatterplot3d)

s3d <- scatterplot3d(dat$TV, dat$radio, dat$sales, pch=19, color="white",
                     xlab="TV Advertisement", ylab="", zlab="Sales")

pos <- s3d$xyz.convert(340, 25, 0)
text(pos$x, pos$y, "radio", srt=40)

orig <- s3d$xyz.convert(dat$TV, dat$radio, dat$sales)
fit  <- s3d$xyz.convert(dat$TV, dat$radio, fitted(res))

posresid <- resid(res) > 0
negresid <- resid(res) < 0

segments(orig$x[negresid], orig$y[negresid], fit$x[negresid], fit$y[negresid], col="blue")
s3d$points3d(dat$TV[negresid], dat$radio[negresid], dat$sales[negresid], pch=19, col="blue")

TVvals <- seq(0, 300, length=50)
radiovals <- seq(0, 50, length=50)

for (i in 1:length(TVvals)) {

   pred <- predict(res, newdata=data.frame(TV = TVvals[i], radio = radiovals))
   s3d$points3d(rep(TVvals[i], length(radiovals)), radiovals, pred, type="l", col="darkgray")

}

for (i in 1:length(radiovals)) {

   pred <- predict(res, newdata=data.frame(TV = TVvals, radio = radiovals[i]))
   s3d$points3d(TVvals, rep(radiovals[i], length(TVvals)), pred, type="l", col="darkgray")

}

radioval <- mean(dat$radio)
pred <- predict(res, newdata=data.frame(TV = TVvals, radio = radioval))
s3d$points3d(TVvals, rep(radioval, length(TVvals)), pred, type="l", col="#fdc086", lwd=8)

pos <- s3d$xyz.convert(300, radioval, max(pred))
text(pos$x, pos$y, "mean radio", pos=4)

radioval <- mean(dat$radio) - sd(dat$radio)
pred <- predict(res, newdata=data.frame(TV = TVvals, radio = radioval))
s3d$points3d(TVvals, rep(radioval, length(TVvals)), pred, type="l", col="#7fc97f", lwd=8)

pos <- s3d$xyz.convert(300, radioval, max(pred))
text(pos$x, pos$y, "mean - sd", pos=4)

radioval <- mean(dat$radio) + sd(dat$radio)
pred <- predict(res, newdata=data.frame(TV = TVvals, radio = radioval))
s3d$points3d(TVvals, rep(radioval, length(TVvals)), pred, type="l", col="#beaed4", lwd=8)

pos <- s3d$xyz.convert(300, radioval, max(pred))
text(pos$x, pos$y, "mean + sd", pos=4)

segments(orig$x[posresid], orig$y[posresid], fit$x[posresid], fit$y[posresid], col="red")
s3d$points3d(dat$TV[posresid], dat$radio[posresid], dat$sales[posresid], pch=19, col="red")

pos <- s3d$xyz.convert(10, 50, 29)
legend(pos$x, pos$y, pch=19, col=c("red", "blue"), legend=c("positive residuals", "negative residuals"))

############################################################################

# sidenote: could also draw the surface using the mgcv package as follows

library(mgcv)

res <- gam(sales ~ TV * radio, data=dat)
vis.gam(res, view=c("TV","radio"), n.grid=50, theta=0, phi=30, zlab="",
        ticktype="detailed", color="topo", xlim=c(0,300), ylim=c(0,50), zlim=c(0,30))

############################################################################

# install the ISLR2 package
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# copy the Credit dataset to dat
dat <- Credit

# regression model with predictors income and student
res <- lm(Balance ~ Income + Student, data=dat)
summary(res)

# scatterplot of Balance versus Income with the regression lines for students
# and non-students added (note that the two lines are parallel to each other)
ivals <- 0:200
plot(Balance ~ Income, data=dat, pch=19, col=ifelse(Student == "Yes", "darkorange", "blue"), xlim=c(0,200))
pred <- predict(res, newdata=data.frame(Income = ivals, Student = "No"))
lines(ivals, pred, lwd=8, col="blue")
pred <- predict(res, newdata=data.frame(Income = ivals, Student = "Yes"))
lines(ivals, pred, lwd=8, col="darkorange")
legend("topleft", inset=.02, lty="solid", lwd=5, col=c("darkorange", "blue"),
       legend = c("students", "non-students"))

# regression model with predictors income and student and their interaction
res <- lm(Balance ~ Income * Student, data=dat)
summary(res)

# how do we interpret the coefficients? write out the model:
#
#                    Balance = beta0 + beta1 * Income + beta2 * StudentYes + beta3 * Income * StudentYes
#
# now plug in for the dummy variable 'StudentYes' the two possible values:
#
# if StudentYes = 0  Balance = beta0 + beta1 * Income
#
# if StudentYes = 1  Balance = beta0 + beta1 * Income + beta2              + beta3 * Income
#                            = (beta0 + beta2) + (beta1 + beta3) * Income
#
# so beta0 and beta1 are the intercept and slope for non-students, while
# (beta0 + beta2) and (beta1 + beta3) are the intercept and slope for students

# scatterplot of Balance versus Income with the regression lines for students
# and non-students added (note that the two lines are no longer parallel)
ivals <- 0:200
plot(Balance ~ Income, data=dat, pch=19, col=ifelse(Student == "Yes", "darkorange", "blue"), xlim=c(0,200))
pred <- predict(res, newdata=data.frame(Income = ivals, Student = "No"))
lines(ivals, pred, lwd=8, col="blue")
pred <- predict(res, newdata=data.frame(Income = ivals, Student = "Yes"))
lines(ivals, pred, lwd=8, col="darkorange")
legend("topleft", inset=.02, lty="solid", lwd=5, col=c("darkorange", "blue"),
       legend = c("students", "non-students"))

############################################################################

# non-linear relationships

# copy the Auto dataset to dat
dat <- Auto

# scatterplot of mpg versus horsepower
plot(mpg ~ horsepower, data=dat, xlab="Horsepower", ylab="Miles per galon",
     ylim=c(10,50))

# fit the linear model
res1 <- lm(mpg ~ horsepower, data=dat)
summary(res1)

# add the regression line based on this model to the scatterplot
hpvals <- 0:250
pred <- predict(res1, newdata=data.frame(horsepower = hpvals))
lines(hpvals, pred, lwd=8, col="darkorange")

# fit the quadratic model (Table 3.10)
dat$horsepower2 <- dat$horsepower * dat$horsepower
res2 <- lm(mpg ~ horsepower + horsepower2, data=dat)
summary(res2)

# can also do it this way (note: need to wrap transformations of predictor
# variables in I())
res2 <- lm(mpg ~ horsepower + I(horsepower^2), data=dat)
summary(res2)

# can also use poly(); note: by default, poly() creates so-called orthogonal
# polynomial terms (they are uncorrelated), but for consistency with the
# results above, we use raw=TRUE to get the raw polynomial terms
res2 <- lm(mpg ~ poly(horsepower, degree=2, raw=TRUE), data=dat)
summary(res2)

# the model is given by:
#
# mpg = beta0 + beta1 * horsepower + beta2 * horsepower^2
#
# we can rewrite this as:
#
#     = beta0 + (beta1 + beta2 * horsepower) * horsepower
#
# so (beta1 + beta2 * horsepower) is the slope for the predictor, which
# changes as a function of predictor; this is the 'trick' that allows the
# regression line to be non-linear

# add the regression line based on this model to the scatterplot
pred <- predict(res2, newdata=data.frame(horsepower = hpvals))
lines(hpvals, pred, lwd=8, col="skyblue2")

# fit the polynomial model to the 5th degree
res5 <- lm(mpg ~ poly(horsepower, degree=5, raw=TRUE), data=dat)
summary(res5)

# add the regression line based on this model to the scatterplot
pred <- predict(res5, newdata=data.frame(horsepower = hpvals))
lines(hpvals, pred, lwd=8, col="seagreen4")

# let's go craaaaazy; polynomial model to the 15th degree
res15 <- lm(mpg ~ poly(horsepower, degree=15), data=dat)
pred <- predict(res15, newdata=data.frame(horsepower = hpvals))
lines(hpvals, pred, lwd=8, col="pink")

############################################################################

### 3.3.3: Potential Problems

# 1. Non-linearity of the Data

# again fit the linear and quadratic models
res1 <- lm(mpg ~ horsepower, data=dat)
res2 <- lm(mpg ~ horsepower + I(horsepower^2), data=dat)

# residuals plots for the linear and quadratic models (Figure 3.9)
par(mfrow=c(1,2))
plot(res1, which=1, caption="Residual Plot for the Linear Fit")
plot(res2, which=1, caption="Residual Plot for the Quadratic Fit")
par(mfrow=c(1,1))

# 2. Correlation of Error Terms

# like Figure 3.10 (simulated time series with no correlation in the data
# versus autocorrelations of 0.5 and 0.9)
set.seed(1234)
n  <- 100
ti <- 1:n
par(mfrow=c(3,1))
rho <- 0
yi <- rnorm(n)
plot(ti, yi, type="o", pch=19, xlab="Time", main=expression(rho==0))
abline(h=0, lty="dotted")
rho <- 0.5
yi <- arima.sim(model=list(ar=rho), n=n)
plot(ti, yi, type="o", pch=19, xlab="Time", main=expression(rho==0.5))
abline(h=0, lty="dotted")
rho <- 0.9
yi <- arima.sim(model=list(ar=rho), n=n)
plot(ti, yi, type="o", pch=19, xlab="Time", main=expression(rho==0.9))
abline(h=0, lty="dotted")
par(mfrow=c(1,1))

# 3. Non-constant Variance of Error Terms

# I don't know which dataset they used in Figure 3.11, so let me illustrate
# this using the Advertising dataset

# load the data
dat <- read.csv("Advertising.csv")

# plot sales versus radio; note how the points fluctuate more (i.e., have a
# larger variance) when the value of radio is large
plot(sales ~ radio, data=dat, pch=19)

# fit the linear model
res <- lm(sales ~ radio, data=dat)

# residual plot; note that the residuals fluctuate more around 0 (i.e., have a
# larger variance) for large fitted values
plot(res, which=1)

# actually, to diagnose heteroscedasticity, a better plot is the following:
# here, we plot the fitted values on the x-axis against the square-root of the
# absolute value of the standardized residuals; if the variance in the errors
# is constant, then the red trend line in this plot should be horizontal, but
# here we see it sloping upwards, indicating higher variance in the errors for
# large fitted values
plot(res, which=3)

par(mfrow=c(1,2))
plot(res1, which=1, caption="Response Y")
plot(res2, which=1, caption="Response log(Y)")
par(mfrow=c(1,1))

# 4. Outliers

# Figure 3.12 and 3.13 seems to be based on some simulated data; I won't
# recreate analogous plots here because I think the discussion in the book is
# pretty clear

# 5. High Leverage Points

# same here, but we can create a figure like the right panel in Figure 3.13
# again using plot() based on the regression model
plot(res, which=5)

# 6. Collinearity

# copy the Credit dataset to dat
dat <- Credit

# scatterplots of Age and Rating versus Limit (Figure 3.14)
par(mfrow=c(1,2))
plot(Age ~ Limit,    data=dat, col="darkred")
plot(Rating ~ Limit, data=dat, col="darkred")
par(mfrow=c(1,1))

# note how collinear Rating and Limit are; this creates problems when we use
# both variables as predictors in the same model

# Table 3.11: Model 1
res <- lm(Balance ~ Age + Limit, data=dat)
round(coef(summary(res)), digits = 4)

# Table 3.11: Model 2
res <- lm(Balance ~ Rating + Limit, data=dat)
round(coef(summary(res)), digits = 4)

# model with all three predictors
res <- lm(Balance ~ Age + Rating + Limit, data=dat)
round(coef(summary(res)), digits = 4)

# compute the variance inflation factors (VIFs) for this model

#install.packages("car")
library(car)
vif(res)

############################################################################

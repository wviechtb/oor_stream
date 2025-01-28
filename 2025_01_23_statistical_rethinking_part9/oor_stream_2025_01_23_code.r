############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-01-23
#
# Topic(s):
# - Statistical Rethinking (https://xcelab.net/rm/)
# - Section(s): 4.5 - 4.6
#
# last updated: 2025-01-28

############################################################################

### 4.5: Curves from lines

# load the rethinking package
library(rethinking)

# get the Howell1 data and put it into 'dat'
dat <- get(data(Howell1))

# inspect the first 6 rows
head(dat)

## 4.5.1: Polynomial regression

# plot the height of the individuals versus their weight (so far when using
# this dataset, we have only used the data from the adults (age >= 18); now,
# we will use all of the data including the children; as a result, we see that
# the relationship between weight and height is not linear)
plot(height ~ weight, data=dat, pch=21, bg="gray", bty="l")

# rewrite the equation at the bottom of page 110:
#
# mu_i = alpha     + beta_1 x_i + beta_2 x^2_i
#      = alpha     + (beta_1 + beta_2 x_i) * x_i
#      = intercept + (slope)               * x_i
#
# so when x_i = 0, then beta_1 is the slope of the linear relationship between
# mu_i and x_i, but if x_i is not 0, then the slope is beta_1 + beta_2 x_i and
# hence the slope depends on x_i itself; as a result, the model allows for a
# non-linear relationship between mu_i and x_i

# standardize weight (using the scale() function)
dat$weight.s  <- c(scale(dat$weight))

# sidenote: scale() returns a matrix, so we use c() to turn this into a plain
# vector before adding this variable to the dataset

# compute the square of the standardized weight values
dat$weight.s2 <- dat$weight.s^2

# fit the quadratic polynomial regression model
model <- alist(height ~ dnorm(mu, sigma),
               mu <- a + b1*weight.s + b2*weight.s2,
               a  ~ dnorm(178, 20),
               b1 ~ dlnorm(0, 1),
               b2 ~ dnorm(0, 1),
               sigma ~ dunif(0, 50))
res <- quap(model, data=dat)
res
precis(res)

# compute the predicted mean height as a function of 30 (standardized) weight
# values between -2.2 and 2 (and the corresponding 95% compatibility intervals)
weight.seq <- seq(from=-2.2, to=2, length.out=30)
pred.dat   <- data.frame(weight.s=weight.seq, weight.s2=weight.seq^2)
mu         <- link(res, data=pred.dat)
mu.mean    <- apply(mu, 2, mean)
mu.ci      <- apply(mu, 2, PI, prob=0.95)

# compute corresponding 95% prediction intervals for the height of individuals
sim.height <- sim(res, data=pred.dat)
height.pi  <- apply(sim.height, 2, PI, prob=0.95)

# sidenote: the book uses mu.PI and height.PI to denote the two intervals
# (where PI stands for percentile interval); here, I will use mu.ci to refer
# to the compatibility intervals for the predicted mean height (for a given
# weight) and height.pi to refer to the prediction interval for the height of
# individuals (for a given weight)

# plot the data again and add the predicted means and intervals to the plot
plot(height ~ weight.s, data=dat, pch=21, bg="lightgray", bty="l",
     xlab="Standardized Weight", ylab="Height")
lines(weight.seq, mu.mean, lwd=3)
shade(mu.ci, weight.seq)
shade(height.pi, weight.seq)

# compute the cube of the standardized weight values
dat$weight.s3 <- dat$weight.s^3

# fit the cubic polynomial regression model
model <- alist(height ~ dnorm(mu, sigma),
               mu <- a + b1*weight.s + b2*weight.s2 + b3*weight.s3,
               a  ~ dnorm(178, 20),
               b1 ~ dlnorm(0, 1),
               b2 ~ dnorm(0, 1),
               b3 ~ dnorm(0, 1),
               sigma ~ dunif(0, 50))
res <- quap(model, data=dat)
res
precis(res)

# compute the predicted means and intervals again based on the cubic model
pred.dat   <- data.frame(weight.s=weight.seq, weight.s2=weight.seq^2, weight.s3=weight.seq^3)
mu         <- link(res, data=pred.dat)
mu.mean    <- apply(mu, 2, mean)
mu.ci      <- apply(mu, 2, PI, prob=0.95)
sim.height <- sim(res, data=pred.dat)
height.pi  <- apply(sim.height, 2, PI, prob=0.95)

# plot the data again and add the predicted means and intervals to the plot
plot(height ~ weight.s, data=dat, pch=21, bg="lightgray", bty="l",
     xlab="Standardized Weight", ylab="Height")
lines(weight.seq, mu.mean, lwd=3)
shade(mu.ci, weight.seq)
shade(height.pi, weight.seq)

# same plot as above, but show weight on the x-axis, not standardized weight
plot(height ~ weight.s, data=dat, pch=21, bg="lightgray", bty="l",
     xlab="Weight", ylab="Height", xaxt="n")
labels <- seq(5, 65, by=10)
at <- (labels - mean(dat$weight)) / sd(dat$weight)
axis(side=1, at=at, labels=round(labels,1))
lines(weight.seq, mu.mean, lwd=3)
shade(mu.ci, weight.seq)
shade(height.pi, weight.seq)

## 4.5.2: Splines

# load the splines package
library(splines)

# load the cherry tree data and examine some summary statistics
dat <- get(data(cherry_blossoms))
precis(dat, prob=0.95)

# plot of the day of year of first bloom versus year
plot(doy ~ year, data=dat, pch=21, bg="gray", bty="l", xlab="year", ylab="day")

# same plot but jitter the day values a bit
plot(jitter(doy, amount=0.5) ~ year, data=dat, pch=21, bg="gray", bty="l",
     xlab="year", ylab="day")

# subset of the data where doy is not missing
dat2 <- dat[complete.cases(dat$doy), ]

# construct the basis function values as shown in Figure 4.12(top)
num.knots <- 5
knots <- quantile(dat2$year, probs=seq(0,1,length.out=num.knots))
B <- bs(dat2$year, knots=knots[-c(1,num.knots)], degree=1, intercept=TRUE)

# Figure 4.12(top): plot of the basis function values
plot(NA, xlim=range(dat2$year), ylim=c(0,1.05),
     xlab="year", ylab="basis value", bty="l", las=1)
apply(B, 2, function(x) lines(dat2$year, x, lwd=8, col="darkgray"))
points(knots, rep(1.04, num.knots), pch=3, lwd=3)
text(knots, 1, 1:num.knots, pos=1, cex=1.2, offset=0.8)

# fit the spline model (using lm() for now for simplicity)
res <- lm(doy ~ 0 + B, data=dat2)
summary(res)

# note: have to remove the intercept, as otherwise the model is overparameterized

# compute predicted values (and corresponding 95% confidence intervals)
pred <- predict(res, interval="confidence")
pred <- data.frame(pred)

# plot the data again and add the predicted values and 95% CI bounds
plot(jitter(doy, amount=0.5) ~ year, data=dat, pch=21, bg="lightgray",
     bty="l", xlab="year", ylab="day")
abline(h=mean(dat2$doy), lty="dashed", lwd=2)
lines(dat2$year, pred$fit, lwd=5)
shade(t(pred[2:3]), dat2$year)

# increase the number of knots to 15 and use degree 3 (cubic) splines
num.knots <- 15
knots <- quantile(dat2$year, probs=seq(0,1,length.out=num.knots))
B <- bs(dat2$year, knots=knots[-c(1,num.knots)], degree=3, intercept=TRUE)

# Figure 4.13(top): plot of the basis function values
plot(NA, xlim=range(dat2$year), ylim=c(0,1.05),
     xlab="year", ylab="basis value", bty="l", las=1)
apply(B, 2, function(x) lines(dat2$year, x, lwd=8, col="darkgray"))
points(knots, rep(1.04, num.knots), pch=3, lwd=3)

# fit the (Bayesian) model
model <- alist(doy ~ dnorm(mu, sigma),
               mu <- a + B %*% w,
               a ~ dnorm(100,10),
               w ~ dnorm(0,10),
               sigma ~ dexp(1))
res <- quap(model, data=list(doy=dat2$doy, B=B),
            start=list(w=rep(0, ncol(B))))
res
precis(res, depth=2)

# sidenote: here, including the intercept does work (removing it essentially
# yields the same fit, but we need to change the priors for w to something
# like dnorm(100,10) if we would do that)

# Figure 4.13(middle): plot of the basis function values times the coefficients
post <- extract.samples(res)
w <- apply(post$w, 2, mean)
plot(NA, xlim=range(dat2$year), ylim=c(-6,6), xlab="year", ylab="basis * weight", bty="l")
for (i in 1:ncol(B)) lines(dat2$year, w[i]*B[,i], lwd=6, col="darkgray")
points(knots, rep(6, num.knots), pch=3, lwd=3)

# compute 95% compatibility intervals for the predicted mean as a function of year
mu <- link(res)
mu.ci <- apply(mu, 2, PI, 0.95)
mu <- apply(mu, 2, mean)

# Figure 3.14(bottom): plot the data again and add the intervals to the plot
plot(jitter(doy, amount=0.5) ~ year, data=dat, pch=21, bg="lightgray",
     bty="l", xlab="year", ylab="day")
abline(h=coef(res)["a"], lty="dashed", lwd=2)
lines(dat2$year, mu, lwd=3)
shade(mu.ci, dat2$year)

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-06-24
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 6.3.1 - 6.3.2
#
# last updated: 2022-06-25

############################################################################

# the pronounciation of 'the lasso':
#
# Tibshirani: https://youtu.be/FlSQgXv7Dvw?t=16
# Hastie:     https://youtu.be/ldqJ2sfosqA?t=365

############################################################################

### 6.3.1: Principal Components Regression

### An Overview of Principal Components Analysis

# we can simulate data like those shown in Figure 6.14 from a bivariate normal
# distribution, making some educated guesses about the true means, true
# variances, and the correlation between the two variables

# load the MASS package
library(MASS)

# simulate two

set.seed(1234)
S <- matrix(c(100,0.8*10*6,0.8*10*6,36), nrow=2, ncol=2)
dat <- mvrnorm(n=100, mu=c(40,20), Sigma=S)
dat <- data.frame(dat)
names(dat) <- c("pop", "ad")

plot(dat$pop, dat$ad, pch=19, col="violet", xlim=c(10,70), ylim=c(0,35),
     xlab="Population", ylab="Ad Spending", cex=1.5)

# however, I went ahead and extracted the data from the figure in the book
# using the juicr package (https://cran.r-project.org/package=juicr)

dat <- structure(list(pop = c(15.74, 18.96, 19.8, 23.37, 24.28, 24.46, 24.98,
26.42, 26.94, 27.43, 27.96, 28.17, 28.27, 28.55, 29.18, 29.29, 29.74, 30.23,
30.48, 30.55, 30.65, 31.07, 32.02, 32.33, 32.82, 32.86, 33.28, 33.91, 33.94,
34.19, 34.89, 34.96, 35.41, 35.48, 35.59, 36.25, 36.64, 37.16, 37.3, 37.48,
37.48, 38.04, 38.63, 38.98, 39.23, 39.26, 39.3, 39.65, 39.75, 39.89, 39.89,
40.28, 40.42, 40.7, 41.16, 41.4, 41.44, 41.44, 41.44, 41.51, 41.61, 42.07,
42.14, 42.21, 42.49, 42.77, 42.98, 43.01, 43.47, 43.96, 44.1, 44.45, 44.45,
44.97, 45.08, 45.36, 46.2, 46.37, 46.72, 47.88, 48.12, 48.19, 48.72, 49.03,
49.28, 49.49, 50.68, 52.15, 52.46, 52.99, 53.02, 53.3, 54.53, 54.74, 55.58,
55.82, 55.96, 58.2, 60.79, 61.77), ad = c(3.62, 9.32, 2.41, 11.54, 15.69,
11.05, 13.73, 10.6, 13.05, 11.01, 22.97, 20.06, 11.05, 8.56, 15.16, 22.06,
17.65, 13.31, 12.45, 15.35, 17.61, 11.62, 17.5, 15.65, 16.37, 19.35, 20.74,
16.71, 18.25, 17.39, 22.82, 17.05, 18.78, 21.95, 19.27, 15.8, 20.14, 18.52,
23.76, 13.2, 18.48, 15.5, 22.78, 17.69, 21.72, 25.27, 22.29, 18.25, 21.65,
11.62, 22.55, 19.54, 19.01, 22.93, 19.08, 19.35, 24.7, 15.99, 16.14, 22.86,
14.03, 15.84, 18.71, 18.86, 23.5, 24.67, 22.48, 24.55, 22.97, 30.1, 19.88,
20.22, 21.27, 21.84, 24.85, 23.31, 21.42, 27.53, 23.12, 20.78, 29.98, 19.27,
25.04, 22.14, 27.53, 27.61, 25.61, 30.47, 24.1, 31.98, 28.55, 28.78, 28.59,
29.57, 25.12, 29.46, 27.46, 33.57, 31.83, 32.28)), row.names = c(2L, 3L, 1L,
5L, 7L, 6L, 8L, 9L, 12L, 10L, 18L, 20L, 11L, 4L, 16L, 19L, 21L, 15L, 14L, 17L,
22L, 13L, 23L, 24L, 25L, 27L, 26L, 29L, 28L, 30L, 34L, 31L, 32L, 35L, 33L,
36L, 65L, 80L, 64L, 37L, 79L, 38L, 66L, 68L, 99L, 63L, 100L, 69L, 98L, 39L,
97L, 78L, 77L, 96L, 76L, 75L, 67L, 71L, 72L, 95L, 40L, 70L, 73L, 74L, 88L,
90L, 87L, 89L, 86L, 47L, 81L, 82L, 83L, 84L, 62L, 85L, 60L, 56L, 61L, 58L,
46L, 59L, 55L, 57L, 94L, 93L, 54L, 45L, 53L, 44L, 92L, 91L, 50L, 49L, 52L,
48L, 51L, 43L, 42L, 41L), class = "data.frame")

# Figure 6.14
plot(dat$pop, dat$ad, pch=19, col="violet", xlim=c(10,70), ylim=c(0,35),
     xlab="Population", ylab="Ad Spending", cex=1.5)

# let's see how good our guestimate of the correlation was
cor(dat$pop, dat$ad)

# mean center the two variables
dat$pop <- dat$pop - mean(dat$pop)
dat$ad  <- dat$ad  - mean(dat$ad)

# let's try some linear combinations (with the restriction that the two
# coefficients, phi1 and phi2, must be such that phi1^2 + phi2^2 = 1) and
# compute their variance

dat$z <- with(dat, 0.707*pop + 0.707*ad)
var(dat$z)

dat$z <- with(dat, 0.800*pop + 0.600*ad)
var(dat$z)

dat$z <- with(dat, 0.900*pop + 0.436*ad)
var(dat$z)

# the variance of the last linear combination is largest, so if we use the
# interpretation that 'variance = information', the last linear combination
# contains more information coming from the variables pop and ad

# what linear combination has the largest variance? let's find this manually

phi1s <- seq(-1, 1, length=100000)
vars  <- rep(NA, 100000)

for (i in 1:length(vars)) {

   phi1 <- phi1s[i]
   # note: phi2 must be sqrt(1-phi1^2) so that phi1^2 + phi2^2 = 1
   phi2 <- sqrt(1 - phi1^2)
   dat$z <- phi1*dat$pop + phi2*dat$ad
   vars[i] <- var(dat$z)

}

# remove variable z from the dataset
dat$z <- NULL

# plot the variance as a function of the phi1 coefficient
plot(phi1s, vars, type="l", xlab="Value for phi1", ylab="Variance of z")

# find the value of phi1 for which the variance is largest
phi1 <- phi1s[which.max(vars)]
round(phi1, digits=3)

# then the value of phi2
phi2 <- sqrt(1 - phi1^2)
round(phi2, digits=3)

# unfortunately, my attempt to extract the data from Figure 6.14 wasn't 100%
# accurate, so we do not get exactly the same values for phi1 and phi2 as in
# the book (0.839 and 0.544, respective), but we are quite close

# so now we can compute the linear combination that has the largest variance
dat$z1 <- phi1*dat$pop + phi2*dat$ad
var(dat$z1)

# variable z1 is the first principal component

# we can also 'extract' a second principal component, z2, that is again a
# linear combination of pop and ad that has the largest possible variance, but
# with the restriction that z1 and z2 must be uncorrelated; this could be done
# manually again as above, but this is tedious; we can in fact find the
# coefficients ('loadings') to construct the first and second principal
# components doing some linear algebra

# we can do such a principal component analysis (PCA) using princomp()
pca <- princomp(dat[c("pop", "ad")])
loadings(pca)
summary(pca)

# if we compare the standard deviation of z1 with what princomp() provides, we
# see a discrepancy; princomp() computes the standard deviation dividing by n
# and not n-1
sd(dat$z1)

# so if we do the same, we get the same SD for z1
sqrt(var(dat$z1) * 99 / 100)

# variables z1 and z2 are also computed by princomp()
head(pca$scores)

# we can compare this with 'our' z1 (which is just slightly different, because
# we used trial-and-error above to find the coefficients, but the calculations
# done by princomp() are in some sense 'exact')
head(dat$z1)

# add the principal components computed by princomp() to the dataset
dat$z1 <- pca$scores[,1]
dat$z2 <- pca$scores[,2]

# check that the correlation between z1 and z2 is really zero
round(cor(dat$z1, dat$z2), digits=8)

# Figure 6.15, right panel (but showing all points)
plot(dat$z1, dat$z2, pch=19, col="violet", xlim=c(-30,30), ylim=c(-10,10),
     xlab="1st Principal Component", ylab="2nd Principal Component")
abline(h=0)

# when doing PCA on two variables that are measured in different units, we
# typically first want to standardize both variables before doing a PCA

# standardize the two variables
dat$std.pop <- c(scale(dat$pop))
dat$std.ad  <- c(scale(dat$ad))

# run PCA again using the standardized variables
pca <- princomp(dat[c("std.pop", "std.ad")])
dat$z1 <- pca$scores[,1]
dat$z2 <- pca$scores[,2]

# Figures 6.16 and 6.17
par(mfrow=c(2,2))
plot(dat$z1, dat$pop, pch=19, col="violet", xlab="1st Principal Component", ylab="Population")
plot(dat$z1, dat$ad,  pch=19, col="violet", xlab="1st Principal Component", ylab="Ad Spending")
plot(dat$z2, dat$pop, pch=19, col="violet", xlab="2nd Principal Component", ylab="Population")
plot(dat$z2, dat$ad,  pch=19, col="violet", xlab="2nd Principal Component", ylab="Ad Spending")
par(mfrow=c(1,1))

# how well can we predict pop and ad from the principal components?
sav <- lm(pop ~ z1 + z2, data=dat)
fitted(sav)
dat$pop
sav <- lm(ad ~ z1 + z2, data=dat)
fitted(sav)
dat$ad

# perfectly! z1 and z2 contain all of the information from the original two
# variables; but z1 by itself accounts for 92% of the variance in pop and ad
sav <- lm(dat$pop ~ z1, data=dat)
summary(sav)
sav <- lm(dat$ad ~ z1, data=dat)
summary(sav)

############################################################################

### The Principal Components Regression Approach

# simulate data like those used in Figure 6.9

set.seed(59)

n <- 50
p <- 45

beta1 <- 1
beta2 <- 1

X <- replicate(p, c(scale(rnorm(n))))
y <- X[,1] * beta1 + X[,2] * beta2 + rnorm(n)

# fit the model including all predictors
res <- lm(y ~ X)
summary(res)

# simulate a large test dataset from the same true model
n.test <- 100000
X.test <- replicate(p, c(scale(rnorm(n.test))))
y.test <- X.test[,1] * beta1 + X.test[,2] * beta2 + rnorm(n.test)

# principal component analysis
pca <- princomp(X)

# extract the principal components in the training data
X.pca <- pca$scores

# use the first 5 principal components in a regression model
res <- lm(y ~ X.pca[,1:5])
summary(res)

# this is principal components regression

# but how many principal components should be used in the model?

# compute the principal components in the test data using the loadings as
# estimated from the training data
X.test.pca <- X.test %*% loadings(pca)

# compute the MSE based on the test data when fitting principal components
# regression models in the training data with increasing complexity (i.e.,
# including more and more of the principal components as predictors)

mse.test <- rep(NA, 45)

for (m in 1:45) {

   tmp <- lm(y ~ X.pca[,1:m])
   pred.test <- cbind(1,X.test.pca[,1:m]) %*% coef(tmp)
   mse.test[m] <- mean((y.test - pred.test)^2)

}

# plot the test MSE as a function of the number of components
plot(1:45, mse.test, type="o", pch=19, xlab="Number of Components",
     ylab="Mean Squared Error")

# this is like the violet line (for the test MSE) in Figure 6.18, right panel,
# but since we cannot simulate the same data as the authors, the figure will
# look different; also, the authors apparently used much larger coefficients
# for beta1 and beta2

############################################################################

# again simulate data with n=50 and p=45, but now make the outcome variable y
# a function of the first 5 principal components

set.seed(59)

n <- 50
p <- 45

beta1 <- 1
beta2 <- 1

X <- replicate(p, c(scale(rnorm(n))))
pca <- princomp(X)
X.pca <- pca$scores
y <- X.pca[,1:5] %*% cbind(rep(1,5)) + rnorm(n)

# fit the model including all predictors
res <- lm(y ~ X)
summary(res)

# simulate a large test dataset, compute the principal components in these
# data using the loadings as estimated from the training data, and then again
# let the outcome variable in the test data be a function of the first 5
# principal components
n.test <- 100000
X.test <- replicate(p, c(scale(rnorm(n.test))))
X.test.pca <- X.test %*% loadings(pca)
y.test <- X.test.pca[,1:5] %*% cbind(rep(1,5)) + rnorm(n.test)

mse.test <- rep(NA, 45)

# compute the MSE based on the test data

for (m in 1:45) {

   tmp <- lm(y ~ X.pca[,1:m])
   pred.test <- cbind(1,X.test.pca[,1:m]) %*% coef(tmp)
   mse.test[m] <- mean((y.test - pred.test)^2)

}

# plot the test MSE as a function of the number of components
plot(1:45, mse.test, type="o", pch=19, xlab="Number of Components",
     ylab="Mean Squared Error")

# now the test MSE is smallest when including 5 components in the principal
# components regression model

############################################################################

### 6.3.2: Partial Least Squares

# the data used below: https://www.statlearning.com/s/Advertising.csv
#download.file("https://www.statlearning.com/s/Advertising.csv", destfile="Advertising.csv")

# read in the data and examine the first 6 rows
dat <- read.csv("Advertising.csv")
head(dat)

# the goal is to predict sales from the TV, radio, and newspaper variables

# remove the X variable from dat
dat$X <- NULL

# fit the model with all three predictors
res <- lm(sales ~ TV + radio + newspaper, data=dat)
summary(res)

# standardize all variables
dat <- data.frame(scale(dat))

# in a simple regression model, when the outcome variable and the predictor
# are both standardized, then the intercept is equal to 0 and the regression
# coefficient is equal to the correlation between the two variables
lm(sales ~ 0 + TV, data=dat)
cor(dat$sales, dat$TV)

# manually do partial least squares regression

tmp <- dat

# the first component is the linear combination that uses the correlations
# between the 3 predictors and outcome variable as coefficients
tmp$z1 <- c(as.matrix(tmp[1:3]) %*% cor(tmp[1:3], tmp[4]))

# remove from each predictor that part that is accounted for by z1
tmp$TV        <- resid(lm(TV ~ z1, data=tmp))
tmp$radio     <- resid(lm(radio ~ z1, data=tmp))
tmp$newspaper <- resid(lm(newspaper ~ z1, data=tmp))

# then compute the second component in the same manner as above
tmp$z2 <- c(as.matrix(tmp[1:3]) %*% cor(tmp[1:3], tmp[4]))

# remove from each predictor that part that is accounted for by z2
tmp$TV        <- resid(lm(TV ~ z2, data=tmp))
tmp$radio     <- resid(lm(radio ~ z2, data=tmp))
tmp$newspaper <- resid(lm(newspaper ~ z2, data=tmp))

# then compute the third component in the same manner as above
tmp$z3 <- c(as.matrix(tmp[1:3]) %*% cor(tmp[1:3], tmp[4]))

# examine the first 6 rows from tmp
head(tmp)

# now use one or multiple components to predict the outcome variable (since
# the intercept must be 0, we can remove it from each model)

res.pls1 <- lm(sales ~ 0 + z1, data=tmp)
summary(res.pls1)

res.pls2 <- lm(sales ~ 0 + z1 + z2, data=tmp)
summary(res.pls2)

res.pls3 <- lm(sales ~ 0 + z1 + z2 + z3, data=tmp)
summary(res.pls3)

# do 10-fold cross-validation to determine the appropriate number of z's to keep

set.seed(1234)
folds <- sample(rep(1:10, each=20))

pred1 <- rep(NA, nrow(dat))
pred2 <- rep(NA, nrow(dat))
pred3 <- rep(NA, nrow(dat))

for (i in 1:10) {

   # split the dataset based on the ith fold into the training and the test data
   dat.train <- dat[folds != i,]
   dat.test  <- dat[folds == i,]

   tmp.train <- dat.train
   tmp.test  <- dat.test

   # note: z1 needs to be computed in the same way for the training and the test data
   tmp.train$z1 <- c(as.matrix(tmp.train[1:3]) %*% cor(tmp.train[1:3], tmp.train[4]))
   tmp.test$z1  <- c(as.matrix(tmp.test[1:3])  %*% cor(tmp.train[1:3], tmp.train[4]))

   # removing from each predictor that part that is accounted for by z1 must
   # also be done in the same way for the training and the test data (so we
   # use the coefficients from the fit to the training data to compute the
   # residuals for the test data)

   fit <- lm(TV ~ z1, data=tmp.train)
   tmp.train$TV <- resid(fit)
   tmp.test$TV  <- tmp.test$TV - predict(fit, newdata=tmp.test)

   fit <- lm(radio ~ z1, data=tmp.train)
   tmp.train$radio <- resid(fit)
   tmp.test$radio  <- tmp.test$radio - predict(fit, newdata=tmp.test)

   fit <- lm(newspaper ~ z1, data=tmp.train)
   tmp.train$newspaper <- resid(fit)
   tmp.test$newspaper  <- tmp.test$newspaper - predict(fit, newdata=tmp.test)

   # compute z2 (again in the same way for the test and the training data)
   tmp.train$z2 <- c(as.matrix(tmp.train[1:3]) %*% cor(tmp.train[1:3], tmp.train[4]))
   tmp.test$z2  <- c(as.matrix(tmp.test[1:3])  %*% cor(tmp.train[1:3], tmp.train[4]))

   # remove from each predictor that part that is accounted for by z2

   fit <- lm(TV ~ z2, data=tmp.train)
   tmp.train$TV <- resid(fit)
   tmp.test$TV  <- tmp.test$TV - predict(fit, newdata=tmp.test)

   fit <- lm(radio ~ z2, data=tmp.train)
   tmp.train$radio <- resid(fit)
   tmp.test$radio  <- tmp.test$radio - predict(fit, newdata=tmp.test)

   fit <- lm(newspaper ~ z2, data=tmp.train)
   tmp.train$newspaper <- resid(fit)
   tmp.test$newspaper  <- tmp.test$newspaper - predict(fit, newdata=tmp.test)

   # compute z3 (again in the same way for the test and the training data)
   tmp.train$z3 <- c(as.matrix(tmp.train[1:3]) %*% cor(tmp.train[1:3], tmp.train[4]))
   tmp.test$z3  <- c(as.matrix(tmp.test[1:3])  %*% cor(tmp.train[1:3], tmp.train[4]))

   # fit the three possible models using the training data
   res.pls1 <- lm(sales ~ 0 + z1,           data=tmp.train)
   res.pls2 <- lm(sales ~ 0 + z1 + z2,      data=tmp.train)
   res.pls3 <- lm(sales ~ 0 + z1 + z2 + z3, data=tmp.train)

   # predict the outcome based on these models for the test data
   pred1[folds == i] <- predict(res.pls1, newdata=tmp.test)
   pred2[folds == i] <- predict(res.pls2, newdata=tmp.test)
   pred3[folds == i] <- predict(res.pls3, newdata=tmp.test)

}

# compute the cross-validated MSE for the three models
mean((dat$sales - pred1)^2)
mean((dat$sales - pred2)^2)
mean((dat$sales - pred3)^2)

# this indicates that the cross-validated MSE will be lowest when keeping all
# three components in the partial least squares regression model

# for comparison with the pls package (see below), compute the cross-validated
# root mean squared error of the predictions (RMSEPs) for the three models
sqrt(mean((dat$sales - pred1)^2))
sqrt(mean((dat$sales - pred2)^2))
sqrt(mean((dat$sales - pred3)^2))

############################################################################

# the above is quite tedious; the pls package can automate all of this

# install and load the pls package
#install.packages("pls")
library(pls)

# partial least squares regression with 10-fold cross-validation
set.seed(1234)
res.pls <- plsr(sales ~ TV + radio + newspaper, data=dat,
                scale=TRUE, center=TRUE, validation="CV")
summary(res.pls)

# note that the cross-validated RMSEPs are very similar to the ones we
# computed manually above (we do not expect them to be identical, since the
# splitting of the data into the 10 folds is random)

############################################################################

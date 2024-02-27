############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-02-22
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 11.6
#
# last updated: 2024-02-27

############################################################################

## 11.7: Nonlinear least squares and maximum likelihood models

# note: also covered non-linear regression models during the stream session on
# 2022-09-22 and also a little bit on 2023-03-23, but the discussion below is
# more thorough

# 11.7.1: Least squares

# example data from Bates & Watts (1988), page 51
x <- c(0.02, 0.02, 0.06, 0.06, 0.11, 0.11, 0.22, 0.22, 0.56, 0.56, 1.10, 1.10)
y <- c(76, 47, 97, 107, 123, 139, 159, 152, 191, 201, 207, 200)

# scatterplot of x versus y
plot(x, y, pch=21, bg="gray", cex=1.5, ylim=c(40,220), bty="n")

# colors to be used below
cols <- palette.colors(6)

# naive fit a simple linear regression model
res1 <- lm(y ~ x)
summary(res1)

# compute predicted values based on the model for values of x between 0 and 1.2
xs <- seq(0, 1.2, length=100)
pred <- predict(res1, newdata=data.frame(x=xs))

# add the regression line based on the predicted values to the plot
lines(xs, pred, lwd=3, col=cols[1])

# fit a quadratic polynomial regression model
res2 <- lm(y ~ x + I(x^2))
summary(res2)

# compute predicted values based on the model for values of x between 0 and 1.2
pred <- predict(res2, newdata=data.frame(x=xs))

# add the regression line based on the predicted values to the plot
lines(xs, pred, lwd=3, col=cols[2])

# fit a cubic polynomial regression model
res3 <- lm(y ~ x + I(x^2) + I(x^3))
summary(res3)

# compute predicted values based on the model for values of x between 0 and 1.2
pred <- predict(res3, newdata=data.frame(x=xs))

# add the regression line based on the predicted values to the plot
lines(xs, pred, lwd=3, col=cols[3])

# in all of the models above, y is a linear function of the parameters and the
# corresponding predictors and hence is of the following general form:
#
# y = beta0 + beta1 * x1 + beta2 * x2 + beta3 * x3 + ... + error
#
# where error ~ N(0, sigma^2)
#
# see: https://en.wikipedia.org/wiki/Linear_regression
#
# lm() fits these models using 'least squares' estimation:
#
# https://en.wikipedia.org/wiki/Least_squares
# https://en.wikipedia.org/wiki/Linear_least_squares
# https://en.wikipedia.org/wiki/Ordinary_least_squares
#
# that is, we want to find the regression coefficients (beta0, beta1, etc.)
# that minimize the sum of the squared errors: SSE = sum((y - pred)^2)
#
# this can be done using 'closed-form equations' (which is what lm() does),
# but for illustration purposes, we can also do this by using numerical
# methods that iteratively find these coefficients

# fit function that computes the SSE for given values of beta[1] and beta[2]
fnreg <- function(beta, x, y) {
   pred <- beta[1] + beta[2] * x
   sum((y - pred)^2)
}

# find the values of beta[1] and beta[2] that minimize the SSE (note: p are
# the 'starting values', which ideally should not be totally off)
res1nlm <- nlm(fnreg, p=c(100,100), hessian=TRUE, x=x, y=y, print.level=2)
res1nlm

# compare the estimates found above with the ones found by lm()
coef(res1)

# note that we request nlm() above to provide us with the 'Hessian matrix'
# (https://en.wikipedia.org/wiki/Hessian_matrix); the inverse of the Hessian
# matrix is proportional to the variance-covariance matrix of the parameter
# estimates; to get the actual var-cov matrix for least squares estimation, we
# need to multiply the inverse by 2 * the estimated error variance; the latter
# we can get from SSE / (n-p), where n is the sample size and p is the number
# of parameters
#
# an explanation for this can be found here: https://max.pm/posts/hessian_ls/

# so we can obtain the standard errors of the regression coefficients as follows
sqrt(diag(2*res1nlm$minimum/(length(y)-2) * solve(res1nlm$hessian)))

# these are the same as what we get from lm()
summary(res1)

# note that there are various algorithms available in R for finding the
# coefficients that minimize some objective function
res1nlm <- optim(c(100,100), fnreg, hessian=TRUE, x=x, y=y)
res1nlm
res1nlm <- nlminb(c(100,100), fnreg, x=x, y=y)
res1nlm

# results may differ slightly

############################################################################

# however, at times we want to fit models where the relationship between y and
# x (or multiple predictors) is of a different functional form; suppose we
# assume that y is a function of x of the following form:
#
# y = beta1 * x / (beta2 + x) + error, where error ~ N(0, sigma^2)
#
# this is a non-linear regression model: https://en.wikipedia.org/wiki/Nonlinear_regression
#
# and this model in particular is the so-called Michaelis-Menten model:
# https://en.wikipedia.org/wiki/Michaelis-Menten_kinetics

# non-linear function that defines the shape of the relationship between x and y
predfun <- function(beta, x) beta[1] * x / (beta[2] + x)

# by looking at the scatterplot, we can make some educated guesses
# ('guestimates') about the value of y for different values of x:
#
# when x = 0.1, then y should be around 125
# when x = 1.0, then y should be around 200
#
# based on these guesses, we can solve for beta1 and beta2 as follows; we
# start by plugging x and y into our model equation:
#
# beta1 * 0.1 / (beta2 + 0.1) = 125
# beta1 * 1.0 / (beta2 + 1.0) = 200
#
# then we solve the first equation for beta1:
#
# beta1 * 0.1 = 125 * (beta2 + 0.1)
# beta1 = 1250 * (beta2 + 0.1)
#
# then we plug this into the second equation and solve for beta2:
#
# 1250 * (beta2 + 0.1) * 1.0 / (beta2 + 1.0) = 200
# (beta2 + 0.1) / (beta2 + 1.0) = 0.16
# (beta2 + 0.1) = 0.16 * (beta2 + 1.0)
# beta2 + 0.1 = 0.16 * beta2 + 0.16
# beta2 - 0.16 * beta2 = 0.16 - 0.1
# beta2 * 0.84 = 0.06
# beta2 = 0.06 / 0.84 = 0.07142857
#
# and now we use this value of beta2 to solve for beta1:
#
# beta1 = 1250 * (0.07142857 + 0.1)
# beta1 = 214.2857

# add the regression line to the plot based on these 'guestimates'
pred <- predfun(beta=c(214.2857, 0.07142857), x=xs)
lines(xs, pred, lwd=3, col=cols[4])

# not bad, but maybe we can do better; we will again use least squares
# estimation for this purpose:
#
# https://en.wikipedia.org/wiki/Non-linear_least_squares

# fit function that computes the SSE for given values of beta[1] and beta[2]
fn <- function(beta, x, y) {
   pred <- (beta[1] * x) / (beta[2] + x)
   sum((y - pred)^2)
}

# find the values of beta[1] and beta[2] that minimize the SSE
res4 <- nlm(fn, p=c(200, 0.1), hessian=TRUE, x=x, y=y)
res4

# add the regression line to the plot based on the best estimates
pred <- predfun(beta=res4$estimate, x=xs)
lines(xs, pred, lwd=3, col=cols[5])

# draw the points again
points(x, y, pch=21, bg="gray", cex=1.5)

# add a legend
legend("bottomright", inset=.02, lty=1, lwd=3, col=cols[1:5], bty="n",
       legend=c("Linear Regression Model", "Quadratic Polynomial Model",
                "Cubic Polynomial Model", "Non-Linear Model (guestimates)",
                "Non-Linear Model (actual estimates)"))

# our 'guestimates' were actually quite good

# let's compare the SSEs for the four models above
sum(resid(res1)^2)
sum(resid(res2)^2)
sum(resid(res3)^2)
sum((y - predfun(res4$estimate, x=x))^2)

# so the non-linear model yields the smallest SSE (and note that it only has
# two parameters, compared to the cubic model, which has 4 parameters)

############################################################################

# to get a better understanding of how the model fitting above works, we can
# also do a 'brute-force' search where we compute the SSE for many
# combinations of beta1 and beta2 (within a sensible range)

beta1s <- seq(190, 240, length=100)
beta2s <- seq(0.05, 0.1, length=100)

ssemat <- matrix(NA, nrow=length(beta1s), ncol=length(beta2s))

for (i in 1:length(beta1s)) {
   for (j in 1:length(beta1s)) {
      ssemat[i,j] <- fn(beta=c(beta1s[i], beta2s[j]), x=x, y=y)
   }
}

# create a perspective plot of the SEE values as a function of beta1 and beta2
tmp <- persp(x=beta1s, y=beta2s, z=ssemat, xlab="beta1", ylab="beta2", zlab="SSE",
             col="gray80", border="gray50", ticktype="detailed",
             theta=135, phi=25, shade=0.7, ltheta=60)

# we started nlm() at beta1=200 and beta2=0.1 above; show this point in the plot
cords <- trans3d(x=200, y=0.1, z=fn(beta=c(200, 0.1), x=x, y=y), pmat=tmp)
points(cords$x, cords$y, pch=19, cex=1.5)

# and via an iterative process it eventually finds the values of beta1 and
# beta2 that minimize the SSE; we can actually visualize the steps taken by
# the algorithm as follows

out <- capture.output(nlm(fn, p=c(200, 0.1), x=x, y=y, print.level=2))
sseit  <- out[grep("Function Value", out) + 1]
sseit  <- sapply(strsplit(sseit, " "), function(x) as.numeric(x[2]))
betait <- out[grep("Parameter", out) + 1]
betait <- t(sapply(strsplit(betait, " "), function(x) as.numeric(x[c(2,5)])))

cords1 <- cords
for (i in 1:length(sseit)) {
   cords2 <- trans3d(x=betait[i,1], y=betait[i,2], z=sseit[i], pmat=tmp)
   if (i >= 2)
      lines(c(cords1$x, cords2$x), c(cords1$y, cords2$y), lwd=3)
   points(cords2$x, cords2$y, pch=19)
   cords1 <- trans3d(x=betait[i,1], y=betait[i,2], z=sseit[i], pmat=tmp)
}

# as we saw above, the minimum SSE is at beta1=212.7 and beta2=0.064

# show this point in the plot
cords <- trans3d(x=res4$estimate[1], y=res4$estimate[2], z=res4$minimum, pmat=tmp)
points(cords$x, cords$y, pch=19, cex=1.5, col="red")

############################################################################

# note that the surface is quite flat around the minimum; so small changes in
# beta1 and/or beta2 would lead to similar SSE values; therefore we should not
# be that confident that the values found are accurate estimates of the true
# values of beta1 and beta2; to quantify their precision, we can again obtain
# their standard errors from the Hessian matrix
se <- sqrt(diag(2 * res4$minimum / (length(y) - 2) * solve(res4$hessian)))

# put the estimates, standard errors, and their ratio into a table
tab <- data.frame(beta=res4$estimate, se=se)
tab$zval <- tab$beta / tab$se
round(tab, 4)

# instead of doing the model fitting manually using nlm() as we have done
# above, we can use the nls() function
dat <- data.frame(x=x, y=y)
res5 <- nls(y ~ beta1 * x / (beta2 + x), start=c(beta1=200, beta2=0.1), data=dat)
options(scipen=100)
summary(res5)

# note: the SEs are slightly different to what we obtained above, as nls()
# appears to use a slightly different approach to computing the standard
# errors, but the difference is typically negligible, especially when the
# sample size gets a bit larger

# for commonly used non-linear models, there are so-called 'self-starting'
# functions that can be used instead of writing out the model as we did above;
# then we also do not have to specify starting values
res6 <- nls(y ~ SSmicmen(x, beta1, beta2), data=dat)
summary(res6)
options(scipen=0)

# again the results are just slightly different because when using SSmicmen(),
# analytic gradients are used for the model fitting instead of numerical ones

############################################################################

# 11.7.2: Maximum likelihood

# for now, let's stick to the model above; so as noted, the model is given by:
#
# y = beta1 * x / (beta2 + x) + error, where error ~ N(0, sigma^2)
#
# so E(y|x) = beta1 * x / (beta2 + x) = mu and Var(y|x) = Var(error) = sigma^2
# and the distribution of y|x is assumed to be normal
#
# so for a given value of x (and given values of beta1, beta2, and sigma^2),
# we can compute the density of the corresponding observed value of y; we can
# call these values f(y_i | x_i)
#
# we can do this for all of the data and then multiply these densities to
# obtain the 'joint density' of observing a particular set of y values given
# their corresponding x values (this assumes that the observed values of y are
# independent); so f(y_1 | x_1) * f(y_2 | x_2) * ... * f(y_n | x_n), which we
# call the 'likelihood'
#
# in maximum likelihood estimation, we want to find the values of the
# parameters (in the case above, beta1, beta2, and sigma^2) that maximize the
# likelihood; these are the so-called maximum likelihood estimates (MLEs);
# however, for numerical reasons, we will maximize the log likelihood, that
# is, log(f(y_1 | x_1) * f(y_2 | x_2) * ... * f(y_n | x_n)), which is
# identical to log(f(y_1 | x_1)) + log(f(y_2 | x_2)) + ... + log(f(y_n |
# x_n)); this is the so-called 'log likelihood'; finally, because the function
# we will use below for finding the parameter estimates does 'minimization',
# we will multiply the log likelihood by -1

# fit function that computes -1 * log likelihood
fnml <- function(par, x, y) {
   mean <- par[1] * x / (par[2] + x)
   var  <- par[3]
   -sum(dnorm(y, mean=mean, sd=sqrt(var), log=TRUE))
}

# use optim() to find the parameter estimates that minimize -1 * log likelihood
res7 <- optim(par=c(200,0.1,100), fnml, hessian=TRUE, x=x, y=y)
res7

# the square root of the estimated error variance is like the 'residual
# standard error' we saw in the output from nls() earlier (but the value below
# is the MLE of sigma, which differs slightly from the least squares estimate)
sqrt(res7$par[3])

# the inverse of the Hessian matrix directly gives an estimate of the
# variance-covariance matrix of the parameter estimates, so we can use this to
# obtain the standard errors of the parameter estimates
se <- sqrt(diag(solve(res7$hessian)))

# put the estimates, standard errors, and their ratio into a table
tab <- data.frame(par=res7$par, se=se)
tab$zval <- tab$par / tab$se
round(tab, 4)

# one problem when trying to find the values of the parameter estimates is
# that sigma^2 cannot be negative (since it is a variance, which must be >=0);
# this can cause problems when using optim(); although this didn't cause any
# problems above, we can avoid this issue by switching to a different
# algorithm that allows us to specify bounds
res8 <- optim(par=c(200,0.1,100), fnml, hessian=TRUE, x=x, y=y,
              method="L-BFGS-B", lower=c(-Inf,-Inf,0), upper=c(Inf,Inf,Inf))
se <- sqrt(diag(solve(res8$hessian)))
tab <- data.frame(par=res8$par, se=se)
tab$zval <- tab$par / tab$se
round(tab, 4)

# the results are very similar

# redraw the scatterplot of x versus y
plot(x, y, pch=21, bg="gray", cex=1.5, ylim=c(40,220), bty="n")

# add the regression line to the plot based on the MLEs
pred <- predfun(beta=res8$par, x=xs)
lines(xs, pred, lwd=3, col=cols[6])

############################################################################

# sidenote: the non-linear model we are using above is given by
#
# y = beta1 * x / (beta2 + x) + error, where error ~ N(0, sigma^2)
#
# which we can rewrite as follows:
#
# y = 1 / (1/beta1 + beta2/beta1 * 1/x) + error
#
# at the end of section 11.6.2 (and at the beginning of section 11.7), the
# manual talks about the possibility of fitting certain non-linear models
# using glm(), which we can actually do here; going back to the session on
# 2024-02-08 (see also section 11.6), note that the model above says that:
#
# eta = 1/beta1 + beta2/beta1 * 1/x
#
# (which is a linear function of 1/x) where the link function is the inverse
# function

# fit the model using glm()
res9 <- glm(y ~ I(1/x), family=gaussian(link=inverse))
summary(res9)

# however, note that the intercept reflects 1/beta1, so to get the estimated
# value of beta1, we have to take the inverse
1 / coef(res9)[1]

# and the slope coefficient reflects beta2/beta1, so to get the estimated
# value of beta2, we have to divide the slope coefficient by the intercept
coef(res9)[2] / coef(res9)[1]

# compare these values to the MLEs we obtained above
round(tab, 4)

############################################################################

# use 'local approximating regressions' (see section 11.8) for the same data

# redraw the scatterplot of x versus y
plot(x, y, pch=21, bg="gray", cex=1.5, ylim=c(40,220), bty="n")

# do local polynomial regression fitting using loess()
res10 <- loess(y ~ x)

# add the regression line to the plot based on the results
pred <- predict(res10, newdata=data.frame(x=xs))
lines(xs, pred, lwd=3, col=cols[1])

# increase the smoothness of the regression line
res10 <- loess(y ~ x, span=1)
pred <- predict(res10, newdata=data.frame(x=xs))
lines(xs, pred, lwd=3, col=cols[2])

############################################################################

# let's replicate the example from section 11.7.2 from the manual

# create the dataset
x <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113, 1.8369, 1.8610, 1.8839)
y <- c( 6, 13, 18, 28, 52, 53, 61, 60)
n <- c(59, 60, 62, 56, 63, 59, 62, 60)

# as discussed in the session on 2024-02-08, we could use glm() to fit a
# logistic regression model to such data
res1 <- glm(cbind(y,n-y) ~ x, family=binomial)
summary(res1)

# we can do the maximum likelihood estimation ourselves; note that in logistic
# regression with a logit link function, the model predicts the log odds of a
# success for a given value of x; the inverse of this function is plogis(),
# which then gives the probability of a success; we then plug this probability
# into dbinom(), which yields the density for the corresponding observed value
# of y; as above, we log transform these values, sum them up, and multiply the
# log likelihood by -1

# fit function that computes -1 * log likelihood
fnml <- function(par, x, y, n) {
   prob <- plogis(par[1] + par[2] * x)
   -sum(dbinom(y, n, prob, log=TRUE))
}

# use optim() to find the parameter estimates that minimize -1 * log likelihood
res2 <- optim(par=c(0,0), fnml, hessian=TRUE, x=x, y=y, n=n)

# obtain the standard errors of the MLEs
se <- sqrt(diag(solve(res2$hessian)))

# put the estimates, standard errors, and their ratio into a table
tab <- data.frame(par=res2$par, se=se)
tab$zval <- tab$par / tab$se
round(tab, 3)

# these are essentially the same results as we obtained above with glm()

# compare the log likelihoods
logLik(res1)
-1 * res2$value

############################################################################

## 11.8: Some non-standard models

# see above for an example of using loess(); fitting the other models is
# beyond the scope of the introduction manual

############################################################################

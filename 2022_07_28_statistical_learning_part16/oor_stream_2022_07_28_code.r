############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-07-28
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 6.4.1 - 6.4.4
#
# last updated: 2022-08-04

############################################################################

### 6.4.2: What Goes Wrong in High Dimensions?

# simulate data like in Figure 6.22 (left panel)

set.seed(1234)

n <- 20
x <- runif(n, -2, 2)
y <- 0 + 5 * x + rnorm(n, 0, 4)

plot(x, y, pch=19, cex=1.5, col="red")

res <- lm(y ~ x)
summary(res)
abline(res, lwd=5)
resid(res)

# simulate data like in Figure 6.22 (right panel)

set.seed(1299)

n <- 2
x <- runif(n, -2, 2)
y <- 0 + 5 * x + rnorm(n, 0, 4)

plot(x, y, pch=19, cex=1.5, col="red", xlim=c(-2,2), ylim=c(-10,10))

res <- lm(y ~ x)
summary(res)
abline(res, lwd=5)
resid(res)

# since n is equal to the number of regression coefficients (intercept plus
# slope), the model fits perfectly, R^2 = 1, and all of the residuals are
# equal to 0

############################################################################

# simulate data where n = 20, p = 20, and none of the predictors are actually
# related to the outcome variable

set.seed(1319)

n <- 20
p <- 20
X <- replicate(p, runif(n, -2, 2))
y <- 0 + rnorm(n)

# simulate a large test dataset based on the same model

n.test <- 100000
X.test <- replicate(p, runif(n.test, -2, 2))
y.test <- 0 + rnorm(n.test)

# fit regression models with 0, 1, 2, ..., p predictors, get R^2, the MSE, and
# the test MSE for these models (and the adjusted R^2, AIC, and BIC)

R2       <- rep(NA, p+1)
MSE      <- rep(NA, p+1)
MSE.test <- rep(NA, p+1)
R2.adj   <- rep(NA, p+1)
AIC      <- rep(NA, p+1)
BIC      <- rep(NA, p+1)

for (i in 0:p) {

   if (i == 0) {
      res <- lm(y ~ 0)
   } else {
      res <- lm(y ~ 0 + X[,1:i])
   }

   R2[i+1]     <- summary(res)$r.squared
   MSE[i+1]    <- mean((y - fitted(res))^2)
   R2.adj[i+1] <- summary(res)$adj.r.squared
   AIC[i+1]    <- AIC(res)
   BIC[i+1]    <- BIC(res)

   if (i == 0) {
      pred.test <- rep(0, n.test)
   } else {
      pred.test <- X.test[,1:i,drop=FALSE] %*% coef(res)
   }

   MSE.test[i+1] <- mean((y.test - pred.test)^2)

}

# like Figure 6.23 (but also include adjusted R^2, AIC, and BIC)

par(mfrow=c(3,2), mar=c(5,4.5,2,2))
plot(0:p, R2,       type="l", xlab="Number of Variables", ylab=expression(R^2),      xlim=c(0,20), lwd=3)
plot(0:p, MSE,      type="l", xlab="Number of Variables", ylab="Training MSE",       xlim=c(0,20), lwd=3)
plot(0:p, MSE.test, type="l", xlab="Number of Variables", ylab="Test MSE",           xlim=c(0,20), lwd=3)
plot(0:p, R2.adj,   type="l", xlab="Number of Variables", ylab=expression(R[adj]^2), xlim=c(0,20), lwd=3)
plot(0:p, AIC,      type="l", xlab="Number of Variables", ylab="AIC",                xlim=c(0,20), lwd=3)
plot(0:p, BIC,      type="l", xlab="Number of Variables", ylab="BIC",                xlim=c(0,20), lwd=3)

# note: with AIC and BIC, 'smaller is better' (i.e., models where the AIC/BIC
# is smaller strike a good balance between fit and complexity)

# note: actually, adjusted R^2 seems to work well here in suggesting that the
# intercept-only model is best; the AIC and BIC values also initially increase
# (again suggesting that one should not include predictors), although when p
# gets large, then suddenly these values drop down, leading to the false
# impression that including many predictors is appropriate (so one would have
# to restrict one's choice of models to those where p isn't getting too close
# to n); but these statistics definitely won't work anymore for model
# selection when n=p or p>n

############################################################################

### 6.4.3: Regression in High Dimensions

# conduct a simulation similar to what is described in this section for the
# case where p = 50 (it isn't entirely clear based on the description in the
# book what the authors did, so the code below is my attempt to create a
# similar simulation; also, we will include forward stepwise selection as an
# alternative model selection strategy)

# note: the code below takes a while to run, so be patient ...

# install and load the glmnet package
#install.packages("glmnet")
library(glmnet)

# install and load the leaps package
#install.packages("leaps")
library(leaps)

set.seed(5678)

iters <- 1000

n <- 100
p <- 50

beta <- 0.2

mat.dfs          <- matrix(NA, nrow=iters, ncol=p+1)
mat.MSE.test     <- matrix(NA, nrow=iters, ncol=p+1)
mat.MSE.test.fws <- matrix(NA, nrow=iters, ncol=p)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (i in 1:iters) {

   setTxtProgressBar(pbar, i)

   # simulate training data
   X <- replicate(p, runif(n, -2, 2))
   y <- 0 + c(X[,1:20] %*% rep(beta,20)) + rnorm(n)

   # simulate test data
   n.test <- 10000
   X.test <- replicate(p, runif(n.test, -2, 2))
   y.test <- 0 + c(X.test[,1:20] %*% rep(beta,20)) + rnorm(n.test)

   # use glmnet() to do lasso regression
   res <- glmnet(X, y)

   # note: glmnet() provides lasso fits for many different values of lambda
   # (from very strong shrinkage to essentially none)

   # compute predicted values in the test data for each lambda value
   pred <- predict(res, newx=X.test)

   # for every lambda value, compute the test MSE
   MSE.test <- apply(pred, 2, function(y.pred) mean((y.test - y.pred)^2))

   # extract the 'degrees of freedom' (non-zero coefficients) corresponding to
   # the various lambda values that glmnet() used
   dfs <- res$df

   # since we might have multiple degrees of freedom values that correspond to
   # different lambda values, take the smallest corresponding test MSE value
   # (so take the MSE for the level of shrinkage just needed to reach a given
   # dfs) and store this in the appropriate column of the mat.MSE.test matrix

   udfs <- unique(dfs)
   mat.dfs[i,udfs+1] <- udfs

   tmp <- tapply(MSE.test, dfs, min)
   mat.MSE.test[i,udfs+1] <- tmp

   # use forward stepwise selection in the training data
   res <- regsubsets(x=X, y=y, nvmax=p, method="forward")

   # for each level of complexity, find the best model in the training data
   # and compute the test MSE based on that model

   for (j in 1:p) {
      incl <- summary(res)$which[j,][-1]
      fit  <- lm(y ~ X[,incl])
      y.pred <- cbind(1,X.test[,incl]) %*% coef(fit)
      mat.MSE.test.fws[i,j] <- mean((y.test - y.pred)^2)
   }

}

close(pbar)

ylims <- c(min(cbind(mat.MSE.test, mat.MSE.test.fws), na.rm=TRUE),
           max(cbind(mat.MSE.test, mat.MSE.test.fws), na.rm=TRUE))

# like Figure 6.24 (middle panel) and also draw the analogous figure for
# forward stepwise selection

par(mfrow=c(1,2))
boxplot(mat.MSE.test,     cex=0.5, xlab="Degrees of Freedom", ylab="Test MSE", ylim=ylims, main="Lasso")
boxplot(mat.MSE.test.fws, cex=0.5, xlab="Degrees of Freedom", ylab="Test MSE", ylim=ylims, main="Stepwise Selection")

# conclusions: we know that the true model has 20 true predictors; however,
# the median test MSE based on lasso regression is lowest for models with a
# higher complexity (with around 30-35 non-zero coefficients); on the other
# hand, the median test MSE based on forward stepwise selection is lowest for
# models with around 20 predictors, which is appropriate; however, the test
# MSE is generally lower when using lasso regression and the test MSE is more
# variable for forward stepwise selection

# note: when running the code above with n <- 1000 (so with a larger training
# dataset), then forward stepwise selection outperforms lasso regression (the
# median test MSE is lowest for models with 20 predictors and it is lower at
# that point than the lowest median test MSE of lasso regression, which still
# suggests a model with too high complexity)

############################################################################

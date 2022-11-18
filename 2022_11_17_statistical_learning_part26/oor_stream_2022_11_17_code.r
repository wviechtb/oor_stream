############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-11-17
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 10.1 - 10.2
#
# last updated: 2022-11-18

############################################################################

### 10.1: Single Layer Neural Networks

# let's try to fit a single layer neural network 'by hand'; we will use the
# 'Hitters' dataset that we have used in some of the previous chapters

# install (if necessary) the ISLR2
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# copy the Hitters dataset to dat
dat <- Hitters

# keep rows where Salary is not missing
dat <- dat[!is.na(dat$Salary),]

# create log transformed Salary variable (Salary is very right-skewed, so this
# helps to make the distribution a bit more symmetric)
dat$logSalary <- log(dat$Salary)

# first we just do a regression model with log(Salary) as outcome and
# variables Years, Hits, HmRun, and Runs as predictors
res <- lm(logSalary ~ Years + Hits + HmRun + Runs, data=dat)
summary(res)

# further below, it will be useful if all variables are scaled similarly; we
# could standardize the variables but instead we rescale them to have a range
# of 0-1; the following function will do this
rescale <- function(x) (x - min(x)) / (max(x) - min(x))

# rescale all variables
dat$rlogSalary <- rescale(dat$logSalary)
dat$rYears     <- rescale(dat$Years)
dat$rHits      <- rescale(dat$Hits)
dat$rHmRun     <- rescale(dat$HmRun)
dat$rRuns      <- rescale(dat$Runs)

# fit the same model as above but using the rescaled predictors
res <- lm(rlogSalary ~ rYears + rHits + rHmRun + rRuns, data=dat)
summary(res)

# note that the fit of the model is the same (we get the same R^2 values and
# the t/F statistics have not changed)

# compute the sum of the squared residuals around the mean; we can consider
# this the 'baseline' performance (i.e., it doesn't use any of the information
# from the features, that is, this is equivalent to fitting a regression model
# with just an intercept)
sse.mean <- sum((dat$rlogSalary - mean(dat$rlogSalary))^2)
sse.mean

# compute the sum of the squared residuals based on the regression model
sse.lm <- sum((dat$rlogSalary - predict(res))^2)
sse.lm

# quite a bit lower (hence we also get an R^2 of 0.4885), which we can also
# compute manually as follows
round(1 - sse.lm / sse.mean, digits=4)

############################################################################

# now let's try a neural network with the same 4 features as input and a
# single hidden layer with 5 units (like in Figure 10.1); let 'par' denote the
# vector with the parameters of this network with the following structure

# par = [beta0, beta1, beta2, beta3, beta4, beta5,
#        w00, w01, w02, w03, w04,
#        w10, w11, w12, w13, w14,
#        ...
#        w50, w51, w52, w53, w54]

# we will use the 'sigmoid' activation function
g <- plogis # same as 1 / (1 + exp(-z))

# the fit or objective function

fitfun <- function(par, y, x1, x2, x3, x4, lambda=0) {

   # equation 10.2 (for the activation of the hidden units)
   A1 <- g(par[7]  + par[8]*x1  + par[9]*x2  + par[10]*x3 + par[11]*x4)
   A2 <- g(par[12] + par[13]*x1 + par[14]*x2 + par[15]*x3 + par[16]*x4)
   A3 <- g(par[17] + par[18]*x1 + par[19]*x2 + par[20]*x3 + par[21]*x4)
   A4 <- g(par[22] + par[23]*x1 + par[24]*x2 + par[25]*x3 + par[26]*x4)
   A5 <- g(par[27] + par[28]*x1 + par[29]*x2 + par[30]*x3 + par[31]*x4)

   # equation 10.3 (regression model of the output on the hidden units)
   yhat <- par[1] + par[2]*A1 + par[3]*A2 + par[4]*A3 + par[5]*A4 + par[6]*A5

   # objective we want to minimize (squared error loss); later on, we will make
   # use of 'regularization' (when lambda > 0) but for now we can ignore this
   obj <- sum((y - yhat)^2) + lambda*sum(par^2)
   return(obj)

}

# try to find the parameter values that minimize the squared error loss; note
# that we have to set 'starting values' for the parameter estimates; let's try
# just setting all estimates to 0

res <- nlminb(rep(0,31), fitfun, y=dat$rlogSalary,
              x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns)
res

# note that the iteration limit was reached without convergence; so let's try
# increasing the maximum number of iterations; and while we are at it, let's
# also increase the maximum number of function evaluations

res <- nlminb(rep(0,31), fitfun, y=dat$rlogSalary,
              x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns,
              control=list(iter.max=10000, eval.max=10000))
res

# now we do get convergence (0 indicates successful convergence)

# parameter estimates
round(res$par, digits=2)

# note that the value of the objective function (i.e., the sum of the squared
# residuals) is much smaller here compared to what we got from our regression
# model

# some of the parameter estimates are quite large; in fact, this is a rather
# difficult optimization problem, as there are likely to be many 'local
# minima' (see section 10.7, which discusses this issue in a bit more detail)

# try a different optimizer; in this case the Nelder-Mead algorithm from
# optim(); again, need to increase the maximum number of iterations to achieve
# convergence

res <- optim(rep(0,31), fitfun, y=dat$rlogSalary,
              x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns,
              control=list(maxit=20000))
res

# we see that the value of the objective function at the solution that was
# found is worse than what we got from nlminb(), so this must be a local
# minimum (the solution from nlminb() is probably not the global minimum
# either, but we don't know that)

# try yet another optimizer; in this case the BFGS algorithm

res <- optim(rep(0,31), fitfun, y=dat$rlogSalary,
              x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns,
              control=list(maxit=20000), method="BFGS")
res

# okay, that just did not work (note that the value of the objective function
# is the same as sse.mean, so this is a terrible solution); but maybe the
# starting values are not so good; try setting all estimates to 1

res <- optim(rep(1,31), fitfun, y=dat$rlogSalary,
              x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns,
              control=list(maxit=20000), method="BFGS")
res

# much better and in fact now we get a solution that is even better than the
# one from nlminb(); so, as we can see, depending on the starting values, we
# might end up with a different solution

# let's go back to nlminb(), trying out a whole bunch of different (random)
# starting values; note that this takes quite a while to run (you can also set
# 'iters' to something smaller if you are not so patient)

set.seed(1234)

iters <- 100

# to store the parameter estimates and the value of the objective function
pars <- matrix(NA, nrow=iters, ncol=31)
values <- rep(NA, iters)

pbar <- txtProgressBar(min=0, max=iters, style=3)

for (i in 1:iters) {

   setTxtProgressBar(pbar, i)

   res <- nlminb(runif(31,-1,1), fitfun, y=dat$rlogSalary,
                 x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns,
                 control=list(iter.max=10000, eval.max=10000))

   if (res$convergence != 0) # skip if we don't get convergence
      next

   pars[i,]  <- res$par
   values[i] <- res$objective

}

close(pbar)

# examine the values of the objective function
values

# the best solution found has this value
min(values, na.rm=TRUE)

# examine the parameter estimates (each row is a different set)
round(pars, digits=2)

# the parameter estimates can differ a lot depending on the starting values,
# although the values of the objective function do not differ that greatly
# across the various solutions

############################################################################

# compare the above to nnet() from the package of the same name

library(nnet)

res <- nnet(rlogSalary ~ rYears + rHits + rHmRun + rRuns,
            data=dat, size=5, linout=TRUE, maxit=1000)
res
sum((dat$rlogSalary - res$fitted.values)^2)

# examine the parameter estimates
summary(res)

# similar objective value as what we found above

############################################################################

# as discussed in section 10.7, one of the methods to improve the fitting of a
# neural network is to use 'regularization' (like in ridge regression and the
# lasso as we discussed in chapter 6); let's try this

res <- nlminb(rep(0,31), fitfun, lambda=10^-3, y=dat$rlogSalary,
              x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns,
              control=list(iter.max=10000, eval.max=10000))
res

res <- nnet(rlogSalary ~ rYears + rHits + rHmRun + rRuns,
            data=dat, size=5, linout=TRUE, maxit=1000, decay=10^-3)
summary(res)

# quite similar in terms of the objective function value and the parameter
# estimates that we get from nlminb() do not include very extreme values
# anymore

# conclusion: when trying to minimize the squared error loss, then there are
# many possible solutions that give very similar values for the objective
# function and the parameter estimates can also drift away far from 0; adding
# a bit of regularization helps to 'stabilize' the estimates

############################################################################

# note: above we did not bother with creating a model fitting and a model
# testing dataset to examine the performance of a particular model on data
# that was not used in the model fitting; in practice, this would of course be
# an important part of trying out different models

############################################################################

# now let's see if we can fit this model using keras; note that setting up
# keras requires installing some additional software (python and tensorflow)
# and the process of doing this depends on your operating system; the book
# provides these instructions:
#
# https://hastie.su.domains/ISLR2/keras-instructions.html
#
# there are also these instructions from RStudio:
#
# https://tensorflow.rstudio.com/install/

# install the keras R package
#install.packages("keras")

# load the keras package
library(keras)

# check if tensorflow is working
tensorflow::tf$keras

# can also use this help function that ISLR2 provides
source(system.file("helpers", "install.R", package = "ISLR2"))
print_py_config()

# create matrix with the predictors and a vector with the outcomes
X <- cbind(x1=dat$rYears, x2=dat$rHits, x3=dat$rHmRun, x4=dat$rRuns)
y <- dat$rlogSalary
n <- length(y)

# set up the model
model <- keras_model_sequential() |>
   layer_dense(units=5, input_shape=4, activation="sigmoid") |>
   layer_dense(units=1) |>
   compile(optimizer="adam", loss="mse")

# fit the model (takes a long time)
res <- fit(model, X, y, epochs=50000, batch_size=n, verbose=FALSE)
res

# some notes: needed to switch optimizer, use a large value for epochs, and
# use a batch_size equal to the sample size to get similar performance as we
# did earlier

# compute the sum of the squared residuals
sum((y - predict(model, X, verbose=FALSE))^2)

# the 'loss' in the output is essentially this (mean squared error)
mean((y - predict(model, X, verbose=FALSE))^2)

# examine loss over epochs
plot(res)

# it seems that the default learning rate of the optimizer is a bit low for
# this example, which is the reason a very large number of epochs are needed
# above to get similar performance as we did earlier; however, I could only
# get the following to run with the following trick (this may be a particular
# issue with my setup)

keras$optimizers$Adam <- keras$optimizers$legacy$Adam

# set up the model
model <- keras_model_sequential() |>
   layer_dense(units=5, input_shape=4, activation="sigmoid") |>
   layer_dense(units=1) |>
   compile(optimizer=optimizer_adam(learning_rate=0.1), loss="mse")

# fit the model (now can use a smaller value for epochs)
res <- fit(model, X, y, epochs=5000, batch_size=n, verbose=FALSE)
sum((y - predict(model, X, verbose=FALSE))^2)

# add regularization as we did earlier
model <- keras_model_sequential() |>
   layer_dense(units=5, input_shape=4, activation="sigmoid", kernel_regularizer=regularizer_l2(0.001)) |>
   layer_dense(units=1) |>
   compile(optimizer=optimizer_adam(learning_rate=0.1), loss="mse")

res <- fit(model, X, y, epochs=5000, batch_size=n, verbose=FALSE)
sum((y - predict(model, X, verbose=FALSE))^2)

# note: not exactly the same, since this seems to add regularization to the
# weights from the inputs to the hidden layer, but we earlier used the sum of
# all coefficients squared in the regularization; the performance is also
# worse than what we found earlier; maybe adjusting the learning rate and/or
# epochs may help

############################################################################

### 10.2: Multilayer Neural Networks

# dataset: http://yann.lecun.com/exdb/mnist/

# code to get these data into R from:
# https://gist.github.com/daviddalpiaz/ae62ae5ccd0bada4b9acd6dbc9008706

# note: these data actually come with the keras package (see section 10.9.2)
# but here we will manually get these data and read them into R

download.file("http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz", destfile="train-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz", destfile="train-labels-idx1-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz",  destfile="t10k-images-idx3-ubyte.gz")
download.file("http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz",  destfile="t10k-labels-idx1-ubyte.gz")

#install.packages("R.utils")

R.utils::gunzip("train-images-idx3-ubyte.gz")
R.utils::gunzip("train-labels-idx1-ubyte.gz")
R.utils::gunzip("t10k-images-idx3-ubyte.gz")
R.utils::gunzip("t10k-labels-idx1-ubyte.gz")

# helper function for visualization
show_digit <- function(arr784, col = gray(12:1 / 12), ...)
   image(matrix(as.matrix(arr784[-785]), nrow = 28)[, 28:1], col = col, ...)

# load image files
load_image_file <- function(filename) {
   ret = list()
   f = file(filename, 'rb')
   readBin(f, 'integer', n = 1, size = 4, endian = 'big')
   n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
   nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
   ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
   x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
   close(f)
   data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file <- function(filename) {
   f = file(filename, 'rb')
   readBin(f, 'integer', n = 1, size = 4, endian = 'big')
   n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
   y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
   close(f)
   y
}

# load images
dat.train <- load_image_file("train-images-idx3-ubyte")
dat.test  <- load_image_file("t10k-images-idx3-ubyte")

# rescale the grayscale values to the 0-1 range (they are 0-255 in the data)
dat.train <- dat.train / 255
dat.test  <- dat.test  / 255

# add labels
dat.train$y <- as.factor(load_label_file("train-labels-idx1-ubyte"))
dat.test$y  <- as.factor(load_label_file("t10k-labels-idx1-ubyte"))

# view test image
show_digit(dat.train[3,])
dat.train.label[3]

############################################################################

# first we want to fit the multinomial logistic regression model; in principle,
# this can be done with the multinom() function from the nnet package (which
# uses the trick that a neural network can be used to fit such a model)

# multinomial regression predicting y from all features
res <- multinom(y ~ ., data=dat.train)

# unfortunately, this fails (too many predictors) ...

# could try fitting this with glmnet

# install.packages("glmnet")
library(glmnet)
res <- glmnet(dat.train[-785], dat.train[[785]], family="multinomial", lambda=0, standardize=FALSE, trace.it=1)

# after a long time, this crashed ...

# try to do linear discriminant analysis on this dataset; to get this to work,
# I had to keep predictors (i.e., pixels) that had at least a non-zero value
# within each of the digit levels; the following convoluted code figures out
# which columns are like that
notconst <- apply(as.data.frame(lapply(split(dat.train[-785], dat.train$y), apply, 2, function(x) any(x > 0))), 1, all)
sum(notconst) # so we are left with 425 predictors

# keep only those predictors in the training and test datasets
dat.train.sub <- dat.train[,c(notconst,TRUE)]
dat.test.sub  <- dat.test[,c(notconst,TRUE)]

# LDA using this subset of predictors
library(MASS)
res <- lda(y ~ ., data=dat.train.sub)

# predict the class in the test data
pred <- predict(res, newdata=dat.test.sub)

# test error rate (almost like the 12.7% as in the book)
mean(dat.test.sub$y != pred$class)

############################################################################

# create matrix with the predictors and a vector with the outcomes
X <- as.matrix(dat.train[-785])
y <- to_categorical(dat.train[[785]], 10)

# set up the model (using ridge regularization)
model <- keras_model_sequential() |>
   layer_dense(units=256, input_shape=784, activation="relu", kernel_regularizer=regularizer_l2(0.001)) |>
   layer_dense(units=128, activation="relu", kernel_regularizer=regularizer_l2(0.001)) |>
   layer_dense(units=10, activation="softmax") |>
   compile(optimizer="rmsprop", loss="categorical_crossentropy", metrics="accuracy")

# examine model
model

# fit the model
system.time(res <- fit(model, X, y, epochs=30, batch_size=128, validation_split=0.2))

# examine the loss and accuracy over epochs
plot(res)

# predicted probabilities for each digit in the test data
pred <- predict(model, as.matrix(dat.test[-785]))

# for each row in this 10 column matrix, find which probability is the largest
# (and subtract 1 to get the predicted digits between 0 and 9)
pred <- apply(pred, 1, which.max) - 1

# test error rate
mean(dat.test$y != pred)

# set up the model (using dropout regularization)
model <- keras_model_sequential() |>
   layer_dense(units=256, input_shape=784, activation="relu") |>
   layer_dropout(rate=0.4) |>
   layer_dense(units=128, activation="relu") |>
   layer_dropout(rate=0.3) |>
   layer_dense(units=10, activation="softmax") |>
   compile(optimizer="rmsprop", loss="categorical_crossentropy", metrics="accuracy")

# examine model
model

# fit the model
system.time(res <- fit(model, X, y, epochs=30, batch_size=128, validation_split=0.2))

# examine the loss and accuracy over epochs
plot(res)

# predicted probabilities for each digit in the test data
pred <- predict(model, as.matrix(dat.test[-785]))

# predicted digit
pred <- apply(pred, 1, which.max) - 1

# test error rate
mean(dat.test$y != pred)

############################################################################

# use keras to fit the multinomial logistic regression model

# set up the model
model <- keras_model_sequential() |>
   layer_dense(units=10, input_shape=784, activation="softmax") |>
   compile(optimizer="rmsprop", loss="categorical_crossentropy", metrics="accuracy")

# examine model
model

# fit the model
system.time(res <- fit(model, X, y, epochs=30, batch_size=128, validation_split=0.2))

# test error rate
pred <- predict(model, as.matrix(dat.test[-785]))
pred <- apply(pred, 1, which.max) - 1
mean(dat.test$y != pred)

############################################################################

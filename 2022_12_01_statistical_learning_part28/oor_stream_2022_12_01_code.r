############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-12-01
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 10.5
#
# last updated: 2022-12-08

############################################################################

# to supress info and warning messages when loading tensorflow
Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "2")

### 10.5: Recurrent Neural Networks

### 10.5.1: Sequential Models for Document Classification

# load the keras package
library(keras)

# load the imdb dataset
imdb <- dataset_imdb(num_words=10000)

# get the word index
word_index <- dataset_imdb_word_index()

# copy the training data and labels into x_train and y_train
x_train <- imdb$train$x
y_train <- imdb$train$y

# do the same for the test data
x_test <- imdb$test$x
y_test <- imdb$test$y

# function to decode a review
decode_review <- function(text, word_index) {
   word <- names(word_index)
   idx <- unlist(word_index, use.names = FALSE)
   word <- c("<PAD>", "<START>", "<UNK>", "<UNUSED>", word)
   idx <- c(0:3, idx + 3)
   words <- word[match(text, idx, 2)]
   paste(words, collapse = " ")
}

# the first review (numbers correspond to the words used)
x_train[[1]]

# decode this review
decode_review(x_train[[1]], word_index)

# get the word count for each review in the training data
wc <- sapply(x_train, length)

# some summary statistics on these word counts
mean(wc)
sd(wc)
range(wc)
median(wc)

# histogram of the word counts
hist(wc, breaks=100, xlab="Word Count", main="")

# proportion of reviews that have 500 words or less
mean(wc <= 500)

# restrict the sequence of words to the last 500 and pad shorter sequences
# with zeros at the beginning
maxlen <- 500
x_train <- pad_sequences(x_train, maxlen = maxlen)
x_test  <- pad_sequences(x_test, maxlen = maxlen)

# pad_sequences() turns the list of sequences into a matrix
dim(x_train)
dim(x_test)

# look at the first row (for the first review)
x_train[1,]

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model (just a regular RNN)
model <- keras_model_sequential() |>
   layer_embedding(input_dim=10000, output_dim=32) |>
   layer_simple_rnn(units=32) |>
   layer_dense(units=1, activation="sigmoid") |>
   compile(optimizer="rmsprop", loss="binary_crossentropy", metrics="accuracy")

# note: while x_train is a matrix with 25,000 rows (for the different reviews)
# and 500 columns (giving the sequence of numbers for the 500 last words used
# in each review), these numbers actually correspond to 10,000 dummy variables
# (since there are 10,000 possible words); therefore, the input dimension for
# layer_embedding() is not 500, but 10,000

# summary information about the model
summary(model)

# save the initial weights
weights <- get_weights(model)

# fit the model (setting 20% of the data aside for validation)
res <- fit(model, x_train, y_train, epochs=10, batch_size=128, validation_split=0.2)

# plot the loss/accuracy as a function of epochs
plot(res)

# maybe we are overtraining and should have used a smaller number of epochs
set_weights(model, weights)
res <- fit(model, x_train, y_train, epochs=5, batch_size=128)

# predicted probabilities of being a positive review for the test data
pred <- predict(model, x_test)

# accuracy (about 85%)
mean((pred > 0.5) == y_test)

# in the book, they got 76% accuracy, but they do not provide sufficient
# details on what exactly they did (except that they mention using dropout
# regularization, so maybe that wasn't such a good idea)

############################################################################

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model (the LSTM RNN)
model <- keras_model_sequential() |>
   layer_embedding(input_dim=10000, output_dim=32) |>
   layer_lstm(units=32) |>
   layer_dense(units=1, activation="sigmoid") |>
   compile(optimizer="rmsprop", loss="binary_crossentropy", metrics="accuracy")

# summary information about the model
summary(model)

# fit the model (setting 20% of the data aside for validation)
res <- fit(model, x_train, y_train, epochs=10, batch_size=128, validation_split=0.2)

# plot the loss/accuracy as a function of epochs
plot(res)

# predicted probabilities of being a positive review for the test data
pred <- predict(model, x_test)

# accuracy (about 86%)
mean((pred > 0.5) == y_test)

############################################################################

### 10.5.2: Time Series Forecasting

# install the ISLR2 package
#install.packages("ISLR2")

# load the ISLR2 package
library(ISLR2)

# put the three variables of interest into a matrix called dat
dat <- as.matrix(NYSE[c("DJ_return", "log_volume","log_volatility")])

# extract the variable that indicates whether a row is training data or not
istrain <- NYSE[["train"]]

# standardize all three variables in dat
dat <- scale(dat)

# function to lag the values in a matrix x by a lag of k
lagm <- function(x, k=1) {
   n <- nrow(x)
   pad <- matrix(NA, k, ncol(x))
   rbind(pad, x[1:(n-k),])
}

# for example, if our original matrix looks like this:
#
# 1)  4  2  6
# 2)  2  3  7
# 3)  1  3  2
# 4)  7  5  6
#
# then running it through lagm() with k=1 will return
#
# 1) NA NA NA
# 2)  4  2  6
# 3)  2  3  7
# 4)  1  3  2

# create a data frame with log(volume) as the first variable and then all
# variables in dat lagged by 1, by 2, ..., and by 5
dat.ar <- data.frame(log_volume = dat[, "log_volume"],
   L1 = lagm(dat, 1), L2 = lagm(dat, 2), L3 = lagm(dat, 3),
   L4 = lagm(dat, 4), L5 = lagm(dat, 5))

# inspect the first 6 rows of the data frame
head(dat.ar)

# remove the first 5 rows from dat.ar (and the istrain variable)
dat.ar  <- dat.ar[-(1:5),]
istrain <- istrain[-(1:5)]

# inspect again the first 6 rows of the data frame
head(dat.ar)

# split the dataset into a training and test dataset
dat.ar.train <- dat.ar[istrain,]
dat.ar.test  <- dat.ar[!istrain,]

# fit an AR(5) model (autoregressive model of the 5th order) using the training data
res <- lm(log_volume ~ ., data=dat.ar.train)
summary(res)

# predicted log(volume) for the test data
pred <- predict(res, dat.ar.test)

# compute R^2 for the test data
1 - mean((pred - dat.ar.test$log_volume)^2) / var(dat.ar.test$log_volume)

############################################################################

# restructure dat.ar into the format needed as input for the RNN
xrnn <- as.matrix(dat.ar[,-1])
xrnn <- array(xrnn, dim=c(nrow(dat.ar),3,5))
xrnn <- xrnn[,,5:1]
xrnn <- aperm(xrnn, c(1,3,2))
dim(xrnn)

# look at the array for the first day (which strictly speaking is the 6th day;
# we removed the first 5 days earlier, since we do not have all lag 5 values
# for days 1:5)
xrnn[1,,]

# so the first dimension of the xrnn array corresponds to the various days,
# the second dimenion to the 5 lagged values for the three variables given by
# the third dimension

# split up the xrnn array into the training and test parts
xrnn.train <- xrnn[istrain,,]
xrnn.test  <- xrnn[!istrain,,]

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model (RNN with 12 hidden units)
model <- keras_model_sequential() |>
   layer_simple_rnn(input_shape=c(5,3), units=12, dropout=0.1, recurrent_dropout=0.1) |>
   layer_dense(units=1) |>
   compile(optimizer="rmsprop", loss="mse")

# summary information about the model
summary(model)

# fit model
res <- fit(model, xrnn.train, dat.ar.train$log_volume, epochs=200, batch_size=64)

# compute R^2 for the test data
pred <- predict(model, xrnn.test)
1 - mean((pred - dat.ar.test$log_volume)^2) / var(dat.ar.test$log_volume)

############################################################################

# two ways of fitting the AR(5) model using a neural network approach

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model
model <- keras_model_sequential() |>
   layer_flatten(input_shape=c(5,3)) |>
   layer_dense(units=1) |>
   compile(optimizer="rmsprop", loss="mse")

# summary information about the model
summary(model)

# fit model
res <- fit(model, xrnn.train, dat.ar.train$log_volume, epochs=200, batch_size=64, verbose=FALSE)

# compute R^2 for the test data
pred <- predict(model, xrnn.test)
1 - mean((pred - dat.ar.test$log_volume)^2) / var(dat.ar.test$log_volume)

# split the dataset into a training and test dataset
x.train <- as.matrix(dat.ar.train[-1])
x.test  <- as.matrix(dat.ar.test[-1])

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model
model <- keras_model_sequential() |>
   layer_dense(input_shape=ncol(x.train), units=1) |>
   compile(optimizer="rmsprop", loss="mse")

# summary information about the model
summary(model)

# fit model
res <- fit(model, x.train, dat.ar$log_volume[istrain], epochs=200, batch_size=64, verbose=FALSE)

# compute R^2 for the test data
pred <- predict(model, x.test)
1 - mean((pred - dat.ar.test$log_volume)^2) / var(dat.ar.test$log_volume)

############################################################################

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# non-linear AR(5) model by adding a hidden layer with 32 units
model <- keras_model_sequential() |>
   layer_dense(input_shape=ncol(x.test), units=32, activation="relu") |>
   layer_dropout(rate = 0.5) |>
   layer_dense(units = 1) |>
   compile(optimizer="rmsprop", loss="mse")

# summary information about the model
summary(model)

# fit model
res <- fit(model, x.train, dat.ar$log_volume[istrain], epochs=100, batch_size=32)

# compute R^2 for the test data
pred <- predict(model, x.test)
1 - mean((pred - dat.ar.test$log_volume)^2) / var(dat.ar.test$log_volume)

############################################################################

# the 'straw man' approach using yesterday's value to predict the current one
pred <- c(NA, dat.ar.test$log_volume[-length(dat.ar.test$log_volume)])

# examine the predicted and actual values in the test data
head(cbind(pred, actual=dat.ar.test$log_volume))
tail(cbind(pred, actual=dat.ar.test$log_volume))

# compute R^2 based on this approach
1 - mean((pred - dat.ar.test$log_volume)^2, na.rm=TRUE) / var(dat.ar.test$log_volume, na.rm=TRUE)

############################################################################

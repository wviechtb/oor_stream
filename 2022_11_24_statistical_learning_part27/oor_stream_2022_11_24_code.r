############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-11-24
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 10.3 - 10.4
#
# last updated: 2022-11-25

############################################################################

### 10.3: Convolutional Neural Networks

# load the keras package (see the script from the last session on how to set
# up keras, which requires some additional software)
library(keras)

# the CIFAR100 dataset: https://www.cs.toronto.edu/~kriz/cifar.html
#
# with the dataset_cifar100() function, we can download the CIFAR100 dataset
# and have it directly available for processing with R
cifar100 <- dataset_cifar100()

# note: this downloads the dataset into the ~/.keras/datasets directory

# cifar100 is a list with two elements called 'train' and 'test'

# check the variable names in the dataset
names(cifar100)

# each of these elements is a list with elements 'x' and 'y' for the image
# data and the labels
str(cifar100$train)

# put the image data into x_train and the labels into g_train for the training data
x_train <- cifar100$train$x
g_train <- cifar100$train$y

# do the same for the test data
x_test  <- cifar100$test$x
g_test  <- cifar100$test$y

# x_train is an array where the first dimension corresponds to the 50,000
# images, the second and third dimensions are the 32x32 pixel locations, and
# the fourth dimension provides the colors intensities for red-green-blue
dim(x_train)

# rescale the color intensities to the 0-1 range
x_train <- x_train / 255
x_test  <- x_test  / 255

# install the jpeg package
#install.packages("jpeg")

# load the jpeg package
library(jpeg)

# random sample 25 numbers from 1:50000
index <- sample(50000, 25)

# draw the image for these 25 randomly selected images
#par(mar=c(0,0,0,0), mfrow=c(5,5))
#for (i in index)
   #plot(as.raster(x_train[i,,,]))

# there are 100 classes and for each there are 500 images
table(g_train)

# turn the class labels into 100 dummy variables
y_train <- to_categorical(g_train, 100)
dim(y_train)

# set the seed to make results reproducible
#use_session_with_seed(1234) # this will be deprecated in the future
tensorflow::set_random_seed(42)

# set up the model
model <- keras_model_sequential() |>
   layer_conv_2d(input_shape=c(32,32,3), filters=32, kernel_size=c(3,3), padding="same", activation="relu") |>
   layer_max_pooling_2d(pool_size=c(2,2)) |>
   layer_conv_2d(filters=64, kernel_size=c(3,3), padding="same", activation="relu") |>
   layer_max_pooling_2d(pool_size=c(2,2)) |>
   layer_conv_2d(filters=128, kernel_size=c(3,3), padding="same", activation="relu") |>
   layer_max_pooling_2d(pool_size=c(2,2)) |>
   layer_conv_2d(filters=256, kernel_size=c(3,3), padding="same", activation="relu") |>
   layer_max_pooling_2d(pool_size=c(2,2)) |>
   layer_flatten() |>
   layer_dropout(rate=0.5) |>
   layer_dense(units=512, activation="relu") |>
   layer_dense(units=100, activation="softmax") |>
   compile(loss="categorical_crossentropy", optimizer="rmsprop", metrics="accuracy")

# summary information about the model
summary(model)

# fit model (this might take several minutes)
system.time(res <- fit(model, x_train, y_train, epochs=30, batch_size=128, validation_split=0.2))

# predicted probabilities for each class in the test data
pred <- predict(model, x_test)

# predicted class
pred <- apply(pred, 1, which.max) - 1

# test error rate (about 54%)
mean(g_test != pred)

# accuracy (about 46%)
mean(g_test == pred)

############################################################################

# compare this to multinomial logistic regression where we use each of the
# 32*32*3 = 3072 features as a predictor (it is silly to even consider this,
# but let's see what happens)

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model
model <- keras_model_sequential() |>
   layer_flatten(input_shape=c(32,32,3)) |>
   layer_dense(units=100, activation="softmax") |>
   compile(loss="categorical_crossentropy", optimizer="rmsprop", metrics="accuracy")

# summary information about the model
summary(model)

# fit model (this might take several minutes)
system.time(res <- fit(model, x_train, y_train, epochs=30, batch_size=128, validation_split=0.2))

# predicted probabilities for each class in the test data
pred <- predict(model, x_test)

# predicted class
pred <- apply(pred, 1, which.max) - 1

# accuracy (about 10%, which is better than randomly chosen one of the 100
# labels, which would yield an accuracy of 1%)
mean(g_test == pred)

# accuracy when randomly choosing one of the 100 labels (about 1%)
mean(g_test == sample(100, 10000, replace=TRUE) - 1)

############################################################################

### 10.4: Document Classification

# we will try some document classification with the IMDB dataset

# dataset_imdb() can be used to download the dataset
imdb <- dataset_imdb(num_words=10000)

# note: only the 10,000 most frequent words are included

# a list with elements 'train and 'test'
names(imdb)

# each contains elements 'x' and 'y' with the features (the words used in a
# particular review indexed by numbers) and the labels (whether the review was
# a positive or negative review)

x_train <- imdb$train$x
y_train <- imdb$train$y

x_test <- imdb$test$x
y_test <- imdb$test$y

# for example, the first review used these words
x_train[[1]]

# and it was a positive review (1 = positive, 0 = negative)
y_train[1]

# get the word index
word_index <- dataset_imdb_word_index()

# the numbers of the words are such that 1 is the most commonly used word, 2
# is the second most commonly used word, and so on; so we can examine the 100
# most commonly used words as follows (note: have to add 3 because that is how
# the index is set up)
sort(unlist(word_index)+3)[1:100]

# function to decode a review
decode_review <- function(text, word_index) {
   word <- names(word_index)
   idx <- unlist(word_index, use.names = FALSE)
   word <- c("<PAD>", "<START>", "<UNK>", "<UNUSED>", word)
   idx <- c(0:3, idx + 3)
   words <- word[match(text, idx, 2)]
   paste(words, collapse = " ")
}

# do this for the first review
decode_review(x_train[[1]], word_index)

# load the Matrix package
library(Matrix)

# function to 'one-hot' encode the reviews
one_hot <- function(sequences, dimension) {
   seqlen <- sapply(sequences, length)
   n <- length(seqlen)
   rowind <- rep(1:n, seqlen)
   colind <- unlist(sequences)
   sparseMatrix(i = rowind, j = colind, dims = c(n, dimension))
}

# do this for the training and test data
x_train_1h <- one_hot(x_train, 10000)
x_test_1h  <- one_hot(x_test, 10000)

# so each of these is now a matrix with 10000 columns where each column
# corresponds to one of the words and it indicates whether that word was used
# in a review or not
dim(x_train_1h)

# proportion of entries that are not zero
nnzero(x_train_1h) / (25000 * 10000)
sum(x_train_1h > 0) / (25000 * 10000)

# install the glmnet package
#install.packages("glmnet")

# load the glmnet package
library(glmnet)

# set the seed to make things reproducible
set.seed(1234)

# 10-fold cross-validation to determine an appropriate value for lambda
res.cv <- cv.glmnet(x_train_1h, y_train, family="binomial", standardize=FALSE,
                    type.measure="class", trace.it=1)

# plot the results
plot(res.cv)

# lambda value with the lowest cross-validated misclassification rate
res.cv$lambda.min

# fit a lasso logistic regression model to these data with the chosen lambda
res <- glmnet(x_train_1h, y_train, family="binomial", standardize=FALSE,
              lambda=res.cv$lambda.min)
res

# predict the class for the test data
pred <- predict(res, x_test_1h, type="class")

# accuracy (about 88%)
mean(y_test == pred)

# for comparison, fit the same model but using a non-sparse feature matrix

x_train_1h <- lapply(x_train, function(x) {
   mat <- rep(0,10000)
   mat[x] <- 1
   return(mat)
})
x_train_1h <- do.call(rbind, x_train_1h)

system.time(res <- glmnet(x_train_1h_non_sparse, y_train, family="binomial",
                          standardize=FALSE, lambda=res.cv$lambda.min))
pred <- predict(res, x_test_1h, type="class")
mean(y_test == pred)

############################################################################

# now use a neural network with two hidden layers (with 16 units each)

# set the seed to make results reproducible
tensorflow::set_random_seed(42)

# set up the model
model <- keras_model_sequential() %>%
   layer_dense(input_shape=10000, units=16, activation="relu") |>
   layer_dense(units=16, activation="relu") |>
   layer_dense(units=1, activation="sigmoid") |>
   compile(optimizer="rmsprop", loss="binary_crossentropy", metrics="accuracy")

# save the initial weights
weights <- get_weights(model)

# fit the model (setting 20% of the data aside for validation)
res <- fit(model, x_train_1h, y_train, epochs=20, batch_size=512, validation_split=0.2)

# plot the loss/accuracy as a function of epochs
#plot(res)

# according to this graph, the accuracy estimated from the validation data
# actually starts to go down after 5 epochs; so reset the weights to the
# initial ones and refit the model using only 5 epochs (using all data)
set_weights(model, weights)
res <- fit(model, x_train_1h, y_train, epochs=5, batch_size=512)

# predicted probabilities of being a positive review for the test data
pred <- predict(model, x_test_1h)

# accuracy (about 88%)
mean((pred > 0.5) == y_test)

############################################################################

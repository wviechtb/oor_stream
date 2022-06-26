############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-11-05
#
# Topic(s):
# - how R 'works' (functions, methods, generics)
# - writing your own functions
#
# last updated: 2020-11-10

############################################################################

# restart the R session (Menu 'Session' - Restart R)

############################################################################

# looking at the code for a function

fivenum

# many R functions are written entirely in R

# some R functions make use of compiled code or are 'primitive' functions

cor # note the .Call() for calling compiled C/C++ code
exp # a primitive function

# might also see things like .Internal(), .C(), .Fortran(), or .External()

# generic functions

summary

# method functions

x <- c(2, 4, 3, 4, 6)
y <- c("M", "M", "F", "F", "F")
z <- factor(y)

dat <- data.frame(x, y, z)
dat

summary(x)
summary(y)
summary(z)
summary(dat)

class(x)
class(y)
class(z)
class(dat)

# methods(): list method functions

methods(summary)

# there is summary.data.frame() so summary(dat) calls summary.data.frame(dat)
# there is no summary.numeric() so summary(x) calls summary.default(x)
# there is no summary.character() so summary(y) calls summary.default(y)
# there is summary.factor() so summary(z) calls summary.factor(z)

summary.data.frame
summary.default
summary.factor

# this is called 'methods dispatch' (a simple object oriented class system
# called S3); there are more sophisticated class systems (called S4 and R5)

# in principle you can call an appropriate method function directly

summary.data.frame(dat)

# but don't do this; let S3 handle the dispatching for you!

# getAnywhere(): see non-visible code

summary.loess

getAnywhere(summary.loess)

# or if you know which package the function is in

stats:::summary.loess

# actually what is happening above is a bit more complex, since when we type
# summary(x) and hit enter, this is actually the same as print(summary(x))

# and the print() function is just another generic

x # same as print(x)
y # same as print(y)
z # same as print(z)
dat # same as print(dat)

print
methods(print) # LOTS!

# note that there is print.loess() and print.summary.loess()

getAnywhere(print.loess)
getAnywhere(print.summary.loess)

# so let's put this all together

x <- 1:20
y <- 2 + log(x) + rnorm(20, 0, 0.2)
plot(x, y, pch=19, col="gray30")
res <- loess(y ~ x)
res
lines(res$x, res$fitted, lwd=2)

class(res)
res # calls print.loess()

res <- summary(res)
class(res)
res # calls print.summary.loess()

# another example

res <- lm(y ~ x)
class(res)
res

getAnywhere(print.lm)

# what happens when we then use this command?

summary(res)

# 1) summary.lm() is called on 'res'

summary.lm

# 2) this returns an object of class 'summary.lm'

# 3) print.summary.lm() is called

print.summary.lm
getAnywhere(print.summary.lm)

############################################################################

# writing your own R functions

double <- function(x) {
   x2 <- x * 2
   return(x2)
}

double(c(2,4,3,5))
double(c(2,4,3,NA,5))

# now let's add another argument which has a default value

double <- function(x, na.rm=FALSE) {
   if (na.rm)
      x <- x[!is.na(x)]
   x2 <- x * 2
   return(x2)
}

double(c(2,4,3,5))
double(c(2,4,3,NA,5))
double(c(2,4,3,NA,5), na.rm=TRUE)

# let's write a more interesting function: we want to write a function that
# can compute the moving average of a numeric vector; for example:
#
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
# -------
#    2
#
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
#    -------
#       3
#
# 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
#       -------
#          4

movavg <- function(x, window=1) {

   n <- length(x)
   mx <- rep(NA, n)

   for (i in (window+1):(n-window)) {
      mx[i] <- mean(x[(i-window):(i+window)])
   }

   return(mx)

}

movavg(1:10)
movavg(c(2,2,3,5,4,5,7,6,8))

x <- 1:20
y <- 2 + log(x) + rnorm(20, 0, 0.2)
plot(x, y, pch=19, col="gray30")
res <- loess(y ~ x)
lines(res$x, res$fitted, lwd=2)

lines(x, movavg(y), lwd=2, col="red")
lines(x, movavg(y, window=2), lwd=2, col="green")
lines(x, movavg(y, window=3), lwd=2, col="blue")

# what if there are missing values?

movavg(c(2,2,3,5,NA,5,7,6,8))

movavg <- function(x, window=1, na.rm=FALSE) {

   n <- length(x)
   mx <- rep(NA, n)

   for (i in (window+1):(n-window)) {
      mx[i] <- mean(x[(i-window):(i+window)], na.rm=na.rm)
   }

   return(mx)

}

movavg(c(2,2,3,5,NA,5,7,6,8))
movavg(c(2,2,3,5,NA,5,7,6,8), na.rm=TRUE)

# what if we provide nonsensical inputs?

movavg(1:4, window=3)
movavg(c("h","e","l","l","o"))

# we need to 'robustify' the function a bit

movavg <- function(x, window=1, na.rm=FALSE) {

   if (!is.numeric(x))
      stop("Need to supply a numeric vector via argument 'x'.")

   n <- length(x)

   if (n < 2*window + 1)
      stop("Vector length is too low for this 'window' size.")

   mx <- rep(NA, n)

   for (i in (window+1):(n-window)) {
      mx[i] <- mean(x[(i-window):(i+window)], na.rm=na.rm)
   }

   return(mx)

}

movavg(1:4, window=3)
movavg(1:7, window=3)
movavg(c("h","e","l","l","o"))

# what about those classes and print functions?

movavg <- function(x, window=1, na.rm=FALSE) {

   if (!is.numeric(x))
      stop("Need to supply a numeric vector via argument 'x'.")

   n <- length(x)

   if (n < 2*window + 1)
      stop("Vector length is too low for this 'window' size.")

   mx <- rep(NA, n)

   for (i in (window+1):(n-window)) {
      mx[i] <- mean(x[(i-window):(i+window)], na.rm=na.rm)
   }

   z <- list(mx=mx, window=window, n=n)
   class(z) <- "movavg"
   return(z)

}

x <- 1:20
y <- 2 + log(x) + rnorm(20, 0, 0.2)
res <- movavg(y)
res
class(res)

# so now let's write a print() method

print.movavg <- function(x) {

   cat("\nMoving Average\n")
   cat("Window Size:", x$window, "\n\n")
   print(x$mx)
   cat("\n")

}

res

# noice! let's write a plot() method

plot.movavg <- function(x) {

   plot(1:x$n, x$mx, type="o")

}

plot(res)

# what is that ... argument?

print
plot

# we can use it to pass additional (undefined) arguments to the function

plot.movavg <- function(x, ...) {

   plot(1:x$n, x$mx, type="o", ...)

}

plot(res)
plot(res, pch=19)
plot(res, pch=19, lwd=2)
plot(res, pch=19, lwd=2, col="red")

############################################################################

# functions are 'protected' environments

add1 <- function(x) {
   x <- x + 1
   return(x)
}

x <- 10
add1(x)
x
x <- add1(x)
x

# variables outside of functions can be found, but don't do this

addxy <- function(x) {
   res <- x + y
   return(res)
}

x <- 10
y <- 20
addxy(x)

rm(y)
addxy(x)

# better

addxy <- function(x, y) {
   res <- x + y
   return(res)
}

x <- 10
y <- 20
addxy(x,y)

############################################################################

# statistical modeling functions typically often return lists

head(mtcars)

res <- lm(mpg ~ cyl + hp + wt + am, data=mtcars)
summary(res)

class(res)
is.list(res)

# what's inside such an object?

str(res)

res$coefficients

# the elements returned should (ideally) also be documented under the help

help(lm)

# but where is R^2? where are the standard errors, t-statistics, p-values?

# they are not actually computed by lm(); this is done by summary()!

z <- summary(res)

class(z)
is.list(z)
str(z)

z$r.squared
z$coefficients

# in general, it is better practice to use 'extractor' functions

coef(res)
coef(z)

# those should also (ideally) be documented under the help

# but there is no extractor function for R^2, so you have to extract it yourself

# to summarize the types of functions that we covered:
# - generics
# - methods
# - extractors

############################################################################

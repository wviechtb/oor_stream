############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-05-11
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 3.1 - 4.3
#
# last updated: 2024-02-23

############################################################################

### 3: Objects, their modes and attributes

############################################################################

## 3.1: Intrinsic attributes: mode and length

# create a numeric vector and check its 'mode'
x <- c(1, 3, 2, 4, 5, 3)
mode(x)

# sidenote: the mode() function in R does not give the 'statistical mode' of
# the data (i.e., the value that appears most often in a set of data values);
# see: https://en.wikipedia.org/wiki/Mode_(statistics)

# check if 'x' is a vector
is.vector(x)

# check if 'x' is an 'atomic vector'
is.atomic(x)

# try to mix numerical values with a string in the same vector
x <- c(1, 3, 2, "Bob", 4, 5)
x
mode(x)
is.vector(x)
is.atomic(x)

# everything has turned into a character string, so we have a character
# vector; R uses various rules for doing this sort of 'type coercion'

# create a numeric vector with a missing value
x <- c(1, 3, 2, NA, 5, 3)
mode(x)

# the 4th value is missing and it is a numeric missing value
x[4]

# in R, this can be explicitly stated as
NA_real_

# a character vector with a missing value
x <- c("Bob", "Sue", NA, "Gil")
mode(x)

# the 3rd value is missing and it is a character missing value
x[3]

# in R, this can be explicitly stated as
NA_character_

# list all elements in the workspace
ls()

# remove x
rm(x)

# list all elements in the workspace
ls()

# character(0) indicates a character vector with no elements, so this shows
# that there are no elements in the workspace

# create a list with three elements
l <- list(x = c(1,3,2,5), y = c("Bob","Sue"), z = 42)
l

# the mode of a list is 'list' (surprise!)
mode(l)

# a list is defined to be a vector in R, but it is not an atomic vector
is.vector(l)
is.atomic(l)

# create the numeric vector 'x' and obtain the five-number summary of its values
x <- c(1, 3, 2, 4, 5, 3, 3, 4, 1, 2, 3, 1, 5, 4, 5, 8, 4, 5)
fivenum(x)

# fivenum() is a function in R; functions in R are also objects, which you can
# inspect and manipulate (and we can easily create new ones)
fivenum

# can also check the mode of such an object
mode(fivenum)

# but (obviously) they are not (atomic) vectors
is.vector(fivenum)
is.atomic(fivenum)

# use length() to find the number of elements in a vector
length(x)

# for a list, length() tells us about the number of elements in the list
length(l)

# the mode and length of an objects are intrinsic attributes

# an object can also have other attributes; for example, lists can have an
# attribute called 'names' which gives the names of the list elements
attributes(l)

# numeric vectors can be of different types
x <- c(2.4, 1.8, 5.7, 3.9)
x

# x is a numeric vector
mode(x)

# but it is a collection of numbers that have a fractional part; in R, this
# means that this numeric vector is of type 'double'
typeof(x)

# we can have another numeric vector that is of type 'integer'; for this, we
# put L after each integer
x <- c(2L, 1L, 5L, 3L)
x
mode(x)
typeof(x)

# if we don't use L, then the numeric vector is of type 'double'
x <- c(2, 1, 5, 3)
x
mode(x)
typeof(x)

# cannot mix doubles and integers in the same vector
x <- c(2, 1, 5L, 3)
x
mode(x)
typeof(x)

# if we try, then the integers get 'promoted' (type coercion!) to doubles

# create a vector of the numbers 0, 1, ..., 9
z <- 0:9
z

# when we use : to create a numeric sequence, it creates an integer vector
typeof(z)

# turn the numeric (integer) vector z into a character vector
digits <- as.character(z)
digits

# this is doing 'explicit type coercion' (note that we already saw 'implicit
# type coercion' earlier, for example when mixing numbers and strings in a
# vector or when mixing doubles and integers in a vector)

# can even turn a character vector into a numeric vector using as.numeric()
# (note: this always creates a 'double' vector)
d <- as.numeric(digits)
d
typeof(d)

# there are also as.double() and as.integer() functions
as.double(d)
as.integer(d)

############################################################################

## 3.2: Changing the length of an object

# we can create an empty numeric vector with numeric()
e <- numeric()
e

# it is of length 0
length(e)

# but this empty vector still has a mode (and a type)
mode(e)
typeof(e)

# similarly, can create empty character or logical vectors
character()
logical()

# note: if you do specify the length of the vector, R initializes these
# different vector types to different default values
numeric(10)
character(10)
logical(10)

# assign the value 17 to the third element of e; since right now, e is of
# length 0, R will happily change the length of the vector to 3, but the first
# and second element will be missing (NA)
e[3] <- 17
e

# in contrast to what is stated in the manual, can even do this when the mode
# of the value that we are assigning to an element is different than the mode
# of the vector itself, but implicit type coercion then happens
e[5] <- "Bob"
e

# create a vector with 10 numbers
alpha <- c(2, 4, 3, 1, 8, 5, 4, 5, 3, 7)
alpha

# select elements 1 through 5 from this vector and assign it on top of alpha
alpha <- alpha[1:5]
alpha

# this effectively shortens alpha

# we can also directly assign a length to a vector and if the length is
# shorter than the current length, it drops the extra elements
length(alpha) <- 3
alpha

# can also assign a longer length than the current one; the additional new
# elements then will be missing values (NAs)
length(alpha) <- 10
alpha

############################################################################

## 3.3: Getting and setting attributes

# create a numeric vector and add some attributes to it
x <- c(2, 4, 3, 1, 6, 8)
attr(x, "date") <- "2023-05-11"
attr(x, "subject") <- c("Bob", "Sue", "Joe", "Gill", "Tom", "Anna")
x

# while in this case, printing x directly shows all of its (non-intrinsic)
# attributes, we can still use attributes() just to list them
attributes(x)

# we can extract a particular attribute in two different ways
attr(x, "subject")
attributes(x)$subject

# depending on the type of object, there are some special types of attributes
# that have a particular purpose; we can illustrate this using a 'named
# vector'

# create a named (numeric) vector with 4 elements
x <- c(Bob = 2, Sue = 4, Joe = 5, Gill = 8)
x
mode(x)
typeof(x)

# the names of such a vector are actually an attribute
attributes(x)

# so the 'names' attribute is a special type of attribute that also changes
# how the vector is printed
x

# another example of this is the 'dim' attribute

# create a numeric vector with 6 elements
x <- c(3, 2, 5, 7, 1, 3)
x

# assign c(3,2) as the 'dim' attribute to 'x'
attr(x, "dim") <- c(3,2)

# this turns the vector into a matrix with 3 rows and 2 columns
x

# list the attributes of x
attributes(x)

############################################################################

## 3.4: The class of an object

# all of the elements in x are numeric
mode(x)

# but it is a matrix and it has that class (and also the class 'array')
class(x)

# illustrate how the class of an object can influence how a function behaves
x <- c(2, 3, 2, 5, 3, 2, 4, 2, 3)
x
summary(x)

# so if we use summary() on a numeric vector, it prints the five-number
# summary (and also the mean)

# turn x into a factor (which can take on the values (the so-called 'levels')
# 1, 2, ..., 5) and assign this to y
y <- factor(x, levels=1:5)
y
summary(y)

# if we use summary() on a factor, it creates a frequency table

# how does R know what to do when using summary() in these different cases?
# this has to do with the class of the object
class(x)
class(y)

# depending on the class of the object given to summary(), the function
# behaves differently

# sidenote: the 'levels' and 'class' of a factor are actually attributes
attributes(y)

# note that some objects have an 'implicit class' that is not an attribute
class(x)
attributes(x)

# NULL means that there are no attributes for x

# manually create a data frame with three variables
dat <- data.frame(subject = c("Bob", "Sue", "Gill", "Tom"),
                  age = c(25, 23, 28, 21), y = c(5, 6, 3, 4))
dat

# the way a data frame is printed (remember: typing 'dat' is just a shortcut
# for 'print(dat)') again has to do with the class of the object
class(dat)

# remove the class from dat
unclass(dat)

# when we do so, we see that a data frame is really just a list, where each
# list element is a variable in the data frame

# consider a more complex example where we fit a linear regression model with
# 'y' as the outcome variable and 'age' as the predictor
res <- lm(y ~ age, data=dat)
res

# when we print 'res', we just get the estimated intercept and slope; to get
# the full regression table (and things like R^2), we can use summary()
summary(res)

# that summary() provides all of this output for a 'regression model object'
# again has to do with the class of the object
class(res)

# remove the class from res
unclass(res)

# now we see that res is actually a list with a whole bunch of different
# elements (some of which make sense, like 'coefficients' or 'residuals', but
# there are others whose meaning/purpose is not so clear, but this is not
# relevant for now)

############################################################################

### 4: Ordered and unordered factors

############################################################################

## 4.1: A specific example

# create a character vector
state <- c("tas", "sa",  "qld", "nsw", "nsw", "nt",  "wa",  "wa", "qld",
           "vic", "nsw", "vic", "qld", "qld", "sa",  "tas", "sa",  "nt",
           "wa",  "vic", "qld", "nsw", "nsw", "wa", "sa",  "act", "nsw",
           "vic", "vic", "act")
state

# turn state into a factor and assign this to statef
statef <- factor(state)
statef

# notice that when you print a factor, it also lists the levels of it; also,
# since we did not use the 'levels' argument when creating the factor(), the
# levels are automatically the unique values that occur in the data

# can also request to list the levels with levels()
levels(statef)

############################################################################

## 4.2: The function tapply() and ragged arrays

# create a numeric vector with the incomes
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54, 62, 69, 70, 42, 56, 61, 61, 61,
             58, 51, 48, 65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)

# compute the mean income for each level of statef
incmeans <- tapply(incomes, statef, mean)
incmeans

# can use any function in tapply() that takes a vector as input and produces a
# single value as output (like the mean or the SD or whatever)
tapply(incomes, statef, sd)

# the output from tapply() is just a vector (with the means or SDs or
# whatever), which we can collect in a data frame
dat <- data.frame(mean = tapply(incomes, statef, mean),
                  sd   = tapply(incomes, statef, sd),
                  n    = tapply(incomes, statef, length))
dat

# create a function called stdError() that computes the standard error of a mean
stdError <- function(x) sqrt(var(x)/length(x))

# compute the standard error of the mean income for each statef level and add
# this as a new variable to 'dat'
dat$se <- tapply(incomes, statef, stdError)
dat

# add the 95% CI limits for the means to the data frame
# see: https://en.wikipedia.org/wiki/Confidence_interval#Example
dat$ci.lb <- dat$mean - qt(.975, df=dat$n-1) * dat$se
dat$ci.ub <- dat$mean + qt(.975, df=dat$n-1) * dat$se
dat

# tapply() is even more flexible; the function that we apply to the values
# within each state level can also yield multiple values; for example, we
# earlier saw the fivenum() function
tapply(incomes, statef, fivenum)

# then tapply() returns a list where each element is the five-number summary
# of a particular level of statef

# by splitting the incomes variable by statef levels, we can see that this is
# a 'ragged array' because the lengths of these vectors are not all the same
split(incomes, statef)

############################################################################

## 4.3: Ordered factors

# create two character vectors
name  <- c("Bob", "Sue", "Tom", "Gill")
speed <- c("slow", "fast", "medium", "slow")

# turn speed into a factor (unordered)
speedf <- factor(speed)
speedf

# note: the factor levels are determined alphabetically

# we can explicitly set what is the first, second, and third level
speedf <- factor(speed, levels=c("slow", "medium", "fast"))
speedf

# but speedf is still an unordered factor; we can make it an ordered factor in
# two different ways
speedf <- factor(speed, levels=c("slow", "medium", "fast"), ordered = TRUE)
speedf
speedf <- ordered(speed, levels=c("slow", "medium", "fast"))
speedf

# note how the levels are printed for an ordered factor

# the information that speedf is now an ordered factor is contained in the
# class of this object (which now actually has two classes)
class(speedf)

############################################################################

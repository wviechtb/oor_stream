############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-04-27
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 1.1 - 2.8
#
# last updated: 2024-02-23

############################################################################

# As noted already in the previous session (on 2023-04-06) where we started
# going through this manual, I might adjust the code at times to make it more
# accessible, potentially leave things out when I think they are unnecessary
# or confusing, and add additional explanations when this can be useful.

############################################################################

### 1: Introduction and preliminaries

############################################################################

## 1.2: Related software and documentation

# just as a note: while the books mentioned here are definitely interesting, I
# would not recommend them to those starting out with R

## 1.5: Using R interactively

# this section is focused on using R under a Unix/Linux operating system,
# which probably isn't relevant to many people; however, it indirectly touches
# on an important concept, namely that of the 'working directory'; when you
# start R, a particular directory (folder) on your computer is set as the
# working directory (which becomes relevant when reading data from an external
# file)

# determine what the current working directory is
getwd()

# one can change this is with setwd(), but this can be tedious; when using
# RStudio, one can easily set the working directory to the location of this
# script via the 'Session' menu, 'Set Working Directory', and then selecting
# 'To Source File Location'; one can also configure RStudio (and other
# software for interacting with R) such that a script (like the present one)
# is automatically opened in RStudio when (double)clicking on the file and
# then the working folder is also automatically set to the location of the
# script

## 1.6: An introductory session

# we went through this introductory session during the stream on 2023/04/06

## 1.7: Getting help with functions and features

# open up the help file for the mean() function
help(mean)
?mean

# at times, have to enclose the argument in quotes
help("[")

# open the general help page
help.start()

# do a search for a term (searches among the installed packages)
??"principal components analysis"

## 1.8: R commands, case sensitivity, etc.

# R is case sensitive, so these are different
x <- 1
X <- 2
x
X

# the same applies to commands, so while there is a mean() function, trying to
# use Mean() or MEAN() would result in an error

# essentially, you should start object and variable names with a letter and
# then after that you can use letters, numbers, ., and _

# avoid special characters specific to certain languages like German Umlauts
# (i.e., stick to abc...z, 012...9, ., and _)

## 1.11: Data permanency and removing objects

# I would generally recommend not to make use of the option to save the
# 'workspace' when quitting R (this has the potential to lead to a cluttered
# workspace and confusion); if one needs to save things, one can do this
# manually with appropriate commands (like write.table(), save(), etc. which
# we might get to later)

# in RStudio, go to the 'Tools' menu, then 'Global Options', and uncheck (if
# it is checked) 'Restore .RData into workspace at startup' and set 'Save
# workspace to .RData on exit' to 'Never'

############################################################################

### 2: Simple manipulations; numbers and vectors

## 2.1: Vectors and assignment

# combine with c() the numbers into a numeric vector and assign this to 'x'
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
x

# in R, there is no special object type for a single number (a 'scalar') so a
# single number is just a vector of length 1
5

# using the assign() function for making the same assignment as above
assign("x", c(10.4, 5.6, 3.1, 6.4, 21.7))

# this might be useful when you want to assign something to an object, but the
# object name itself is a variable
var <- "age"
var
assign(var, c(19, 34, 22, 58))
age

# using the following would just overwrite the existing 'var' object and puts
# the numeric vector into 'var'
var <- c(19, 34, 22, 58)
var

# can reverse the arrow in the assignment operation (uncommon to use this)
c(10.4, 5.6, 3.1, 6.4, 21.7) -> x

# line-by-line style syntax
x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
y <- sqrt(x)
my <- mean(y)
my

# nested style syntax
my <- mean(sqrt(c(10.4, 5.6, 3.1, 6.4, 21.7)))
my

# pipe style syntax
c(10.4, 5.6, 3.1, 6.4, 21.7) |> sqrt() |> mean() -> my
my

# this also works because <- happens last
my <- c(10.4, 5.6, 3.1, 6.4, 21.7) |> sqrt() |> mean()
my

# a vector can also be one of the inputs to c()
y <- c(x, 0, x)
y

## 2.2: Vector arithmetic

# this illustrates 'recycling' behavior
v <- 2*x + y + 1
v

# an unusual example, but since y is of length 11 and x is of length 5, x
# needs to be repeated twice and the first number from x then gets added as
# the last element for this operation to work (but we do get a warning message
# that this is a bit funky)

# max(), min(), and range() functions
min(x)
max(x)
range(x)

# length() to get the number of elements in a vector
length(x)

# take the sum of the elements in x
sum(x)

# since sum() and length() (and the other functions above) return vectors, we
# can then easily do further things with them
sum(x) / length(x)

# of course we can use mean(x) directly, but this illustrate the idea that the
# results returned from functions can be used for further steps

# sample variance of the elements in x
var(x)

# we could do the calculations manually with this
sum((x - mean(x))^2) / (length(x) - 1)

# just to make this very clear, we can break down the steps as follows
mean(x)
x - mean(x)
(x - mean(x))^2
sum((x - mean(x))^2)
length(x)
length(x) - 1
sum((x - mean(x))^2) / (length(x) - 1)

# sort the elements in x
sort(x)

# but again, note that x in itself in unchanged (unless of course we 'back
# assign' what sort(x) returns with x <- sort(x))

# illustrate the order() function
x
order(x)

# so the 3rd element from x is the lowest number, the 2nd element is the next
# highest number, the 4th element comes next, and so on

# illustrate pmin() and pmax() (parallel minimum/maximum)
x
pmax(x, 6)
pmin(x, 6)

## 2.3: Generating regular sequences

# create a sequence of numbers from 1 to 30
1:30

# : has high precedence, so need to use parentheses as needed
n <- 10
1:n-1
1:(n-1)

# backwards sequence
30:1

# seq() is a more flexible way for creating sequences, but these two are the same
seq(2,10)
2:10

# look at the help file for the seq() function
help(seq)

# we see that the first two arguments are called 'from' and 'to', so we can
# also use 'named arguments' and then the order does not matter
seq(from=1, to=30)
seq(to=30, from=1)

# illustrate the 'by' argument of seq()
seq(-5, 5, by=.2)

# illustrate the 'length.out' argument of seq()
seq(1, 50, length.out=8)
seq(1, 52, length.out=8)

# illustrate the 'along' argument of seq()
x
seq(along=x)

# repeat the 'x' vector 5 times
rep(x, times=5)

# repeat each element of 'x' 5 times
rep(x, each=5)

## 2.4: Logical vectors

x
temp <- x > 13
temp

# check for exact equality
x == 5.6

# illustrate | (or)
large <- x > 13
small <- x < 5
large
small
large | small

# illustrate !
large
!large

# can use logical vectors also in arithmetic in which case FALSE is treated as
# a 0 and TRUE is treated as a 1
large
lage * 5

## 2.5: Missing values

# cannot do this to indicate that the value for the third element is missing
c(4,2,,6)
c(4,2, ,6)

# use NA instead
c(4,2,NA,6)

# any operation on an NA becomes an NA
z <- c(4,2,NA,6)
z
z * 5

# this might also apply to statistical functions we use
mean(z)

# check the help file for the mean() function
help(mean)

# set the 'na.rm' argument to TRUE to remove NAs before computing the mean
mean(z, na.rm=TRUE)

# illustrate the is.na() function
z <- c(1:3,NA)
z
ind <- is.na(z)
ind

# this does not check if elements are NA, but makes a comparison of each
# element with NA, whose result is undecidable and hence NA
z == NA

# NaN = not a number
0/0
Inf - Inf

## 2.6: Character vectors

name <- c("Bob", "Sue", NA, "Joe")
name

# paste two strings together
paste("Bob", "Johnson")

# paste two vectors together
year <- c(1984, 1988, 1975, 1997)
paste(name, year)

# illustrate recycling of one vector and the 'sep' argument
labs <- paste(c("X","Y"), 1:10, sep="")
labs

# illustrate the 'collapse' argument
paste(name, collapse=", ")
paste(name, year, collapse=", ")

## 2.7: Index vectors; selecting and modifying subsets of a data set

x <- c(2,5,NA,7,6,3,NA,5,7,4)
x

# using a logical vector for selecting elements
!is.na(x)
y <- x[!is.na(x)]
y

# can read this as: give me from x the elements that are not missing

# a more complex example
y <- x[!is.na(x) & x > 5]
y

# can read this as: give me from x the elements that are not missing AND that
# are greater than 5

# this can be useful for example to select the data for a particular subgroup;
# say the group variable is coded as 1, 2, 3; then we could do this
grp <- c(1,3,2,2,1,3,2,3,1,2)
grp == 1
x[grp == 1]

# using an index vector for selecting elements
x[4]        # give me the 4th element
x[c(4,6)]   # give me elements 4 and 6
x[2:5]      # give me elements 2 through 5
x[c(4,6,4)] # can also repeat an index value

# this can be useful for example when creating a figure (e.g., scatterplot)
# and we want to use a different color for different groups; in essence, we
# need to have a color for each person, which we could get as follows
grp
c("blue", "red", "green")[grp]

# using negative indices for excluding elements
x
x[-c(2,4)] # remove elements 2 and 4
x[-(1:5)]  # remove elements 1 through 5

# create a named vector
fruit <- c("orange" = 5, "banana" = 10, "apple" = 1, "peach" = 20)
fruit

# can also leave off the quotes
fruit <- c(orange = 5, banana = 10, apple = 1, peach = 20)
fruit

# another way to create a named vector is to add names to an unnamed vector
fruit <- c(5, 10, 1, 20)
fruit
names(fruit) <- c("orange", "banana", "apple", "peach")
fruit

# can then use the names to select elements from the vector
fruit[c("apple","orange")]

# this can be useful when the group variable is a character variable
grp <- c("trt", "ctrl", "ctrl", "trt", "ctrl", "trt", "ctrl", "trt")
c(trt="blue", ctrl="red")[grp]

# replace the missing values in x with a 0
x
x[is.na(x)] <- 0
x

############################################################################

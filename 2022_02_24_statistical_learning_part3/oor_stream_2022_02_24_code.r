############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-02-24
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 2.3
#
# last updated: 2022-03-05

############################################################################

### basic commands

# create a vector of numbers
x <- c(1, 3, 2, 5)
x

# can also use = to do so
x = c(1, 6, 2)
x

# another vector
y = c(1, 4, 3)

# check the length of both vectors
length(x)
length(y)

# add up the elements in each vector elementwise
x + y

# list objects in your 'workspace'
ls()

# remove objects x and y
rm(x, y)

# list objects in your 'workspace'
ls()

# remove all objects in your workspace
rm(list = ls())

# open the documentation of the matrix function
?matrix

# this is a shortcut for using help
help("matrix")

# create a 2x2 matrix with the numbers 1-4
x <- matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x

# can also leave out the argument names
x <- matrix(c(1, 2, 3, 4), 2, 2)

# but note: this can be confusing, so only do this for the first argument
x <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
x

# fill in the numbers 'by row'
matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)

# take the square-root of each element in x
sqrt(x)

# square each element in x
x^2

# generate 50 random values from a standard normal distribution
# in other words: x_i ~ N(0,1) for i = 1, ..., 50
x <- rnorm(50)
x

# add to the elements in x random values from a normal distribution with mean
# 50 and standard deviation 0.1
y <- x + rnorm(50, mean = 50, sd = 0.1)
y

# correlation between x and y
cor(x, y)

# set the seed of the random number generator to make things reproducible;
# note that both sets of 50 values are exactly the same
set.seed(1303)
rnorm(50)
set.seed(1303)
rnorm(50)

# generate 100 values from a standard normal distribution
set.seed(3)
y <- rnorm(100)

# get the mean, variance, and standard deviation of the values in y
mean(y)
var(y)
sqrt(var(y))
sd(y)

############################################################################

### graphics

# simulate some data
x <- rnorm(100)
y <- rnorm(100)

# create a scatterplot of x versus y
plot(x, y)

# add axis labels and a title
plot(x, y, xlab = "this is the x-axis", ylab = "this is the y-axis",
     main = "Plot of X vs Y")

# save a figure to a pdf
pdf("figure.pdf")
plot(x, y, col = "green")
dev.off()

# seq() for creating sequences
x <- seq(1, 10)
x

# can also use this syntax
x <- 1:10
x

# for other types of sequences, can use the 'length' or 'by' arguments
seq(0, 1, length = 11)
seq(0, 10, by = 2)

# contour plots
x <- seq(-pi, pi, length = 50)
x
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))
contour(x, y, f)

# use argument 'nlevels' to adjust the number of contour levels in the plot
# and 'add=TRUE' means to add the contours to an already open contour plot
contour(x, y, f, nlevels = 45, add = TRUE)

# another (silly) example
fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)

# filled contour plot using the viridis color palette
filled.contour(x, y, fa, nlevels = 15, color.palette = hcl.colors)

# or we can use the image() function for this
image(x, y, fa)

# better use the viridis color palette here as well
image(x, y, fa, col = hcl.colors(12))

# perspective plot; use arguments 'theta' and 'phi' to change the points from
# which we look at the 3-d surface
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)
persp(x, y, fa, theta = 30, phi = 40, col = "lightblue")
persp(x, y, fa, theta = 30, phi = 40, col = "lightblue", shade = 0.4)

############################################################################

### indexing data

# create a 4x4 matrix
A <- matrix(1:16, nrow = 4, ncol = 4)
A

# give me from A the element in row 2 and column 3
A[2,3]

# give me the submatrix of A with rows 1 and 3 and columns 2 and 4
A[c(1,3), c(2,4)]

# rows 1-3 and columns 2-4
A[1:3, 2:4]

# give me rows 1 and 2 and all columns
A[1:2,]

# give me all rows and columns 1 and 2
A[,1:2]

# careful: when you take a single row, it turns into a vector
A[1,]

# to avoid that this happens, use drop=FALSE
A[1,,drop=FALSE]

# same issue when taking just a single column
A[,1]
A[,1,drop=FALSE]

# give me the submatrix of A with rows 1 and 3 removed
A[-c(1,3),]

# remove rows 1 and 3 and columns 1, 3, and 4
A[-c(1,3), -c(1,3,4)]
A[-c(1,3), -c(1,3,4), drop=FALSE]

# get the dimensions of the matrix A
dim(A)

############################################################################

### loading data

# 1) download the dataset from here: https://www.statlearning.com/s/Auto.data
#    and here: https://www.statlearning.com/s/Auto.csv

# can also use R to download the files
#download.file("https://www.statlearning.com/s/Auto.data", destfile="Auto.data")
#download.file("https://www.statlearning.com/s/Auto.csv",  destfile="Auto.csv")

# 2) put the datasets into the same directory/folder as this R script

# 3) set the working directory to the location of this R script; you can do
#    this with RStudio (menu 'Session', Set Working Directory, To Source File
#    Location, which sets the working directory with the setwd() function to
#    the currently opened R script); if your computer is configured so that R
#    scripts are automatically opened in RStudio, then RStudio will set the
#    working directory automatically (this only works when RStudio is not
#    already running)

# read in the data from Auto.data using the read.table() function
Auto <- read.table("Auto.data")

# show the first 6 rows of the dataset
head(Auto)

# get a spreadsheet-like view of the dataset
View(Auto)

# first row is a header row, missings are indicated with ?
Auto <- read.table("Auto.data", header = TRUE, na.strings = "?")
head(Auto)

# read in a csv file (note: header=TRUE is the default for read.csv())
Auto <- read.csv("Auto.csv", na.strings = "?")
head(Auto)
dim(Auto)
Auto[1:4, ]

# remove any row that has at least one missing value
Auto <- na.omit(Auto)
dim(Auto)

# get the variable names of the dataset
names(Auto)

############################################################################

### additional graphical and numerical summaries

# this will generate an error because R does not know where to find variables
# 'cylinders' and 'mpg'
plot(cylinders, mpg)

# can use the 'dollar notation' to specify where to find these variables
plot(Auto$cylinders, Auto$mpg)

# we are not going to use attach() as in the book, because this is not good
# practice; instead, we could use the 'data' argument to avoid the repeated
# use of Auto$
plot(mpg ~ cylinders, data = Auto)

# or could use with()
with(Auto, plot(cylinders, mpg))

# turn the cylinders variable into a factor
Auto$cylinders <- factor(Auto$cylinders)

# could use plot() as in the book, but to be explicit, create boxplots with
# the boxplot() function
boxplot(mpg ~ cylinders, data = Auto)
boxplot(mpg ~ cylinders, data = Auto, col = "red")
boxplot(mpg ~ cylinders, data = Auto, col = "red", varwidth = TRUE)
boxplot(mpg ~ cylinders, data = Auto, col = "red", varwidth = TRUE, horizontal = TRUE)
boxplot(mpg ~ cylinders, data = Auto, col = "red", varwidth = TRUE,
        xlab = "Number of Cylinders", ylab = "Miles per Galon")

# histograms
hist(Auto$mpg)
hist(Auto$mpg, xlab = "Miles per Galon", main = "")
hist(Auto$mpg, col = "lightblue")
hist(Auto$mpg, col = "lightblue", breaks = 15)

# note: breaks = 15 is a 'suggestion' for the number of breakpoints, so we are
# not guaranteed to get exactly 15 breakpoints (and hence 14 bins); we can
# also explicitly set the exact location of the breakpoints, which guarantees
# that we will get 14 bins; note that the lowest and highest bins are empty
# (i.e., they have a frequency of 0) but there are now exactly 14 bins
with(Auto, hist(mpg, col = "lightblue", breaks = seq(5, 50, length=15)))

# scatterplot matrix
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, data = Auto,
      pch = 19, cex = 0.5)

# the identify function allows you to interactively identify data points;
# left-click on points to label them, right-click to stop with this; the
# position of the select points is returned, so we can then examine these rows
# in the data frame
plot(mpg ~ horsepower, data = Auto)
rows <- with(Auto, identify(horsepower, mpg, labels = name))
rows
Auto[rows,]

# once we have identified some particular points to label, can also do the
# labeling manually using the text() function (pos = 4 means to place the
# labels to the right of the points)
rows <- c(103, 153, 331)
plot(mpg ~ horsepower, data = Auto)
with(Auto, text(horsepower[rows], mpg[rows], name[rows], pos = 4))

# a quick overview of all variables in a data frame
summary(Auto)

# get summary statistics of a single numeric variable
summary(Auto$mpg)

# for factor variables, summary() gives a frequency table
summary(Auto$cylinders)

############################################################################

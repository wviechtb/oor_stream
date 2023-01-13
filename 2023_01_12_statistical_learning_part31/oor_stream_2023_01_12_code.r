############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-01-12
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 12.2.3 - 12.2.5
#
# last updated: 2023-01-13

############################################################################

### 12.2.3: The Proportion of Variance Explained

# copy the USArrests dataset to dat
dat <- USArrests

# dimensions of the dataset
dim(dat)

# examine the first 6 rows of the dataset
head(dat)

# standardize all 4 variables and put them into X
X <- apply(dat, 2, scale)

# after standardization, each variable has a variance of 1
apply(X, 2, var)

# so the total variance (when we standardize each variable) is simply p
sum(apply(X, 2, var))
ncol(X)

# this is (12.8) when all variables are standardized

# in the book, they are not assuming that the variables are standardized (only
# mean centered), but in many cases, we do standardize the variables before a
# PCA, so let's also focus on this situation

# do the PCA with prcomp()
res <- prcomp(X)
res

# the standard deviations squared are the variances of the PCs
res$sdev^2

# this is essentially what they are referring to in equation (12.9)

# and equation (12.10) is therefore just the variances of the PCs divided by the
# total variance to begin with
res$sdev^2 / ncol(X)

# and the sum of the variances of the PCs is equal to p
sum(res$sdev^2)

# therefore we can also write
res$sdev^2 / sum(res$sdev^2)

# compute the cumulative sum of these values
cumsum(res$sdev^2 / sum(res$sdev^2))

# so, the first PC explains about 62% of the total variance, the second PC
# explains about 25%, so the first and second PC together explain roughly 87%
# (and so on if we want to pay attention also to the third and fourth PC)

# try out the decomposition in equation (12.11)
M <- 2
phi <- res$rotation[,1:M]
z <- res$x[,1:M]
pred <- z %*% t(phi)
n <- 50
sum(res$sdev[1:M]^2) + 1/(n-1) * sum((X - pred)^2)
#                  #   #                         #
####################   ###########################
# Var of first 2 PCs   MSE of 2-dimensional approximation

# note that we have to compute the last term using 1/(n-1), because scale()
# standardized each variable using their variances (or really, their standard
# deviations) computed using 1/(n-1) and the variances of the PCs (given by
# res$sdev^2) are also computed using 1/(n-1)

# Figure 13.3
par(mfrow=c(1,2))
plot(1:ncol(X), res$sdev^2 / sum(res$sdev^2), type="o", col="blue",
     xlab="Principal Component", ylab="Prop. Variance Explained",
     ylim=c(0,1), xaxt="n", lwd=3)
axis(side=1, at=1:4)
plot(1:ncol(X), cumsum(res$sdev^2) / sum(res$sdev^2), type="o", col="blue",
     xlab="Principal Component", ylab="Cumulative Prop. Variance Explained",
     ylim=c(0,1), xaxt="n", lwd=3)
axis(side=1, at=1:4)
par(mfrow=c(1,1))

# note: a more 'traditional' scree plot just puts the variances of the PCs on
# the y-axis (https://en.wikipedia.org/wiki/Scree_plot)
plot(1:ncol(X), res$sdev^2, type="o", col="blue",
     xlab="Principal Component", ylab="Variance of the PCs", xaxt="n", lwd=3)
axis(side=1, at=1:4)

############################################################################

### 12.2.4: More on PCA

## Scaling the Variables

# all of the results obtained earlier actually used standardized variables,
# because the 4 variables were not measured in the same units (in particular,
# UrbanPop uses a different scale than the other three variables)

## Uniqueness of the Principal Components

# to illustrate that the signs can be flipped, do a PCA and keep 2 components

res <- prcomp(X)
res

M <- 2
phi <- res$rotation[,1:M]
z <- res$x[,1:M]
pred <- z %*% t(phi)
head(pred)

# now flip the sign of phi and z
phi <- -res$rotation[,1:M]
z <- -res$x[,1:M]
pred <- z %*% t(phi)
head(pred)

# the predicted values are exactly the same

## Deciding How Many Principal Components to Use

# we have already seen the scree plot earlier, but as noted in the book (and
# see also Wikipedia), there are criticisms of this approach for choosing the
# number of components to keep; an approach that is not mentioned in the book,
# but that could be considered state-of-the-art, is 'parallel analysis' (see:
# https://en.wikipedia.org/wiki/Parallel_analysis)

# in a parallel analysis, we reshuffle each variable independently of the
# other variables, which we can easily do with the sample() function, applying
# this to each column in X
X.s <- apply(X, 2, sample)

# in such a reshuffled dataset, any relationships between the variables are
# purely coincidental
cor(X.s)

# now we use these reshuffled data in a PCA
tmp <- prcomp(X.s)
tmp

# this tells us what kind of variances of the PCs we might expect to see if
# the variables are essentially unrelated to each other (or again, if there
# are only coincidental relationships between variables)

# we can repeat this process many times, saving the variances of the PCs from
# each iteration

set.seed(1234)

iters <- 1000

evmat <- matrix(NA, nrow=iters, ncol=ncol(X))

for (i in 1:iters) {
   X.s <- apply(X, 2, sample)
   tmp <- prcomp(X.s)
   evmat[i,] <- tmp$sdev^2
}

# and then we can see what the average value of these variances is
apply(evmat, 2, mean)

# we then compare the actual variances with these averages
res$sdev^2

# often this is done by drawing a scree plot and adding the mean variances to it
plot(1:ncol(X), res$sdev^2, type="o", col="blue",
     xlab="Principal Component", ylab="Variance of the PCs",
     xaxt="n", lwd=3)
axis(side=1, at=1:4)
lines(1:4, apply(evmat, 2, mean), type="o", col="red", lwd=3)
legend("topright", inset=.02, lwd=3, col=c("blue","red"),
       legend = c("PCs of the Actual Data", "PCs of the Reshuffled Data"))

# and only PCs with variances that are larger than the mean variances of the
# PCs from the reshuffled data are worth keeping; so in this case, the
# parallel analysis would suggest to only keep the very first principal
# component

# instead of doing this manually, we can use the 'psych' package for this

# install the 'psych' package (if needed)
#install.packages("psych")

# load the 'psych' package
library(psych)

# run a parallel analysis
fa.parallel(X, fa="pc", n.iter=1000, sim=FALSE)

############################################################################

### 12.2.5 Other Uses for Principal Components

# since we already did this in section 6.3.1, no need to repeat this here

############################################################################

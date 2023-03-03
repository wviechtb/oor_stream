############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-01-26
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 12.4.1
#
# last updated: 2023-03-03

############################################################################

### 12.4.1: K-Means Clustering

# a small toy example; say there are n=4 people that we want to cluster into
# k=2 groups; there are 7 possible ways of doing so

# (1), (2,3,4)
# (2), (1,3,4)
# (3), (1,2,4)
# (4), (1,2,3)
# (1,2), (3,4)
# (1,3), (2,4)
# (1,4), (2,3)

# note that (1), (2,3,4) is the same as (2,3,4), (1) since what we consider to
# be the 'first' and the 'second' cluster is arbitrary

# the equation to compute the total number of such partitionings is:
# https://en.wikipedia.org/wiki/Stirling_numbers_of_the_second_kind

n <- 4
k <- 2
1/factorial(k) * sum((-1)^(0:k) * choose(k,0:k) * (k-(0:k))^n)

# make up some data for the 4 individuals on p=2 features
x1 <- c(3,4,6,8)
x2 <- c(5,3,6,9)

# plot the data
plot(x1, x2, pch=19)
text(x1, x2, 1:4, pos=2)

# compute the total within-cluster variation for each of the 7 partitionings

assign <- read.table(text = "
1 2 2 2
2 1 2 2
2 2 1 2
2 2 2 1
1 1 2 2
1 2 1 2
1 2 2 1")

totalWC <- rep(0, nrow(assign))

for (r in 1:nrow(assign)) {

   C1 <- which(assign[r,] == 1)
   C2 <- which(assign[r,] == 2)

   distx1 <- outer(x1[C1], x1[C1], "-")
   distx2 <- outer(x2[C1], x2[C1], "-")
   WC1 <- (sum(distx1^2) + sum(distx2^2)) / length(C1)

   distx1 <- outer(x1[C2], x1[C2], "-")
   distx2 <- outer(x2[C2], x2[C2], "-")
   WC2 <- (sum(distx1^2) + sum(distx2^2)) / length(C2)

   totalWC[r] <- WC1 + WC2

}

totalWC
assign[which.min(totalWC),]

# so we see that the (1,2),(3,4) clustering leads to the smallest objective
# function value, although (4),(1,2,3) is just a little bit worse

# when n is small, we can examine all possible partitionings, but this quickly
# becomes infeasible when n is large; in that case, we have to use something
# like algorithm 12.2 which hopefully will also find the partitioning that
# leads to the best solution (the one with the smallest total within-cluster
# variation)

############################################################################

# algorithm 12.2

X <- cbind(x1,x2)

# 1. randomly assign the observations to the K clusters

assign <- c(1,1,1,2)
assign

k2means <- function(assign, X) {

   assign.old <- rep(0,length(assign))

   # 2. iterate until cluster assignments stop changing

   while (sum(abs(assign.old - assign)) != 0) {

      print(assign.old)

      assign.old <- assign

      # (a) compute the cluster centroids
      centroid1 <- colMeans(X[assign==1,,drop=FALSE])
      centroid2 <- colMeans(X[assign==2,,drop=FALSE])

      # (b) assign each observation to the cluster whose centroid is closest
      distto1 <- apply(X, 1, function(x) sqrt(sum((x - centroid1)^2)))
      distto2 <- apply(X, 1, function(x) sqrt(sum((x - centroid2)^2)))
      assign <- ifelse(distto1 < distto2, 1, 2)

   }

   return(assign)

}

k2means(assign, X)

# when we start with the (1,2,3),(4) assignment, the algorithm doesn't find a
# better clustering and is actually getting stuck in a local optimum (we saw
# earlier that (1,2),(3,4) is the optimal solution)

# let's try out a different starting assignment

assign <- c(1,2,2,1)
k2means(assign, X)

# now the algorithm does find the optimal solution

# so the solution that the algorithm finds may not be the global optimum and
# the solution can depend on the starting assignment!

############################################################################

# try out the algorithm on a larger dataset

# simulate some data where there are essentially two groups
set.seed(1234)
x1 <- c(rnorm(100, 6, 1), rnorm(50, 3, 1))
x2 <- c(rnorm(100, 3, 1), rnorm(50, 8, 1))
X <- cbind(x1,x2)

# plot the data
plot(x1, x2, pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)))

# randomly assign the 150 people to two clusters
assign <- sample(c(1,2), 150, replace=TRUE)

# compute the centroids
centroid1 <- colMeans(X[assign == 1,])
centroid2 <- colMeans(X[assign == 2,])

# plot the data again but with color to show the starting arrangement
plot(x1, x2, pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)),
     col=ifelse(assign==1, "dodgerblue", "firebrick"))

# add the centroids to the plot
points(centroid1[1], centroid1[2], pch=21, cex=4, lwd=5, bg="dodgerblue")
points(centroid2[1], centroid2[2], pch=21, cex=4, lwd=5, bg="firebrick")

# run the algorithm
assign <- k2means(assign, X)

# recompute the centroids based on the solution found
centroid1 <- colMeans(X[assign == 1,])
centroid2 <- colMeans(X[assign == 2,])

# plot the data again, but also show the region where points will be assigned
# to the first versus second cluster

plot(NA, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)), xlab="x1", ylab="x2")

x1s <- seq(min(x1), max(x1), length=500)
x2s <- seq(min(x2), max(x2), length=500)
Xs <- expand.grid(x1s, x2s)
distto1 <- apply(Xs, 1, function(x) sqrt(sum((x - centroid1)^2)))
distto2 <- apply(Xs, 1, function(x) sqrt(sum((x - centroid2)^2)))
assign.grid <- ifelse(distto1 < distto2, 1, 2)
cols <- ifelse(assign.grid==1, rgb(.6,.9,1),
                               rgb(1,.6,.6))
points(Xs[,1], Xs[,2], pch=15, cex=0.9, col=cols, bg=cols)

points(x1, x2, pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)),
     col=ifelse(assign==1, "dodgerblue", "firebrick"))

points(centroid1[1], centroid1[2], pch=21, cex=4, lwd=5, bg="dodgerblue")
points(centroid2[1], centroid2[2], pch=21, cex=4, lwd=5, bg="firebrick")

############################################################################

# now do k-means clustering using the kmeans() function
res <- kmeans(X, centers=2)
res

# compare the solution found by kmeans() with the one we found earlier
table(assign, res$cluster)

# they are the same (except that the cluster 1 is cluster 2 and vice-versa)

# use 20 random starts (the function provides the best solution)
res <- kmeans(X, centers=2, nstart=20)
res

# note: when using kmeans(), remember to always use set.seed() to make the
# results fully reproducible; for example, we get two different solutions for
# k=3 clusters when running kmeans() twice
kmeans(X, centers=3)
kmeans(X, centers=3)

# but when fixing the seed before running kmeans(), the result will be the same
set.seed(1234)
kmeans(X, centers=3)
set.seed(1234)
kmeans(X, centers=3)

############################################################################

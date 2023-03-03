############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-03-02
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 12.4.2 - 12.4.3
#
# last updated: 2023-03-03

############################################################################

### 12.4.2: Hierarchical Clustering

# simulate some data where there are essentially two groups
set.seed(1234)
x1 <- c(rnorm(100, 6, 1), rnorm(50, 3, 1))
x2 <- c(rnorm(100, 3, 1), rnorm(50, 8, 1))
X <- cbind(x1,x2)

# plot the data
plot(x1, x2, pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)))

# compute Euclidean distances of the n=150 observations based on the p=2 features
d <- dist(X) # note: method="euclidean" is the default

# do hierarchical clustering
res <- hclust(d) # note: method="complete" is the default

# plot the dendrogram
plot(res)

# can interactively identify clusters by clicking on the plot (left click to
# draw boxes, right click to stop)
grp <- identify(res)
grp

# alternatively, we can use rect.hclust() to find the cut that creates a
# desired number of clusters in the plot
plot(res)
grp <- rect.hclust(res, k=2)
grp

# if we just want the group assignment according to a desired number of
# clusters, can also use the cutree() function
grp <- cutree(res, k=2)
grp

# get the group means on the features based on the group assignment
by(X, grp, colMeans)

# compare complete, average, and single linkage
par(mfrow=c(1,3))
res <- hclust(d, method="complete")
plot(res, main="Complete Linkage")
res <- hclust(d, method="average")
plot(res, main="Average Linkage")
res <- hclust(d, method="single")
plot(res, main="Single Linkage")

# plot the data again and label the points that are merged last according to
# single linkage
par(mfrow=c(1,1))
plot(x1, x2, pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)))
id <- c(31,87,125)
text(x1[id], x2[id], id, pos=4)

############################################################################

### 12.4.3: Practical Issues in Clustering

# a possible way to get a sense of whether the clusters are 'real' is to do
# something like 10-fold cross-validation, where we repeat k-means clustering
# leaving out 1/10th of the data each time

# 10-fold cross-validation

fold <- sample(rep(1:10, each=15))

par(mfrow=c(2,5))

for (i in 1:10) {

   res <- kmeans(X[fold!=i,], centers=2)
   plot(x1[fold!=i], x2[fold!=i], pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)),
        xlab="x1", ylab="xs", col=ifelse(res$cluster==1, "dodgerblue", "firebrick"))

}

# here, we see that the clusters that are being formed are quite consistent
# (the switching of the colors can be ignored)

# now simulate data where there really is only a single group
set.seed(2345)
x1 <- c(rnorm(100, 5, 1), rnorm(50, 5, 1))
x2 <- c(rnorm(100, 5, 1), rnorm(50, 5, 1))
X <- cbind(x1,x2)

# again do 10-fold CV

fold <- sample(rep(1:10, each=15))

for (i in 1:10) {

   res <- kmeans(X[fold!=i,], centers=2)
   plot(x1[fold!=i], x2[fold!=i], pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)),
        xlab="x1", ylab="xs", col=ifelse(res$cluster==1, "dodgerblue", "firebrick"))

}

# here, we see that the clusters that are being formed are less consistent

############################################################################

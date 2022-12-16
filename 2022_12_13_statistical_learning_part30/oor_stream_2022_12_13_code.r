############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-12-13
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 12.2.1 - 12.2.2
#
# last updated: 2022-12-16

############################################################################

### 12.2.1: What Are Principal Components?

# to illustrate the idea of constructing the first principal component, let's
# simulate the simplest case where we only have two variables (that are
# correlated to some extent); we will also standardize the two variables

set.seed(1235)

n <- 100
x1 <- scale(rnorm(n))[,1]
x2 <- scale(0.8 * x1 + rnorm(n))[,1]

# correlation between x1 and x2
cor(x1, x2)

# plot the data
plot(x1, x2, pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)))

# try out some linear combinations that fulfill the restriction that the sum
# of squared coefficients is equal to 1 and compute the corresponding variance

phi1 <- c(sqrt(.5), sqrt(.5))
sum(phi1^2)
var(phi1[1]*x1 + phi1[2]*x2)

phi1 <- c(.8, .6)
sum(phi1^2)
var(phi1[1]*x1 + phi1[2]*x2)

phi1 <- c(.6, .8)
sum(phi1^2)
var(phi1[1]*x1 + phi1[2]*x2)

phi1 <- c(.6, -.8)
sum(phi1^2)
var(phi1[1]*x1 + phi1[2]*x2)

# now we want to find the two coefficients that maximize the variance of the
# linear combination; let's do this by trial and error by computing the
# variance of a very large number of linear combinations we can form

phi11 <- seq(-1, 1, length=10000)

varcomb1 <- rep(NA_real_, length(phi11))
varcomb2 <- rep(NA_real_, length(phi11))

for (i in 1:length(phi11)) {
   phi21 <- sqrt(1 - phi11[i]^2) # this must be the value of phi21 given phi11
   varcomb1[i] <- var(phi11[i] * x1 + phi21 * x2) # variance of the linear combination
   phi21 <- -1*phi21 # but phi21 could also be negative
   varcomb2[i] <- var(phi11[i] * x1 + phi21 * x2) # variance of the linear combination
}

# plot phi11 against the variance of the linear combination
plot(phi11, varcomb1, type="l", lwd=3)
lines(phi11, varcomb2, lwd=3, lty="dotted")

# find the value of phi1 for which the variance is as large as possible
phi11 <- phi11[which.max(varcomb1)]

# compute the corresponding value of phi2
phi21 <- sqrt(1 - phi11^2)

# print the values
c(phi11, phi21)

# note: there are technically two possibilities; either phi11 and phi21 are
# both positive or they are both negative (which would give the same maximized
# variance as shown in the plot above); so in essence, the sign of the
# coefficients could be flipped

# add a vertical line at phi1 and a horizontal line at the maximum variance
abline(v=phi11, lty="dotted")
abline(h=max(varcomb1), lty="dotted")

# compute the first principal component (PC)
z1 <- phi11 * x1 + phi21 * x2
z1

# this newly derived variable has the largest possible variance
var(z1)

# in practice, we wouldn't want to find the 'loadings' by hand this way

# while in principle, one could formulate the problem of finding the loadings
# as an optimization problem as shown in (12.3), this is actually slightly
# tricky, since this is a non-convex optimization problem; however, it turns
# out that we don't need to do this via optimization at all, since we can use
# an eigenvalue-eigenvector decomposition to find them

# put x1 and x2 into matrix X
X <- cbind(x1, x2)

# then var(X) gives us the variance-covariance matrix of the two variables
var(X)

# since x1 and x2 are standardized, var(X) is actually the correlation matrix
cor(X)

# now do the eigenvalue-eigenvector decomposition of this correlation matrix
evd <- eigen(cor(X))
evd

# show that cor(X) is identical to the eigenvector matrix times the diagonal
# eigenvalue matrix times the inverse of the eigenvector matrix
cor(X)
evd$vectors %*% diag(evd$values) %*% solve(evd$vectors)

# note that the first column of evd$vectors contains the loadings for the
# first PC (these values are slightly different than the values we found
# earlier using trial-and-error because that approach is not as accurate)

# recompute the first PC based on the more accurate loadings from eigen()
phi1 <- evd$vectors[,1]
z1 <- phi1[1] * x1 + phi1[2] * x2
z1

# the second column of evd$vectors contains the loadings for the second PC
phi2 <- evd$vectors[,2]
z2 <- phi2[1] * x1 + phi2[2] * x2
z2

# double-check that the sum of the squared loadings for the two PCs are 1
sum(phi1^2)
sum(phi2^2)

# check that the first and second PC are uncorrelated
cor(z1, z2)

# note: the eigenvalues are identical to the variances of the two PCs
evd$values
c(var(z1), var(z2))

# plot the original data again and next to it the PCs
par(mfrow=c(1,2), pty="s")
plot(x1, x2, pch=19, xlim=range(c(x1,x2)), ylim=range(c(x1,x2)))
abline(a=0, b=evd$vectors[1,1] / evd$vectors[2,1], lty="dotted")
abline(a=0, b=evd$vectors[1,2] / evd$vectors[2,2], lty="dotted")
plot(z1, z2, pch=19, xlim=range(c(z1,z2)), ylim=range(c(z1,z2)))
abline(h=0, lty="dotted")
abline(v=0, lty="dotted")

############################################################################

# as in the book, we will now use the USArrests dataset for a PCA

# copy the USArrests dataset to dat
dat <- USArrests

# dimensions of the dataset
dim(dat)

# examine the first 6 rows of the dataset
head(dat)

# standardize all 4 variables and put them into X
X <- apply(dat, 2, scale)

# add the states as row names to X
rownames(X) <- rownames(dat)

# correlation matrix
cor(X)

# get the eigenvectors and values
evd <- eigen(cor(X))
evd

# the columns in evd$vectors give the loadings for the 4 PCs; note that the
# signs of the eigenvectors we obtain above may actually be flipped compared
# to what is shown in Table 12.1 in the book (this can depend on your CPU and
# the linear algebra routines that R uses)

# to ensure that the signs are the same as in the book, check the sign of the
# element in position (1,1); if it is negative, flip the sign of the entire
# eigenvector matrix
if (evd$vectors[1,1] < 0)
   evd$vectors <- -1 * evd$vectors

# put the eigenvectors into a separate object
evec <- evd$vectors

# now compute the first and second PC
z1 <- c(X %*% evec[,1])
z2 <- c(X %*% evec[,2])

# plot the first PC against the second PC
par(mfrow=c(1,1), mar=c(5,5,3,3))
plot(z1, z2, cex=0, xlim=range(c(z1,z2)), ylim=range(c(z1,z2)),
     xlab="First Principal Component", ylab="Second Principal Component")
abline(h=0, lty="dotted")
abline(v=0, lty="dotted")
text(z1, z2, rownames(dat), col="dodgerblue")

# add arrows for the loadings on the first and second PC
par(usr=c(-1.2,1.2,-1.2,1.2))
arrows(0, 0, evec[,1], evec[,2], col="orange", lwd=2)
axis(side=3, col.axis="orange", col.ticks="orange")
axis(side=4, col.axis="orange", col.ticks="orange")
text(1.25*evec[,1], 1.25*evec[,2], colnames(dat), col="orange", cex=1.2, font=2)

# create a loading plot (separate plot of the loadings instead of adding them
# to the 'biplot' above)
plot(evec[,1], evec[,2], pch=19, xlim=c(-1,1), ylim=c(-1,1),
     xlab="PC1 Loading", ylab="PC2 Loading")
text(evec[,1], evec[,2], colnames(dat), pos=4)
abline(h=0, lty="dotted")
abline(v=0, lty="dotted")

############################################################################

# in practice, we don't want to do PCA by hand as we did above; R comes with
# the princomp() and prcomp() functions to do a PCA

# use princomp() to do a PCA
res <- princomp(X, cor=TRUE)
res

# print the loadings (note: by default, loadings below 0.1 (in absolute value)
# are not shown; setting 'cutoff' to 0 shows all loadings)
print(loadings(res), digits=7, cutoff=0)

# show the first 6 rows of the PCs
head(res$scores)

# use prcomp() function to do PCA
res <- prcomp(X, center=TRUE, scale.=TRUE)
res

# note that the loadings have flipped signs again; let's flip their sign
res$rotation <- -1 * res$rotation

# element 'x' contains the actual PCs; flip their signs as well
res$x <- -1 * res$x

# print again
res

# show the first 6 rows for the PCs
head(res$x)

# create the biplot
biplot(res, col=c("dodgerblue","orange"), scale=0)

# note: the values of the PCs are slightly different when using princomp() and
# when using prcomp(); this has to do with how variances are computed in the
# two functions; prcomp() uses the commonly divisor of N-1; also, as noted in
# help(prcomp), this function uses a singular value decomposition for getting
# the loadings, which is "the preferred method for numerical accuracy"

# the 'psych' package also provides a function for PCA

# install the 'psych' package (if needed)
#install.packages("psych")

# load the 'psych' package
library(psych)

# use psych::principal() to do PCA
res <- principal(X, nfactors=ncol(X), rotate="none")
res

# note that the values shown are 'standardized loadings', that is, these are
# the loadings rescaled by the square root of the eigenvalues; we can undo
# this as follows
t(t(res$loadings[,]) / sqrt(res$values))

# show the first 6 rows for the PCs
head(res$scores)

# again, these are different than the ones we found earlier, because these are
# standardized PC scores; we can undo this as follows
res$scores <- t(t(res$scores) * sqrt(res$values))
head(res$scores)

############################################################################

### 12.2.2: Another Interpretation of Principal Components

# say M = 2 and we want to compute the approximate values of the 'Murder'
# variable based on the first two PCs; pull out the loading matrix and the
# principal component scores from 'res' and use the same notation
res <- prcomp(X, center=TRUE, scale.=TRUE)
phi <- res$rotation[,1:2]
z <- res$x[,1:2]
pred.murder <- z[,1] * phi[1,1] + z[,2] * phi[1,2]
pred.murder

# plot the 'predicted' against the actual value for this variable
plot(pred.murder, X[,"Murder"], pch=19, xlim=c(-2,2.5), ylim=c(-2,2.5),
     xlab="Predicted Values Based on PC1 and PC2", ylab="Actual Value")

# predict the values of all 4 variables based on the first two PCs
pred <- z %*% t(phi)

# compute the squared distance between the actual and predicted value for each
# variable, sum up these squared distances over the states for each variable,
# and then sum these sums over the 4 variables
sum(apply((X - pred)^2, 2, sum))

# the scores and loadings and for the first two PCs minimize this value

# try to obtain the scores and loadings by framing this as an optimization
# problem as shown in (12.6)
optfun <- function(par, X) {
   z <- matrix(par[1:100], nrow=50, ncol=2)
   phi <- matrix(par[101:108], nrow=4, ncol=2)
   pred <- z %*% t(phi)
   sum(apply((X - pred)^2, 2, sum))
}

opt <- nlminb(rep(1,108), optfun, X=X)
opt

# note that the value for the objective function is the same as above

# get the scores and loadings from 'opt'
z.opt <- matrix(opt$par[1:100], nrow=50, ncol=2)
phi.opt <- matrix(opt$par[101:108], nrow=4, ncol=2)

# predict the values of all 4 variables based on the first two PCs from 'opt'
pred.opt <- z.opt %*% t(phi.opt)
rownames(pred.opt) <- rownames(dat)
colnames(pred.opt) <- colnames(dat)

# compare the two sets of predicted values (not exactly identical because the
# optimization approach introduces some slight inaccuracies, but this actually
# works)
head(pred)
head(pred.opt)

# note: the values of z.opt and phi.opt do not match up with z and phi because
# there is not a unique solution (see also footnote 4), but as we see above,
# the predicted values (or the 'projections' of the original data onto the
# plane defined by the first two PCs that best fits the data) are the same

############################################################################

# the optimization approach above can be simplified, since the principal
# component scores in z are a function of phi (the loadings) and X; so we do
# not really need to optimize over the scores

optfun <- function(par, X) {
   phi <- matrix(par[1:8], nrow=4, ncol=2)
   z <- X %*% phi
   pred <- z %*% t(phi)
   sum(apply((X - pred)^2, 2, sum))
}

opt <- nlminb(rep(1,8), optfun, X=X)
opt

phi.opt <- matrix(opt$par, nrow=4, ncol=2)
z.opt <- X %*% phi.opt
pred.opt <- z.opt %*% t(phi.opt)
rownames(pred.opt) <- rownames(dat)
colnames(pred.opt) <- colnames(dat)

head(pred)
head(pred.opt)

# this again gives us the same predicted values

############################################################################

# the above suggests that we may be able to use the same optimization approach
# to conduct a PCA in general; so instead of restricting the optimization over
# M (with M < p) PCs, we could also do the optimization over all p PCs and try
# to find their loadings; but we also need some restrictions, namely that the
# sum of the squared loadings for each PC is equal to 1 and that the variance
# of the scores on the first PC is as large as possible, followed by making
# the scores on the other PCs as large as possible under the restriction that
# the PC scores are uncorrelated; after some trial-and-error, it appears that
# it is sufficient to check for uncorrelatedness of the scores

optfun <- function(par, X) {
   p <- ncol(X)
   phi <- matrix(par[1:p^2], nrow=p, ncol=p)
   z <- X %*% phi
   pred <- z %*% t(phi)
   # the first part of this sum should be zero since the difference between
   # the observed and predicted values must be zero and the second part should
   # be zero since the correlation matrix of the principal component scores
   # should be an identity matrix
   sum(apply((X - pred)^2, 2, sum)) + sum((cor(z) - diag(p))^2)
}

# do PCA (and flip signs again) and extract the loadings for all 4 PCs
res <- prcomp(X, center=TRUE, scale.=TRUE)
res$rotation <- -1 * res$rotation
res$x <- -1 * res$x
phi <- res$rotation
z <- res$x

# now try to do the same via the optimization approach above
p <- ncol(X)
opt <- nlminb(rep(1,p^2), optfun, X=X)
opt

# extract the estimates (loadings) and the PC scores
phi.opt <- matrix(opt$par[1:p^2], nrow=p, ncol=p)
z.opt <- X %*% phi.opt

# reorder the columns based on the variance in the PC scores
phi.opt <- phi.opt[,order(apply(z.opt, 2, var), decreasing=TRUE)]
z.opt <- X %*% phi.opt

# add dimension names to phi.opt and z.opt
dimnames(phi.opt) <- dimnames(phi)
colnames(z.opt) <- colnames(z)

# compare the results
phi
phi.opt

# compare the variances of the PC scores
apply(z, 2, var)
apply(z.opt, 2, var)

# check that the correlations are 0
round(cor(z), 6)
round(cor(z.opt), 6)

# check that the sums of the squared loadings are equal to 1
apply(phi, 2, function(x) sum(x^2))
apply(phi.opt, 2, function(x) sum(x^2))

# this works! not sure how well this generalizes, but I tried this out for
# several different datasets and it worked for all of them (sometimes had to
# increase the number of iterations to obtain convergence)

############################################################################

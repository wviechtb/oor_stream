############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-10-18
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 9.1.1 - 9.2.2
#
# last updated: 2022-10-22

############################################################################

### 9.1.1: What Is a Hyperplane?

# simulate some data like in Figure 9.1
set.seed(1234)
n <- 1000
x1 <- runif(n, -1.5, 1.5)
x2 <- runif(n, -1.5, 1.5)

# plot the data
plot(x1, x2, pch=19, cex=0.5, col=ifelse(1+2*x1+3*x2 > 0, "#30b5ff", "#c27ac0"))

# the hyperplane is defined by 1 + 2*x1 + 3*x2 = 0; we can rearrange this:
#
#    1 +   2*x1 + 3*x2 = 0
#    1 +   2*x1        = -3*x2
# -1/3 - 2/3*x1        = x2
#
# hence, we have a line with an intercept of -1/3 and a slope of -2/3

# draw the hyperplane
abline(a = -1/3, b = -2/3, lwd=3)

# in general: for a hyperplane of the form b0 + b1*x1 + b2*x2 = 0, the
# intercept of the line is given by -b0/b2 and the slope is given by -b1/b2

# note that any rescaling of the coefficients by some constant c does not
# change the hyperplane! so b0*c + b1*c*x1 + b2*c*x2 = 0 leads to the same
# intercept and slope, since the constant c cancels out

############################################################################

### 9.1.2: Classification Using a Separating Hyperplane

# data for figure 9.2
dat <- structure(list(x1 = c(-0.25, 0.35, -0.59, -0.68, -1.54, -0.02, 0.02,
-1.15, -0.66, 0.89, -0.23, -0.44, 0.48, 1.95, 1.65, 1.41, 2.03, 3.18, 0.64,
1.17, 1.07, 0.99, 2.63, 3.32), x2 = c(3.55, 2.88, 2.9, 2.89, 1.82, 1.66, 1.66,
1.39, 1.13, 2.02, 0.36, -0.18, -0.19, 0.68, 0.48, 0.08, 0.01, -0.22, -0.77,
-0.87, -0.98, -1.58, -0.94, -1.1), group = c("blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple")), row.names = c(NA, -24L), class =
"data.frame")
dat

# draw figure 9.2
par(pty="s") # to generate a square plotting region
plot(dat$x1, dat$x2, col=ifelse(dat$group=="blue", "#30b5ff", "#c27ac0"),
     pch=19, cex=1.5, xlim=c(-1.5,3.5), ylim=c(-1.5,3.5), xlab="x1", ylab="x2")

# say the hyperplane is given by 0.2 + -1.5*x1 + 1.0*x2
b0 <-  0.2
b1 <- -1.5
b2 <-  1.0

# add the hyperplane to the plot
abline(a = -b0/b2, b = -b1/b2, lwd=3)

# add y variable to the dataset
dat$y <- ifelse(dat$group == "blue", 1, -1)
dat

# check that (9.8) is positive for all points
all(dat$y * (b0 + b1*dat$x1 + b2*dat$x2) > 0)

# this must be the case for a hyperplane that perfectly separates the groups

############################################################################

### 9.1.3: The Maximal Margin Classifier

# calculate the (shortest/perpendicular) distance of each point to the
# hyperplane defined above; the equation for this can be found here:
# https://en.wikipedia.org/wiki/Distance_from_a_point_to_a_line
dist <- abs(b0 + b1*dat$x1 + b2*dat$x2) / sqrt(b1^2 + b2^2)
dist

# determine the smallest distance
min(dist)

# determine which point has the smallest distance
mindist <- which.min(dist)
mindist

# draw that point in red
points(dat$x1[mindist], dat$x2[mindist], pch=19, cex=1.5, col="red")

# draw in the distance to the hyperplane for this point (note: the equation
# for the coordinates on the hyperplane is also given under the link above)
arrows(dat$x1[mindist], dat$x2[mindist],
       (b2*( b2*dat$x1[mindist] - b1*dat$x2[mindist]) - b1*b0) / (b1^2 + b2^2),
       (b1*(-b2*dat$x1[mindist] + b1*dat$x2[mindist]) - b2*b0) / (b1^2 + b2^2),
       code=3, length=0.15, lwd=2)

# the maximal margin classifier is the hyperplane that perfectly separates the
# two groups and that creates the largest possible distance between all points
# and the hyperplane; in essence, this means that the smallest distance should
# be as large as possible

############################################################################

### 9.1.4: Construction of the Maximal Margin Classifier

# find the maximal margin classifier based on the given algorithm; this is an
# optimization problem with one equality constraint (9.10) and one inequality
# constraint (9.11); we can do this with the constrOptim.nl() function from
# the alabama package

# install (if necessary) the alabama package
#install.packages("alabama")

# load the alabama package
library(alabama)

# this is the function we want to maximize (9.9); it takes a vector with the
# parameter values as input (i.e., for b0, b1, b2, and M) and the x1, x2, and
# y values and simply returns M (since M should be maximized)
maxfun <- function(par, x1, x2, y)
   par[4] # note: the 4th value in par is M

# function to define the equality constraint (9.10)
heqfun <- function(par, x1, x2, y)
   par[2]^2 + par[3]^2 - 1 # b1^2 + b2^2 - 1 = 0

# function to define the inequality constraint (9.11)
hinfun <- function(par, x1, x2, y)
   y * (par[1] + par[2]*x1 + par[3]*x2) - par[4] # y * (b0 + b1*x1 + b2*x2) - M >= 0

# set the starting values for b0, b1, and b2 in 'par'; we use the same values
# as above, but now rescale them so they fulfill the equality constraint
b.start <- c(b0, b1, b2) / sqrt(b1^2 + b2^2)

# run the optimizer; by default the function does minimization, but we can
# turn this into a maximization problem with control.optim=list(fnscale=-1);
# the values given to 'par' are the starting values for the parameters
res <- constrOptim.nl(par=c(b.start,0), fn=maxfun, heq=heqfun, hin=hinfun,
                      x1=dat$x1, x2=dat$x2, y=dat$y, control.optim=list(fnscale=-1))
res

# extract the coefficients for the maximal margin classifier
b0 <- res$par[1]
b1 <- res$par[2]
b2 <- res$par[3]

# draw figure 9.3
par(pty="s")
plot(dat$x1, dat$x2, col=ifelse(dat$group=="blue", "#30b5ff", "#c27ac0"),
     pch=19, cex=1.5, xlim=c(-1.5,3.5), ylim=c(-1.5,3.5), xlab="x1", ylab="x2")

# add the hyperplane for the maximal margin classifier to the plot
abline(a = -b0/b2, b = -b1/b2, lwd=3)

# check that the equality constraint is fulfilled
b1^2 + b2^2

# extract the value of M (note that this is larger than min(dist) above, since
# the maximal margin classifier maximizes the smallest distance)
M <- res$par[4]
M

# calculate the distances (note: because of the equality constraint, the
# equation for calculating the distances simplifies to this)
dist <- abs(b0 + b1*dat$x1 + b2*dat$x2)
dist

# determine the smallest distance (which is M)
min(dist)

# determine which points have the smallest distance (there are actually
# multiple); for this, we check which points have a distance that is equal to
# or smaller than the smallest distance plus a bit of a judge factor (to allow
# for some numerical imprecision)
mindist <- which(dist <= min(dist) + .Machine$double.eps^0.25)
mindist

# draw these points in red
points(dat$x1[mindist], dat$x2[mindist], pch=19, cex=1.5, col="red")

# draw in the distances to the hyperplane for these points
arrows(dat$x1[mindist], dat$x2[mindist],
       (b2*( b2*dat$x1[mindist] - b1*dat$x2[mindist]) - b1*b0) / (b1^2 + b2^2),
       (b1*(-b2*dat$x1[mindist] + b1*dat$x2[mindist]) - b2*b0) / (b1^2 + b2^2),
       code=3, length=0.15, lwd=2)

# figure out the value we need to add/subtract to the intercept for the margin
abs(dat$x2[mindist] - (-b0/b2 + -b1/b2 * dat$x1[mindist]))[1]

# add the margin to the plot
abline(a = -b0/b2 - margin, b = -b1/b2, lwd=3, lty="dotted")
abline(a = -b0/b2 + margin, b = -b1/b2, lwd=3, lty="dotted")

# prediction for a new data point (e.g., test data)
b0 + b1*0.2 + b2*1

# add predictions for an entire grid of points to the plot
x1x2grid <- expand.grid(x1=seq(-4,4,length=101), x2=seq(-4,4,length=101))
points(x1x2grid[,1], x1x2grid[,2], pch=19, cex=0.1,
       col=ifelse(b0 + b1*x1x2grid[,1] + b2*x1x2grid[,2] > 0, "#30b5ff", "#c27ac0"))

############################################################################

# try the same with svm() from the e1071 package

# create group factor variable (needed below by svm() function)
dat$fgroup <- factor(dat$group)

# install (if necessary) the e1071 package
#install.packges("e1071")

# load the e1071 package
library(e1071)

# when setting the 'cost' argument high, then this yields the same as above
res.svm <- svm(fgroup ~ x1 + x2, data=dat, kernel="linear", scale=FALSE, cost=10)

# examine the coefficients and compare these to the ones we found above
coefcomp <- data.frame(from.svm = coef(res.svm), own = c(b0, b1, b2))
coefcomp

# they are not the same because the coefficients given by svm() do not fulfill
# the equality constraint (b1^2 + b2^2 is not equal to 1)
coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2

# but we can rescale the coefficients so they fulfill the equality constraint
# and now they are the same as the one we found (within a margin of error)
coefcomp[1] <- coefcomp[1] / sqrt(coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2)
round(coefcomp, digits=4)

# extract the coefficients that svm() provided
b0 <- coef(res.svm)[[1]]
b1 <- coef(res.svm)[[2]]
b2 <- coef(res.svm)[[3]]

# draw again the line for the hyperplane (in red) and check that it is the
# same as the one we already drew earlier
abline(a = -b0/b2, b = -b1/b2, lwd=3, col="red")

# can also plot res.svm directly, but note that x1 and x2 are flipped
plot(res.svm, dat[c("x1","x2","fgroup")], col=c("#30b5ff","#c27ac0"))

# we could get the same arrangement by reordering the predictors
res.svm <- svm(fgroup ~ x2 + x1, data=dat, kernel="linear", scale=FALSE, cost=10)
plot(res.svm, dat[c("x1","x2","fgroup")], col=c("#30b5ff","#c27ac0"))

############################################################################

# actually the manual optimization we did earlier is not really how this
# problem should be solved; the more common way of phrasing the problem is:
#
# minimize ||beta|| over b0, b1, b2
# subject to the constraint: y * (b0 + b1*x1 + b2*x2) >= 1 for all points
#
# see: https://en.wikipedia.org/wiki/Support_vector_machine#Hard-margin
#
# this is also explained in more detail in: Hastie, T., Tibshirani, R., and
# Friedman, J. (2008). The elements of statistical learning: Data mining,
# inference, and prediction (2nd Edition). Springer. (equation 12.4)
#
# this is a convex optimization problem:
# https://en.wikipedia.org/wiki/Convex_optimization
#
# an R package for this is CVXR

# install (if necessary) the CVXR package
#install.packages("CVXR")

# load the CVXR package
library(CVXR)

# set up the data and the problem and solve it
X <- cbind(1, dat$x1, dat$x2)
y <- dat$y
beta <- Variable(3)
objective <- norm2(beta)
constraints <- list(y * (X %*% beta) >= 1)
problem <- Problem(Minimize(objective), constraints)
result <- solve(problem)
c(result$getValue(beta))

# compare this against what we got from svm()
c(b0, b1, b2)

############################################################################

### 9.2.1: Overview of the Support Vector Classifier

### 9.2.2: Details of the Support Vector Classifier

# data for figure 9.7
dat <- structure(list(x1 = c(-1.71, -1.43, -0.34, 0.33, -0.27, -0.6, -0.08,
-0.47, -0.76, 0.51, 0.61, 1.54, 0.18, 0.65, 1.49, 0.92, 0.95, 1.03, 1.92,
1.94, 2.03, 2.34, 1.76, 2.1), x2 = c(2.97, 1.87, 1.57, 1.67, 1.41, 1.04, 1.25,
-0.32, -2.97, 0.84, 1.15, 1.52, 0.66, 0.62, 1.52, 0.12, -0.65, -0.58, -0.08,
-0.01, -0.1, -0.12, -0.95, -1.07), group = c("blue", "blue", "blue", "blue",
"blue", "blue", "blue", "blue", "blue", "blue", "blue", "blue", "purple",
"purple", "purple", "purple", "purple", "purple", "purple", "purple",
"purple", "purple", "purple", "purple"), y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1), fgroup =
structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), levels = c("blue", "purple"), class =
"factor")), row.names = c(NA, -24L), class = "data.frame")
dat

# values for the 'cost' argument (figured these out by trial-and-error)
costvals <- c(0.01, 0.08, 0.5, 3)

# note: the cost argument is inversely related to the value of C described in
# the book chapter

par(mfrow=c(2,2), pty="s")

for (i in 1:length(costvals)) {

   # plot the data
   plot(dat$x1, dat$x2, col=ifelse(dat$group=="blue", "#30b5ff", "#c27ac0"),
        pch=19, cex=1.5, xlab="x1", ylab="x2")

   # get the support vector classifier
   res.svm <- svm(fgroup ~ x1 + x2, data=dat, kernel="linear",
                  scale=FALSE, cost=costvals[i])

   # put circles around the support vectors
   symbols(dat$x1[res.svm$index], dat$x2[res.svm$index], inches=FALSE,
           add=TRUE, circles=rep(.1,length(res.svm$index)), lwd=2)

   # extract the rescaled coefficients
   b0 <- coef(res.svm)[[1]] / sqrt(coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2)
   b1 <- coef(res.svm)[[2]] / sqrt(coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2)
   b2 <- coef(res.svm)[[3]] / sqrt(coef(res.svm)[[2]]^2 + coef(res.svm)[[3]]^2)

   # add the hyperplane for the support vector classifier to the plot
   abline(a = -b0/b2, b = -b1/b2, lwd=3)

   # determine which support vector is on the correct side of the hyperplane
   # and that has the largest distance to the hyperplane and get its distance
   # (based on this, we can determine the margin)
   dist <- abs(b0 + b1*dat$x1 + b2*dat$x2)
   maxdist <- dist == max(dist[1:nrow(dat) %in% res.svm$index & res.svm$fitted == dat$fgroup])
   M <- dist[maxdist]

   # figure out the value we need to add/subtract to the intercept for the margin
   margin <- abs(dat$x2[maxdist] - (-b0/b2 + -b1/b2 * dat$x1[maxdist]))[1]

   # add the margin to the plot
   abline(a = -b0/b2 - margin, b = -b1/b2, lwd=3, lty="dotted")
   abline(a = -b0/b2 + margin, b = -b1/b2, lwd=3, lty="dotted")

   title(paste0("Cost Value: ", costvals[i]))

}

############################################################################

# fit again the support vector classifier with a specific cost value
cost <- 0.5
res.svm <- svm(fgroup ~ x1 + x2, data=dat, kernel="linear", scale=FALSE, cost=cost)
coef(res.svm)

# analogous to what we did earlier, we can phrase the problem as follows:
#
# minimize 1/2 * ||beta||^2 + C * sum(eps) over b0, b1, b2
# subject to the constraints: eps >= 0 and y * (b0 + b1*x1 + b2*x2) >= 1 - eps
#
# see: Hastie et al. (2008), equation (12.8)
#
# we can again solve this with the help of the CVXR package

X <- cbind(1, dat$x1, dat$x2)
y <- dat$y
beta <- Variable(3)
eps <- Variable(nrow(dat))
objective <- 1/2 * norm2(beta)^2 + cost * sum(eps)
constraints <- list(eps >= 0, y * (X %*% beta) >= 1 - eps)
problem <- Problem(Minimize(objective), constraints)
result <- solve(problem)
c(result$getValue(beta))

# this matches up nicely; it also works with larger cost values, but for some
# reason, when using smaller cost values the results are different

# Hastie et al. (2008), equation (12.25) provides another formulation of the
# problem which is also how this is is presented on Wikipedia; see:
# https://en.wikipedia.org/wiki/Support_vector_machine#Soft-margin

minfun <- function(par, x1, x2, y, lambda)
   sum(pmax(0, 1 - y * (par[1] + par[2]*x1 + par[3]*x2))) + lambda/2 * (par[2]^2 + par[3])

res <- nlminb(c(0.2,-1,0.5), minfun, x1=dat$x1, x2=dat$x2, y=dat$y, lambda=1/cost)
res$par

# sort of close; this is actually a bit of a tricky minimization problem
# because of pmax(0, ...), since different values for par could lead to the
# same value of minfun()

############################################################################

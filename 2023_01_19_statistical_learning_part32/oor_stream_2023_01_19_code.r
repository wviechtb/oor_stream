############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-01-19
#
# Topic(s):
# - An Introduction to Statistical Learning (https://www.statlearning.com)
# - Section(s): 12.3
#
# last updated: 2023-01-23

############################################################################

### 12.3: Missing Values and Matrix Completion

# copy the USArrests dataset to dat
dat <- USArrests

# dimensions of the dataset
dim(dat)

# examine the first 6 rows of the dataset
head(dat)

# standardize all 4 variables and put them into X
X <- scale(dat)
X

# using X[,], we can get rid of the attributes (means and SDs) that scale()
# attaches to the matrix it returns (don't need these)
X <- X[,]
X

# create a copy of X with missing values (in 20 randomly selected states, one
# randomly selected variable has a missing value)
set.seed(15)
nomit <- 20
ina <- sample(1:50, nomit)
inb <- sample(1:4, nomit, replace=TRUE)
Xna <- X
Xna[cbind(ina, inb)] <- NA
Xna

############################################################################

## algorithm 12.1

# step 1: do mean imputation for missing values within each variable

means <- colMeans(Xna, na.rm=TRUE)
means

for (i in 1:4) {
   Xna[,i] <- ifelse(is.na(Xna[,i]), means[i], Xna[,i])
}

Xna

# step 2

obj.old <- 0
change <- 1

while (change > 10^-8) {

   # step (a): do a PCA on the current version of Xna (keeping only M=1 of the
   # four PCs) and extract the component scores and the loadings (note: by
   # default, prcomp() does mean centering, but we want to switch this off in
   # this case)
   M <- 1
   res <- prcomp(Xna, center=FALSE)
   A <- res$x[,M,drop=FALSE]
   B <- res$rotation[,M,drop=FALSE]

   # step (b): predict the values that were missing to begin with based on the
   # PCA (i.e. update the imputations for the missing values based on the PCA)
   pred <- A %*% t(B)
   Xna[cbind(ina, inb)] <- pred[cbind(ina, inb)]

   # step (c): compute the objective (note: as shown in equation (12.14), this
   # is computed only for the entries that were not missing to begin with;
   # however, since we replaced in step (b) the missing entries in Xna with
   # the predicted values, the difference between Xna and pred for these
   # entries is 0 and hence we can just sum up all of the squared differences)
   obj.new <- sum((Xna - pred)^2)

   # compute the change in the objective value (new minus old)
   change <- abs(obj.new - obj.old)

   # set the value of the old objective to the current one
   obj.old <- obj.new

   # print the change
   print(change)

}

# copy Xna to pred
pred <- Xna

# since we have the original data, we can compare the actual values with the
# predicted values and compute the sum of the squared differences
sum((X[cbind(ina, inb)] - pred[cbind(ina, inb)])^2)

# Figure 12.5
cols <- c("#ff9233", "#29d0d0", "#81c57a", "#575757")
plot(X[cbind(ina, inb)], pred[cbind(ina, inb)], xlab="Original Value",
     ylab="Imputed Value", xlim=c(-1.8,1.8), ylim=c(-1.8,1.8), pch=19,
     col=cols[inb])
abline(a=0, b=1, lty="dashed")
text(X[cbind(ina, inb)], pred[cbind(ina, inb)], state.abb[ina], pos=3, cex=0.8)
legend("bottomright", inset=.02, col=cols, pch=19, legend=colnames(X))

# notes: 1) the figure in the book is missing two points (for ND and CA)
#        2) using the state.abb dataset for the two-letter abbreviations (but
#           need to check first that cbind(rownames(X), state.abb) matches)

# correlation between the actual and imputed values
round(cor(X[cbind(ina, inb)], pred[cbind(ina, inb)]), digits=2)

# correlation between the actual and imputed values when we predict based on a
# PCA with the complete dataset (again keeping only M=1 of the four PCs)
res <- prcomp(X)
A <- res$x[,M,drop=FALSE]
B <- res$rotation[,M,drop=FALSE]
pred.compl <- A %*% t(B)
round(cor(X[cbind(ina, inb)], pred.compl[cbind(ina, inb)]), digits=2)

############################################################################

# skipping the part where they repeat the above 100 times

############################################################################

# algorithm 12.1 "typically provides a good solution" to solving (12.12);
# however, we have covered previously how to solve (12.6) via optimization and
# this is really no different than (12.12) except that the sum is taken only
# over the non-missing values, so let's try solving (12.12) directly

# recreate the dataset with the missing values
Xna[cbind(ina, inb)] <- NA

# objective function to be minimized
optfun <- function(par, X) {
   A <- matrix(par[1:50], nrow=50, ncol=1)
   B <- matrix(par[51:54], nrow=4, ncol=1)
   pred <- A %*% t(B)
   sum((X - pred)^2, na.rm=TRUE)
}

# run the optimizer
opt <- nlminb(rep(1,54), optfun, X=Xna)
opt

# compute the predicted values based on the solution found
A.opt <- matrix(opt$par[1:50], nrow=50, ncol=1)
B.opt <- matrix(opt$par[51:54], nrow=4, ncol=1)
pred.opt <- A.opt %*% t(B.opt)
rownames(pred.opt) <- rownames(dat)
colnames(pred.opt) <- colnames(dat)
pred.opt

# compare the predicted values obtained via algorithm 12.1 with the predicted
# values found via the optimization approach (they are essentially identical)
data.frame(alg12.1 = pred[cbind(ina, inb)], opt = pred.opt[cbind(ina, inb)])

# note: as discussed previously, the estimates found (for the scores and
# loadings) via this approach are not unique, but the predicted values are; so
# we can solve (12.12) directly without needing algorithm 12.1

############################################################################

# recreate the dataset with the missing values
Xna[cbind(ina, inb)] <- NA

# let's compare the approach above with a more 'traditional' imputation method
# like regression imputation as implemented in the 'mice' package

# install the mice package
#install.packages("mice")

# load the mice package
library(mice)

# do plain old regression imputation (there is a bit of randomness because the
# starting values for the missing observations are random, but after about 20
# iterations, this will converge to the same imputed values)
imp <- mice(Xna, method="norm.predict", m=1, maxit=20, set.seed=1234)
Xna.imp <- complete(imp)

# correlation between the actual and imputed values
round(cor(X[cbind(ina, inb)], Xna.imp[cbind(ina, inb)]), digits=2)

# interestingly, this does worse than the PCA-based imputation approach above

# do regression imputation, but now add some random noise on top of the
# predicted values to reflect the uncertainty in the predictions; we repeat
# this process 10 times to generate 10 imputed datasets
imp <- mice(Xna, method="norm.nob", m=10, maxit=20, set.seed=1234)
Xna.imp <- complete(imp, action="all")

# correlation between the actual and imputed values for all 10 datasets
cors <- sapply(1:10, function(i)
               cor(X[cbind(ina, inb)], Xna.imp[[i]][cbind(ina, inb)]))
round(cors, digits=2)
mean(cors)

############################################################################

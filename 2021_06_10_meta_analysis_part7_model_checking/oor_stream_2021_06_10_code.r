############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-06-10
#
# Topic(s):
# - meta-analysis (part 7: model checking)
#
# last updated: 2021-06-11

############################################################################

# install the CRAN version of the metafor package
install.packages("metafor")

# install the 'devel' version of the metafor package
install.packages("remotes")
remotes::install_github("wviechtb/metafor")

# load metafor package
library(metafor)

############################################################################

# in essence, we are covering the contents from this book chapter:
#
# Viechtbauer, W. (2021). Model checking in meta-analysis. In C. H. Schmid, T.
# Stijnen, & I. R. White (Eds.), Handbook of meta-analysis (pp. 219-254). Boca
# Raton, FL: CRC Press.
#
# https://www.wvbauer.com/lib/exe/fetch.php/articles:viechtbauer2021a.pdf

############################################################################

# copy data into 'dat'

dat <- dat.viechtbauer2021

# calculate log odds ratios and corresponding sampling variances

dat <- escalc(measure="OR", ai=xTi, n1i=nTi, ci=xCi, n2i=nCi, add=1/2, to="all", data=dat)
dat

# number of studies

k <- nrow(dat)

# fit models

res.CE <- rma(yi, vi, data=dat, method="FE")
res.CE
predict(res.CE, transf=exp, digits=2)

res.RE <- rma(yi, vi, data=dat, method="DL")
res.RE
predict(res.RE, transf=exp, digits=2)

res.MR <- rma(yi, vi, mods = ~ dose, data=dat, method="FE")
res.MR

res.ME <- rma(yi, vi, mods = ~ dose, data=dat, method="DL")
res.ME

# forest and bubble plot

par(mar=c(5,4,1,2))

forest(dat$yi, dat$vi, psize=0.8, efac=0, xlim=c(-4,6), ylim=c(-3,23),
       cex=1, width=c(5,5,5), xlab="Log Odds Ratio (LnOR)")
addpoly(res.CE, row=-1.5, cex=1, width=c(5,5,5), mlab="CE Model")
addpoly(res.RE, row=-2.5, cex=1, width=c(5,5,5), mlab="RE Model")
text(-4, 22, "Trial",         pos=4, font=2)
text( 6, 22, "LnOR [95% CI]", pos=2, font=2)
abline(h=0)

tmp <- regplot(res.ME, xlim=c(0,250), ylim=c(-1,1.5), predlim=c(0,250), shade=FALSE, digits=1,
               xlab="Dosage (mg per day)", psize="seinv", plim=c(NA,5), bty="l", las=1,
               lty=c("solid", "dashed"), label=TRUE, labsize=0.8, offset=c(1,0.7))
res.sub <- rma(yi, vi, mods = ~ dose, data=dat, method="DL", subset=-6)
abline(res.sub, lty="dotted")
points(tmp$xi, tmp$yi, pch=21, cex=tmp$psize, col="black", bg="darkgray")

par(mar=c(5,4,4,2))

# standardized deleted (studentized) residuals for the models

rstudent(res.CE)
rstudent(res.RE)
rstudent(res.MR)
rstudent(res.ME)

# number of standardized deleted residuals larger than +-1.96 in each model

sum(abs(rstudent(res.CE)$z) >= qnorm(.975))
sum(abs(rstudent(res.MR)$z) >= qnorm(.975))
sum(abs(rstudent(res.RE)$z) >= qnorm(.975))
sum(abs(rstudent(res.ME)$z) >= qnorm(.975))

# probability of observing 1 or more, 2 or more, 3 or more, or 4 or more
# standardized deleted residuals larger than +-1.96 by chance alone when k=20

sum(dbinom(1:20, 20, .05))
sum(dbinom(2:20, 20, .05))
sum(dbinom(3:20, 20, .05))
sum(dbinom(4:20, 20, .05))

# plot of the standardized deleted residuals for the RE and ME models

plot(NA, NA, xlim=c(1,20), ylim=c(-4,4), xlab="Study", ylab="Standardized (Deleted) Residual",
     xaxt="n", main="Random-Effects Model", las=1)
axis(side=1, at=1:20)
abline(h=c(-1.96,1.96), lty="dotted")
abline(h=0)
points(1:20, rstandard(res.RE)$z, type="o", pch=19, col="gray70")
points(1:20, rstudent(res.RE)$z,  type="o", pch=19)
legend("top", pch=19, col=c("gray70","black"), lty="solid",
       legend=c("Standardized Residuals","Standardized Deleted Residuals"), bty="n")

plot(NA, NA, xlim=c(1,20), ylim=c(-4,4), xlab="Study", ylab="Standardized (Deleted) Residual",
     xaxt="n", main="Mixed-Effects Model", las=1)
axis(side=1, at=1:20)
abline(h=c(-1.96,1.96), lty="dotted")
abline(h=0)
points(1:20, rstandard(res.ME)$z, type="o", pch=19, col="gray70")
points(1:20, rstudent(res.ME)$z,  type="o", pch=19)
legend("top", pch=19, col=c("gray70","black"), lty="solid",
       legend=c("Standardized Residuals","Standardized Deleted Residuals"), bty="n")

# Baujat plots

baujat(res.CE, main="Common-Effects Model", xlab="Squared Pearson Residual", ylim=c(0,5), las=1)
baujat(res.ME, main="Mixed-Effects Model", ylim=c(0,2), las=1)

# GOSH plots (note: these take a while to run! might want to decrease 'subsets' value)

#load("results_gosh.rdata") # saved results before the stream

res.GOSH.CE <- gosh(res.CE, subsets=1000000)
res.GOSH.CE
plot(res.GOSH.CE, cex=0.15, out=6, xlim=c(-0.25,1.25), breaks=c(200,100))

res.GOSH.ME <- gosh(res.ME, subsets=100000)
res.GOSH.ME
plot(res.GOSH.ME, cex=0.2, het="tau2", out=6, breaks=50, adjust=0.6, las=1)

# plot of treatment dosage against the standardized residuals

plot(dat$dose, rstandard(res.ME)$z, pch=19, xlab="Dosage (mg per day)",
     ylab="Standardized Residual", xlim=c(0,250), ylim=c(-2.5,2.5), las=1)
abline(h=c(-1.96,1.96), lty="dotted", lwd=2)
abline(h=0)
title("Standardized Residual Plot")
text(dat$dose[6], rstandard(res.ME)$z[6], "6", pos=4, offset=0.4)

# quadratic polynomial model

rma(yi, vi, mods = ~ dose + I(dose^2), data=dat, method="DL")

# lack-of-fit model

resLOF <- rma(yi, vi, mods = ~ dose + factor(dose), data=dat, method="DL", btt=3:9)
resLOF

# scatter plot to illustrate the lack-of-fit model

regplot(res.ME, xlim=c(0,250), ylim=c(-1.0,1.5), xlab="Dosage (mg per day)", ci=FALSE,
        predlim=c(0,250), psize=1, pch=19, col="gray60", digits=1, lwd=1, bty="l", las=1)
dosages <- sort(unique(dat$dose))
lines(dosages, fitted(resLOF)[match(dosages, dat$dose)], type="o", pch=19, cex=2, lwd=2)
points(dat$dose, dat$yi, pch=19, col="gray60")
legend("bottomright", legend=c("Linear Model", "Lack-of-Fit Model"), pch=c(NA,19), col="black",
       lty="solid", lwd=c(1,2), pt.cex=c(1,2), seg.len=4, bty="n")

# checking normality of the standardized deleted residuals

qqnorm(res.ME, type="rstudent", main="Standardized Deleted Residuals", pch=19, label="out",
       lwd=2, pos=24, ylim=c(-4,3), lty=c("solid", "dotted"), las=1)

# checking normality of the random effects

sav <- qqnorm(ranef(res.ME)$pred, main="BLUPs of the Random Effects", cex=1, pch=19,
              xlim=c(-2.2,2.2), ylim=c(-0.6,0.6), las=1)
abline(a=0, b=sd(ranef(res.ME)$pred), lwd=2)
text(sav$x[6], sav$y[6], "6", pos=4, offset=0.4)

# hat values for the CE and RE models

plot(NA, NA, xlim=c(1,20), ylim=c(0,0.21), xaxt="n", las=1, xlab="Study", ylab="Hat Value")
axis(1, 1:20, cex.axis=1)
points(hatvalues(res.CE), type="o", pch=19, col="gray70")
points(hatvalues(res.RE), type="o", pch=19)
abline(h=1/20, lty="dotted", lwd=2)
title("Hat Values for the CE/RE Models")
legend("topright", pch=19, col=c("gray70","black"), lty="solid",
       legend=c("Common-Effects Model", "Random-Effects Model"), bty="n")

# heatmap of the hat matrix for the ME model

cols <- colorRampPalette(c("blue", "white", "red"))(101)
h <- hatvalues(res.ME, type="matrix")
image(1:nrow(h), 1:ncol(h), t(h[nrow(h):1,]), axes=FALSE,
      xlab="Influence of the Observed Effect of Study ...", ylab="On the Fitted Value of Study ...",
      col=cols, zlim=c(-max(abs(h)),max(abs(h))))
axis(1, 1:20, tick=FALSE)
axis(2, 1:20, labels=20:1, las=1, tick=FALSE)
abline(h=seq(0.5,20.5,by=1), col="white")
abline(v=seq(0.5,20.5,by=1), col="white")
points(1:20, 20:1, pch=19, cex=0.4)
title("Heatmap for the Mixed-Effects Model")

# plot of leverages versus standardized residuals for the ME model

plot(hatvalues(res.ME), rstudent(res.ME)$z, pch=19, cex=0.2+3*sqrt(cooks.distance(res.ME)),
     las=1, xlab="Leverage (Hat Value)", ylab="Standardized Deleted Residual",
     xlim=c(0,0.35), ylim=c(-3.5,2.5))
abline(h=c(-1.96,1.96), lty="dotted", lwd=2)
abline(h=0, lwd=2)
ids <- c(3,6,9)
text(hatvalues(res.ME)[ids] + c(0,0.013,0.010), rstudent(res.ME)$z[ids] - c(0.18,0,0), ids)
title("Leverage vs. Standardized Deleted Residuals")

# plot of the Cook's distances for the ME model

plot(1:20, cooks.distance(res.ME), ylim=c(0,1.6), type="o", pch=19, las=1, xaxt="n", yaxt="n",
     xlab="Study", ylab="Cook's Distance")
axis(1, 1:20, cex.axis=1)
axis(2, seq(0,1.6,by=0.4), las=1)
title("Cook's Distances")

# plot of the influence diagnostics for the ME model

inf <- influence(res.ME)
inf
plot(inf)

# fit mixed-effects model without studies 3 and/or 6

rma(yi, vi, mods = ~ dose, data=dat, method="DL", subset=-3)
rma(yi, vi, mods = ~ dose, data=dat, method="DL", subset=-6)
rma(yi, vi, mods = ~ dose, data=dat, method="DL", subset=-c(3,6))

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-06-03
#
# Topic(s):
# - meta-analysis (part 6: publication bias)
#
# last updated: 2021-06-03

############################################################################

# topics covered below:
#
# - publication / small-sample bias
#   - funnel plots
#   - failsafe-N analysis
#   - regression test for funnel plot asymmetry
#   - rank correlation test for funnel plot asymmetry
#   - trim and fill method
#   - PET and PEESE methods
#   - selection models
#   - contour-enhanced funnel plot
#   - test of excess significance
#   - p-uniform analysis
#   - p-curve analysis
#   - funnel plot variations

############################################################################

# install the CRAN version of the metafor package
install.packages("metafor")

# install the 'devel' version of the metafor package
install.packages("remotes")
remotes::install_github("wviechtb/metafor")

# load metafor package
library(metafor)

############################################################################

# meta-analysis of studies examining the risk of lung cancer due to
# environmental tobacco smoke (ETS) exposure
#
# source: Hackshaw, A. K., Law, M. R., & Wald, N. J. (1997). The accumulated
# evidence on lung cancer and environmental tobacco smoke. British Medical
# Journal, 315(7114), 980–988. https://doi.org/10.1136/bmj.315.7114.980
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2127653/
#
# see also: Hackshaw, A. K. (1998). Lung cancer and passive smoking.
# Statistical Methods in Medical Research, 7(2), 119–136.
# https://doi.org/10.1177/096228029800700203
#
# one of the included studies: https://doi.org/10.1001/jama.1994.03510460044031
#
# note: the outcome measure in this meta-analysis was the log odds ratio (and
# the values are already included in the dataset)

# for a description of the dataset, see:
help(dat.hackshaw1998)

# copy data to 'dat'
dat <- dat.hackshaw1998
dat

# illustrate the calculation of the (log) OR for study 33: Fontham et al. (1994)
tmp <- escalc(measure="OR", ai=433, n1i=651, ci=766, n2i=1253)
tmp
summary(tmp, transf=exp, digits=2)

# illustrate the back-calculation of the log(OR) and sampling variance
dat$yi2 <- round(log(dat$or), digits=4)
dat$vi2 <- round(((log(dat$or.ub) - log(dat$or.lb)) / (2*qnorm(.975)))^2, digits=4)
dat
dat$yi2 <- dat$vi2 <- NULL

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# back-transform to odds ratios (and get 95% CI/PI)
predict(res, transf=exp, digits=2)

# funnel plot
funnel(res)
funnel(res, ylim=c(0,0.8), las=1, atransf=exp, at=log(c(.25,.5,1,2,4,8)))

# failsafe-N analysis using the Rosenthal approach
fsn(yi, vi, data=dat)

# failsafe-N analysis using Orwin's method (unweighted and weighted)
fsn(yi, vi, data=dat, type="Orwin", target=log(1.05))
fsn(yi, vi, data=dat, type="Orwin", target=log(1.05), weighted=TRUE)

# failsafe-N analysis using Rosenberg's method
fsn(yi, vi, data=dat, type="Rosenberg")

# regression test for funnel plot asymmetry
regtest(res) # uses the SEs as predictor by default
regtest(res, predictor="vi") # use the variances as predictor

# add the standard errors to the dataset
dat$sei <- sqrt(dat$vi)

# run the regression test (with SEs as predictor) manually
rma(yi, vi, mods = ~ sei, data=dat)

# rank correlation test for funnel asymmetry
ranktest(dat$yi, dat$vi)

# trim and fill method
trimfill(res)

# funnel plot with 'filled-in' studies added
taf <- trimfill(res)
funnel(taf, ylim=c(0,0.8), las=1, atransf=exp, at=log(c(.25,.5,1,2,4,8)))

# predicted average odds ratio based on the trim and fill method
predict(taf, transf=exp, digits=2)

# PET and PEESE methods
regtest(res)
regtest(res, predictor="vi")

# illustrate the idea behind PET and PEESE
funnel(res, ylim=c(0,0.8), las=1, atransf=exp, at=log(c(.25,.5,1,2,4,8)))
reg <- regtest(res)
se <- seq(0,0.8,length=100)
lines(coef(reg$fit)[1] + coef(reg$fit)[2]*se, se, lwd=2)
reg <- regtest(res, predictor="vi")
lines(coef(reg$fit)[1] + coef(reg$fit)[2]*se^2, se, lwd=2, lty="dotted")

# fit 'logistic' selection model
sel <- selmodel(res, type="logistic", alternative="greater")
sel
plot(sel)

# fit a 'step function' selection model
sel <- selmodel(res, type="stepfun", alternative="greater", steps=c(.025,.10,.50,1))
sel
plot(sel, add=TRUE, col="red")

# add legend
legend("topright", inset=.01, lty="solid", lwd=3, col=c("black","red"),
       legend=c("Logistic Selection Model", "Step Function Selection Model"))

# contour-enhanced funnel plot
funnel(res, xlim=c(-2.5,2.5), ylim=c(0,0.8), refline=0, level=c(90,95,99),
       shade=c("white","gray55","gray75"), legend=TRUE)

# conduct test of excess significance
tes(dat$yi, dat$vi)
tes(dat$yi, dat$vi, theta=0.15)

# install puniform package
#install.packages("puniform")

# load puniform package
library(puniform)

# histogram of the p-values for testing H0: theta = 0 in each study
hist(summary(dat)$pval, main="", xlab="p-value", breaks=seq(0,1,by=.10))

# histogram of the p-values for testing H0: theta = .15 in each study
hist(summary(dat, H0=.15)$pval, main="", xlab="p-value", breaks=seq(0,1,by=.10))

# carry out p-uniform analysis
puniform(yi=dat$yi, vi=dat$vi, side="right")

# compare with equal-effects model
rma(yi, vi, method="FE", data=dat)

# show that p-uniform is a special case of a selection model (using ML estimation)
puniform(yi=dat$yi, vi=dat$vi, side="right", method="ML")
dat$zi <- dat$yi / sqrt(dat$vi)
res <- rma(yi, vi, data=dat, method="FE", subset=(zi > 1.96))
sel <- selmodel(res, type="stepfun", steps=.025, alternative="greater", delta=c(1,1e-10))
sel

# install dmetar package
#remotes::install_github("MathiasHarrer/dmetar")

# load dmetar package
library(dmetar)

# p-curve analysis
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pcurve.html
sav <- pcurve(data.frame(studlab=dat$study, TE=dat$yi, seTE=sqrt(dat$vi)))
sav

# standard funnel plot with the SEs on the y-axis
funnel(res)

# put the sampling variances on the y-axis
funnel(res, yaxis="vi")

# cannot use this because we do not know the sample sizes of the studies
funnel(res, yaxis="ni")

# let's use a different dataset; copy data into 'dat' and examine data
dat <- dat.egger2001
dat

# compute the log odds ratios for all studies except for the ISIS-4 trial
dat <- escalc(measure="OR", ai=ai, n1i=n1i, ci=ci, n2i=n2i, data=dat, subset=-16)
dat

# fit an equal-effects model
res <- rma(yi, vi, data=dat, method="FE")
res

# examine the funnel plot
funnel(res)

# put 1/sqrt(ni) on the y-axis
funnel(res, yaxis="sqrtninv", xlim=c(-4,3), ylim=c(0,.16))

# standard regression test
regtest(res)

# again using 1/sqrt(ni) as the predictor
regtest(res, predictor="sqrtninv")

# funnel plot with 1/sqrt(ni) on the y-axis showing which
# studies yielded significant results
pvals <- summary(dat)$pval
funnel(res, yaxis="sqrtninv", xlim=c(-4,3), ylim=c(0,.16),
       pch = ifelse(pvals <= .05, 19, 21))

############################################################################

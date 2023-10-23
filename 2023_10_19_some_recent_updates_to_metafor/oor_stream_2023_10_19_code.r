############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-10-19
#
# Topic(s):
# - Some Recent Updates to the metafor Package
# - https://www.metafor-project.org
#
# last updated: 2023-10-23

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

### illustrate the setmfopt() and getmfopt() functions

# calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg, data=dat.bcg,
                            slab=paste0(author, ", ", year))
dat

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res

# average risk ratio with 95% CI
predict(res, transf=exp)

# the old way of setting some options was rather clunky and involved creating
# certain predefined objects in the workspace

# create .rmspace object and set it to TRUE
.rmspace <- TRUE

# re-examine the output
res

# note that the empty lines at the beginning and end of the output are gone
# now; this can be useful for example when creating Rmarkdown documents, where
# the output is already clearly distinguishable from the code

# remove the .rmspace object
rm(.rmspace)

# re-examine the output
res

# the approach above for setting such options is outdated

# get the metafor package options
getmfopt()

# set the space option to FALSE
setmfopt(space=FALSE)

# re-examine the output
res

# set the space option back to TRUE
setmfopt(space=TRUE)

# re-examine the output
res

############################################################################

### look at some other options can that be set/adjusted

# look at the documentation for setmfopt() to see what other options can be set
help(setmfopt)

# re-examine the output
res

# set the number of digits to print in the output to 2
print(res, digits=2)

# we can set the number of digits globally
setmfopt(digits=2)

# re-examine the output
res

# specify how various elements of the output should be rounded
setmfopt(digits = c(est=2, se=3, test=2, pval=3, ci=2, var=3, sevar=3, fit=3, het=3))

# re-examine the output
res

# reset the digits option
setmfopt(digits=NULL)

# see help(misc_options) for further details

# when loading the crayon package, we can get styled output
library(crayon)

# re-examine the output
res

# forest plot
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.9, header="Author(s) and Year", shade=TRUE)
op <- par(cex=0.9, font=2)
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"))
par(op)

# funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

# we can force plots to have a dark background
setmfopt(theme="dark")

# forest plot
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.xpos=c(-9.5,-8,-6,-4.5),
       cex=0.9, header="Author(s) and Year", shade=TRUE)
op <- par(cex=0.9, font=2)
text(c(-9.5,-8,-6,-4.5), res$k+2, c("TB+", "TB-", "TB+", "TB-"))
text(c(-8.75,-5.25),     res$k+3, c("Vaccinated", "Control"))
par(op)

# funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

# can also use custom themes
setmfopt(theme="custom", fg="yellow", bg="deepskyblue4")

# all colors used in plots created by metafor are chosen relative to the
# foreground and background colors

# funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

# in RStudio, can set theme to "auto" in which case the foreground and
# background colors of plots are chosen according to the RStudio theme
setmfopt(theme="auto")

# funnel plot
funnel(res, ylim=c(0,0.8), las=1, digits=list(3L,1),
       atransf=exp, at=log(c(0.125, 0.25, 0.5, 1, 2, 4)))

############################################################################

### updates to the fsn() function

# copy the dat.hackshaw1998 dataset to dat
dat <- dat.hackshaw1998

# fit an equal-effects model
res <- rma(yi, vi, data=dat, method="EE")
res
predict(res, transf=exp)

# funnel plot
funnel(res)

# run the regression test for funnel plot asymmetry
regtest(res)

# file drawer analysis using the Rosenthal approach
fsn(yi, vi, data=dat)

# file drawer analysis using the Orwin approach, calculating the number of
# studies averaging null results that it would take to reduce the pooled odds
# ratio to 1.05 (i.e., a 5% increase in the odds of lung cancer in exposed
# women compared to unexposed women); note: the analysis is done with log odds
# ratios, so we also have to specify the target effect size in log units
fsn(yi, vi, data=dat, type="Orwin", target=log(1.05))

# demonstrate that if we add 104 studies with null results with sampling
# variances equal to the harmonic mean of the sampling variances of the
# original studies, then we indeed get the target pooled effect size
yi.all <- c(dat$yi, rep(0, 104))
vi.all <- c(dat$vi, rep(1/mean(1/dat$vi), 104))
res <- rma(yi.all, vi.all, method="EE")
predict(res, transf=exp)

# file drawer analysis using the Rosenberg approach
fsn(yi, vi, data=dat, type="Rosenberg")

# demonstrate that if we add 202 studies with null results with sampling
# variances equal to the harmonic mean of the sampling variances of the
# original studies, then we indeed get a pooled effect size whose p-value is
# equal to 0.05 (or rather, as close to 0.05 as possible)
yi.all <- c(dat$yi, rep(0, 202))
vi.all <- c(dat$vi, rep(1/mean(1/dat$vi), 202))
rma(yi.all, vi.all, method="EE")

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res
predict(res, transf=exp)

# file drawer analysis using the general approach, where we want to determine
# how many studies it would take to reduce the significance level of the
# pooled effect size to 0.05 based on a random-effects model
fsn(yi, vi, data=dat, type="General")

# can also pass a model object to fsn()
fsn(res)

# file drawer analysis using the general approach, where we want to determine
# how many studies it would take to reduce the pooled effect size to log(1.05)
# based on a random-effects model
fsn(yi, vi, data=dat, type="General", target=log(1.05))
fsn(res, target=log(1.05))

# try out the exact calculation method
fsn(yi, vi, data=dat, type="General", exact=TRUE)
fsn(yi, vi, data=dat, type="General", target=log(1.05), exact=TRUE)

############################################################################

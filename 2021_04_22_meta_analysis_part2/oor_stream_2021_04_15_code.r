############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-04-15
#
# Topic(s):
# - meta-analysis (part 1: standard methods)
# - meta-analysis (part 2: standard methods)
# - meta-analysis (part 3: standard methods)
# - meta-analysis (part 4: standard methods)
# - meta-analysis (part 5: standard methods)

# last updated: 2021-06-18

# note: the same code was used in the streams on 2021-04-22, 2021-05-06,
# 2021-05-13, and 2021-05-20

############################################################################

# topics covered below:
#
# - calculation of various outcome measures (log risk ratios, standardized mean
#   differences, r-to-z transformed correlation coefficients, ...)
# - the equal-effects model and the random-effects model
# - example meta-analyses with log risk ratios, standardized mean differences,
#   r-to-z transformed correlation coefficients, and log odds ratios)
# - the test for heterogeneity (Q-test)
# - the prediction interval for the true outcomes/effects
# - measures of heterogeneity (i.e., tau^2, I^2)
# - forest plots
# - meta-regression (with quantitative and categorical predictors/moderators)
# - computing predicted average outcomes/effects (and their back-transformation
#   if this is relevant)
# - the pseudo R^2 statistic (i.e., heterogeneity accounted for)
# - testing linear contrasts
# - subgrouping (and how this differs from fitting a meta-regression with a
#   categorical predictor/moderator)
# - bubble plots
# - meta-regression models with multiple predictors and interactions
# - simple slopes and marginal relationships
# - variance inflation factors (VIFs)

############################################################################

# install the CRAN version of the metafor package
install.packages("metafor")

# install the 'devel' version of the metafor package
install.packages("remotes")
remotes::install_github("wviechtb/metafor")

# load metafor package
library(metafor)

############################################################################

# meta-analysis examining the effectiveness of the BCG vaccine for preventing
# tuberculosis infections
#
# source: Colditz, G. A., Brewer, T. F., Berkey, C. S., Wilson, M. E.,
# Burdick, E., Fineberg, H. V., & Mosteller, F. (1994). Efficacy of BCG
# vaccine in the prevention of tuberculosis: Meta-analysis of the published
# literature. Journal of the American Medical Association, 271, 698-702.
# https://doi.org/10.1001/jama.1994.03510330076038
#
# one of the included studies: https://doi.org/10.1136/bmj.1.5336.973
#                              https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1630784/

# for a description of the dataset, see:
help(dat.bcg)

# copy the BCG vaccine data to 'dat'
dat <- dat.bcg
dat

# compute log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
dat

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res

# compute the standard errors of the log risk ratios
dat$stderr <- sqrt(dat$vi)
dat

# can also use the standard errors as input
res <- rma(yi, sei=stderr, data=dat)
res

# this also works
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
res

# back-transform the results to risk ratios and obtain the prediction interval
predict(res, transf=exp, digits=2)

# draw a very basic forest plot of the results
forest(res, header=TRUE, addpred=TRUE)

# draw a nicer one
forest(res, header=TRUE, atransf=exp, slab=paste0(dat$author, ", ", dat$year),
       xlim=c(-12,5), at=log(c(.05,.25,.50,1,2,4)), addpred=TRUE, order=dat$year,
       ilab=cbind(dat$ablat,dat$alloc), ilab.xpos=c(-7,-5))
text(-7, 15, "Lattitude",  font=2)
text(-5, 15, "Allocation", font=2)

# instead of transforming the x-axis, can back-transform the values directly
forest(res, header=TRUE, transf=exp, at=c(0,0.5,1,2), xlim=c(-1.5,3), refline=1)

# forest plot with studies ordered by absolute latitude
forest(res, header=TRUE, atransf=exp, slab=paste0(dat$author, ", ", dat$year),
       xlim=c(-12,5), at=log(c(.05,.25,.50,1,2,4)), addpred=TRUE, order=dat$year,
       ilab=cbind(dat$ablat,dat$alloc), ilab.xpos=c(-7,-5))
text(-7, 15, "Lattitude",  font=2)
text(-5, 15, "Allocation", font=2)

# fit mixed-effects meta-regression model with absolute latitude as predictor
res <- rma(yi, vi, mods = ~ ablat, data=dat)
res

# computed the predicted average risk ratio at 13 and 55 degrees
predict(res, newmods=13, transf=exp, digits=2)
predict(res, newmods=55, transf=exp, digits=2)

# compute predicted effects for 0-60 degrees in steps of 10
predict(res, newmods=seq(0, 60, by=10), transf=exp, digits=2, addx=TRUE)

# draw bubble plot
regplot(res, xlab="Absolute Latitude", atransf=exp, las=1, ylim=log(c(0.15,2)),
        at=log(c(0.2,0.5,1,2)), digits=1, xlim=c(0,60), predlim=c(0,60),
        refline=log(1), bty="l")

# show how the pseudo R^2 value is computed
res0 <- rma(yi, vi, data=dat)
res1 <- rma(yi, vi, mods = ~ ablat, data=dat)
round(100 * (res0$tau2 - res1$tau2) / res0$tau2, digits=2)

# fit meta-regression model with publication year centered at 1970
res <- rma(yi, vi, mods = ~ I(year-1970), data=dat)
res
regplot(res)
predict(res, newmods=1980-1970, transf=exp, digits=2)

# use latitude and year as predictors
res <- rma(yi, vi, mods = ~ ablat + year, data=dat)
res

# estimate the average risk ratio at 30 degrees in 1970
predict(res, newmods=c(30,1970), transf=exp, digits=2)

# bubble plots with marginal relationships
regplot(res, mod="ablat")
regplot(res, mod="year")

# fit model with alloc as moderator
res <- rma(yi, vi, mods = ~ alloc, data=dat)
res
model.matrix(res)

# intercept       = estimated average log risk ratio for studies using alternating allocation
# allocrandom     = estimated difference in the average log risk ratio between studies using
#                   random allocation compared to studies using alternating allocation
# allocsystematic = estimated difference in the average log risk ratio between studies using
#                   systematic allocation compared to studies using alternating allocation

# predicted average risk ratio for alternating, random, and systematic allocation
predict(res, newmods=c(0,0), transf=exp, digits=2)
predict(res, newmods=c(1,0), transf=exp, digits=2)
predict(res, newmods=c(0,1), transf=exp, digits=2)

# fit model with alloc as moderator removing the intercept
res <- rma(yi, vi, mods = ~ alloc - 1, data=dat)
res

# allocalternate  = estimated average log risk ratio for studies using alternating allocation
# allocrandom     = estimated average log risk ratio for studies using random allocation
# allocsystematic = estimated average log risk ratio for studies using systematic allocation

# predicted average risk ratio for alternating, random, and systematic allocation
predict(res, newmods=c(1,0,0), transf=exp, digits=2)
predict(res, newmods=c(0,1,0), transf=exp, digits=2)
predict(res, newmods=c(0,0,1), transf=exp, digits=2)

# test contrasts between the three alloc levels
anova(res, X=c(-1,1,0))
anova(res, X=c(-1,0,1))
anova(res, X=c(0,-1,1))

# change the reference level for alloc
res <- rma(yi, vi, mods = ~ relevel(factor(alloc), ref="random"), data=dat)
res

# subgrouping
res.a <- rma(yi, vi, data=dat, subset=alloc=="alternate")
res.r <- rma(yi, vi, data=dat, subset=alloc=="random")
res.s <- rma(yi, vi, data=dat, subset=alloc=="systematic")

# in the subgrouping approach, we let tau^2 to differ across subgroups (although
# that makes very little sense here with so few studies in each subgroup)
sav <- sapply(list(res.a,res.r,res.s), function(x) c(estimate=x$beta, tau2=x$tau2, k=x$k))
colnames(sav) <- c("res.a", "res.r", "res.s")
round(sav, digits=4)

############################################################################

# meta-analysis examining the effectiveness of school-based writing-to-learn
# interventions on academic achievement
#
# source: Bangert-Drowns, R. L., Hurley, M. M., & Wilkinson, B. (2004). The
# effects of school-based writing-to-learn interventions on academic
# achievement: A meta-analysis. Review of Educational Research, 74(1), 29-58.
# https://doi.org/10.3102/00346543074001029 https://www.jstor.org/stable/3516060
#
# one of the included studies: https://www.jstor.org/stable/27558364
#
# note: the standardized mean differences are already computed for the studies
# in this dataset, so there is no need to use the escalc() function; positive
# values indicate a higher mean level of academic achievement in the group
# receiving the writing-to-learn intervention compared to the control group

# for a description of the dataset, see:
help(dat.bangertdrowns2004)

# copy data to 'dat'
dat <- dat.bangertdrowns2004
dat

# illustrate the calculation of the SMD for study 14: Ganguli (1989)
escalc(measure="SMD", m1i=342, sd1i=68, n1i=27, m2i=303, sd2i=75, n2i=23)

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# obtain the prediction interval
predict(res, digits=2)

# draw forest plot
forest(res, header=TRUE, slab=paste0(dat$author, ", ", dat$year), xlim=c(-5,5), addpred=TRUE)

# fit a meta-regression model with length as moderator
res <- rma(yi, vi, mods = ~ length, data=dat)
res

# predicted average standardized mean difference for 1 and 15 weeks
predict(res, newmods=1, digits=2)
predict(res, newmods=15, digits=2)

# range of the length variable
range(dat$length, na.rm=TRUE)

# so without extrapolation, could also predicted the average outcome for 24 weeks
predict(res, newmods=24, digits=2)

# draw a bubble plot showing the results of the studies and the whole regression line
regplot(res, xlab="Intervention Length (in weeks)", xlim=c(0,25), ylim=c(-1,1.5),
        predlim=c(0,25), las=1, digits=1, bty="l")

# fit meta-regression model with length and feedback as moderators
res <- rma(yi, vi, mods = ~ length + feedback, data=dat)
res

# intrcpt  = estimated average SMD for studies where the intervention length
#            is 0 weeks and no feedback is provided
# length   = how much the average SMD changes per one-week increase in
#            intervention length
# feedback = difference in the average SMD when feedback is provided compared
#            to when no feedback is provided

# fit meta-regression model with length and meta as moderators
res <- rma(yi, vi, mods = ~ length + meta, data=dat)
res

# predicted average SMD for length=10 and meta=0
predict(res, newmods=c(10,0))
predict(res, newmods=c(meta=0, length=10))

# draw bubble plot
regplot(res, mod="length", xlab="Intervention Length (in weeks)",
        xlim=c(0,25), ylim=c(-1,1.5), predlim=c(0,25), las=1,
        digits=1, bty="l")

# the regression line shows the 'marginal' relationship between length and the
# average SMD (so the 'meta' variable is held constant at its mean, which, for
# a 0/1 variable, means that it is set equal to the proportion of 1's)

# draw bubble plot and show the simple slopes when meta=0 and meta=1
sav <- regplot(res, mod="length", xlab="Intervention Length (in weeks)",
               xlim=c(0,25), ylim=c(-1,1.5), las=1, digits=1, bty="l",
               pred=TRUE, ci=FALSE, bg=ifelse(dat$meta==1, "#f79646", "#4f81bd"))
preds <- predict(res, newmods=cbind(0:25, 0))
lines(0:25, preds$pred, col="#4f81bd", lwd=3)
preds <- predict(res, newmods=cbind(0:25, 1))
lines(0:25, preds$pred, col="#f79646", lwd=3)
points(sav$xi, sav$yi, pch=21, col="black", bg=sav$bg, cex=sav$psize)
legend("topright", pch=21, pt.cex=2, pt.bg=c("#f79646","#4f81bd"),
       col="black", legend=c("meta = 1", "meta = 0"))

# correlation between the two predictors/moderators
cor(dat$length, dat$meta, use="complete.obs")

# fit meta-regression model with length and meta as moderators and their interaction
res <- rma(yi, vi, mods = ~ length * meta, data=dat)
res

# predicted average SMD when length = 10 and meta=0 or meta=1
predict(res, newmods=c(10,0,10*0))
predict(res, newmods=c(10,1,10*1))
predict(res, newmods=rbind(c(10,0,10*0), c(10,1,10*1)))

# we can also make use of model.matrix() to do the coding for us
mm <- model.matrix(~ length * meta, data=data.frame(length=10, meta=c(0,1)))[,-1]
predict(res, newmods=mm)

# draw bubble plot and show the simple slopes when meta=0 and meta=1
sav <- regplot(res, mod="length", xlab="Intervention Length (in weeks)",
               xlim=c(0,25), ylim=c(-1,1.5), las=1, digits=1, bty="l",
               pred=FALSE, ci=FALSE, bg=ifelse(dat$meta==1, "#f79646", "#4f81bd"))
preds <- predict(res, newmods=cbind(0:25, 0, 0))
lines(0:25, preds$pred, col="#4f81bd", lwd=3)
preds <- predict(res, newmods=cbind(0:25, 1, 0:25))
lines(0:25, preds$pred, col="#f79646", lwd=3)
points(sav$xi, sav$yi, pch=21, col="black", bg=sav$bg, cex=sav$psize)
legend("topright", pch=21, pt.cex=2, pt.bg=c("#f79646","#4f81bd"),
       col="black", legend=c("meta = 1", "meta = 0"))

# compute the intercept when meta=0 and meta=1
predict(res, newmods=c(0,0,0), intercept=TRUE)
predict(res, newmods=c(0,1,0), intercept=TRUE) # intercept=TRUE is the default
# (the intercept when meta=0 is already in the output from res)

# compute the simple slope for length when meta=0 and meta=1
predict(res, newmods=c(1,0,0), intercept=FALSE)
predict(res, newmods=c(1,0,1), intercept=FALSE)
# (the slope for length when meta=0 is already in the output from res)

# use a different parameterization to fit the same model
dat$fmeta <- factor(dat$meta)
res <- rma(yi, vi, mods = ~ fmeta + fmeta:length - 1, data=dat)
res

# fmeta0 and fmeta1 are the intercepts when meta=0 and meta=1
# fmeta0:length and fmeta1:length are the slopes of length when meta=0 and meta=1

# predicted average SMD for meta=0 when length=10
predict(res, newmods=c(1,0,10,0))
# predicted average SMD for meta=1 when length=10
predict(res, newmods=c(0,1,0,10))

# how to visualize meta-regression analyses with a categorical predictor
res <- rma(yi, vi, data=dat, slab=paste0(author, ", ", year))
forest(res, cex=0.5)
res <- rma(yi, vi, mods = ~ meta, data=dat)
res
preds <- predict(res, newmods=c(0,1))
preds
addpoly(preds$pred, sei=preds$se, rows=-2, cex=0.5, mlab=c("meta = 0", "meta = 1"))

# not a super nice example; a much nicer one is here:
# https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups

# fit meta-regression model with length, wic, feedback, and meta as moderators
res <- rma(yi, vi, mods = ~ length + wic + feedback + meta, data=dat)
res

# variance inflation factors (VIFs) can be used to diagnose multicollinearity
# see: https://en.wikipedia.org/wiki/Variance_inflation_factor

# compute VIFs for the model above
vif(res)

# can also examine the correlation matrix among the moderators
round(cor(model.matrix(res)[,-1]), 2)

# illustrate what happens when two moderators are very highly correlated
dat$meta2 <- dat$meta
dat$meta2[1:3] <- c(0,1,0)
res <- rma(yi, vi, mods = ~ length + wic + feedback + meta + meta2, data=dat)
res
vif(res)
round(cor(model.matrix(res)[,-1]), 2) # r = 0.89 for meta and meta2

############################################################################

# meta-analysis on the relationship between class attendance and class
# performance / grade point average in college students
#
# source: personal communication
#
# original meta-analysis: Crede, M., Roch, S. G., & Kieszczynka, U. M. (2010).
# Class attendance in college: A meta-analytic review of the relationship of
# class attendance with grades and student characteristics. Review of
# Educational Research, 80(2), 272-295. https://doi.org/10.3102/0034654310362998
# https://www.jstor.org/stable/40658464
#
# one of the included studies: https://doi.org/10.1037/0022-0663.72.1.16
#
# note: the data used in the meta-analysis by Crede et al. (2010) are slightly
# different than the data included in this dataset (but just slightly)

# for a description of the dataset, see:
help(dat.crede2010)

# copy data to 'dat'
dat <- dat.crede2010
dat

# we will focus on the relationship between class attendance and performance
# (i.e., the grade) within the class (i.e., when the criterion is 'grade')

# calculate r-to-z transformed correlations and corresponding sampling variances
# (note: using 'subset' to select those rows where criterion is 'grade')
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat, subset=criterion=="grade")
dat

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# back-transform the results to correlations and obtain the prediction interval
predict(res, transf=transf.ztor, digits=2)

# draw forest plot
forest(res, header=TRUE, atransf=transf.ztor, addpred=TRUE, order="obs", xlim=c(-1,2.2),
       at=transf.rtoz(seq(-0.3,0.9,by=0.3)), refline=coef(res), psize=1)

# meta-regression to compare science vs nonscience classes
res <- rma(yi, vi, mods = ~ class, data=dat)
res

# intrcpt      = estimated average r-to-z transformed correlation for nonscience classes
# classscience = estimated difference in the average r-to-z transformed correlation for
#                science versus nonscience classes

# estimated average correlation for nonscience and science classes
predict(res, newmods=0, transf=transf.ztor, digits=2)
predict(res, newmods=1, transf=transf.ztor, digits=2)

# meta-regression to examine publication year as a potential moderator
res <- rma(yi, vi, mods = ~ year, data=dat)
res

# predicted average correlations for 1973 and 2009
predict(res, newmods=1973, transf=transf.ztor, digits=2)
predict(res, newmods=2009, transf=transf.ztor, digits=2)

# draw a bubble plot based on the model
regplot(res, atransf=transf.ztor, at=transf.rtoz(c(0,.2,.4,.6,.8,.9)), las=1,
        xlim=c(1970,2010), predlim=c(1970,2010), psize=1, bty="l", digits=1)

# rescale year so that the intercept reflects the estimated average r-to-z
# transformed correlation in 1980 and the slope reflects a 10-year increase
dat$yearrs <- (dat$year - 1980) / 10
res <- rma(yi, vi, mods = ~ yearrs, data=dat)
res

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

############################################################################
############################################################################

# topics covered so far:
# - calculation of various outcome measures (log risk ratios, standardized mean
#   differences, r-to-z transformed correlation coefficients, ...)
# - the equal-effects model and the random-effects model
# - example meta-analyses with log risk ratios, standardized mean differences,
#   r-to-z transformed correlation coefficients, and log odds ratios)
# - the test for heterogeneity (Q-test)
# - the prediction interval for the true outcomes/effects
# - measures of heterogeneity (i.e., tau^2, I^2)
# - forest plots
# - meta-regression (with quantitative and categorical predictors/moderators)
# - computing predicted average outcomes/effects (and their back-transformation
#   if this is relevant)
# - the pseudo R^2 statistic (i.e., heterogeneity accounted for)
# - testing linear contrasts
# - subgrouping (and how this differs from fitting a meta-regression with a
#   categorical predictor/moderator)
# - bubble plots
# - meta-regression models with multiple predictors and interactions
# - simple slopes and marginal relationships
# - variance inflation factors (VIFs)

############################################################################

# install the CRAN version of the metafor package
install.packages("metafor")

# install the 'devel' version of the metafor package
install.packages("remotes")
remotes::install_github("wviechtb/metafor")

# load metafor package
library(metafor)

############################################################################

# meta-analysis examining the effectiveness of the BCG vaccine for preventing
# tuberculosis infections
#
# source: Colditz, G. A., Brewer, T. F., Berkey, C. S., Wilson, M. E.,
# Burdick, E., Fineberg, H. V., & Mosteller, F. (1994). Efficacy of BCG
# vaccine in the prevention of tuberculosis: Meta-analysis of the published
# literature. Journal of the American Medical Association, 271, 698-702.
# https://doi.org/10.1001/jama.1994.03510330076038
#
# one of the included studies: https://doi.org/10.1136/bmj.1.5336.973
#                              https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1630784/

# for a description of the dataset, see:
help(dat.bcg)

# copy the BCG vaccine data to 'dat'
dat <- dat.bcg
dat

# compute log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
dat

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res

# compute the standard errors of the log risk ratios
dat$stderr <- sqrt(dat$vi)
dat

# can also use the standard errors as input
res <- rma(yi, sei=stderr, data=dat)
res

# this also works
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat)
res

# back-transform the results to risk ratios and obtain the prediction interval
predict(res, transf=exp, digits=2)

# draw a very basic forest plot of the results
forest(res, header=TRUE, addpred=TRUE)

# draw a nicer one
forest(res, header=TRUE, atransf=exp, slab=paste0(dat$author, ", ", dat$year),
       xlim=c(-12,5), at=log(c(.05,.25,.50,1,2,4)), addpred=TRUE, order=dat$year,
       ilab=cbind(dat$ablat,dat$alloc), ilab.xpos=c(-7,-5))
text(-7, 15, "Lattitude",  font=2)
text(-5, 15, "Allocation", font=2)

# instead of transforming the x-axis, can back-transform the values directly
forest(res, header=TRUE, transf=exp, at=c(0,0.5,1,2), xlim=c(-1.5,3), refline=1)

# forest plot with studies ordered by absolute latitude
forest(res, header=TRUE, atransf=exp, slab=paste0(dat$author, ", ", dat$year),
       xlim=c(-12,5), at=log(c(.05,.25,.50,1,2,4)), addpred=TRUE, order=dat$year,
       ilab=cbind(dat$ablat,dat$alloc), ilab.xpos=c(-7,-5))
text(-7, 15, "Lattitude",  font=2)
text(-5, 15, "Allocation", font=2)

# fit mixed-effects meta-regression model with absolute latitude as predictor
res <- rma(yi, vi, mods = ~ ablat, data=dat)
res

# computed the predicted average risk ratio at 13 and 55 degrees
predict(res, newmods=13, transf=exp, digits=2)
predict(res, newmods=55, transf=exp, digits=2)

# compute predicted effects for 0-60 degrees in steps of 10
predict(res, newmods=seq(0, 60, by=10), transf=exp, digits=2, addx=TRUE)

# draw bubble plot
regplot(res, xlab="Absolute Latitude", atransf=exp, las=1, ylim=log(c(0.15,2)),
        at=log(c(0.2,0.5,1,2)), digits=1, xlim=c(0,60), predlim=c(0,60),
        refline=log(1), bty="l")

# show how the pseudo R^2 value is computed
res0 <- rma(yi, vi, data=dat)
res1 <- rma(yi, vi, mods = ~ ablat, data=dat)
round(100 * (res0$tau2 - res1$tau2) / res0$tau2, digits=2)

# fit meta-regression model with publication year centered at 1970
res <- rma(yi, vi, mods = ~ I(year-1970), data=dat)
res
regplot(res)
predict(res, newmods=1980-1970, transf=exp, digits=2)

# use latitude and year as predictors
res <- rma(yi, vi, mods = ~ ablat + year, data=dat)
res

# estimate the average risk ratio at 30 degrees in 1970
predict(res, newmods=c(30,1970), transf=exp, digits=2)

# bubble plots with marginal relationships
regplot(res, mod="ablat")
regplot(res, mod="year")

# fit model with alloc as moderator
res <- rma(yi, vi, mods = ~ alloc, data=dat)
res
model.matrix(res)

# intercept       = estimated average log risk ratio for studies using alternating allocation
# allocrandom     = estimated difference in the average log risk ratio between studies using
#                   random allocation compared to studies using alternating allocation
# allocsystematic = estimated difference in the average log risk ratio between studies using
#                   systematic allocation compared to studies using alternating allocation

# predicted average risk ratio for alternating, random, and systematic allocation
predict(res, newmods=c(0,0), transf=exp, digits=2)
predict(res, newmods=c(1,0), transf=exp, digits=2)
predict(res, newmods=c(0,1), transf=exp, digits=2)

# fit model with alloc as moderator removing the intercept
res <- rma(yi, vi, mods = ~ alloc - 1, data=dat)
res

# allocalternate  = estimated average log risk ratio for studies using alternating allocation
# allocrandom     = estimated average log risk ratio for studies using random allocation
# allocsystematic = estimated average log risk ratio for studies using systematic allocation

# predicted average risk ratio for alternating, random, and systematic allocation
predict(res, newmods=c(1,0,0), transf=exp, digits=2)
predict(res, newmods=c(0,1,0), transf=exp, digits=2)
predict(res, newmods=c(0,0,1), transf=exp, digits=2)

# test contrasts between the three alloc levels
anova(res, X=c(-1,1,0))
anova(res, X=c(-1,0,1))
anova(res, X=c(0,-1,1))

# change the reference level for alloc
res <- rma(yi, vi, mods = ~ relevel(factor(alloc), ref="random"), data=dat)
res

# subgrouping
res.a <- rma(yi, vi, data=dat, subset=alloc=="alternate")
res.r <- rma(yi, vi, data=dat, subset=alloc=="random")
res.s <- rma(yi, vi, data=dat, subset=alloc=="systematic")

# in the subgrouping approach, we let tau^2 to differ across subgroups (although
# that makes very little sense here with so few studies in each subgroup)
sav <- sapply(list(res.a,res.r,res.s), function(x) c(estimate=x$beta, tau2=x$tau2, k=x$k))
colnames(sav) <- c("res.a", "res.r", "res.s")
round(sav, digits=4)

############################################################################

# meta-analysis examining the effectiveness of school-based writing-to-learn
# interventions on academic achievement
#
# source: Bangert-Drowns, R. L., Hurley, M. M., & Wilkinson, B. (2004). The
# effects of school-based writing-to-learn interventions on academic
# achievement: A meta-analysis. Review of Educational Research, 74(1), 29-58.
# https://doi.org/10.3102/00346543074001029 https://www.jstor.org/stable/3516060
#
# one of the included studies: https://www.jstor.org/stable/27558364
#
# note: the standardized mean differences are already computed for the studies
# in this dataset, so there is no need to use the escalc() function; positive
# values indicate a higher mean level of academic achievement in the group
# receiving the writing-to-learn intervention compared to the control group

# for a description of the dataset, see:
help(dat.bangertdrowns2004)

# copy data to 'dat'
dat <- dat.bangertdrowns2004
dat

# illustrate the calculation of the SMD for study 14: Ganguli (1989)
escalc(measure="SMD", m1i=342, sd1i=68, n1i=27, m2i=303, sd2i=75, n2i=23)

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# obtain the prediction interval
predict(res, digits=2)

# draw forest plot
forest(res, header=TRUE, slab=paste0(dat$author, ", ", dat$year), xlim=c(-5,5), addpred=TRUE)

# fit a meta-regression model with length as moderator
res <- rma(yi, vi, mods = ~ length, data=dat)
res

# predicted average standardized mean difference for 1 and 15 weeks
predict(res, newmods=1, digits=2)
predict(res, newmods=15, digits=2)

# range of the length variable
range(dat$length, na.rm=TRUE)

# so without extrapolation, could also predicted the average outcome for 24 weeks
predict(res, newmods=24, digits=2)

# draw a bubble plot showing the results of the studies and the whole regression line
regplot(res, xlab="Intervention Length (in weeks)", xlim=c(0,25), ylim=c(-1,1.5),
        predlim=c(0,25), las=1, digits=1, bty="l")

# fit meta-regression model with length and feedback as moderators
res <- rma(yi, vi, mods = ~ length + feedback, data=dat)
res

# intrcpt  = estimated average SMD for studies where the intervention length
#            is 0 weeks and no feedback is provided
# length   = how much the average SMD changes per one-week increase in
#            intervention length
# feedback = difference in the average SMD when feedback is provided compared
#            to when no feedback is provided

# fit meta-regression model with length and meta as moderators
res <- rma(yi, vi, mods = ~ length + meta, data=dat)
res

# predicted average SMD for length=10 and meta=0
predict(res, newmods=c(10,0))
predict(res, newmods=c(meta=0, length=10))

# draw bubble plot
regplot(res, mod="length", xlab="Intervention Length (in weeks)",
        xlim=c(0,25), ylim=c(-1,1.5), predlim=c(0,25), las=1,
        digits=1, bty="l")

# the regression line shows the 'marginal' relationship between length and the
# average SMD (so the 'meta' variable is held constant at its mean, which, for
# a 0/1 variable, means that it is set equal to the proportion of 1's)

# draw bubble plot and show the simple slopes when meta=0 and meta=1
sav <- regplot(res, mod="length", xlab="Intervention Length (in weeks)",
               xlim=c(0,25), ylim=c(-1,1.5), las=1, digits=1, bty="l",
               pred=TRUE, ci=FALSE, bg=ifelse(dat$meta==1, "#f79646", "#4f81bd"))
preds <- predict(res, newmods=cbind(0:25, 0))
lines(0:25, preds$pred, col="#4f81bd", lwd=3)
preds <- predict(res, newmods=cbind(0:25, 1))
lines(0:25, preds$pred, col="#f79646", lwd=3)
points(sav$xi, sav$yi, pch=21, col="black", bg=sav$bg, cex=sav$psize)
legend("topright", pch=21, pt.cex=2, pt.bg=c("#f79646","#4f81bd"),
       col="black", legend=c("meta = 1", "meta = 0"))

# correlation between the two predictors/moderators
cor(dat$length, dat$meta, use="complete.obs")

# fit meta-regression model with length and meta as moderators and their interaction
res <- rma(yi, vi, mods = ~ length * meta, data=dat)
res

# predicted average SMD when length = 10 and meta=0 or meta=1
predict(res, newmods=c(10,0,10*0))
predict(res, newmods=c(10,1,10*1))
predict(res, newmods=rbind(c(10,0,10*0), c(10,1,10*1)))

# we can also make use of model.matrix() to do the coding for us
mm <- model.matrix(~ length * meta, data=data.frame(length=10, meta=c(0,1)))[,-1]
predict(res, newmods=mm)

# draw bubble plot and show the simple slopes when meta=0 and meta=1
sav <- regplot(res, mod="length", xlab="Intervention Length (in weeks)",
               xlim=c(0,25), ylim=c(-1,1.5), las=1, digits=1, bty="l",
               pred=FALSE, ci=FALSE, bg=ifelse(dat$meta==1, "#f79646", "#4f81bd"))
preds <- predict(res, newmods=cbind(0:25, 0, 0))
lines(0:25, preds$pred, col="#4f81bd", lwd=3)
preds <- predict(res, newmods=cbind(0:25, 1, 0:25))
lines(0:25, preds$pred, col="#f79646", lwd=3)
points(sav$xi, sav$yi, pch=21, col="black", bg=sav$bg, cex=sav$psize)
legend("topright", pch=21, pt.cex=2, pt.bg=c("#f79646","#4f81bd"),
       col="black", legend=c("meta = 1", "meta = 0"))

# compute the intercept when meta=0 and meta=1
predict(res, newmods=c(0,0,0), intercept=TRUE)
predict(res, newmods=c(0,1,0), intercept=TRUE) # intercept=TRUE is the default
# (the intercept when meta=0 is already in the output from res)

# compute the simple slope for length when meta=0 and meta=1
predict(res, newmods=c(1,0,0), intercept=FALSE)
predict(res, newmods=c(1,0,1), intercept=FALSE)
# (the slope for length when meta=0 is already in the output from res)

# use a different parameterization to fit the same model
dat$fmeta <- factor(dat$meta)
res <- rma(yi, vi, mods = ~ fmeta + fmeta:length - 1, data=dat)
res

# fmeta0 and fmeta1 are the intercepts when meta=0 and meta=1
# fmeta0:length and fmeta1:length are the slopes of length when meta=0 and meta=1

# predicted average SMD for meta=0 when length=10
predict(res, newmods=c(1,0,10,0))
# predicted average SMD for meta=1 when length=10
predict(res, newmods=c(0,1,0,10))

# how to visualize meta-regression analyses with a categorical predictor
res <- rma(yi, vi, data=dat, slab=paste0(author, ", ", year))
forest(res, cex=0.5)
res <- rma(yi, vi, mods = ~ meta, data=dat)
res
preds <- predict(res, newmods=c(0,1))
preds
addpoly(preds$pred, sei=preds$se, rows=-2, cex=0.5, mlab=c("meta = 0", "meta = 1"))

# not a super nice example; a much nicer one is here:
# https://www.metafor-project.org/doku.php/plots:forest_plot_with_subgroups

# fit meta-regression model with length, wic, feedback, and meta as moderators
res <- rma(yi, vi, mods = ~ length + wic + feedback + meta, data=dat)
res

# variance inflation factors (VIFs) can be used to diagnose multicollinearity
# see: https://en.wikipedia.org/wiki/Variance_inflation_factor

# compute VIFs for the model above
vif(res)

# can also examine the correlation matrix among the moderators
round(cor(model.matrix(res)[,-1]), 2)

# illustrate what happens when two moderators are very highly correlated
dat$meta2 <- dat$meta
dat$meta2[1:3] <- c(0,1,0)
res <- rma(yi, vi, mods = ~ length + wic + feedback + meta + meta2, data=dat)
res
vif(res)
round(cor(model.matrix(res)[,-1]), 2) # r = 0.89 for meta and meta2

############################################################################

# meta-analysis on the relationship between class attendance and class
# performance / grade point average in college students
#
# source: personal communication
#
# original meta-analysis: Crede, M., Roch, S. G., & Kieszczynka, U. M. (2010).
# Class attendance in college: A meta-analytic review of the relationship of
# class attendance with grades and student characteristics. Review of
# Educational Research, 80(2), 272-295. https://doi.org/10.3102/0034654310362998
# https://www.jstor.org/stable/40658464
#
# one of the included studies: https://doi.org/10.1037/0022-0663.72.1.16
#
# note: the data used in the meta-analysis by Crede et al. (2010) are slightly
# different than the data included in this dataset (but just slightly)

# for a description of the dataset, see:
help(dat.crede2010)

# copy data to 'dat'
dat <- dat.crede2010
dat

# we will focus on the relationship between class attendance and performance
# (i.e., the grade) within the class (i.e., when the criterion is 'grade')

# calculate r-to-z transformed correlations and corresponding sampling variances
# (note: using 'subset' to select those rows where criterion is 'grade')
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat, subset=criterion=="grade")
dat

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# back-transform the results to correlations and obtain the prediction interval
predict(res, transf=transf.ztor, digits=2)

# draw forest plot
forest(res, header=TRUE, atransf=transf.ztor, addpred=TRUE, order="obs", xlim=c(-1,2.2),
       at=transf.rtoz(seq(-0.3,0.9,by=0.3)), refline=coef(res), psize=1)

# meta-regression to compare science vs nonscience classes
res <- rma(yi, vi, mods = ~ class, data=dat)
res

# intrcpt      = estimated average r-to-z transformed correlation for nonscience classes
# classscience = estimated difference in the average r-to-z transformed correlation for
#                science versus nonscience classes

# estimated average correlation for nonscience and science classes
predict(res, newmods=0, transf=transf.ztor, digits=2)
predict(res, newmods=1, transf=transf.ztor, digits=2)

# meta-regression to examine publication year as a potential moderator
res <- rma(yi, vi, mods = ~ year, data=dat)
res

# predicted average correlations for 1973 and 2009
predict(res, newmods=1973, transf=transf.ztor, digits=2)
predict(res, newmods=2009, transf=transf.ztor, digits=2)

# draw a bubble plot based on the model
regplot(res, atransf=transf.ztor, at=transf.rtoz(c(0,.2,.4,.6,.8,.9)), las=1,
        xlim=c(1970,2010), predlim=c(1970,2010), psize=1, bty="l", digits=1)

# rescale year so that the intercept reflects the estimated average r-to-z
# transformed correlation in 1980 and the slope reflects a 10-year increase
dat$yearrs <- (dat$year - 1980) / 10
res <- rma(yi, vi, mods = ~ yearrs, data=dat)
res

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

############################################################################

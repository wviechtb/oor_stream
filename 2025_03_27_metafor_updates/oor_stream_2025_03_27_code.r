############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-27
#
# Topic(s):
# - new features in the metafor package (version 4.8-0)
#
# last updated: 2025-04-01

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

# copy the BCG vaccine dataset to 'dat'
dat <- dat.bcg

# inspect the dataset
dat

# tpos - number of TB positive cases in the treated (vaccinated) group
# tneg - number of TB negative cases in the treated (vaccinated) group
# cpos - number of TB positive cases in the control (non-vaccinated) group
# cneg - number of TB negative cases in the control (non-vaccinated) group
#
# these variables denote the values in 2x2 tables of the form:
#
#           TB+    TB-
#         +------+------+
# treated | tpos | tneg |
#         +------+------+
# control | cpos | cneg |
#         +------+------+

# calculate log risk ratios and corresponding sampling variances (and use
# the 'slab' argument to store study labels as part of the data frame)
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg,
              data=dat, slab=paste(author, year, sep=", "))
dat

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res

# If the true effects (in this case, the true log risk ratios) are not all the
# same in the studies, then we say that the true effects are heterogeneous. If
# this is the case, then this indicates that the BCG vaccine is not equally
# effective across studies (for whatever reasons). We can test the null
# hypothesis (that there is NO heterogeneity, or that the true effects are
# homogeneous) with the Q-test; in the present case, the test is significant,
# so we reject the null hypothesis and conclude that there is heterogeneity.
# Typically, the next question at this point is: How much heterogeneity is
# there? Does the effectiveness of the vaccine vary just a bit across studies
# or is the vaccine sometimes very effective and sometimes not effective at
# all? One statistic that is often considered in this context is I^2, which
# indicates how much out of the total variability in the observed effects
# (which consists of heterogeneity and sampling variability) can be attributed
# to heterogeneity. While this is interesting information and can be easily
# interpreted (since I^2 is a percentage and therefore has a scale that is
# easy to understand), it is not an absolute measure of heterogeneity (e.g.,
# Borenstein et al., 2017) and should not be interpreted this way (e.g., based
# on rules of thumb such as '25% = low', '50% = medium', and '75% = high'
# heterogeneity). Instead, the estimate of tau^2 directly indicates how much
# variance there is in the true effects and therefore is an absolute measure
# of heterogeneity. Unfortunately, the estimate of tau^2 is hard to interpret
# (in the present case, it is the estimated variance in the true log risk
# ratios). Instead, it is better to report a prediction interval, which
# roughly is of the form: estimate of mu +- 1.96 * sqrt(estimate of tau^2),
# which indicates (under the assumption that the true effects are normally
# distributed) where 95% of the true effects are expected to fall (Riley et
# al., 2011). We can then think about whether this range covers quite extreme
# effects or whether most of the effects fall quite closely to mu (the average
# true effect). Note that various refinements have been proposed to the
# computation of the prediction interval, but the differences are often
# negligible when k (the number of studies) is not very small. See:
#
# https://www.metafor-project.org/doku.php/faq#for_random-effects_models_fitt
#
# The prediction interval should always be reported (IntHout et al., 2016).

# obtain the 95% prediction interval (pi.lb and pi.ub)
predict(res)

# back-transform the results to the risk ratio scale
predict(res, transf=exp, digits=2)

# So while the estimate of mu of 0.49 indicates that the infection risk is *on
# average* about 50% lower for vaccinated individuals, the prediction interval
# suggests that 95% of the true risk ratios are expected to fall between 0.15
# (which corresponds to an 85% lower infection risk in vaccinated individuals)
# to 1.55 (which corresponds to a 55% higher infection risk). Maybe we should
# not interpret the latter as really indicating that the risk may be higher in
# some circumstances for vaccinated individuals, but certainly as an indicator
# that there may be studies where the vaccine is not effective at all (i.e.,
# where the risk ratio is 1).

# forest plot with extra annotations
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       mlab="", shade=TRUE, addpred=TRUE)
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=0.9, font=2)

# add text with the Q-test result, I^2, and the tau^2 estimate
text(-16, -1, pos=4, cex=0.9, bquote(paste(
      "RE Model (Q = ", .(fmtx(res$QE, digits=2)),
      ", df = ", .(res$k - res$p), ", ",
      .(fmtp2(res$QEp)), "; ",
      I^2, " = ", .(fmtx(res$I2, digits=1)), "%, ",
      tau^2, " = ", .(fmtx(res$tau2, digits=2)), ")")))

# When using addpred=TRUE in forest(), the dotted horizontal line around the
# summary polygon corresponds to the prediction interval. However, this looks
# similar to the horizontal lines around the individual study estimates above,
# which are confidence intervals, which has the potential to lead to some
# confusion / misinterpretation. Therefore, some alternative ways of
# visualizing the prediction interval have been added to the metafor package,
# via the 'predstyle' argument.

# show the prediction interval as a bar below the summary polygon
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       shade=TRUE, addpred=TRUE, predstyle="bar")

# show the prediction interval as a shaded bar below the summary polygon,
# where the shading corresponds to the density of the prediction distribution
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       shade=TRUE, addpred=TRUE, predstyle="shade")

# show the entire predictive distribution below the summary polygon
forest(res, atransf=exp, at=log(c(0.05, 0.25, 1, 4)), xlim=c(-16,6),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, header="Author(s) and Year",
       shade=TRUE, addpred=TRUE, predstyle="dist")

# Note the area shaded in light gray to the right of 1 in the distribution.
# This area corresponds to the probability that the true effect (i.e., the
# true risk ratio) is larger than 1, that is, the probability that the vaccine
# is not effective.

# compute this probability (note: the computation is done on the untransformed
# scale, so we compute the probability that the true log risk ratio is >= 0)
pred <- predict(res)
pnorm(0, mean=pred$pred, sd=pred$pi.se, lower.tail=FALSE)

############################################################################

# One can also compute prediction intervals when the analysis is carried out
# with more complex (multilevel / multivariate) models. Let's consider the
# case of a multilevel meta-analysis.

# copy the data into 'dat'
dat <- dat.konstantopoulos2011
head(dat, 10)

# fit a multilevel model
res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
res

# get the prediction interval
predict(res)

# draw a forest plot (note: have to increase the height of the plotting device
# for the plot to look somewhat nice) and add the prediction interval as a bar
forest(res, cex=0.8, efac=c(0,1), predstyle="bar")

# add the entire predictive distribution
forest(res, cex=0.8, efac=c(0,1,1,0.9), xlim=c(-3,2.8), alim=c(-1,1.5), steps=6, predstyle="dist")

############################################################################

# copy the data into 'dat'
dat <- dat.berkey1998
dat

# construct the block diagonal variance-covariance matrix of the observed
# outcomes based on variables v1i and v2i
V <- vcalc(vi=1, cluster=author, rvars=c(v1i, v2i), data=dat)

# fit a multivariate model
res <- rma.mv(yi, V, mods = ~ 0 + outcome,
              random = ~ outcome | trial, struct="UN", data=dat)
res

# with models with moderators, the predict function gives the fitted values
# for the individual rows of the dataset
predict(res)

# but we can also specify the values of the moderator variable(s) to obtain
# the predicted effect for a particular combination of moderator values
predict(res, newmods=c(1,0))
predict(res, newmods=c(0,1))

# but in order to obtain the corresponding prediction intervals, we also have
# to specify the levels of the inner factor of '~ outcome | trial' (i.e., the
# outcome level(s))
pred.AL <- predict(res, newmods=c(1,0), tau2.levels="AL")
pred.PD <- predict(res, newmods=c(0,1), tau2.levels="PD")
pred.AL
pred.PD

# when drawing forest plots based on model objects that contain moderators,
# the fitted values are indicated for each data point as a polygon
forest(res, slab=paste(author, year, sep=", "))

# in this case, it would make more sense to add these polygons below the
# estimates for the individual studies (like in a regular forest plot)
forest(res, slab=paste(author, year, sep=", "), addfit=FALSE, ylim=c(-6,13),
       ilab=outcome, ilab.lab="Outcome", shade=c(1:2,5:6,9:10))
abline(h=0)
addpoly(pred.AL, rows=-1, mlab="Pooled AL Estimate", predstyle="dist")
addpoly(pred.PD, rows=-4, mlab="Pooled PD Estimate", predstyle="dist")

############################################################################

# let's go back to the BCG vaccine dataset
dat <- dat.bcg

# fit a random-effects model (here, we don't use escalc() but instead specify
# in the call to rma() what measure to compute and provide the needed inputs)
res <- rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat,
           slab=paste(author, year, sep=", "))
res

# draw a L'Abbé plot based on the model
labbe(res)

# 'Group 1' corresponds to the control groups, while 'Group 2' to the
# vaccinated groups, so let's add axis labels to clarify this
labbe(res, bty="l", grid=TRUE, xlab="Log Proportion (Control Group)",
      ylab="Log Proportion (Vaccinated Group)")

# The plot shows for each study what the (log transformed) risk was for the
# individuals in the first (control) group on the x-axis versus the risk for
# the individuals in the second (vaccinated) group on the y-axis. The solid
# diagonal line corresponds to the case where the risks are the same in the
# two groups. Note that most points fall below this line, corresponding to a
# lower infection risk in the vaccinated group. The dashed line corresponds to
# the estimated pooled effect. The idea behind this plot goes back to an
# article by L'Abbé and colleagues (1987).

# we can also add the region that corresponds to the confidence interval
# around the dashed line for the pooled effect
labbe(res, bty="l", grid=TRUE, ci=TRUE, xlab="Log Proportion (Control Group)",
      ylab="Log Proportion (Vaccinated Group)")

# we can also add the prediction interval region and add a legend
labbe(res, bty="l", grid=TRUE, ci=TRUE, pi=TRUE, legend=TRUE,
      xlab="Log Proportion (Control Group)", ylab="Log Proportion (Vaccinated Group)")

# back-transform the axes to proportions and save what the function returns invisibly
sav <- labbe(res, bty="l", grid=TRUE, ci=TRUE, pi=TRUE, legend=TRUE, transf=exp,
             xlab="Proportion (Control Group)", ylab="Proportion (Vaccinated Group)")

# could use identify() to label points (left click next to points to add the
# label to it, right click to stop labeling)
#identify(sav$x, sav$y, labels=sav$slab)

# add the study id to all points where the risk is > 0.03 in the control group
text(sav$x[sav$x > 0.03], sav$y[sav$x > 0.03], sav$ids[sav$x > 0.03], pos=1)

# say we conduct the meta-analysis with log odds ratios
res <- rma(measure="OR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat,
           slab=paste(author, year, sep=", "))
res

# then by default the arm-level log odds are shown in the L'abbé plot
labbe(res)

# we can back-transform the log odds to proportions using the inverse logit
# transformation (and show the entire curve that results from 0-1 on each axis)
labbe(res, transf=transf.ilogit, xlim=c(0,1), ylim=c(0,1))

# also add the confidence and prediction interval regions
labbe(res, transf=transf.ilogit, ci=TRUE, pi=TRUE,
      bty="l", grid=TRUE, legend=TRUE)

############################################################################

# meta-analysis on the effect of the color red on attractiveness ratings

# copy the data into 'dat' and select only the results for male raters
dat <- dat.lehmann2018
dat <- dat[dat$Gender == "Males",]
dat[c(1,6,48:49)]

# fit a random-effects model (using ML estimation)
res <- rma(yi, vi, data=dat, method="ML")
res

# step function selection model (3PSM)
sel <- selmodel(res, type="stepfun", alternative="greater", steps=.025)
sel

# plot the selection function (not super interesting)
plot(sel, ylim=c(0,1))

# step function selection model that only applies to the non-preregistered
# studies (since these are presumably free of publication bias)
sel <- selmodel(res, type="stepfun", alternative="greater", steps=.025,
                subset=Preregistered=="Not Pre-Registered")
sel

############################################################################

### References

# Borenstein, M., Higgins, J. P. T., Hedges, L. V., & Rothstein, H. R. (2017).
# Basics of meta-analysis: I^2 is not an absolute measure of heterogeneity.
# Research Synthesis Methods, 8(1), 5-18. https://doi.org/10.1002/jrsm.1230
#
# IntHout, J., Ioannidis, J. P., Rovers, M. M., & Goeman, J. J. (2016). Plea
# for routinely presenting prediction intervals in meta-analysis. BMJ Open,
# 6(7), e010247. https://doi.org/10.1136/bmjopen-2015-010247
#
# L'Abbé, K. A., Detsky, A. S., & O'Rourke, K. (1987). Meta-analysis in
# clinical research. Annals of Internal Medicine, 107(2), 224-233.
# https://doi.org/10.7326/0003-4819-107-2-224
#
# Riley, R. D., Higgins, J. P. T., & Deeks, J. J. (2011). Interpretation of
# random effects meta-analyses. British Medical Journal, 342, d549.
# https://doi.org/10.1136/bmj.d549

############################################################################

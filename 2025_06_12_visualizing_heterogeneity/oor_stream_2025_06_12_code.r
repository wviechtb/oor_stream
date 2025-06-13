############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-06-12
#
# Topic(s):
# - Visualizing Heterogeneity in Forest Plots (in connection with ESMARConf2025)
#
# last updated: 2025-06-13

############################################################################

# this script is based on what I presented during my ESMARConf2025 talk:
#
# Viechtbauer, W. (2025). A tutorial on how to visualize the amount of
# heterogeneity in forest plots. Evidence Synthesis & Meta-Analysis in R
# Conference, Online.

############################################################################

# install (if not already installed) the metafor package
#install.packages("metafor")

# note: for some of the things that are shown below to work properly, you need
# to install the 'development version' of the metafor package at the moment
# (this won't be necessary once a new version of metafor goes to CRAN); so for
# now, you should run the following two commands to install the devel version
#install.packages("remotes")
#remotes::install_github("wviechtb/metafor")

# load the metafor package
library(metafor)

############################################################################

### compute the effect sizes and fit a random-effects model

# tuberculosis is still one of the most common causes of death globally:
# https://ourworldindata.org/tuberculosis

# meta-analysis examining the effectiveness of the BCG vaccine for preventing
# tuberculosis infections

# source: Colditz, G. A., Brewer, T. F., Berkey, C. S., Wilson, M. E.,
# Burdick, E., Fineberg, H. V., & Mosteller, F. (1994). Efficacy of BCG
# vaccine in the prevention of tuberculosis: Meta-analysis of the published
# literature. Journal of the American Medical Association, 271(9), 698-702.
# https://doi.org/10.1001/jama.1994.03510330076038

# copy the data to 'dat' and examine the data
dat <- dat.bcg
dat

# tpos  - number of TB positive cases in the treated (vaccinated) group
# tneg  - number of TB negative cases in the treated (vaccinated) group
# cpos  - number of TB positive cases in the control (non-vaccinated) group
# cneg  - number of TB negative cases in the control (non-vaccinated) group
#
# these variables denote the values in 2x2 tables of the form:
#
#           TB+    TB-
#         +------+------+
# treated | tpos | tneg |
#         +------+------+
# control | cpos | cneg |
#         +------+------+
#
# year  - publication year of the study
# ablat - absolute latitude of the study location (in degrees)
# alloc - method of treatment allocation (random, alternate, or systematic assignment)

# calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg,
              data=dat, slab=paste(author, year, sep=", ")) # also add study labels
dat

# yi = the log risk ratios
# vi = the corresponding sampling variances

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res

# back-transform the results and obtain the 95% prediction interval
pred <- predict(res, transf=exp, digits=2)
pred

# the results indicate that vaccinated individuals have, on average, roughly
# half the risk of a TB infection than non-vaccinated individuals (or we can
# say that the risk of a TB infection is, on average, around 50% lower for
# vaccinated individuals); however, based on the Q-test, the effectiveness of
# the vaccine appears to vary across studies; the prediction interval suggests
# that there may be studies where the risk of a TB infection is 85% lower for
# vaccinated individuals, while on the other hand there may be studies where
# the risk is 55% higher

############################################################################

### standard forest plot

# draw a standard forest plot
sav <- forest(res, xlim=c(-15,6), cex=0.85, header="Author(s) and Year",
              atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
              ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
              ilab.xpos=c(-9.5,-8,-6,-4.5), shade=TRUE, efac=c(0,1,1))
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=sav$cex, font=2)

# add information about the test of the overall effect and heterogeneity
par(xpd=NA)
text(sav$xlim[1], -2, pos=4, cex=sav$cex,
     bquote(paste("Test of the overall effect: Z=",
            .(fmtx(res$zval, digits=2)), ", ",
            .(fmtp(res$pval, digits=3, pname="p", add0=TRUE, equal=TRUE)))))
text(sav$xlim[1], -3, pos=4, cex=sav$cex,
     bquote(paste("Heterogeneity: Q=",
            .(fmtx(res$QE, digits=2)), ", df=", .(res$k-res$p), ", ",
            .(fmtp(res$QEp, digits=3, pname="p", add0=TRUE, equal=TRUE)), "; ",
            hat(tau)*""^2, "=", .(fmtx(res$tau2, digits=2)), ", ",
            I^2, "=", .(round(res$I2)), "%")))
par(xpd=FALSE)

############################################################################

### illustrate different ways of visualizing the prediction interval

# prediction interval indicated by a horizontal line through the diamond (predstyle="line")
forest(res, xlim=c(-15,6), cex=0.85, header="Author(s) and Year",
       atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), shade=TRUE, efac=c(0,1,1),
       addpred=TRUE, predstyle="line")
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=sav$cex, font=2)

# prediction interval indicated by another polygon (predstyle="polygon")
forest(res, xlim=c(-15,6), cex=0.85, header="Author(s) and Year",
       atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), shade=TRUE, efac=c(0,1,1),
       addpred=TRUE, predstyle="polygon", col=c("black","white"))
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=sav$cex, font=2)

# prediction interval indicated by a horizontal bar (predstyle="bar")
forest(res, xlim=c(-15,6), cex=0.85, header="Author(s) and Year",
       atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), shade=TRUE, efac=c(0,1,1),
       addpred=TRUE, predstyle="bar")
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=sav$cex, font=2)

# prediction interval indicated by a shaded bar (predstyle="shade")
forest(res, xlim=c(-15,6), cex=0.85, header="Author(s) and Year",
       atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), shade=TRUE, efac=c(0,1,1),
       addpred=TRUE, predstyle="shade")
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=sav$cex, font=2)

# prediction interval indicated via the predictive distribution (predstyle="dist")
forest(res, xlim=c(-15,6), cex=0.85, header="Author(s) and Year",
       atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
       ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
       ilab.xpos=c(-9.5,-8,-6,-4.5), shade=TRUE, efac=c(0,1,1),
       addpred=TRUE, predstyle="dist")
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=sav$cex, font=2)

# light gray shaded regions: 2.5% in the tail areas of the predictive
# distribution (i.e., this corresponds to the 95% prediction interval)
#
# dark gray shaded region: the area opposite in sign (on the original log
# scale) to the estimated average effect
#
# this area corresponds to the probability that the true log risk ratio is
# above 0 (or that the true risk ratio is above 1) and hence the probability
# that the treatment/vaccine is not effective in a particular study

# we can compute the size of this area / probability as follows
pred <- predict(res)
round(pnorm(0, mean=coef(res), sd=pred$pi.se, lower.tail=FALSE), digits=2)

############################################################################

### subgrouping

# recode the alloc variable into random="yes"/"no"
dat$random <- ifelse(dat$alloc == "random", "yes", "no")

# reorder studies by 'random' and their effect sizes
dat <- sort_by(dat, ~ random + yi)

# fit a random-effects model within each level of 'random'
res0 <- rma(yi, vi, data=dat, subset=random=="no")
res1 <- rma(yi, vi, data=dat, subset=random=="yes")

# draw a forest plot of the effect size estimates
sav <- with(dat, forest(yi, vi, xlim=c(-12,6), ylim=c(-6,16),
       cex=0.85, header="Author(s) and Year",
       atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
       ilab=random, ilab.lab="Random", shade=TRUE, efac=c(0,1,1)))
abline(h=0)

# add the summary polygons and predictive distributions for each subgroup to the plot
pred0 <- predict(res0)
pred1 <- predict(res1)
addpoly(pred0, rows=-1, mlab="Pooled Estimate for Non-Random Allocation", predstyle="dist")
addpoly(pred1, rows=-4, mlab="Pooled Estimate for Random Allocation",     predstyle="dist")

############################################################################

### location-scale models

# meta-analysis on the effectiveness of writing-to-learn interventions on
# academic achievement

# source: Bangert-Drowns, R. L., Hurley, M. M., & Wilkinson, B. (2004). The
# effects of school-based writing-to-learn interventions on academic
# achievement: A meta-analysis. Review of Educational Research, 74(1), 29-58.
# https://doi.org/10.3102/00346543074001029 https://www.jstor.org/stable/3516060

# note: the standardized mean differences are already computed for the studies
# in this dataset, so there is no need to use the escalc() function; positive
# values indicate a higher mean level of academic achievement in the group
# receiving a writing-to-learn intervention compared to the control group

# copy the data to 'dat', reorder by sample size, and examine the data
dat <- dat.bangertdrowns2004
dat <- sort_by(dat, ~ ni)
head(dat)

# fit a random-effects model
res <- rma(yi, vi, data=dat)
res

# also get the 95% prediction interval
predict(res, digits=2)

# the results indicate that students receiving the intervention perform, on
# average, about 0.22 standard deviations better on some measure of academic
# achievement (e.g., final grade, an exam/quiz/test score); however, there
# again appears to be heterogeneity in the effects; the prediction interval
# suggests that there may be studies where the intervention group actually
# does worse (by about 0.23 standard deviations), while there may also be
# studies where the intervention group does 0.67 standard deviation better

# use sample size as a moderator in a meta-regression model
res <- rma(yi, vi, mods = ~ ni, data=dat)
res

# calculated the predicted average standardized mean difference for studies
# with a sample size of 20, 100, 200, or 500
predict(res, newmods = c(20,100,200,500))

# show a bubble plot corresponding to the model (and also add the prediction
# interval to the plot)
regplot(res, xlab="Sample Size", bty="l", grid=TRUE, las=1, pi=TRUE, legend=TRUE)

# the model above assumes that, at a given sample size, the true effects are
# normally distributed around the average effect with a constant amount of
# (residual) heterogeneity (see the estimate of tau^2 above)

# fit a location-scale model with the sample size as predictor for the size of
# the average effect and for the amount of heterogeneity
res <- rma(yi, vi, mods = ~ ni, scale = ~ ni, data=dat)
res

# calculated the predicted average standardized mean difference for studies
# with a sample size of 20, 100, 200, or 500
predict(res, newmods = c(20,100,200,500))

# note that we do not get prediction intervals (because we did not specify the
# values of ni for the scale part of the model)

# if we specify the corresponding sample sizes for the scale part, then we
# also get the prediction intervals
predict(res, newmods = c(20,100,200,500), newscale = c(20,100,200,500))

# note that the prediction interval is much wider for n=20 compared to n=500

# we can also just get the predicted tau^2 values
predict(res, newscale = c(20,100,200,500), transf=exp)

# so we see that the predicted tau^2 is much larger for n=20 compared to n=500

# draw a forest plot of the effect size estimates
# note: only plotting a subset of the estimates
sav <- with(dat, forest(yi, vi, xlim=c(-5,3.5), ylim=c(-9,20),
       cex=0.85, header="Author(s) and Year", ilab=ni, ilab.lab="Sample Size",
       slab=paste(author, year, sep=", "), efac=c(0,1,1),
       subset=c(1:5,30:34,44:48), rows=rev(c(1:5, 7:11, 13:17)), shade=seq(1,17,by=2)))
abline(h=0)
text(sav$xlim[1], c(6,12), "...", pos=4, cex=sav$cex)
text(sav$xlim[2], c(6,12), "...", pos=2, cex=sav$cex)

# add the summary polygons and predictive distributions for three values of ni to the plot
pred1 <- predict(res, newmods=20, newscale=20)
pred2 <- predict(res, newmods=100, newscale=100)
pred3 <- predict(res, newmods=500, newscale=500)
addpoly(pred1, rows=-1, mlab="Pooled Estimate for n=20",  predstyle="dist")
addpoly(pred2, rows=-4, mlab="Pooled Estimate for n=100", predstyle="dist")
addpoly(pred3, rows=-7, mlab="Pooled Estimate for n=500", predstyle="dist")

# draw a bubble plot where we show how the prediction interval gets really
# narrow as the sample size increases (have to use 'pred' argument of the
# regplot() function for this)
nis <- seq(0, 600, by=1)
pred <- predict(res, newmods=nis, newscale=nis)
regplot(res, xlab="Sample Size", bty="l", grid=TRUE, las=1, pi=TRUE,
        legend=TRUE, pred=pred, xvals=nis)

############################################################################

### multivariate models

# meta-analysis comparing surgical and non-surgical treatments for medium-
# severity periodontal disease
#
# source: Berkey, C. S., Hoaglin, D. C., Antczak-Bouckoms, A., Mosteller, F.,
# & Colditz, G. A. (1998). Meta-analysis of multiple outcomes by regression
# with random effects. Statistics in Medicine, 17(22), 2537–2550.
# https://doi.org/10.1002/(sici)1097-0258(19981130)17:22<2537::aid-sim953>3.0.co;2-c

# copy the data to 'dat' and examine the data
dat <- dat.berkey1998
dat

# construct the block diagonal variance-covariance matrix of the observed
# outcomes based on variables v1i and v2i
V <- vcalc(vi=1, cluster=author, rvars=c(v1i, v2i), data=dat)

# examine the V matrix (0 entries are shown as .)
print(unname(as.table(V)), zero.print=".")

# fit the multivariate model
res <- rma.mv(yi, V, mods = ~ 0 + outcome,
              random = ~ outcome | trial, struct="UN", data=dat)
res

# compute prediction intervals for each outcome
pred.AL <- predict(res, newmods=c(1,0), tau2.levels="AL", digits=2)
pred.PD <- predict(res, newmods=c(0,1), tau2.levels="PD", digits=2)
pred.AL
pred.PD

# draw a forest plot of the effect size estimates
sav <- with(dat, forest(yi, vi, xlim=c(-3,2.2), ylim=c(-6,13), cex=0.9,
       slab=paste(author, year, sep=", "), ilab=outcome, ilab.lab="Outcome",
       efac=c(0,1,1), shade=c(1:2,5:6,9:10)))
abline(h=0)

# add the summary polygons and predictive distributions for each outcome to the plot
addpoly(pred.AL, rows=-1, mlab="Pooled Estimate for Outcome AL", predstyle="dist")
addpoly(pred.PD, rows=-4, mlab="Pooled Estimate for Outcome PD", predstyle="dist")

############################################################################

### non-normal true effects

# back to the BCG vaccine dataset
dat <- dat.bcg
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat,
              slab=paste(author, year, sep=", "))
res <- rma(yi, vi, data=dat)

# draw the plot
sav <- forest(res, xlim=c(-15,6), cex=0.85, header="Author(s) and Year",
              atransf=exp, at=log(2^(-4:3)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4,8),
              ilab=cbind(tpos, tneg, cpos, cneg), ilab.lab=c("TB+","TB-","TB+","TB-"),
              ilab.xpos=c(-9.5,-8,-6,-4.5), shade=TRUE, efac=c(0,1,1),
              ylim=c(-4,16), addpred=TRUE, predstyle="dist")
text(c(-8.75,-5.25), res$k+2.8, c("Vaccinated", "Control"), cex=sav$cex, font=2)

# kernel density estimation of the distribution of true effects (based on Wang
# & Lee, 2019; DOI: 10.1002/jrsm.1345)
thetai <- coef(res) + sqrt(res$tau2 / (res$tau2 + dat$vi)) * (dat$yi - coef(res))
dens <- density(thetai, bw=0.2, from=-4, to=4, n=2^16)

# add the non-parametric PI distribution to the plot
addpoly(res, addpred=TRUE, predstyle="dist", preddist=dens, row=c(NA,-3),
        mlab=c("", "Predictive Distribution (non-parametric) [95% PI]"),
        predlim=log(c(1/8,2)), efac=c(1,0.9))

# add information about the test of the overall effect and heterogeneity
par(xpd=NA)
text(sav$xlim[1], -4, pos=4, cex=sav$cex,
     bquote(paste("Test of the overall effect: Z=",
            .(fmtx(res$zval, digits=2)), ", ",
            .(fmtp(res$pval, digits=3, pname="p", add0=TRUE, equal=TRUE)))))
text(sav$xlim[1], -5, pos=4, cex=sav$cex,
     bquote(paste("Heterogeneity: Q=",
            .(fmtx(res$QE, digits=2)), ", df=", .(res$k-res$p), ", ",
            .(fmtp(res$QEp, digits=3, pname="p", add0=TRUE, equal=TRUE)), "; ",
            hat(tau)*""^2, "=", .(fmtx(res$tau2, digits=2)), ", ",
            I^2, "=", .(round(res$I2)), "%")))
par(xpd=FALSE)

############################################################################

### an example of a meta-analysis of correlation coefficients

# meta-analysis on the correlation between conscientiousness (one of the big-5
# personality traits) and medication adherence

# source: Molloy, G. J., O'Carroll, R. E., & Ferguson, E. (2014).
# Conscientiousness and medication adherence: A meta-analysis. Annals of
# Behavioral Medicine, 47(1), 92–101. ⁠https://doi.org/10.1007/s12160-013-9524-4⁠

# copy the data to 'dat' and examine the data
dat <- dat.molloy2014
dat

# compute the r-to-z transformed correlations and corresponding sampling variances
dat <- escalc(measure="ZCOR", ri=ri, ni=ni, data=dat,
              slab=paste(authors, year, sep=", "))
dat

# fit a random-effects model to the r-to-z transformed correlations
res <- rma(yi, vi, data=dat)
res

# back-transform the results and obtain the 95% prediction interval
predict(res, transf=transf.ztor, digits=2)

# draw the forest plot with the predictive distribution
forest(res, predstyle="dist", efac=c(0,1,1), shade=TRUE, order="obs",
       atransf=transf.ztor, at=transf.rtoz(seq(-0.4,0.6,by=0.2)))

############################################################################

### an example of a meta-analysis with no evidence for heterogeneity

# meta-analysis of long-term trials examining the effectiveness of beta
# blockers to prevent reinfarction

# source: Yusuf, S., Peto, R., Lewis, J., Collins, R., & Sleight, P. (1985).
# Beta blockade during and after myocardial infarction: An overview of the
# randomized trials. Progress in Cardiovascular Disease, 27(5), 335–371.
# ⁠https://doi.org/10.1016/s0033-0620(85)80003-7⁠

# copy the data to 'dat' and examine the data
dat <- dat.yusuf1985

# compute log risk ratios for the data from table 11 (nonfatal reinfarction
# from long-term trials of beta blockers)
dat <- escalc(measure="RR", ai=ai, n1i=n1i,
                            ci=ci, n2i=n2i, data=dat, subset=(table=="11"))
dat

# negative log risk ratios indicate a lower reinfarction risk in the group
# receiving beta blockers

# fit a random-effects model to the log risk ratios
res <- rma(yi, vi, data=dat)
res

# back-transform the results and obtain the 95% prediction interval
predict(res, transf=exp, digits=2)

# so those receiving beta blockers have a 24% lower reinfarction risk; here,
# there is no evidence for heterogeneity (the Q-test is not significant and
# the estimate of tau^2 is zero)l as a result, the prediction interval is
# identical to the confidence interval

# draw the forest plot
forest(res, xlim=c(-5.5,3.0), predstyle="dist", efac=c(0,1,1), shade=TRUE,
       atransf=exp, at=log(2^(-4:2)), at.lab=c("¹⁄₁₆","⅛","¼","½",1,2,4))

############################################################################

# meta-analysis on the effects of modified school calendars on student
# achievement

# source: Konstantopoulos, S. (2011). Fixed effects and variance components
# estimation in three-level meta-analysis. Research Synthesis Methods, 2(1),
# 61–76. ⁠https://doi.org/10.1002/jrsm.35⁠

# copy data into 'dat' and examine data
dat <- dat.konstantopoulos2011
head(dat, 9)

# fit a multilevel random-effects model
res <- rma.mv(yi, vi, random = ~ 1 | district/school, data=dat)
res

# get the prediction interval
predict(res, digits=2)

# draw a forest plot of the effect size estimates
# note: only plotting a subset of the estimates
sav <- with(dat, forest(yi, vi, ylim=c(-8.5,16), cex=0.85, efac=c(0,1,1),
       slab=paste0("District: ", district, " School: ", school),
       subset=c(1:8,53:56), rows=rev(c(1:8, 10:13))))
abline(h=0)
text(sav$xlim[1], 9, "...", pos=4, cex=sav$cex)
text(sav$xlim[2], 9, "...", pos=2, cex=sav$cex)

# add the summary polygon
addpoly(res, row=-1)

# add the predictive distribution for how the average true effect of studies
# may vary (this reflects between-study heterogeneity)
dist <- list(x = seq(-3, 3, length.out=10000))
dist$y <- dnorm(dist$x, mean=coef(res), sd=sqrt(res$sigma2[1] + vcov(res)))
addpoly(res, row=c(NA,-3), predstyle="dist", preddist=dist,
        mlab=c("", "Predictive Distribution for the\nAverage True Effect of Studies"))

# add the predictive distribution for how the true effects of studies may
# vary, assuming that the average true effect of studies is equal to the
# overall pooled estimate (this reflects within-study heterogeneity)
dist <- list(x = seq(-3, 3, length.out=10000))
dist$y <- dnorm(dist$x, mean=coef(res), sd=sqrt(res$sigma2[2] + vcov(res)))
addpoly(res, row=c(NA,-5), predstyle="dist", preddist=dist,
        mlab=c("", "Predictive Distribution for the\nTrue Effects within Studies"))

# add the predictive distribution that combines between- and within-study
# heterogeneity; this reflects how true effects may vary overall
dist <- list(x = seq(-3, 3, length.out=10000))
dist$y <- dnorm(dist$x, mean=coef(res), sd=sqrt(res$sigma2[1] + res$sigma2[2] + vcov(res)))
addpoly(res, row=c(NA,-7), predstyle="dist", preddist=dist,
        mlab=c("", "Predictive Distribution for the True Effects"))

############################################################################

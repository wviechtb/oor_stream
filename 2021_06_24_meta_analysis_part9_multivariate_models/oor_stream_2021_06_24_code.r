############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-06-24
#
# Topic(s):
# - meta-analysis (part 9: multivariate models)
#
# last updated: 2021-07-13

############################################################################

# install the metafor package
#install.packages("metafor")

# install the clubSandwich package
#install.packages("clubSandwich")

# load the metafor package
library(metafor)

# load the clubSandwich package
library(clubSandwich)

############################################################################

# copy dataset to 'dat'
dat <- dat.bornmann2007

# compute log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=waward, n1i=wtotal,
                            ci=maward, n2i=mtotal, data=dat)

# fit multilevel model
res <- rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat)
res

# compute the ICC
round(res$sigma2[1] / sum(res$sigma2), digits=4)

# compute the total variance
round(sum(res$sigma2), digits=4)

# fit the same model using a 'multivariate' parameterization
res <- rma.mv(yi, vi, random = ~ factor(obs) | study, struct="CS", data=dat)
res

############################################################################

# copy data into 'dat' and examine data
dat <- dat.berkey1998
dat

# construct the variance-covariance matrices of the observed outcomes
V <- lapply(split(dat[c("v1i", "v2i")], dat$trial), as.matrix)
V <- bldiag(V)
V

# fit multivariate model
res <- rma.mv(yi, V, mods = ~ outcome - 1, data = dat,
              random = ~ outcome | trial, struct = "UN")
res

# estimate difference between the two outcomes
predict(res, newmods=c(1,-1))

# test difference between the two outcomes
anova(res, L=c(1,-1))

# fit random-effects model for the two outcomes separately
res.AL <- rma(yi, vi, data=dat, subset=outcome=="AL")
res.AL
res.PD <- rma(yi, vi, data=dat, subset=outcome=="PD")
res.PD

# note: the multivariate model does not require that all studies have measured
# both response variables, this just happens to be the case here; but suppose
# that study 1 only measured PD, study 2 only measured AL, and the remaining
# studies measured both PD and AL; then the dataset would look like this
sub <- dat[-c(2,3),]
sub

# and the V matrix like this
Vsub <- V[-c(2,3),-c(2,3)]
Vsub

# fit multivariate model
res <- rma.mv(yi, Vsub, mods = ~ outcome - 1, data = sub,
              random = ~ outcome | trial, struct = "UN")
res

# note that the estimate of rho is now only based on the 3 studies that
# measured both response variables and hence is even less accurate, but this
# is a rather small dataset to begin with for such a complex analysis

# LRT for testing H0: tau^2.1 = 0
res1 <- rma.mv(yi, V, mods = ~ outcome - 1, data = dat,
               random = ~ outcome | trial, struct = "UN")
res0 <- rma.mv(yi, V, mods = ~ outcome - 1, data = dat,
               random = ~ outcome | trial, struct = "UN", tau2=c(0,NA))
res0
anova(res0, res1)

# LRT for testing H0: tau^2.2 = 0
res0 <- rma.mv(yi, V, mods = ~ outcome - 1, data = dat,
               random = ~ outcome | trial, struct = "UN", tau2=c(NA,0))
res0
anova(res0, res1)

# LRT for testing H0: tau^2.1 = tau^2.2 = 0 (i.e., no heterogeneity whatsoever)
res0 <- rma.mv(yi, V, mods = ~ outcome - 1, data = dat)
res0
anova(res0, res1)

# note: this is conceptually the same as what the QE-test is testing (but
# the results are not identical as these are different types of tests)

# an example of a meta-regression model in this context (here we allow for
# the moderating effect of year (centered at 1983) to differ for the two
# outcome types)
res <- rma.mv(yi, V, mods = ~ 0 + outcome + outcome:I(year-1983), data = dat,
              random = ~ outcome | trial, struct = "UN")
res

############################################################################

# copy data into 'dat' and examine first 10 rows
dat <- dat.assink2016
head(dat, 10)

# fit multilevel model
res <- rma.mv(yi, vi, random = ~ 1 | study/esid, data=dat)
res

# use the multivariate parameterization
res <- rma.mv(yi, vi, random = ~ factor(esid) | study, data=dat)
res

# use cluster-robust inference methods
robust(res, cluster=dat$study)

# or use coef_test() and conf_int() from clubSandwich
coef_test(res, vcov="CR2", cluster=dat$study)
conf_int(res, vcov="CR2", cluster=dat$study)

# construct approx V matrix assuming r=0.6 for the sampling errors
V <- impute_covariance_matrix(dat$vi, cluster=dat$study, r=0.6)
round(bldiag(V)[1:10,1:10], digits=4)

# fit multivariate model
res <- rma.mv(yi, V, random = ~ factor(esid) | study, data=dat)
res

# use cluster-robust inference methods
robust(res, cluster=dat$study)

# or use coef_test() and conf_int() from clubSandwich
coef_test(res, vcov="CR2", cluster=dat$study)
conf_int(res, vcov="CR2", cluster=dat$study)

# install robumeta package
#install.packages("robumeta")

# load the robumeta package
library(robumeta)

# fit robust variance meta-analysis model
rob <- robu(yi ~ 1, data=dat, studynum=study, var.eff.size=vi, modelweights="CORR", rho=0.6)
rob
rob$reg_table$b.r
c(rob$mod_info$tau.sq)

# we can replicate the robu() results with metafor (note: the estimate of
# tau^2 cannot be directly replicated since robu() uses a method-of-moments
# estimator while rma.mv() -- which in principle could also be used to obtain
# this estimate -- uses (restricted) maximum likelihood estimation; but we can
# just take the tau^2 value as reported by robu() and use it to calculate the
# variances of the estimates incorporating this tau^2 value)
#dat$vi. <- c(tapply(dat$vi, dat$study, length)[dat$study] * (tapply(dat$vi, dat$study, mean)[dat$study] + 0.08606488))
dat$vi. <- ave(dat$vi, dat$study, FUN=length) * (ave(dat$vi, dat$study, FUN=mean) + 0.08606488)
res <- rma(yi, vi., data=dat, method="FE")
c(res$beta)
coef_test(res, vcov="CR2", cluster=dat$study)
conf_int(res, vcov="CR2", cluster=dat$study)

# conceptually the method-of-moments estimator of tau^2 used by robu() is
# analogous to using this model with REML estimation
V <- impute_covariance_matrix(dat$vi, cluster=dat$study, r=0.6)
res <- rma.mv(yi, V, random = ~ 1 | study, data = dat)
res$sigma2

# now let's do some moderator analyses

# fit multivariate model with publication year as moderator
res <- rma.mv(yi, V, mods = ~ year, random = ~ factor(esid) | study, data=dat)
res
coef_test(res, vcov="CR2", cluster=dat$study)

# fit multivariate model with delinquency type as moderator
res <- rma.mv(yi, V, mods = ~ deltype, random = ~ factor(esid) | study, data=dat)
res
coef_test(res, vcov="CR2", cluster=dat$study)

# simultaneous test of coefficients 2 and 3 (like the QM-test)
Wald_test(res, constraints=constrain_zero(2:3), vcov="CR2", cluster=dat$study)

# fit multivariate model with delinquency type as moderator and allow tau^2
# to differ for the different types and allow different correlations for the
# different pairs of types
res <- rma.mv(yi, V, mods = ~ deltype, random = ~ deltype | study, struct="UN", data=dat)
res
par(mfrow=c(3,2))
profile(res)

# this model is overparameterized (not enough studies include multiple types)

# could consider a simpler model but adding a random effect at the observation level
res <- rma.mv(yi, V, mods = ~ deltype, random = list(~ deltype | study, ~ 1 | id), struct="CS", data=dat)
res
profile(res) # looks ok although the estimate of rho drifts towards 1

# this is actually identical to this model (but note that sigma^2.2 = 0)
res <- rma.mv(yi, V, mods = ~ deltype, random = ~ 1 | study/deltype/id, data=dat)
res

############################################################################

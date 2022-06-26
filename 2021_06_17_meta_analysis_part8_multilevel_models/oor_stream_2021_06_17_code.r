############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-06-17
#
# Topic(s):
# - meta-analysis (part 8: multilevel models)
#
# last updated: 2021-06-18

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

# copy dataset to 'dat'
dat <- dat.landenberger2005

# compute log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=n.cbt.non,  bi=n.cbt.rec,
                            ci=n.ctrl.non, di=n.ctrl.rec, data=dat)

# fit random-effects model
res <- rma(yi, vi, data=dat)
res

# compute average odds ratio (and corresponding 95% CI/PI)
predict(res, transf=exp, digits=2)

# create an obs variable that is unique for every row
dat$obs <- 1:nrow(dat)

# we can also fit the RE model using rma.mv()
res <- rma.mv(yi, vi, random = ~ 1 | obs, data=dat)
res

############################################################################

# copy dataset to 'dat'
dat <- dat.bornmann2007

# compute log odds ratios and corresponding sampling variances
dat <- escalc(measure="OR", ai=waward, n1i=wtotal,
                            ci=maward, n2i=mtotal, data=dat)

# within-study averaging assuming independent sampling errors
agg <- aggregate(dat, cluster=dat$study, struct="ID")

# fit random-effects model
res <- rma(yi, vi, data=agg)
res

# compute average odds ratio (and corresponding 95% CI/PI)
predict(res, transf=exp, digits=2)

# fit random-effects model
res <- rma(yi, vi, data=agg)
res

# compute average odds ratio (and corresponding 95% CI/PI)
predict(res, transf=exp, digits=2)

# fit multilevel model
res <- rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat)
res

# compute average odds ratio (and corresponding 95% CI/PI)
predict(res, transf=exp, digits=2)

# compute the ICC
round(res$sigma2[1] / sum(res$sigma2), 2)

# fit model assuming homogeneity within studies
res <- rma.mv(yi, vi, random = ~ 1 | study, data=dat)
res

# compute average odds ratio (and corresponding 95% CI/PI)
predict(res, transf=exp, digits=2)

# LRT comparing the two models
res0 <- rma.mv(yi, vi, random = ~ 1 | study, data=dat)
res1 <- rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat)
anova(res0, res1)

# another way of doing the same LRT
res0 <- rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat, sigma2=c(NA,0))
res1 <- rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat)
anova(res0, res1)

# we can also test if the between-study variance is equal to 0
res0 <- rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat, sigma2=c(0,NA))
res1 <- rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat)
anova(res0, res1)

# fit the same multilevel model in different ways
rma.mv(yi, vi, random = ~ 1 | study/obs, data=dat)
dat$rowid <- 1:nrow(dat)
rma.mv(yi, vi, random = list(~ 1 | study, ~ 1 | rowid), data=dat)
dat$study.obs <- paste0(dat$study, ".", dat$obs)
rma.mv(yi, vi, random = list(~ 1 | study, ~ 1 | study.obs), data=dat)
rma.mv(yi, vi, random = ~ 1 | study/rowid, data=dat)

############################################################################

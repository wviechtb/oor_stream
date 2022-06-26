############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-07-08
#
# Topic(s):
# - meta-analysis (part 10: some review and dose response models)
#
# last updated: 2021-07-13

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

# copy data into 'dat'
dat <- dat.obrien2003
head(dat, 10)

# within-study mean center the BMI variable
dat$bmicent <- dat$bmi - ave(dat$bmi, dat$study)

# compute the log odds of preeclampsia and corresponding sampling variances
dat <- escalc(measure="PLO", xi=cases, ni=total, data=dat)
head(dat, 10)

# fit multilevel meta-regression model to examine the relationship between the
# (centered) BMI variable and the risk of preeclampsia
res1 <- rma.mv(yi, vi, mods = ~ bmicent, random = ~ 1 | study/grp, data=dat)
res1

# draw scatterplot with regression line
tmp <- regplot(res1, xlab=expression("Within-Study Mean Centered BMI"~(kg/m^2)),
               ylab="Preeclampsia Prevalence", transf=transf.ilogit,
               las=1, bty="l", at=seq(0,.18,by=.02), digits=2)

# draw dotted lines connecting the points within studies
invisible(sapply(split(dat, dat$study), function(x) lines(x$bmicent, transf.ilogit(x$yi), lty="dotted")))

# draw points again (on top of lines)
points(tmp)

# model with random intercepts and slopes for studies
res2 <- rma.mv(yi, vi, mods = ~ bmicent,
               random = ~ bmicent | study,
               struct="GEN", data=dat)
res2

# model with random intercepts and slopes for studies and random effects at the outcome level
dat$id <- 1:nrow(dat)
res3 <- rma.mv(yi, vi, mods = ~ bmicent,
               random = list(~ bmicent | study, ~ 1 | id),
               struct="GEN", data=dat)
res3

# LRTs comparing the different models
anova(res1, res2) # not nested so not a meaningful LRT
anova(res1, res3)
anova(res2, res3)

# add results from res3 to the plot
xs <- seq(-10,10,length=100)
preds <- predict(res3, newmods=xs, transf=transf.ilogit)
lines(xs, preds$pred, lwd=3, col="red")
lines(xs, preds$ci.lb, lwd=1, col="red", lty="dashed")
lines(xs, preds$ci.ub, lwd=1, col="red", lty="dashed")

# or just draw results based on res3
tmp <- regplot(res3, xlab=expression("Within-Study Mean Centered BMI"~(kg/m^2)),
               ylab="Preeclampsia Prevalence", transf=transf.ilogit,
               las=1, bty="l", at=seq(0,.18,by=.02), digits=2)
invisible(sapply(split(dat, dat$study), function(x) lines(x$bmicent, transf.ilogit(x$yi), lty="dotted")))
points(tmp)

# can also add a dash of color
install.packages("pals")
library(pals)
tmp <- regplot(res3, xlab=expression("Within-Study Mean Centered BMI"~(kg/m^2)),
               ylab="Preeclampsia Prevalence", transf=transf.ilogit,
               las=1, bty="l", at=seq(0,.18,by=.02), digits=2,
               bg = watlington(13)[dat$study])
invisible(sapply(split(dat, dat$study), function(x) lines(x$bmicent, transf.ilogit(x$yi), lty="dotted")))
points(tmp)

############################################################################

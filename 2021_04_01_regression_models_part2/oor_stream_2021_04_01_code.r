############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-04-01
#
# Topic(s):
# - fundamentals of regression modeling with R (part 2)
#
# last updated: 2021-04-07

############################################################################

# read in the data
dat <- read.delim("data_survey_edit.txt", na.strings="", as.is=TRUE)

# look at first 6 rows of the dataset
head(dat)

# to make things a bit easier below, keep only participants with complete data
# on the variables pss, source, rses, and sex
dat <- dat[complete.cases(dat[c("pss", "source", "rses", "sex")]),]

# collapse the source variable down to a smaller number of levels
table(dat$source)
dat$source <- ifelse(dat$source %in% c("children", "family", "friendships", "spouse/partner"), "interpers", dat$source)
dat$source <- ifelse(dat$source %in% c("health/illness", "lack of time", "life in general", "money/finances"), "other", dat$source)
table(dat$source)

############################################################################

# fit a simple regression model with a quantitative predictor (rses)
res <- lm(pss ~ rses, data=dat)
summary(res)

# scatterplot of pss versus rses (with jittering)
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(rses, amount=0.4), data=dat,
     xlab="Rosenberg Self-Esteem Scale", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(10,40), ylim=c(10,50))

# add the regression line to the scatterplot
abline(res, lwd=3)

############################################################################

# fit a regression model with source and rses as predictors
# note: source is a 'categorical' predictor
res <- lm(pss ~ source + rses, data=dat)
summary(res)

# draw scatterplot again (and color the points by group)
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(rses, amount=0.4), data=dat,
     pch=19, cex=0.5, xlim=c(10,40), ylim=c(10,50),
     xlab="Rosenberg Self-Esteem Scale", ylab="Perceived Stress Scale",
     col=ifelse(dat$source == "interpers", "green", ifelse(dat$source == "other", "red", "blue")))

# add the regression line for the 'interpers' group
abline(a=coef(res)[1], b=coef(res)[4], lwd=3, col="green")

# add the regression line for the 'other' group
abline(a=coef(res)[1]+coef(res)[2], b=coef(res)[4], lwd=3, col="red")

# add the regression line for the 'work' group
abline(a=coef(res)[1]+coef(res)[3], b=coef(res)[4], lwd=3, col="blue")

# (install if necessary and) load the 'car' package
#install.packages("car")
library(car)

# test if the intercepts differ for 'other' versus 'work'
linearHypothesis(res, hypothesis.matrix=c(0,1,-1,0))

# fit the same model but without the intercept term
res <- lm(pss ~ 0 + source + rses, data=dat)
summary(res)

# draw scatterplot again
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(rses, amount=0.4), data=dat,
     xlab="Rosenberg Self-Esteem Scale", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(10,40), ylim=c(10,50))

# add the three regression lines to the scatterplot
abline(a=coef(res)[1], b=coef(res)[4], lwd=3, col="green")
abline(a=coef(res)[2], b=coef(res)[4], lwd=3, col="red")
abline(a=coef(res)[3], b=coef(res)[4], lwd=3, col="blue")

# can also use the coefficient names instead of their position
abline(a=coef(res)["sourceinterpers"], b=coef(res)["rses"], lwd=3, col="green")
abline(a=coef(res)["sourceother"],     b=coef(res)["rses"], lwd=3, col="red")
abline(a=coef(res)["sourcework"],      b=coef(res)["rses"], lwd=3, col="blue")

############################################################################

# now fit the model with an interaction between source and rses
res <- lm(pss ~ source * rses, data=dat)
summary(res)

# this notation is actually a shortcut for a model with the two main effects and their interaction
res <- lm(pss ~ source + rses + source:rses, data=dat)
summary(res)

# draw scatterplot again
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(rses, amount=0.4), data=dat,
     xlab="Rosenberg Self-Esteem Scale", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(10,40), ylim=c(10,50))

# add the three regression lines to the scatterplot
abline(a=coef(res)["(Intercept)"],                            b=coef(res)["rses"],                                 lwd=3, col="green")
abline(a=coef(res)["(Intercept)"] + coef(res)["sourceother"], b=coef(res)["rses"] + coef(res)["sourceother:rses"], lwd=3, col="red")
abline(a=coef(res)["(Intercept)"] + coef(res)["sourcework"],  b=coef(res)["rses"] + coef(res)["sourcework:rses"],  lwd=3, col="blue")

# test if the intercepts differ for 'other' versus 'work'
linearHypothesis(res, hypothesis.matrix=c(0,1,-1,0,0,0))

# test if the slopes differ for 'other' versus 'work'
linearHypothesis(res, hypothesis.matrix=c(0,0,0,0,1,-1))

# can also do the following
linearHypothesis(res, hypothesis.matrix="sourceother - sourcework")
linearHypothesis(res, hypothesis.matrix="sourceother:rses - sourcework:rses")

# note: comparing the intercepts doesn't make much sense here (just for illustration)

# fit the same model but parameterized differently
res <- lm(pss ~ 0 + source + source:rses, data=dat)
summary(res)

# this model directly provides the three intercepts and the three slopes for
# the three groups; the three slopes are the so-called 'simple slopes'

# draw scatterplot again
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(rses, amount=0.4), data=dat,
     xlab="Rosenberg Self-Esteem Scale", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(10,40), ylim=c(10,50))

# add the three regression lines to the scatterplot
abline(a=coef(res)["sourceinterpers"], b=coef(res)["sourceinterpers:rses"], lwd=3, col="green")
abline(a=coef(res)["sourceother"],     b=coef(res)["sourceother:rses"],     lwd=3, col="red")
abline(a=coef(res)["sourcework"],      b=coef(res)["sourcework:rses"],      lwd=3, col="blue")

############################################################################

# draw scatterplot again
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(rses, amount=0.4), data=dat,
     xlab="Rosenberg Self-Esteem Scale", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(10,40), ylim=c(10,50))

# fit simple regression models within each group
res.i <- lm(pss ~ rses, data=dat, subset=source=="interpers")
res.o <- lm(pss ~ rses, data=dat, subset=source=="other")
res.w <- lm(pss ~ rses, data=dat, subset=source=="work")

# add the three regression lines to the scatterplot
abline(res.i, lwd=3, col="green")
abline(res.o, lwd=3, col="red")
abline(res.w, lwd=3, col="blue")

# show the results for all three models
rbind(coef(summary(res.i)), coef(summary(res.o)), coef(summary(res.w)))[c(1,3,5,2,4,6),]

# compare this to the results from the model we fitted earlier
res <- lm(pss ~ 0 + source + source:rses, data=dat)
coef(summary(res))

# the coefficients are the same, but the SEs differ; when we fit the three
# simple regression models, we allow the residual standard error to differ
# across the three groups, while model 'res' assumes that the residual
# standard error is the same for the three groups

c(sigma(res.i), sigma(res.o), sigma(res.w))
sigma(res)

# as a result, the SEs of the coefficients will differ slightly

############################################################################

# let's go back to a simple regression model
res <- lm(pss ~ rses, data=dat)
summary(res)

# how is R^2 computed? (https://en.wikipedia.org/wiki/R-squared)
res1 <- lm(pss ~ rses, data=dat)
res0 <- lm(pss ~ 1, data=dat)
(sum(resid(res0)^2) - sum(resid(res1)^2)) / sum(resid(res0)^2)
summary(res1)

# draw scatterplot again and add the regression line
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(rses, amount=0.4), data=dat,
     xlab="Rosenberg Self-Esteem Scale", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(0,40), ylim=c(0,50))
abline(res, lwd=3)

# now fit the model without an intercept term
res <- lm(pss ~ 0 + rses, data=dat)
summary(res)

# add the regression line (which is forced to have an intercept of 0 now)
abline(a=0, b=coef(res), lty="dotted", lwd=3)
abline(v=0)
abline(h=0)

# why is R^2 so high? the 'baseline' model against which we compare model
# 'res1' assumes that the average outcome is 0; that is a very poor model and
# hence adding 'rses' as a predictor does lead to a great reduction in the
# residual sum of squares
res1 <- lm(pss ~ 0 + rses, data=dat)
res0 <- lm(pss ~ 0, data=dat)
(sum(resid(res0)^2) - sum(resid(res1)^2)) / sum(resid(res0)^2)

# however, this is a rather misleading value of R^2; generally, we should
# never remove the intercept in models where the predictor (or all of the
# predictors) are quantitative variables (there are of course exceptions to
# this)

# how is that different from the case where the predictor was categorical?
res <- lm(pss ~ source, data=dat)
summary(res)
res <- lm(pss ~ 0 + source, data=dat)
summary(res)

# when we remove the intercept, we still get the same model, it's just
# parameterized differently; however, the R^2 from the model without the
# intercept is again misleading

############################################################################

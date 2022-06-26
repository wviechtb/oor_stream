############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-04-08
#
# Topic(s):
# - fundamentals of regression modeling with R (part 3)
#
# last updated: 2021-04-09

############################################################################

# what we covered so far (in the last 2 streams):
#
# - simple regression model with a quantitative predictor
# - simple regression model with a categorical predictor
#   - changing the 'reference level' in such a model
#   - what happens when you 'remove the intercept' in such a model
#   - linear contrasts using linearHypothesis() from the 'car' package
#     (e.g., to compare different groups against each other)
# - regression model with a quantitative and a categorical predictor
#   - what happens when you 'remove the intercept' in such a model
#   - linear contrasts using linearHypothesis() from the 'car' package
# - the model as above but with an interaction between the two predictors
#   - how to 'parameterize' the model such that we directly get the intercepts
#     and slopes of the three groups
#   - contrast the model above with the approach where we fit three separate
#     simple regression models (one per group)
# - how R^2 is computed for models with and without an intercept term

############################################################################

# read in the data
dat <- read.delim("data_survey_edit.txt", na.strings="", as.is=TRUE)

# look at first 6 rows of the dataset
head(dat)

# to make things a bit easier below, keep only participants with complete data
# on the variables pss, source, rses, sex, posaff, and negaff
dat <- dat[complete.cases(dat[c("pss", "source", "rses", "sex", "posaff", "negaff")]),]

# collapse the source variable down to a smaller number of levels
table(dat$source)
dat$source <- ifelse(dat$source %in% c("children", "family", "friendships", "spouse/partner"), "interpers", dat$source)
dat$source <- ifelse(dat$source %in% c("health/illness", "lack of time", "life in general", "money/finances"), "other", dat$source)
table(dat$source)

############################################################################

# a model with two categorical predictors
res <- lm(pss ~ source + sex + source:sex, data=dat)
summary(res)

# or we can use the shortcut notation
res <- lm(pss ~ source*sex, data=dat)
summary(res)

# - intercept: the average PSS value for females when source = 'interpers'
# - sourceother and sourcework: the difference in the average PSS value when
#   source = 'other' or 'work' compared to source = 'interpers' for females
# - sexmale: the difference in the average PSS value between males and females
#   when source = 'interpers'
# - sourceother:sexmale and sourcework:sexmale: the difference between males
#   and females with respect to the difference in the average PSS value when
#   source = 'other' or 'work' compared to source = 'interpers' (hence, for
#   example, if I want to know how 'other' differs from 'interpers' for males,
#   I need to compute: 0.7036 + -2.0880 = -1.3844)

# fit the same model, but parameterize it in such a way that we directly get
# the estimated average PSS value for each of the 6 groups
res <- lm(pss ~ 0 + source:sex, data=dat)
summary(res)

# note: 25.0600 - 26.4444 = -1.3844 is the difference between source = 'other'
# and source = 'interpers' for males (as we also calculated above, but here
# this calculation is a lot more intuitive)

# install (if necessary) and load the 'car' package
#install.packages("car")
library(car)

# test all pairwise contrasts between the source levels for the females
linearHypothesis(res, hypothesis.matrix="sourceother:sexfemale - sourceinterpers:sexfemale")
linearHypothesis(res, hypothesis.matrix="sourcework:sexfemale - sourceinterpers:sexfemale")
linearHypothesis(res, hypothesis.matrix="sourcework:sexfemale - sourceother:sexfemale")

# test all pairwise contrasts between the source levels for the males
linearHypothesis(res, hypothesis.matrix="sourceother:sexmale - sourceinterpers:sexmale")
linearHypothesis(res, hypothesis.matrix="sourcework:sexmale - sourceinterpers:sexmale")
linearHypothesis(res, hypothesis.matrix="sourcework:sexmale - sourceother:sexmale")

# how to do a Bonferroni correction for these 6 pairwise contrasts; could look
# into the 'multcomp' package which may be able to do this in a concise way;
# but we can also just collect the 6 p-values in a vector and then feed this to
# the p.adjust() function to do the Bonferroni correction 'manually'
pvals <- c(linearHypothesis(res, hypothesis.matrix="sourceother:sexfemale - sourceinterpers:sexfemale")$"Pr(>F)"[2],
           linearHypothesis(res, hypothesis.matrix="sourcework:sexfemale - sourceinterpers:sexfemale")$"Pr(>F)"[2],
           linearHypothesis(res, hypothesis.matrix="sourcework:sexfemale - sourceother:sexfemale")$"Pr(>F)"[2],
           linearHypothesis(res, hypothesis.matrix="sourceother:sexmale - sourceinterpers:sexmale")$"Pr(>F)"[2],
           linearHypothesis(res, hypothesis.matrix="sourcework:sexmale - sourceinterpers:sexmale")$"Pr(>F)"[2],
           linearHypothesis(res, hypothesis.matrix="sourcework:sexmale - sourceother:sexmale")$"Pr(>F)"[2])
p.adjust(pvals, method="bonferroni")

# note: the 'unadjusted' p-values are so large to begin with that we just get
# p = 1 for all these adjusted p-values with the Bonferroni correction

############################################################################

# a model with a three-way interaction (of two categorical predictors and one
# numerical predictor)
res <- lm(pss ~ source*sex*rses, data=dat)
summary(res)

# note: one could interpret each coefficient in the manner described above,
# but this gets quite tedious, especially for the three-way interaction terms
# (e.g., the coefficient for sourceother:sexmale:rses indicates how different
# the difference between source = 'other' and source = 'interpers' is for
# males compared to females with respect to the slope of the relationship
# between rses and pss)

# we can parameterize the model in such a way that we directly get the 6
# intercepts and 6 slopes for each of the 6 groups
res <- lm(pss ~ 0 + source:sex + source:sex:rses, data=dat)
summary(res)

# now it is easy to test linear contrasts (e.g., test if the slope of the
# relationship between rses and pss differs for source = 'interpers' and
# source = 'work' for females)
linearHypothesis(res, hypothesis.matrix="sourceinterpers:sexfemale:rses - sourcework:sexfemale:rses")

############################################################################

# model with two quantitative predictors
res <- lm(pss ~ posaff + negaff, data=dat)
summary(res)

# scatterplot of pss versus posaff (with jittering)
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(posaff, amount=0.4), data=dat,
     xlab="Positive Affect", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(10,50), ylim=c(10,50))

# how can draw the regression line into the scatterplot for this model?

# compute the predicted average PSS value for posaff = 10 and posaff = 50 when
# negaff is equal to its mean
mean(dat$negaff)
24.32280 + -0.20631*10 + 0.48072*19.41687
24.32280 + -0.20631*50 + 0.48072*19.41687
coef(res)[1] + coef(res)[2] * 10 + coef(res)[3] * mean(dat$negaff)
coef(res)[1] + coef(res)[2] * 50 + coef(res)[3] * mean(dat$negaff)

# that gives us this line for the scatterplot
abline(a = coef(res)[1] + coef(res)[3] * mean(dat$negaff), b = coef(res)[2])

# this is sometimes called the 'marginal' regression line (also called the
# marginal association / marginal effect)

# note: if there are missing values in the data, then the subjects included in
# fitting the model above may differ from those with complete data on negaff;
# so, when computing the mean of negaff, we should only base this on those
# subjects actually included in the analysis; we can do this by computing the
# mean based on the 'model matrix'
abline(a = coef(res)[1] + coef(res)[3] * colMeans(model.matrix(res))[3], b = coef(res)[2])
# or maybe even better (so we don't have to count coefficient numbers)
abline(a = coef(res)["(Intercept)"] + coef(res)["negaff"] * colMeans(model.matrix(res))["negaff"], b = coef(res)["posaff"])

# instead of holding negaff at its mean, we could also hold it at one standard
# deviation below or above its mean
abline(a = coef(res)[1] + coef(res)[3] * (mean(dat$negaff) - sd(dat$negaff)), b = coef(res)[2], lty="dotted")
abline(a = coef(res)[1] + coef(res)[3] * (mean(dat$negaff) + sd(dat$negaff)), b = coef(res)[2], lty="dashed")

# can also use partial regression plots to illustrate the association between
# pss and posaff while 'controlling' for negaff
# https://en.wikipedia.org/wiki/Partial_regression_plot
res1 <- lm(pss ~ negaff, data=dat)
res2 <- lm(posaff ~ negaff, data=dat)
plot(residuals(res2), residuals(res1), pch=19, cex=0.5)
abline(a=0, b=coef(res)["posaff"])

# model with an interaction between two continuous predictors
res <- lm(pss ~ posaff*negaff, data=dat)
summary(res)

# scatterplot of pss versus posaff (with jittering)
set.seed(1234)
plot(jitter(pss, amount=0.4) ~ jitter(posaff, amount=0.4), data=dat,
     xlab="Positive Affect", ylab="Perceived Stress Scale",
     pch=19, cex=0.5, xlim=c(10,50), ylim=c(10,50))

# compute the predicted average PSS value for posaff = 10 and posaff = 50 when
# negaff is equal to its mean
20.469345 + -0.089754*10 + 0.659114*mean(dat$negaff) + -0.005543*mean(10*dat$negaff)
20.469345 + -0.089754*50 + 0.659114*mean(dat$negaff) + -0.005543*mean(50*dat$negaff)

# that would give us this line
abline(a = coef(res)[1] + coef(res)[3] * mean(dat$negaff), b = coef(res)[2] + coef(res)[4]*mean(dat$negaff))

# draw the lines when negaff is one standard deviation below or above its mean
abline(a = coef(res)[1] + coef(res)[3] * (mean(dat$negaff) - sd(dat$negaff)), b = coef(res)[2] + coef(res)[4]*(mean(dat$negaff) - sd(dat$negaff)), lty="dotted")
abline(a = coef(res)[1] + coef(res)[3] * (mean(dat$negaff) + sd(dat$negaff)), b = coef(res)[2] + coef(res)[4]*(mean(dat$negaff) + sd(dat$negaff)), lty="dashed")

# note: the lines are no longer parallel, since the slope of the relationship
# between pss and posaff depends on negaff in this model (since it includes an
# interaction between posaff and negaff)

# you might also consider checking out the emmeans package, which does this
# kind of stuff in a more automated manner
# https://cran.r-project.org/package=emmeans

############################################################################

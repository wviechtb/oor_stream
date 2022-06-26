############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2021-03-18
#
# Topic(s):
# - fundamentals of regression modeling with R (part 1)
#
# last updated: 2021-04-08

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

# fit a regression model with pss as outcome and source as predictor
res <- lm(pss ~ source, data=dat)
summary(res)

# coding is like this:
# sourceother = 1 if source = "other" and 0 otherwise
# sourcework  = 1 if source = "work"  and 0 otherwise
# hence, if source = "interpers", then sourceother = 0 and sourcework = 0
# so, "interpers" is the so-called reference level
# sourceother is the difference between "other" and "interpers"
# sourcework  is the difference between "work"  and "interpers"
# these are 'contrasts'
# F-statistics tests that both of these contrasts are 0 (i.e, it is testing
# the null hypothesis that the average PSS value is the same in the 3 groups)

# change the reference level to "work" (note: relevel() only work on factors,
# so we must explicitly turn the source variable into a factor before we can
# use the relevel() function on it)
res <- lm(pss ~ relevel(factor(source), ref="work"), data=dat)
summary(res)

# [...]interpers is the difference between "interpers" and "work"
# [...]other     is the difference between "other"     and "work"
# note: F-statistic is the same, so it does not matter which reference level
# we use if we want to test if there are differences between the groups

# same model but remove the intercept
res <- lm(pss ~ 0 + source, data=dat)
#same as: res <- lm(pss ~ source - 1, data=dat)
summary(res)

# coding is like this:
# sourceinterps = 1 if source = "interpers" and 0 otherwise
# sourceother   = 1 if source = "other"     and 0 otherwise
# sourcework    = 1 if source = "work"      and 0 otherwise
# so the coefficients are the average PSS values for the 3 groups
# the F-statistic is testing whether the average PSS value is 0 in all 3 groups
# (this is a somewhat meaningless thing to test here, since PSS values range
# from 10 to 50 and so we know a priori that the mean PSS value could never be
# 0 in any of the groups anyway)

# fit of the two models is the same, they are just 'parameterized' differently
res <- lm(pss ~ source, data=dat)
logLik(res)
sigma(res)
res <- lm(pss ~ 0 + source, data=dat)
logLik(res)
sigma(res)

# (install if necessary and) load the 'car' package
#install.packages("car")
library(car)

# fit model without the intercept again
res <- lm(pss ~ 0 + source, data=dat)
summary(res)

# using linearHypothesis(), we can test linear contrasts we are interested in

# test if 1*sourceinterpers + -1*sourceother + 0*sourcework is significant
linearHypothesis(res, hypothesis.matrix=c(1,-1,0))
# test the other two pairwise contrasts
linearHypothesis(res, hypothesis.matrix=c(1,0,-1))
linearHypothesis(res, hypothesis.matrix=c(0,1,-1))

# note: the p-values for these contrasts are the same as we obtained earlier
# (when we fitted the models with the intercept)

# can also other contrasts that may be of interest

# 1*sourceinterpers - 1/2*sourceother - 1/2*sourcework
# 1*sourceinterpers - (sourceother + sourcework) / 2
# (i.e., test if the average PSS value of the "interpers" group is different
# from the average of the PSS values of the "other" and "work" groups, or more
# precisely, the average of the two averages of these two groups)
linearHypothesis(res, hypothesis.matrix=c(1,-1/2,-1/2))

# fit the model with the intercept
res <- lm(pss ~ source, data=dat)
summary(res)
linearHypothesis(res, hypothesis.matrix=c(0,1,-1))

# (mean_other - mean_interpers) - (mean_work - mean_interpers)
#  mean_other - mean_interpers  -  mean_work + mean_interpers
#  mean_other - mean_work
# so this is the same as the contrast above where we test other vs work

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-11-19
#
# Topic(s):
# - a comparison of R, jamovi, and JASP
#
# last updated: 2020-11-19

############################################################################

# The code below is what we did in R during the stream. We ran the same types
# of analyses in jamovi and JASP and compared the results.

############################################################################

# restart the R session (in RStudio: Menu 'Session' - Restart R)

############################################################################

# set working directory (adjust to your own computer; in RStudio, go to the
# Session menu, 'Set Working Directory', and 'To Source File Location')
setwd("~/temp")

# install/load jmv package
#install.packages("jmv")
library(jmv)

# install/load psych package
#install.packages("psych")
library(psych)

# also need to install the GPArotation package
#install.packages("GPArotation")

############################################################################

# read in the data
dat <- read.table("data_survey_edit.txt", header=TRUE, sep="\t",
                  as.is=TRUE, na.strings = "")

# look at first 6 rows of the dataset
head(dat)

# get descriptives for 'age'
summary(dat$age)
sd(dat$age)

# histogram for 'age' plus density
hist(dat$age, main="Histogram for Age", xlab="Age", freq=FALSE)
res <- density(dat$age)
lines(res$x, res$y)

# descriptives for 'age' separately for female and male subjects
by(dat$age, dat$sex, summary)
by(dat$age, dat$sex, sd)

# histograms for male and female subjects separately
par(mfrow=c(2,1)) # split plotting device into 2 rows and 1 column
hist(dat$age[dat$sex == "male"], main="Histogram for Age", xlab="Age", freq=FALSE)
res <- density(dat$age[dat$sex == "male"])
lines(res$x, res$y)
hist(dat$age[dat$sex == "female"], main="Histogram for Age", xlab="Age", freq=FALSE)
res <- density(dat$age[dat$sex == "female"])
lines(res$x, res$y)

# scatterplot of age versus pss (= perceived stress scale)
par(mfrow=c(1,1))
plot(pss ~ age, data=dat, pch=19, cex=0.5, xlab="Age", col="blue",
     ylab="Perceived Stress Scale", main="Some Awesome Title")

# code copied from jamovi (after switching on "syntax mode")
corrMatrix(
   data = dat,
   vars = vars(age, pss),
   plots = TRUE,
   plotDens = TRUE,
   plotStats = TRUE)

# ANOVA (pss as outcome and martial status as grouping variable)
res <- aov(pss ~ marital, data=dat)
summary(res)
TukeyHSD(res)

# ANOVA not assuming equal variances in the different groups
res <- oneway.test(pss ~ marital, data=dat)
res

# regression (pss as outcome and age as predictor)
res <- lm(pss ~ age, data=dat)
summary(res)

# regression (pss as outcome, age, rses, and sex as predictors)
res <- lm(pss ~ age + rses + sex, data=dat)
summary(res)

# chi-square test of association between sex and source
table(dat$source, dat$sex)
chisq.test(dat$sex, dat$source)
chisq.test(dat$sex, dat$source, correct=FALSE)

# EFA of the pss (parallel analysis to determine the number of factors)
pss <- dat[grep("pss[0-9]", names(dat))]
fa.parallel(pss, fa="fa", fm="pa", n.iter=1000, sim=FALSE)
res <- fa(pss, nfactors=3, rotate="oblimin", fm="pa")
print(res, cut=0.3)

############################################################################

# a few potentially useful links:

# - Undergraduate Statistics with JASP
#   https://osf.io/t56kg/
#
# - JASP vs. Jamovi: An In-Depth Comparison
#   https://www.youtube.com/watch?v=7IFVySGysls
#
# - jamovi in the cloud
#   https://cloud.jamovi.org/

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-10-15
#
# Topic(s):
# - psychometrics (reliability, factor analysis)
#
# last updated: 2020-10-22

############################################################################

# restart the R session (Menu 'Session' - Restart R)

############################################################################

# how to create an 'axis break'

# read in data

load("data_survey_edit.rdata")

head(dat)

# just for illustration purposes, make two subjects in the dataset really old

tmp <- dat
tmp$age[1] <- 229
tmp$age[2] <- 237

plot(tmp$age, tmp$pss, pch=19, cex=0.5,
     xlab="Age", ylab="Stress", xlim=c(0,250), ylim=c(10,50))

if (!suppressWarnings(require(plotrix))) install.packages("plotrix")

library(plotrix)

gap.plot(tmp$age, tmp$pss, pch=19, cex=0.5,
         xlab="Age", ylab="Stress", xlim=c(0,250), ylim=c(10,50),
         gap=c(104,218), gap.axis="x", xtics=c(0,25,50,75,100,225,250))

############################################################################

# psychometrics
# https://en.wikipedia.org/wiki/Psychometrics

# read in data

load("data_survey_edit.rdata")

head(dat)

# install (if necessary) the 'psych' package and load it

if (!suppressWarnings(require(psych))) install.packages("psych")

library(psych)

# install (if necessary) the 'GPArotation' package

if (!suppressWarnings(require(GPArotation))) install.packages("GPArotation")

# split-half reliability

dat$lotr_even <- with(dat, lotr2 + lotr4 + lotr6)
dat$lotr_odd  <- with(dat, lotr1 + lotr3 + lotr5)
with(dat, cor(lotr_even, lotr_odd, use="complete.obs"))

# now use the Spearman-Brown equation to get the split-half reliability
# https://en.wikipedia.org/wiki/Spearman–Brown_prediction_formula

r <- with(dat, cor(lotr_even, lotr_odd, use="complete.obs"))
2 * r / (1 + r)

# Cronbach's alpha
# https://en.wikipedia.org/wiki/Cronbach's_alpha

names(dat)
grep("lotr", names(dat))
grep("lotr[0-9]", names(dat))

alpha(dat[grep("lotr[0-9]", names(dat))])
alpha(dat[grep("mastery[0-9]", names(dat))])
alpha(dat[grep("pss[0-9]", names(dat))])
alpha(dat[grep("rses[0-9]", names(dat))])

alpha(dat[c("panas1", "panas4", "panas6", "panas7", "panas9", "panas12", "panas13", "panas15", "panas17", "panas18")])
alpha(dat[c("panas2", "panas3", "panas5", "panas8", "panas10", "panas11", "panas14", "panas16", "panas19", "panas20")])

# all possible ways of splitting the LOTR scale (6 items)

splt <- combn(6, 3)
splt

# note: the second half of splt mirrors the first part, so we don't need it

splt <- splt[,1:10]
splt

# now get all split-half reliabilities

lotr <- dat[grep("lotr[0-9]", names(dat))]

r <- rep(NA, ncol(splt))

for (i in 1:ncol(splt)) {

   tot1 <- rowSums(lotr[splt[,i]])
   tot2 <- rowSums(lotr[-splt[,i]])
   r[i] <- cor(tot1, tot2)

}

# convert correlations into split-half reliabilities

rel <- 2 * r / (1 + r)

# summary statistics for the split-half reliabilities

round(summary(rel), 2)
round(sd(rel), 2)

# Cronbach's alpha =~ average of all split-half reliabilities

# note: there is also the splitHalf() function from the psych package

splitHalf(lotr)

# scree plot
# https://en.wikipedia.org/wiki/Scree_plot

pss <- dat[grep("pss[0-9]", names(dat))]

scree(pss, factors=FALSE)

# parallel analysis
# https://en.wikipedia.org/wiki/Parallel_analysis

fa.parallel(pss, fa="pc", n.iter=1000, sim=FALSE)

# principal component analysis (PCA)
# https://en.wikipedia.org/wiki/Principal_component_analysis

principal(pss, nfactors=2, rotate="oblimin")

# parallel analysis based on EFA using 'principal axis factoring'

fa.parallel(pss, fa="fa", fm="pa", n.iter=1000, sim=FALSE)

# note: this suggests 3 factors, but will stick to 2 factors below (one can
# also examine the results when setting nfactors=3 to see if they make sense)

# exploratory factor analysis (EFA) using 'principal axis factoring'
# https://en.wikipedia.org/wiki/Exploratory_factor_analysis

fa(pss, nfactors=2, rotate="oblimin", fm="pa")

# confirmatory factor analysis (CFA)
# https://en.wikipedia.org/wiki/Confirmatory_factor_analysis

# install (if necessary) the 'lavaan' package and load it

if (!suppressWarnings(require(lavaan))) install.packages("lavaan")

library(lavaan)

# one-factor CFA model for the perceived stress scale (PSS)

model <- 'STRESS =~ pss1 + pss2 + pss3 + pss4 + pss5 + pss6 + pss7 + pss8 + pss9 + pss10'

res <- cfa(model, data=dat, estimator="ML", std.lv=TRUE)
summary(res, fit.measures=TRUE, standardized=TRUE)

# compute McDonald's omega based on the one-factor model

sum(coef(res)[1:10])^2 / (sum(coef(res)[1:10])^2 + sum(coef(res)[11:20]))

# two-factor CFA model for the perceived stress scale (PSS)

model <- '
STRESS.POS =~ pss1 + pss2 + pss3 + pss6 + pss9 + pss10
STRESS.NEG =~ pss4 + pss5 + pss7 + pss8'

res <- cfa(model, data=dat, estimator="ML", std.lv=TRUE)
summary(res, fit.measures=TRUE, standardized=TRUE)

# compute McDonald's omega for each factor in the two-factor model

sum(coef(res)[1:6])^2 / (sum(coef(res)[1:6])^2 + sum(coef(res)[11:16]))
sum(coef(res)[7:10])^2 / (sum(coef(res)[7:10])^2 + sum(coef(res)[17:20]))

# conduct a model comparison likelihood ratio test (does the two-factor model
# fit significantly better than the one factor model?)

model <- 'STRESS =~ pss1 + pss2 + pss3 + pss4 + pss5 + pss6 + pss7 + pss8 + pss9 + pss10'
res1 <- cfa(model, data=dat, estimator="ML", std.lv=TRUE)
model <- '
STRESS.POS =~ pss1 + pss2 + pss3 + pss6 + pss9 + pss10
STRESS.NEG =~ pss4 + pss5 + pss7 + pss8'
res2 <- cfa(model, data=dat, estimator="ML", std.lv=TRUE)

anova(res1, res2)

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-03-09
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 1.1 - 1.3
#
# last updated: 2023-03-10

############################################################################

### 1.2: Why learn regression?

# the datasets used throughout the book are available either via the rosdata
# package or via this GitHub repo: https://github.com/avehtari/ROS-Examples

# install the rosdata package (first need to install the 'remotes' package)

#install.packages("remotes")
#remotes::install_github("avehtari/ROS-Examples", subdir="rpackage")

# load the rosdata package
library(rosdata)

# copy the hibbs dataset to dat
dat <- hibbs

# look at the data
dat

# plot growth on the x-axis versus vote on the y-axis (Figure 1.1a)
plot(vote ~ growth, data=dat, xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", pch=NA, xaxt="n", yaxt="n",
     xlim=c(-0.5,4.5), ylim=c(43,62), bty="l")
abline(h=50, col="gray")
with(dat, text(growth, vote, year, pos=4))
axis(side=1, at=0:4, labels=paste0(0:4, "%"))
axis(side=2, at=c(45,50,55,60), labels=paste0(c(45,50,55,60), "%"))
title("Forecasting the election from the economy")

# fit regression model (using the method of least squares)
res <- lm(vote ~ growth, data=dat)
summary(res)

# plot growth versus vote and add the regression line (Figure 1.1b)
plot(vote ~ growth, data=dat, xlab="Average recent growth in personal income",
     ylab="Incumbent party's vote share", pch=19, xaxt="n", yaxt="n",
     xlim=c(-0.5,4.5), ylim=c(43,62), bty="l")
abline(h=50, col="gray")
points(vote ~ growth, data=dat, pch=19)
axis(side=1, at=0:4, labels=paste0(0:4, "%"))
axis(side=2, at=c(45,50,55,60), labels=paste0(c(45,50,55,60), "%"))
title("Data and linear fit")
abline(res, lwd=3)
text(3, coef(res)[1] + coef(res)[2]*3, pos=4, offset=2,
     paste0("y = ", round(coef(res)[1], 1), " + ", round(coef(res)[2], 1), " x"))

# install the rstanarm package
#install.packages("rstanarm")

# load the rstanarm package
library(rstanarm)

# fit the model using stan_glm() (first setting the seed of the random number
# generator to make the results fully reproducible)
set.seed(1234)
res <- stan_glm(vote ~ growth, data=hibbs)
res

# get more detailed information about the fitted model
summary(res)

# extract the posterior samples
post <- as.data.frame(res)
head(post)

# histogram of the posterior distribution for the slope
hist(post$growth, xlab="Slope", main="")

# median of the posterior distribution for the slope
median(post$growth)

# 95% credible interval
quantile(post$growth, probs=c(.025,.975))

############################################################################

### 1.3: Some examples of regression

## Estimating public opinion from an opt-in internet survey

# not sure if these data are available somewhere

## A randomized experiment on the effect of an educational television program

# copy the electric dataset to dat
dat <- electric

# histogram of the post-treatment scores for control and treatment classes for
# grades 1-4 (Figure 1.2)

par(mfcol=c(2,4), mgp=c(3,0,0), mar=rep(1.5,4), oma=c(0,8,3,0))

for (g in 1:4) {

   sub <- dat[dat$grade == g & dat$treatment == 0,]
   hist(sub$post_test, ylim=c(0,10), breaks=seq(40,130,by=5),
        xaxt="n", yaxt="n", xlab="", ylab="", main="")
   axis(side=1, at=c(50,75,100), lty="blank")
   abline(v=mean(sub$post_test), lwd=5)

   mtext(side=3, paste("Grade", g), line=2, font=2)

   if (g == 1)
      mtext(side=2, "Control\nclasses", las=1, font=2, line=3)

   sub <- dat[dat$grade == g & dat$treatment == 1,]
   hist(sub$post_test, ylim=c(0,10), breaks=seq(40,130,by=5),
        xaxt="n", yaxt="n", xlab="", ylab="", main="")
   axis(side=1, at=c(50,75,100), lty="blank")
   abline(v=mean(sub$post_test), lwd=5)

   if (g == 1)
      mtext(side=2, "Treated\nclasses", las=1, font=2, line=3)

}

# close plotting device (to reset changes made above back to the defaults)
dev.off()

## Estimating the effects of United Nations peacekeeping, using pre-treatment
## variables to adjust for differences between treatment and control groups

# download the peacekeeping dataset file from the book website
download.file("https://github.com/avehtari/ROS-Examples/raw/master/Peacekeeping/data/pk%26pkept_old.dta",
              destfile="pk&pkept_old.dta")

# load the foreign package
library(foreign)

# read in the data (in Stata format)
dat <- read.dta("pk&pkept_old.dta")

# relevant variables for this discussion:
#
# morewar  = indicates whether there was a return to civil war (1=yes, 0=no)
# pk_dum   = indicates whether peacekeepers were present (1=yes, 0=no)
# pcw      = post cold war termination (the analysis in the book is restricted
#            to cases where this is equal to 1)
# cfdate   = date when follow-up started
# faildate = date when there was a return to civil war
# hazard1  = badness of conditions in country at start of the follow-up

# only keep variables we need
dat <- dat[c("morewar", "pk_dum", "pcw", "cfdate", "faildate", "hazard1")]

# select rows where pcw=1 and cfdate is not missing
dat <- dat[dat$pcw == 1 & !is.na(dat$cfdate),]

# for countries where faildate is missing, set it to the end of the follow-up period
dat$faildate[is.na(dat$faildate)] <- "2004-12-31"

# compute time between start and end of follow-up (in years, approximately)
dat$delay <- as.vector(dat$faildate - dat$cfdate) / 365.24

# transform hazard1 variable into badness score
dat$badness <- log(dat$hazard1) / 2 + 8

# contingency table of presence/absence peacekeepers and return to war yes/no
tab <- table(peacekeepers=dat$pk_dum, war=dat$morewar)
tab

# turn frequencies into proportions over rows
round(prop.table(tab, margin=1), digits=2)

# the first column of this table shows that 34% of countries without
# peacekeepers and 56% of countries with peacekeepers remained at peace

# mean delay (in years) for return to war with and without peacekeepers
mean(dat$delay[dat$morewar == 1 & dat$pk_dum == 1])
mean(dat$delay[dat$morewar == 1 & dat$pk_dum == 0])

# histogram for years until return to war with and without peacekeepers (Figure 1.3)
par(mfrow=c(1,2))
hist(dat$delay[dat$morewar == 1 & dat$pk_dum == 1], breaks=seq(0,8,by=.5),
     xlab="Years until return of war", main="With Peacekeepers")
hist(dat$delay[dat$morewar == 1 & dat$pk_dum == 0], breaks=seq(0,8,by=.5),
     xlab="Years until return of war", main="Without Peacekeepers")

# scatterplot of badness score versus delay with and without peacekeepers (Figure 1.4)
par(mfrow=c(1,2), mar=c(5,5,4,2))
plot(delay ~ badness, dat=dat, subset=pk_dum==1, pch=ifelse(morewar==1, 19, 21),
     xlab="Pre−treatment measure of problems with the country",
     ylab="Delay (in years) between start and end of follow-up\n(open circles where conflict did not return)",
     bty="n", xlim=c(1,7), main="With Peacekeeping")
plot(delay ~ badness, dat=dat, subset=pk_dum==0, pch=ifelse(morewar==1, 19, 21),
     xlab="Pre−treatment measure of problems with the country",
     ylab="Delay (in years) between start and end of follow-up\n(open circles where conflict did not return)",
     bty="n", xlim=c(1,7), main="Without Peacekeeping")

## Estimating the effect of gun laws, and the difficulty of inference using
## regression with a large number of predictors

# the article: https://doi.org/10.1016/S0140-6736(15)01026-0

# not sure if these data are available somewhere

############################################################################

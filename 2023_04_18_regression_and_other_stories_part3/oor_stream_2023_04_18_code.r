############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-04-18
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 2.1 - 2.2
#
# last updated: 2023-05-04

############################################################################

### 2.1: Examining where data come from

# load the foreign package
library(foreign)

# download the hdi.dat and the vote/income datasets from the book GitHub page
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/HDI/data/hdi.dat", destfile="hdi.dat")
download.file("https://github.com/avehtari/ROS-Examples/raw/master/HDI/data/state%20vote%20and%20income%2C%2068-00.dta", destfile="state_vote_income.dta")

# read in the data
dat1 <- read.table("hdi.dat", header=TRUE)
dat2 <- read.dta("state_vote_income.dta")

# only keep the year 2000 data from dat2
dat2 <- dat2[dat2$st_year == 2000,]

# merge the two datasets
dat <- merge(dat1, dat2, by.x="state", by.y="st_state")

# note: since dat2 does not include info on Washington, D.C., the merged
# dataset also doesn't include this row (so we are left with 50 rows)
nrow(dat)

# Figure 2.2(a)
plot(dat$st_income, dat$hdi, xlab="Average state income in 2000",
     ylab="Human Development Index", type="n")
text(dat$st_income, dat$hdi, dat$st_stateabb)

# Figure 2.2(b)
plot(rank(dat$st_income), rank(dat$hdi), xlab="Average state income in 2000",
     ylab="Human Development Index", type="n")
text(rank(dat$st_income), rank(dat$hdi), dat$st_stateabb)

# correlation between the ranks
round(cor(rank(dat$st_income), rank(dat$hdi)), digits=2)

# sidenote: this is the same as the Spearman correlation
round(cor(dat$st_income, dat$hdi, method="spearman"), digits=2)

# install and load the usmap package
# install.packages("usmap")
library(usmap)

# install and load the ggplot2 package
# install.packages("ggplot2")
library(ggplot2)

# merge dat onto the statepop dataset that coems with the usmap package (this
# allows the plot_usmap() function to do its magic)
dat <- merge(statepop, dat, by.x="full", by.y="state")

# Figure 2.1
plot_usmap(data=dat, values="hdi", color="white") +
   scale_fill_stepsn(name="HDI", breaks=c(0,.8,.85,.9,.95,1), right=FALSE,
                     colors=paste0("gray", c(90,70,50,30,10))) +
   theme(legend.position="bottom", legend.justification="center")

############################################################################

### Details of measurement can be important

# note: the following is not an exact recreation of Figure 2.3 (the code that
# was used by the authors for the pre-processing of the data can be found at
# https://github.com/avehtari/ROS-Examples/tree/master/Pew), but the following
# essentially produces the same figure)

# download the dataset
download.file("https://github.com/avehtari/ROS-Examples/raw/master/Pew/data/pew_research_center_june_elect_wknd_data.dta", destfile="pew_research_center_june_elect_wknd_data.dta")

# also download the codebook
download.file("https://github.com/avehtari/ROS-Examples/raw/master/Pew/data/Pew_Codebook.pdf", destfile="pew_codebook.pdf")

# read in the dataset
dat <- read.dta("pew_research_center_june_elect_wknd_data.dta")

# remove people who refused to answer the income variable
dat <- dat[dat$income != "dk/refused",]

# drop the now unused 'dk/refused' level
dat$income <- droplevels(dat$income)

# split up plotting device into one row and two columns
par(mfrow=c(1,2))

## Figure 2.3(a)

# frequency table of the ideo variable
table(dat$ideo)

# get rid of the 'dk/refused' level by making these values missing and
# dropping the unused levels
dat$ideo[dat$ideo == "dk/refused"] <- NA
dat$ideo <- droplevels(dat$ideo)

# frequency table of the ideo variable
table(dat$ideo)

# contingency table of income versus ideo
tab <- table(dat$income, dat$ideo)
tab

# reverse the order of the columns
tab <- tab[,5:1]
tab

# turn the table into proportions across rows
tab <- prop.table(tab, margin=1)
tab

# compute cumulative sums across rows (and since this ends up transposing the
# table, we transpose it back) and turn these cumulative proportions into
# percentages
tab <- t(apply(tab, 1, cumsum)) * 100
tab

# set up an empty plot
plot(NA, xlim=c(1,9), ylim=c(0,100), xlab="", ylab="",
     xaxt="n", yaxt="n", xaxs="i", yaxs="i")
axis(side=1, at=1:9, labels=FALSE)
axis(side=1, at=c(1.4, 5, 8.6), labels=c("Low income", "Middle income", "High income"), tick=FALSE)
axis(side=2, at=c(0,50,100), labels=c("0%", "50%", "100%"))

# add shading of the regions to the plot
tab <- cbind(0,tab)
cols <- paste0("gray", c(55,65,75,85,95))
for (i in 2:ncol(tab)) {
   polygon(c(1:9,9:1), c(tab[,i],rev(tab[,i-1])), col=cols[i-1])
}

# add labels
text(5, rowMeans(cbind(tab[5,2:6], tab[5,1:5])),
     c("Very liberal", "Liberal", "Moderate", "Conservative", "Very conservative"))

# add title
title("Self-declared political ideology, by income")

## Figure 2.3(b)

# frequency tables of the party and partyln variables
table(dat$party)
table(dat$partyln)

# turn party and partyln into a single partyident variable
dat$partyident <- NA
dat$partyident[dat$party   == "independent"]     <- 3
dat$partyident[dat$partyln == "lean democrat"]   <- 2
dat$partyident[dat$partyln == "lean republican"] <- 4
dat$partyident[dat$party   == "republican"]      <- 5
dat$partyident[dat$party   == "democrat"]        <- 1

# frequency table of the partyident variable
table(dat$partyident)

# contingency table of income versus partyident
tab <- table(dat$income, dat$partyident)
tab

# turn the table into proportions across rows
tab <- prop.table(tab, margin=1)
tab

# compute cumulative sums across rows (and since this ends up transposing the
# table, we transpose it back) and turn these cumulative proportions into
# percentages
tab <- t(apply(tab, 1, cumsum)) * 100
tab

# set up an empty plot
plot(NA, xlim=c(1,9), ylim=c(0,100), xlab="", ylab="",
     xaxt="n", yaxt="n", xaxs="i", yaxs="i")
axis(side=1, at=1:9, labels=FALSE)
axis(side=1, at=c(1.4, 5, 8.6), labels=c("Low income", "Middle income", "High income"), tick=FALSE)
axis(side=2, at=c(0,50,100), labels=c("0%", "50%", "100%"))

# add shading of the regions to the plot
tab <- cbind(0,tab)
cols <- paste0("gray", c(55,65,75,85,95))
for (i in 2:ncol(tab)) {
   polygon(c(1:9,9:1), c(tab[,i],rev(tab[,i-1])), col=cols[i-1])
}

# add labels
text(5, rowMeans(cbind(tab[5,2:6], tab[5,1:5])),
     c("Democrat", "Lean Dem.", "Independent", "Lean Rep.", "Republican"))

# add title
title("Self-declared party identification, by income")

# to examine if self-declared political ideology and/or part identification
# can be predicted from income, we could use multinomial logistic regression
# (since the outcome variable is a 5-level factor); here, we'll use a simpler
# approach where we treat the ideology and party identification variables each
# as a numeric continuum

# levels of the ideo factor
levels(dat$ideo)

# therefore: 1 = very conservative, ..., 5 = very liberal

# fit linear regression model using the level integers as outcome
summary(lm(as.integer(ideo) ~ income, data=dat))

# the negative coefficients indicate that those with a higher income tend to
# have a lower (i.e., more conservative) ideo level, but the relationship is
# actually quite weak as expected based on Figure 2.3(a); also, differences
# between those with a somewhat higher income level are minimal (and in fact
# those with very high income are almost indistinguishable from those with the
# very lowest income level)

# partyident is already coded as a numeric variable, where 1 = democrat, 2 =
# lean democrat, 3 = independent, 4 = lean republican, 5 = republican

# fit linear regression model
summary(lm(partyident ~ income, data=dat))

# the positive coefficients indicate that those with a higher income tend to
# have a higher partyident level (i.e., leaning towards republican); here, the
# relationship is stronger and the coefficients keep increasing with higher
# income levels

# create a table of proportions of ideology within each party identification
# group; we see that there is actually quite some relationship between these
# two variables (Democrats or leaning Democrats tend to be more liberal and
# vice-versa
prop.table(table(dat$partyident, dat$ideo), margin=1)

# given this, it is quite surprising/interesting that one can reach actually
# rather different conclusions as to how strongly income predicts these two
# variables

############################################################################

### 2.2: Validity and reliability

# nothing to reproduce here

############################################################################

############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-10-08
#
# Topic(s):
# - data wrangling (merging, reshaping, categorizing, ...)
# - heatmaps and intensity plots
#
# last updated: 2020-10-08

############################################################################

# restart the R session (Menu 'Session' - Restart R)

############################################################################

# how to change the position of a variable in a dataset

load("data_survey_edit.rdata")

head(dat)

# move 'lotr' variable after 'lotr6' item

if (!suppressWarnings(require(dplyr))) install.packages("dplyr")

library(dplyr)

dat <- relocate(dat, "lotr", .after = "lotr6")

head(dat)

# note: this could be done without loading any additional packages, but the
# code to do this in an elegant/flexible way gets a bit cumbersome

############################################################################

# how to replace missing values (e.g., with the mean)

dat$pss

dat$pss[is.na(dat$pss)] <- mean(dat$pss, na.rm=TRUE)

dat$pss

############################################################################

rm(list=ls())

# merge two datasets by a common id

dat1 <- data.frame(id  = c(1, 3, 4, 5, 7),
                   age = c(30, 34, 28, 21, 29),
                   sex = c("f", "m", "m", "f", "m"))

dat2 <- data.frame(id = c(1, 2, 4, 5, 6),
                   pss = c(29, 22, 19, 31, 27))

dat1
dat2

# merge so that only individuals present in both data frames are included

dat <- merge(dat1, dat2, by="id")
dat

# merge so that individuals present in the first data frame are included

dat <- merge(dat1, dat2, by="id", all.x=TRUE)
dat

# merge so that individuals present in either data frame are included

dat <- merge(dat1, dat2, by="id", all=TRUE)
dat

############################################################################

rm(list=ls())

# restructure a dataset from wide to long format

dat.wide <- data.frame(subj = 1:5,
                       y1 = c(5,3,6,7,3),
                       y2 = c(4,NA,6,5,4),
                       y3 = c(2,3,4,4,1))
dat.wide

dat.long <- reshape(dat.wide, direction="long", varying=list(2:4),
                    v.names="y", timevar="week", idvar="id")
dat.long

dat.long <- reshape(dat.wide, direction="long", varying=list(c("y1", "y2", "y3")),
                    v.names="y", timevar="week", idvar="id")
dat.long

# remove 'id' variable (a bit redundant, since we already have 'subj')

dat.long$id <- NULL

# order data frame by 'subj' variable

dat.long <- dat.long[order(dat.long$subj),]
dat.long

# set the row names to consecutive integers

rownames(dat.long) <- NULL
dat.long

# restructure a dataset from long to wide format

reshape(dat.long, direction="wide", idvar="subj", timevar="week",
        v.names = "y", sep="")

############################################################################

# per-group operations in long format datasets

dat.long

# get the mean of y for each subject

aggregate(dat.long$y, by=list(dat.long$subj), FUN=mean)

aggregate(dat.long$y, by=list(dat.long$subj), FUN=mean, na.rm=TRUE)

# can also use by() for this

by(dat.long$y, dat.long$subj, mean)

by(dat.long$y, dat.long$subj, mean, na.rm=TRUE)

# add the mean of y for each subject to the dataset

dat.long$ym <- ave(dat.long$y, dat.long$subj, FUN=mean)
dat.long

# unfortunately cannot use na.rm=TRUE with ave() directly

dat.long$ym <- ave(dat.long$y, dat.long$subj,
                   FUN = function(x) mean(x, na.rm=TRUE))
dat.long

# compute deviations of y from the subject-level means

dat.long$ymc <- dat.long$y - dat.long$ym
dat.long

############################################################################

rm(list=ls())

# dichotomizing / categorizing / collapsing

# read in data

load("data_survey_edit.rdata")

# dichotomize a quantitative variable ('median split')

dat$highpa <- ifelse(dat$posaff > median(dat$posaff), 1, 0)
head(dat)
table(dat$highpa)
table(dat$posaff, dat$highpa)

# categorize a quantitative variable

dat$pss
table(dat$pss)

dat$pss.lvl <- ifelse(dat$pss > 25, 1, 0)
table(dat$pss.lvl)

dat$pss.lvl <- cut(dat$pss, breaks=c(0, 20, 30, 40, 50))
table(dat$pss.lvl)

# (0,20] means 'just above 0 to 20 inclusive'

dat$pss.lvl <- cut(dat$pss, breaks=c(0, 20, 30, 40, 50), labels=1:4)
table(dat$pss.lvl)

dat$pss.lvl <- cut(dat$pss, breaks=c(0, 20, 30, 40, 50), labels=c("low", "medium", "high", "very high"))
table(dat$pss.lvl)

# note: the resulting variable is a 'factor'

levels(dat$pss.lvl)

# collapse some levels of a categorical variable

dat$stress <- dat$source
table(dat$stress, useNA="ifany")

dat$stress <- ifelse(dat$stress == "children",       "interpers", dat$stress)
dat$stress <- ifelse(dat$stress == "family",         "interpers", dat$stress)
dat$stress <- ifelse(dat$stress == "friendships",    "interpers", dat$stress)
dat$stress <- ifelse(dat$stress == "spouse/partner", "interpers", dat$stress)
table(dat$stress, useNA="ifany")

# a shorter way

dat$stress <- NULL
dat$stress <- ifelse(dat$source %in% c("children", "family", "friendships", "spouse/partner"), "interpers", dat$source)
table(dat$stress, useNA="ifany")

# collapse it further

dat$stress <- ifelse(dat$stress %in% c("health/illness", "lack of time", "life in general", "money/finances"), "other", dat$stress)
table(dat$stress, useNA="ifany")

############################################################################

rm(list=ls())

# change more than 2 categories into color names

dat <- data.frame(x = c("grp1", "grp2", "grp2", "grp3", NA, "grp1"), stringsAsFactors = FALSE)
dat

# since version 4.x of R, data.frame() no longer converts string variables to
# factors by default, so 'stringsAsFactors = FALSE' isn't really needed then,
# but I keep this here for backwards compatibility

# creating color names for two groups is easy with ifelse()

ifelse(dat$x == "grp1", "red", "blue")

# for more than two groups, can use nested if-else statements

ifelse(dat$x == "grp1", "red", ifelse(dat$x == "grp2", "blue", "green"))

# using as.numeric(factor())

c("red", "blue", "green")[as.numeric(factor(dat$x, levels=c("grp1", "grp2", "grp3")))]

# using with() together with {} and base code

with(dat, {
   cols <- NA
   cols[x == "grp1"] <- "red"
   cols[x == "grp2"] <- "blue"
   cols[x == "grp3"] <- "green"
   cols
})

# this also works nicely when multiple groups should receive the same color name

with(dat, {
   cols <- NA
   cols[x %in% c("grp1", "grp2")] <- "red"
   cols[x %in% "grp3"] <- "green"
   cols
})

# using switch() with sapply()

sapply(dat$x, switch,
   grp1 = "red",
   grp2 = "blue",
   grp3 = "green",
   NA
)

# using car::recode()

if (!suppressWarnings(require(car))) install.packages("car")

library(car)

recode(dat$x , "
   'grp1' = 'red';
   'grp2' = 'blue';
   'grp3' = 'green'
")

# using dplyr::case_when()

if (!suppressWarnings(require(dplyr))) install.packages("dplyr")

library(dplyr)

with(dat, case_when(
   x == "grp1" ~ "red",
   x == "grp2" ~ "blue",
   x == "grp3" ~ "green"
))

# let's use this for a real example

rm(list=ls())

# read in data

load("data_survey_edit.rdata")

dat$stress <- ifelse(dat$source %in% c("children", "family", "friendships", "spouse/partner"), "interpers", dat$source)
dat$stress <- ifelse(dat$stress %in% c("health/illness", "lack of time", "life in general", "money/finances"), "other", dat$stress)
table(dat$stress, useNA="ifany")

plot(dat$pss, dat$posaff)

plot(dat$pss, dat$posaff, pch=19)

plot(dat$pss, dat$posaff, pch=19, cex=0.5)

plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     pch=19, cex=0.5)

plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Stress", ylab="Positive Affect", pch=19, cex=0.5)

cols <- with(dat, {
   cols <- NA
   cols[stress == "interpers"] <- "red"
   cols[stress == "work"]      <- "blue"
   cols[stress == "other"]     <- "green"
   cols[is.na(stress)]         <- "yellow"
   cols
})

table(dat$stress, cols, useNA="ifany")

plot(jitter(dat$pss, amount=0.5), jitter(dat$posaff, amount=0.5),
     xlab="Pereceived Stress Scale", ylab="Positive Affect", pch=19, cex=0.5, col=cols)

legend("topright", legend = c("interpers","work","other","missing"), col = c("red","blue","green","yellow"), pch = 19, cex = 0.75)

############################################################################

rm(list=ls())

# string manipulations

id <- c("DB1965", "PL1967", "ES1975")
id

substr(id, start=3, stop=6)

as.numeric(substr(id, start=3, stop=6))

id <- c("DB1965", "PLK1967", "ES1975")
id

as.numeric(substr(id, start=3, stop=6))

nchar(id)
nchar(id)-3

substr(id, start=nchar(id)-3, stop=nchar(id))
as.numeric(substr(id, start=nchar(id)-3, stop=nchar(id)))

############################################################################

rm(list=ls())

# heatmaps
# https://en.wikipedia.org/wiki/Heat_map

# read in data

load("data_survey_edit.rdata")

# create a dataset for which we want to create a heatmap

mat <- cor(dat[c("age", "lotr", "mastery", "pss", "rses", "posaff", "negaff")],
           use = "complete.obs")
mat

dat <- as.data.frame(mat)
rownames(dat) <- NULL
dat <- cbind(var=colnames(dat), dat)
dat

# remove first column

dat[2:8]
dat[-1]

# a basic heatmap

heatmap(dat[-1])

# heatmap doesn't take data frames as input; need to supply a matrix

heatmap(as.matrix(dat[-1]))

# add row names based on 'var' variable

heatmap(as.matrix(dat[-1]), labRow=dat$var)

# switch to viridis color scheme

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var)

# reverse the order of the columns

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE)

# no dendograms

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA)

# more space for margins (default is c(5,5))

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA, margins=c(7,7))

# don't rescale values

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA, margins=c(7,7), scale="none")

# add a legend (manually)

heatmap(as.matrix(dat[-1]), col=hcl.colors(50), labRow=dat$var, revC=TRUE,
        Rowv=NA, Colv=NA, margins=c(7,14), scale="none")

legend("right", fill=hcl.colors(21), legend=round(seq(min(dat[-1]),max(dat[-1]),length=21), 2))

# install (if necessary) the 'pheatmap' package and load it

if (!suppressWarnings(require(pheatmap))) install.packages("pheatmap")

library(pheatmap)

pheatmap(as.matrix(dat[-1]), col=hcl.colors(50), labels_row=dat$var)

pheatmap(as.matrix(dat[-1]), col=hcl.colors(50),
         labels_row=dat$var, fontsize=14)

pheatmap(as.matrix(dat[-1]), col=hcl.colors(50), fontsize=14,
         labels_row=paste(" ", dat$var, "    "),
         labels_col=paste(" ", dat$var, "    "))

pheatmap(as.matrix(dat[-1]), col=hcl.colors(50), fontsize=14,
         labels_row=paste(" ", dat$var, "    "),
         labels_col=paste(" ", dat$var, "    "),
         cluster_rows=FALSE, cluster_col=FALSE)

# install (if necessary) the 'heatmaply' package and load it

if (!suppressWarnings(require(heatmaply))) install.packages("heatmaply")

library(heatmaply)

heatmaply(dat[-1])
ggheatmap(dat[-1])

############################################################################

# plot of intensity at x and y coordinates

set.seed(1234)
n <- 200
dat <- data.frame(x = runif(n), y = runif(n))
dat$intensity <- 0.55 - 1 * (dat$x-0.7)^2 + 1 * (dat$y-0.2)^3 + rnorm(n, 0, .04)
head(dat)
range(dat$intensity)

# install (if necessary) the 'akima' package and load it

if (!suppressWarnings(require(akima))) install.packages("akima")

library(akima)

# interpolate points

res <- interp(dat$x, dat$y, dat$intensity)

filled.contour(res, color=hcl.colors, xlab="x", ylab="y")

res$z[is.na(res$z)] <- 0

filled.contour(res, color=hcl.colors, xlab="x", ylab="y")

filled.contour(res, color=hcl.colors, xlab="x", ylab="y", nlevels=100)

par(fg = NA)
filled.contour(res, color=hcl.colors, xlab="x", ylab="y", nlevels=100)

dev.off()

# something really fancy

if (!suppressWarnings(require(plotly))) install.packages("plotly")

library(plotly)

plot_ly(x = ~ res$x, y = ~ res$y, z = ~ res$z) %>%
add_surface(
   contours = list(
      z = list(
         show = TRUE,
         usecolormap = TRUE,
         highlightcolor = "#ff0000",
         project = list(z=TRUE)
      )
   )
) %>%
layout(
   scene = list(
      camera = list(
         eye = list(x=1.87, y=0.88, z=-0.64)
      )
   )
)

############################################################################

rm(list=ls())

# some built-in color palettes

load("data_survey_edit.rdata")

par(mar=c(5,9,4,2))

boxplot(pss ~ marital, data=dat, col=rainbow(8),
        xlab="PSS", ylab="", pch=19, horizontal=TRUE, las=1,
        main="Perceived Stress by Marital Status", boxwex=0.6)

# some of the built-in color palettes

palette.pals()

# plot 8 colors from each palette (and from rainbow)

pals  <- palette.pals()
npals <- length(pals)
cols  <- 8

par(mar=c(2,8,2,2), las=1)
plot(NA, NA, xlim=c(1,8), ylim=c(1,npals+1), xlab="", ylab="", xaxt="n", yaxt="n")
axis(side=2, at=1:(npals+1), labels=c(pals, "Rainbow"))
for (i in 1:npals) {
   points(1:cols, rep(i,cols), pch=19, cex=4, col=palette.colors(cols, palette=pals[i]))
}
points(1:cols, rep(i+1,cols), pch=19, cex=4, col=rainbow(cols))

# now pick one you like

par(mar=c(5,9,4,2))

boxplot(pss ~ marital, data=dat, col=palette.colors(8, palette="Set 2"),
        xlab="PSS", ylab="", pch=19, horizontal=TRUE, las=1,
        main="Perceived Stress by Marital Status", boxwex=0.6)

# palette.colors() is also useful if you quickly need a number of colors for
# points or lines corresponding to different groups (e.g., in a scatterplot)

############################################################################

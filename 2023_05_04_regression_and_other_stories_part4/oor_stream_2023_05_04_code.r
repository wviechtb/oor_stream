############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-05-04
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 2.3
#
# last updated: 2023-05-18

############################################################################

### 2.3: All graphs are comparisons

## Simple scatterplots

# download the dataset
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/HealthExpenditure/data/healthdata.txt", destfile="healthdata.txt")

# read in the data
dat <- read.table("healthdata.txt", header=TRUE)

# inspect the first 6 rows of the dataset
head(dat)

# Figure 2.4
plot(dat$spending, dat$lifespan, type="n", bty="l", xlim=c(0,7500), xaxs="i",
     xlab="Health care spending (PPP US$)", ylab="Life expectancy (years)")
text(dat$spending, dat$lifespan, dat$country)

# in the book, they left out some countries from the figure for clarity, so
# let's do the same thing and remove the countries shown below from dat
dat <- dat[!is.element(dat$country,
                       c("Netherlands", "Belgium", "Germany", "Ireland",
                         "Iceland", "Greece", "Italy", "Sweden", "UK")),]

# Figure 2.4
plot(dat$spending, dat$lifespan, type="n", bty="l", xlim=c(0,7500), xaxs="i",
     xlab="Health care spending (PPP US$)", ylab="Life expectancy (years)")
text(dat$spending, dat$lifespan, dat$country)

## Displaying more information on a graph

# the dataset for this example is not available on the book website; one
# possibility would be to use digitizing software to manually extract the data
# from Figure 2.5, but this would be quite tedious given the large number of
# points (and multiple points may overlap, which we might not recognize from
# the figure)

# the book repo contains a pdf of Figure 2.5, which we can download
download.file("https://github.com/avehtari/ROS-Examples/raw/master/Redistricting/redistrict.pdf", destfile="redistrict.pdf")

# since a pdf is essentially just a text file that describes (using the
# PostScript language: https://en.wikipedia.org/wiki/PostScript) what should
# be displayed in the pdf, we can open up this pdf file with a text editor and
# copy the relevant information about the position of the points to a dataset;
# however, it takes a bit of extra effort to transform this information into a
# dataset that we make use of in R

# after doing so, we can construct the following dataset
dat <- structure(list(x = c(0.007, 0.001, 0.031, 0.031, 0.037, 0.022, 0.057,
0.039, -0.021, -0.006, -0.021, -0.019, -0.018, -0.024, -0.047, -0.047, 0.012,
-0.021, -0.025, 0.016, 0.031, 0.004, 0.019, -0.007, 0.002, -0.007, -0.005,
0.045, 0.037, 0.005, 0.048, -0.011, -0.003, -0.007, 0.014, 0.012, 0.003, 0.03,
0.014, -0.041, -0.016, -0.013, -0.021, -0.032, -0.039, -0.057, -0.023, -0.004,
0.013, 0.032, 0.053, -0.008, 0.001, 0.008, -0.002, 0.003, 0.015, -0.002,
0.002, -0.004, 0.036, 0.028, 0.003, 0.013, -0.02, -0.019, -0.025, -0.052,
-0.052, -0.022, -0.015, 0.04, 0.053, 0.066, -0.057, 0.009, 0.025, 0.063,
0.051, 0.012, 0.063, 0.058, -0.008, -0.007, -0.029, -0.016, 0.003, -0.027,
-0.022, -0.02, 0.009, 0.04, 0.066, 0.055, 0.021, 0.069, -0.026, -0.005,
-0.015, 0.012, 0.005, -0.022, -0.036, -0.06, -0.014, 0.019, 0.021, 0.047,
0.043, 0.021, 0.011, 0.041, 0.024, -0.019, -0.006, -0.07, 0.058, -0.015,
-0.016, -0.021, 0.001, -0.002, -0.006, -0.015, -0.003, 0.05, -0.05, -0.034,
0.03, -0.002, -0.049, -0.014, -0.003, 0, 0.015, 0.015, -0.006, -0.014, 0.009,
0.042, -0.019, -0.014, 0.058), y = c(-0.01, 0.019, 0.021, 0.026, 0.03, 0.046,
0.029, 0.021, -0.006, -0.011, -0.009, -0.008, -0.011, -0.037, -0.037, -0.039,
-0.002, -0.028, 0.013, 0.028, 0.021, 0.016, -0.01, 0.029, -0.002, -0.005,
0.045, 0.037, -0.002, 0.048, -0.011, -0.011, -0.012, 0.009, 0.007, -0.008,
0.025, 0.01, 0.023, -0.037, -0.002, 0.009, -0.02, -0.028, -0.046, -0.026,
-0.025, 0.006, 0.026, 0.046, 0.043, -0.006, 0.002, 0.013, 0.007, 0.019, 0.002,
-0.002, 0, 0.003, 0.019, -0.005, 0.007, -0.028, -0.027, 0.047, -0.055, -0.057,
-0.027, -0.02, 0.01, 0.048, 0.061, 0.061, -0.083, 0.012, 0.05, 0.038, 0.044,
0.05, 0.045, 0.059, 0, -0.023, -0.01, 0.009, -0.008, -0.016, -0.014, 0.003,
0.023, 0.049, 0.038, 0.025, 0.052, 0.035, -0.01, -0.006, 0.02, 0.013, -0.026,
-0.028, -0.051, -0.015, -0.02, 0.014, 0.04, 0.036, 0.051, 0.004, 0.01, 0.012,
0.001, -0.01, 0.006, -0.004, 0, 0.014, 0.005, -0.013, -0.024, -0.007, -0.019,
-0.007, -0.001, -0.014, -0.056, -0.013, 0.02, 0.005, -0.002, -0.005, -0.009,
0.029, 0.005, 0.036, 0, -0.021, -0.007, 0.005, 0.004, 0.013, -0.021), grp =
rep(1:4, times = c(111,7,10,15))), row.names = c(NA, 143L), class = "data.frame")

# note: for grp, the coding is: 1 = no redistricting, 2 = bipartisan
# redistricting, 3 = Republican redistricting, 4 = Democratic redistricting

# Figure 2.5
par(mar=c(5,10,4,10))
plot(dat$x, dat$y, lwd=3.8, xlim=c(-.08,.07), ylim=c(-.08,.07),
     pch=c(15,21,4,19)[dat$grp], cex=c(0.35,1.3,1,1.3)[dat$grp],
     xlab="Estimated partisan bias in previous election",
     ylab="Estimated partisam bias\n(adjusted for state)")
axis(side=2, at=par("usr")[1:2], las=1, tick=FALSE,
     labels=c("(favors Republicans)", "(favors Democrats)"))

# turn grp into a factor
dat$grpfac <- factor(dat$grp, levels=1:4, labels=c("no redistricting", rep("redistricting", 3)))

# create a variable that is equal to 1 for grp=4, -1 for grp=3, and 0 otherwise
dat$bipartdev <- 0
dat$bipartdev[dat$grp == 4] <-  1
dat$bipartdev[dat$grp == 3] <- -1

# fit a regression model similar to what the authors must have fitted
res <- lm(y ~ x + bipartdev + grpfac + x:grpfac, data=dat)
summary(res)

# add the regression lines for the 4 groups to the plot
xs <- seq(par("usr")[1], par("usr")[2], length=100)
pred <- predict(res, newdata=data.frame(x=xs, bipartdev=0, grpfac="no redistricting"))
lines(xs, pred)
mtext(side=4, at=tail(pred, 1), "no redistricting", las=1, line=0.4)
pred <- predict(res, newdata=data.frame(x=xs, bipartdev=0, grpfac="redistricting"))
lines(xs, pred)
mtext(side=4, at=tail(pred, 1), "bipartisan redistrict", las=1, line=0.4)
pred <- predict(res, newdata=data.frame(x=xs, bipartdev=-1, grpfac="redistricting"))
lines(xs, pred)
mtext(side=4, at=tail(pred, 1), "Rep. redistrict", las=1, line=0.4)
pred <- predict(res, newdata=data.frame(x=xs, bipartdev=1, grpfac="redistricting"))
lines(xs, pred)
mtext(side=4, at=tail(pred, 1), "Dem. redistrict", las=1, line=0.4)

# the results are not exactly identical (I had to guess what kind of model the
# authors have fitted), but the general pattern is the same

## Multiple plots

# download the data
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Names/data/allnames_clean.csv", destfile="allnames_clean.csv")

# read in the data
dat <- read.csv("allnames_clean.csv")

# inspect the first 6 lines and first 8 columns
dat[1:6, 1:8]

# only keep the boys in the dataset
dat <- dat[dat$sex == "M",]

# add the last letter of each name as a new variable to the dataset
dat$lastletter <- substr(dat$name, nchar(dat$name), nchar(dat$name))

# turn lastletter into a factor with all letters as factor levels
dat$lastletter <- factor(dat$lastletter, levels=letters)

# function to create figures like 2.6 and 2.7
plotletters <- function(x) {

   # get the total frequency of each last letter in 1906
   agg <- aggregate(x, list(letter = dat$lastletter), sum, drop=FALSE)

   # in case a letter never occurred, the resulting frequency will be NA, so
   # turn this into a 0
   agg$x[is.na(agg$x)] <- 0

   # create the barplot
   plot(1:26, agg$x/sum(agg$x)*100, type="h", lwd=6,
        ylab="Percentage of boys born", xaxt="n", bty="n", xlab="")
   par(xpd=TRUE)
   mtext(letters, side=1, at=1:26)

}

# Figure 2.6
par(mar=c(3,4,4,2))
plotletters(dat$X1906)
title("Last letter of boys' names in 1906")

# Figure 2.7
par(mfrow=c(1,2), mar=c(3,4,4,2))
plotletters(dat$X1956)
title("Last letter of boys' names in 1956")
plotletters(dat$X2006)
title("Last letter of boys' names in 2006")
par(mfrow=c(1,1), mar=c(5,4,2,2))

# get the position of variables that start with X followed by either 1 or 2
posyearvars <- grep("^X[1,2]", names(dat))

# for each variable from 'dat' that is such a variable, do the aggregation of
# the frequencies for the same last letter, and turn these frequencies into
# percentages, and return the resulting data frame; the resulting object will
# be a matrix, with each row corresponding to a letter and each column
# corresponding to a year
agg <- sapply(dat[posyearvars], function(x) {
   agg <- aggregate(x, list(letter = dat$lastletter), sum, drop=FALSE)
   agg$x[is.na(agg$x)] <- 0
   agg$x / sum(agg$x) * 100
})

# add the letters as row names
rownames(agg) <- letters

# inspect the first three columns for the resulting matrix
agg[,1:3]

# get the actual year values from the column names as a numeric variable
years <- as.numeric(substr(colnames(agg), 2, 5))
years

# Figure 2.8 (but using colors for letters d, n, y instead of line types)
plot(years, rep(NA,length(years)), ylim=c(0,40), xlab="", bty="l",
     ylab="Percentage of all boys' names that year", yaxt="n")
axis(side=2, at=c(0,20,40), labels=c("0%","20%","40%"))
apply(agg, 1, function(y) lines(years, y, col="gray70"))
lines(years, agg["d",], col="dodgerblue",  lwd=4)
lines(years, agg["n",], col="firebrick",   lwd=4)
lines(years, agg["y",], col="forestgreen", lwd=4)
text(1936, agg["d","X1936"], "D", pos=3)
text(1982, agg["n","X1982"], "N", pos=3)
text(1976, agg["y","X1976"], "Y", pos=3)
title("Last letters of boys' names")

# read in the data again
dat <- read.csv("allnames_clean.csv")

# using a data frame that only contains boys' names and the year variables, do
# the following for every year (i.e., every column): find the position of the
# top 10 most frequent names, sum up the corresponding frequencies, and turn
# this into a percentage out of the total frequency for that year
perc.top10.b <- sapply(dat[dat$sex == "M",posyearvars], function(x) {
   pos.top10 <- order(x, decreasing=TRUE)[1:10]
   sum(x[pos.top10]) / sum(x) * 100
})

# do the same for the girls' names
perc.top10.g <- sapply(dat[dat$sex == "F",posyearvars], function(x) {
   pos.top10 <- order(x, decreasing=TRUE)[1:10]
   sum(x[pos.top10]) / sum(x) * 100
})

# Figure 2.9
plot(years, rep(NA,length(years)), ylim=c(0,45), xlab="Year", bty="l",
     ylab="Percentage", yaxt="n")
axis(side=2, at=c(0,20,40), labels=c("0%","20%","40%"))
lines(years, perc.top10.b, lwd=3)
lines(years, perc.top10.g, lwd=3)
text(1930, perc.top10.b["X1930"], "Boys",  pos=3)
text(1930, perc.top10.g["X1930"], "Girls", pos=1)
title("Total popularity of top 10 names each year, by sex")

## Grids of plots

# download the data
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/1948.asc", destfile="1948.asc")
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/1950.asc", destfile="1950.asc")
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/1968.asc", destfile="1968.asc")
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/1970.asc", destfile="1970.asc")
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/1988.asc", destfile="1988.asc")
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Congress/data/1990.asc", destfile="1990.asc")

prepdat <- function(year) {

   # read in the data
   dat <- data.frame(year, read.table(paste0(year, ".asc")))

   # give the variables in the dataset proper variable names
   names(dat) <- c("year", "statecode", "unknown", "inc", "votedem", "voterep")

   # based on the statecode variable, recreate the region variable
   dat$region <- floor(dat$statecode/20) + 1

   # compute the percentage of votes for the democratic candidate
   dat$votedemperc <- with(dat, 100 * votedem / (votedem + voterep))

   # return the dataset
   return(dat)

}

plotpair <- function(year1, year2, xaxis=FALSE, showregion=FALSE) {

   # read in the data for two years using prepdat()
   dat1 <- prepdat(year1)
   dat2 <- prepdat(year2)

   # combine the relevant variables from the two datasets into a single dataset
   dat <- data.frame(region = dat1$region,
                     inc    = abs(dat1$inc),
                     dvote1 = dat1$votedemperc,
                     dvote2 = dat2$votedemperc)

   # create the contested and voteswing variables
   dat$contested <- with(dat, abs(dvote1 - 50) < 30 & abs(dvote2 - 50) < 30)
   dat$voteswing <- with(dat, dvote2 - dvote1)

   # keep only rows where inc is -1, 0, or 1 and when it is a contested election
   dat <- dat[dat$inc %in% c(-1, 0, 1) & dat$contested,]

   # region names
   regions <- c("Northeast", "Midwest", "South", "West")

   # for each region, create a scatterplot
   for (i in 1:4) {
      plot(NA, NA, xlim=c(20,80), ylim=c(-35,30), xaxt="n", yaxt="n", bty="n", xlab="", ylab="")
      abline(h=0)
      with(dat[dat$inc == 1 & dat$region==i,], points(dvote1, voteswing, pch=19, col="gray60", cex=0.5))
      with(dat[dat$inc == 0 & dat$region==i,], points(dvote1, voteswing, pch=19, col="black",  cex=0.7))
      if (xaxis) {
         axis(side=1, at=c(25,50,75), labels=c("25%","50%","75%"))
         mtext(side=1, "Dem. vote in election 1", line=2.5, cex=0.7)
      }
      axis(side=2, at=c(-25,0,25), labels=c("-25%","0%","25%"))
      if (showregion)
         mtext(side=3, regions[i], line=2)
      if (i == 1) {
         mtext(side=2, paste0(year1, "\nto\n", year2), line=7, las=1, adj=0.5)
         mtext(side=2, "Vote swing", line=3, cex=0.7)
      }
   }

}

# Figure 2.10
par(mfrow=c(3,4), mar=c(1,2,0,2), oma=c(3,8,5,0))
plotpair(1948, 1950, showregion=TRUE)
plotpair(1968, 1970)
plotpair(1988, 1990, xaxis=TRUE)

############################################################################

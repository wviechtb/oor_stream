############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-05-18
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 2.4
#
# last updated: 2023-05-23

############################################################################

### 2.4: Data and adjustment: trends in mortality rates

# download the datasets
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/AgePeriodCohort/data/white_nonhisp_death_rates_from_1999_to_2013.txt", destfile="white_nonhisp_death_rates_from_1999_to_2013.txt")
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/AgePeriodCohort/data/white_nonhisp_death_rates_from_1999_to_2013_by_sex.txt", destfile="white_nonhisp_death_rates_from_1999_to_2013_by_sex.txt")

# read in the data
dat <- read.table("white_nonhisp_death_rates_from_1999_to_2013.txt", header=TRUE)

# inspect the first 20 rows of the dataset
head(dat, 20)

# compute the total number of deaths and the population size in each year
# between 1999 and 2013 for people between 45 and 54 years of age
dat <- aggregate(dat[c("Deaths", "Population")], list(Year = dat$Year), sum)

# inspect the aggregated dataset
dat

# compute the raw death rate for each year
dat$Rates <- with(dat, Deaths / Population)

# Figure 2.11a
par(mar=c(3,5,4,2))
plot(dat$Year, dat$Rates, type="n", bty="l",
     xlab="", ylab="Death rate among non-Hisp whites 45-54",
     main="Raw death rates\nfor 45-54-year-old non-Hisp whites")
grid()
lines(dat$Year, dat$Rates, lwd=2)

# make a copy of dat for Figure 2.11c
sav <- dat

# read in the data again
dat <- read.table("white_nonhisp_death_rates_from_1999_to_2013.txt", header=TRUE)

# compute the mean age within the 45 to 54 age group in the years 1999 to 2013
years <- sort(unique(dat$Year))
mage <- sapply(years, function(year) {
   weighted.mean(sort(unique(dat$Age)), dat[dat$Year == year, "Population"])
})

# Figure 2.11b
par(mar=c(3,5,4,2))
plot(years, mage, type="n", bty="l",
     xlab="", ylab="Avg age among non-Hisp whites 45-54",
     main="But the average age in this group is going up!")
grid()
lines(years, mage, lwd=2)

# read in the data again
dat <- read.table("white_nonhisp_death_rates_from_1999_to_2013.txt", header=TRUE)

# extract the 2013 mortality rates
rates2013 <- with(dat[dat$Year == 2013,], Deaths / Population)

# compute the mortality rate for people within the 45 to 54 age group in each
# year assuming that the 2013 mortality rates apply to all the years
rates.adj <- sapply(years, function(year) {
   weighted.mean(rates2013, dat$Population[dat$Year == year])
})

# Figure 2.11c
par(mar=c(3,5,4,2))
plot(sav$Year, sav$Rates, type="n", bty="l",
     xlab="", ylab="Death rate among non-Hisp whites 45-54",
     main="The trend in raw death rates since 2005\ncan be explained by age-aggregation bias")
grid()
lines(sav$Year, sav$Rates, lwd=2)
lines(years, rates.adj, lwd=2, col="red", lty="dotted")
text(2001, sav$Rates[sav$Year == 2001], "Raw death rate", pos=4)
text(2001, rates.adj[sav$Year == 2001], "Expected just from\nage shift", pos=3)

# take the unweighted average of the death rates in each year (this implies
# that we are assuming an equal number of people for each age)
rates.avg <- sapply(years, function(year) {
   mean(dat$Deaths[dat$Year == year] / dat$Population[dat$Year == year])
})

# rescale the rates relative to 1999
rates.avg <- rates.avg / rates.avg[1]

# Figure 2.12a
par(mar=c(3,5,4,2))
plot(years, rates.avg, type="n", bty="l",
     xlab="", ylab="Age-adj death rate, relative to 1999",
     main="Trend in age-adjusted death rate\nfor 45-54-year-old non-Hisp whites")
grid()
lines(years, rates.avg, lwd=2)

# use the 1999 and 2013 population sizes to compute adjusted death rates
rates.avg.1999 <- sapply(years, function(year) {
   weighted.mean(dat$Deaths[dat$Year == year] / dat$Population[dat$Year == year], dat$Population[dat$Year == 1999])
})
rates.avg.2013 <- sapply(years, function(year) {
   weighted.mean(dat$Deaths[dat$Year == year] / dat$Population[dat$Year == year], dat$Population[dat$Year == 2013])
})

# rescale the rates relative to 1999
rates.avg.1999 <- rates.avg.1999 / rates.avg.1999[1]
rates.avg.2013 <- rates.avg.2013 / rates.avg.2013[1]

# Figure 2.12b
par(mar=c(3,5,4,2))
plot(years, rates.avg, type="n", bty="l", ylim=c(1,1.065),
     xlab="", ylab="Age-adj death rate, relative to 1999",
     main="It doesn't matter too much what age adjustment\nto use for 45-54-year-old non-Disp whites")
grid()
lines(years, rates.avg, lwd=2)
lines(years, rates.avg.1999, lwd=2, lty="dashed")
lines(years, rates.avg.2013, lwd=2, lty="dotted")
text(2003, rates.avg.1999[years == 2003], "Using 1999\nage dist", pos=3)
text(2004, rates.avg.2013[years == 2004], "Using 2013\nage dist", pos=1)

# read in the data
dat <- read.table("white_nonhisp_death_rates_from_1999_to_2013_by_sex.txt", header=TRUE)

# select rows for age 45 to 54
dat <- dat[dat$Age %in% 45:54,]

# create two subsets for females and males
dat.f <- dat[dat$Male == 0,]
dat.m <- dat[dat$Male == 1,]

# compute age-adjusted death rates for females and males
rates.avg.f <- sapply(years, function(year) {
   mean(dat.f$Deaths[dat.f$Year == year] / dat.f$Population[dat.f$Year == year])
})
rates.avg.m <- sapply(years, function(year) {
   mean(dat.m$Deaths[dat.m$Year == year] / dat.m$Population[dat.m$Year == year])
})

# rescale the rates relative to 1999
rates.avg.f <- rates.avg.f / rates.avg.f[1]
rates.avg.m <- rates.avg.m / rates.avg.m[1]

# Figure 2.12c
par(mar=c(3,5,4,2))
plot(years, rates.avg.f, type="n", bty="l", ylim=c(1,1.10),
     xlab="", ylab="Age-adj death rate, relative to 1999",
     main="Age-adjusted death rates for non-Hispanic whites\naged 45-54: Trends for women and men")
grid()
lines(years, rates.avg.f, lwd=2, col="red")
lines(years, rates.avg.m, lwd=2, col="blue")
text(2010, rates.avg.f[years == 2010], "Women", pos=1)
text(2010, rates.avg.m[years == 2010], "Men", pos=1)

# note: we will skip trying to reproduce Figure 2.13

############################################################################

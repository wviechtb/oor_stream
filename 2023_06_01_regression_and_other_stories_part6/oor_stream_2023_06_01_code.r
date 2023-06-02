############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-06-01
#
# Topic(s):
# - Regression and Other Stories (https://avehtari.github.io/ROS-Examples/)
# - Section(s): 3.1 - 3.4
#
# last updated: 2023-06-02

############################################################################

### 3.1: Weighted averages

# weighted average of the ages of people in the US, Mexico, and Canada
(310000000 * 36.8 + 112000000 * 26.7 + 34000000 * 40.7) / (310000000 + 112000000 + 34000000)

# create vectors with the mean age values and population sizes
mean.age <- c(36.8, 26.7, 40.7)
pop.size <- c(310000000, 112000000, 34000000)

# use weighted.mean() to obtain the weighted average
weighted.mean(mean.age, pop.size)

# the weights that are used for computing the weighted average
pop.size / sum(pop.size)

# another example: prevalence of hypertension in the US, Mexico, and Canada
# (based on some very quick googling)
prevalence <- c(0.47, 0.18, 0.25)
weighted.mean(prevalence, pop.size)

# so among all 456 million people in North America, about 38% have hypertension

############################################################################

### 3.2: Vectors and matrices

# download the dataset for this example from the book website
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/ElectionsEconomy/data/hibbs.dat", destfile="hibbs.dat")

# read in the data
dat <- read.table("hibbs.dat", header=TRUE)

# inspect the dataset
dat

# fit a regression model predicting the vote variable from the growth variable
res <- lm(vote ~ growth, data=dat)
summary(res)

# show only the regression coefficients rounded to one decimal place
round(coef(res), 1)

# note: in the book, the coefficients shown are based on the Bayesian model
# that was fitted in chapter 1; here, let's stick to the non-Bayesian results

# so, the model says: predicted vote = 46.2 + 3.1 * growth

# predicted vote when growth is equal to -1
predict(res, newdata=data.frame(growth=-1))

# predicted vote when growth is equal to 0
predict(res, newdata=data.frame(growth=0))

# predicted vote when growth is equal to 3
predict(res, newdata=data.frame(growth=3))

# we can do this in a single line of code and include even more growth values
newdat <- data.frame(growth=-1:4)
cbind(newdat, pred=predict(res, newdata=newdat))

# create the X matrix
X <- as.matrix(cbind(intercept=1, newdat))
X

# create the column vector with the regression coefficients
b <- cbind(coef(res))
b

# multiply to get the predicted values manually
X %*% b

############################################################################

### 3.3: Graphing a line

# download the dataset corresponding to the example
download.file("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Mile/data/mile2.txt", destfile="mile2.txt")

# see Wikipedia for more details on this sporting event and the dataset
# https://en.wikipedia.org/wiki/Mile_run_world_record_progression

# read in the data
dat <- read.table("mile2.txt", header=TRUE)
dat

# create a variable that combines year and month (as a fraction of 12 months)
dat$year.month <- with(dat, yr + month / 12)

# create a variable that combines the min and sec variable into total seconds
dat$seconds <- with(dat, min*60 + sec)

# inspect the dataset
dat

# fit a regression model predicting seconds from year.month
res <- lm(seconds ~ year.month, data=dat)
summary(res)

# plot of the data
plot(seconds ~ year.month, data=dat, xlim=c(1900,2001), ylim=c(215,265),
     xaxs="i", bty="l", pch=21, bg="gray", type="o", lty="dotted",
     xlab="Year", ylab="Time (seconds)")

# add the regression line from the model to the figure
abline(res, lwd=3)

# add the equation for the line
text(1950, 242, pos=4, expression(y == 1007 - 0.393*x))

# center year.month at 1900 to make the intercept more meaningful
res <- lm(seconds ~ I(year.month-1900), data=dat)
summary(res)

# now the intercept refers to the predicted time in 1900

############################################################################

### 3.4: Exponential and power-law growth and decline;
###      logarithmic and log-log relationships

# exponential growth example
curve(1.5*10^9 * 2^((x-1900)/50), from=1900, to=2000, lwd=3,
      xlab="Year", ylab="Population Size")

# draw the same line again, but now in the A*exp(b*x) form
curve(1.5*10^9 * exp(log(2)/50 * (x-1900)), from=1900, to=2000, lwd=3,
      xlab="Year", ylab="Population Size", add=TRUE, col="red")

# let's see what is really going in the world; let's get the data from Wikipedia
# https://en.wikipedia.org/wiki/World_population#Annual_population_growth

# unfortunately, this page only provides data starting at 1951; also, we will
# only use the data up to 1970 (since after that the growth appears to be more
# or less linear, which kind of defeats the purpose of this little exercise)
dat <- structure(list(year = 1951:1970, size = c(2584034261, 2630861562,
2677608960, 2724846741, 2773019936, 2822443282, 2873306090, 2925686705,
2979576185, 3034949748, 3091843507, 3150420795, 3211001009, 3273978338,
3339583597, 3407922630, 3478769962, 3551599127, 3625680627, 3700437046)),
row.names = c(NA, 20L), class = "data.frame")

# examine the dataset
dat

# plot of year versus size
plot(size ~ year, data=dat, pch=21, bg="gray", type="o", lty="dotted")

# if there is exponential growth, then the relationship between log(size) and
# year should be roughly linear and that is indeed the case
plot(log(size) ~ year, data=dat, pch=21, bg="gray", type="o", lty="dotted")

# fit the regression model (on the log scale)
res <- lm(log(size) ~ year, data=dat)
summary(res)

# the increase in population size per 1-year increase
exp(1.882e-02)

# so we see that between 1951 and 1970, the world population increased by a
# factor of 1.02 (or roughly 2%) every year

# the increase in population size per 10-year increase
exp(1.882e-02 * 10)

# for every 10 years, the population size increased by a factor of 1.21 (or 21%)

# to see why exp(b) gives the factor by which the population size increases
# for a one-year increase, note that the model is given by this:
#
# log(size) = a + b*year
#
# so to compute how much log(size) changes when year goes up by one unit, we
# can compute:
#
# log(size | year+1) - log(size | year) = (a + b*(year+1)) - (a + b*year)
#                                       = a + b*year + b - a - b*year
#                                       =              b
#
# since:
#
# log(size | year+1) - log(size | year) = log((size | year+1) / (size | year))
#
# then when we exponentiate both sides of the equation, we get:
#
# (size | year+1) / (size | year) = exp(b)

# data for Figure 3.4
dat <- structure(list(bm = c(0.0211, 0.0224, 0.2952, 0.4274, 0.436, 0.7261,
2.8292, 2.2479, 2.7456, 3.1582, 2.9447, 3.1582, 4.5722, 5.2593, 11.134,
15.1803, 19.8857, 39.2519, 39.6464, 47.9424, 62.1779, 68.0335, 135.6394,
247.1511, 492.749, 502.7032, 614.0031, 3714.5024), rate = c(0.1703, 0.2952,
1.4049, 1.6161, 1.9937, 2.3632, 3.819, 6.0496, 5.9895, 6.4883, 7.5383, 8.8463,
10.9135, 14.0132, 17.8143, 23.8075, 26.8429, 39.6464, 51.4186, 57.9743,
62.8028, 82.2695, 113.2956, 270.4264, 290.0345, 403.4288, 561.1566,
2100.6456), names = c("Mouse", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
NA, NA, NA, NA, NA, NA, NA, NA, "Man", NA, NA, NA, NA, NA, "Elephant")),
row.names = c(NA, -28L), class = "data.frame")

# Figure 3.4
plot(log(dat$bm), log(dat$rate), pch=19, xaxt="n", yaxt="n", bty="l",
     xlim=log(c(0.01,10000)), ylim=log(c(0.1,2000)),
     xlab="Body mass (kg)", ylab="Metabolic rate (watt)")
pos <- c(0.01,0.1,1,10,100,1000,10000)
axis(side=1, at=log(pos), label=pos)
pos <- c(0.1,1,10,100,1000)
axis(side=2, at=log(pos), label=pos, las=2)
text(log(dat$bm)[1],  log(dat$rate)[1],  dat$name[1],  pos=4)
text(log(dat$bm)[22], log(dat$rate)[22], dat$name[22], pos=2)
text(log(dat$bm)[28], log(dat$rate)[28], dat$name[28], pos=2)

# fit the log-log regression model
res <- lm(log(rate) ~ log(bm), data=dat)
summary(res)

# add the regression line to the plot
abline(res, lwd=3)

# note: the intercept and slope given in the book (1.4 and 0.74, respectively)
# are slightly different from what we find above (1.2 and 0.76, respectively),
# but are fairly close and the plot and fitted line essentially look the same

# Figure 3.5a (but with all species)
plot(log(dat$bm), log(dat$rate), pch=19, bty="l",
     xlab="log(body mass in kg)", ylab="log(metabolic rate in watts)")
abline(res, lwd=3)
text(log(dat$bm)[1],  log(dat$rate)[1],  dat$name[1],  pos=4)
text(log(dat$bm)[22], log(dat$rate)[22], dat$name[22], pos=2)
text(log(dat$bm)[28], log(dat$rate)[28], dat$name[28], pos=2)

# Figure 3.5b (but with all species)
plot(dat$bm, dat$rate, pch=19, bty="l",
     xlab="body mass in kg", ylab="metabolic rate in watts")
text(dat$bm[1],  dat$rate[1],  dat$name[1],  pos=4)
text(dat$bm[22], dat$rate[22], dat$name[22], pos=3)
text(dat$bm[28], dat$rate[28], dat$name[28], pos=2)

# compute the predicted log(rate) as a function of log(bm) for all values of
# bm between 1 and 4000
newdat <- data.frame(bm=1:4000)
pred <- predict(res, newdata=newdat)

# add the line for the relationship between bm and rate to the plot
lines(newdat$bm, exp(pred), lwd=3)

# to express the model results in terms of y (i.e., the rate) directly, we
# exponentiate the intercept estimate
exp(coef(res)[1])

# and then we can write: rate = 3.23 * bm^0.76

# draw the line again based on this equation (but use unrounded values)
lines(newdat$bm, 3.232339 * newdat$bm^0.75511, col="red", lwd=3)

############################################################################

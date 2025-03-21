############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-03-20
#
# Topic(s):
# - the tinyplot package
#
# last updated: 2025-03-21+

############################################################################

### tinyplot package

# a "lightweight extension of the base R graphics system, with support for
# automatic legends, facets, themes, and various other enhancements."
#
# relevant links:
# - https://cran.r-project.org/package=tinyplot
# - https://grantmcdermott.com/tinyplot/

# install the tinyplot package from CRAN
#install.packages("tinyplot")

# actually, we will install the version that is available on the R-universe
# website, since it is slightly newer
install.packages("tinyplot", repos="https://grantmcdermott.r-universe.dev")

# install the palmerpenguins packages
install.packages("palmerpenguins")

# load the packages
library(tinyplot)
library(palmerpenguins)

############################################################################

# copy the penguins dataset to dat (and make it a regular data frame)
dat <- data.frame(penguins)

# inspect the first 6 rows
head(dat)

# frequency table of the species variable
table(dat$species)

############################################################################

# a default scatterplot of two variables (y-axis: bill_length_mm, x-axis: flipper_length_mm)
plot(bill_length_mm ~ flipper_length_mm, data=dat)

# a bit of customization to make the plot look nicer
plot(bill_length_mm ~ flipper_length_mm, data=dat, pch=21, bg="gray",
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l", las=1)

############################################################################

# say we want to recreate the figure shown here: https://allisonhorst.github.io/palmerpenguins/

# set up the plot (but don't actually show the points)
plot(bill_length_mm ~ flipper_length_mm, data=dat, type="n",
     xlab="Flipper length (mm)", ylab="Bill length (mm)", bty="l", las=1)

# add a grid
grid()

# save the species levels as a vector
species <- levels(dat$species)

# vectors with the colors and the same colors with some transparency added
cols <- c("darkorange","purple","cyan4")
cols.t <- adjustcolor(cols, alpha.f=0.7)

# now add the points, with different plotting symbols and colors for the three
# species (Adelie = darkorange, Chinstrap = purple, Gentoo = cyan4)
points(bill_length_mm ~ flipper_length_mm, data=penguins,
       pch=c(19,17,15)[species], col=cols.t[species], cex=1.2)

# fit the model that allows for different intercepts and slopes for the species
res <- lm(bill_length_mm ~ 0 + species + flipper_length_mm:species, data=dat)
summary(res)

# add the regression lines to the plot
xrange <- by(dat$flipper_length_mm, dat$species, range, na.rm=TRUE)
for (i in 1:length(species)) {
   pred <- predict(res, newdata=data.frame(species=species[i], flipper_length_mm=xrange[[i]]))
   lines(xrange[[i]], pred, lwd=5, col=cols[i])
}

# add a legend
legend("bottomright", pch=c(19,17,15), lwd=5, col=cols.t, legend=species,
       cex=0.9, pt.cex=1.2, bty="n", title="Penguin species")

# add text at the top
mtext("Flipper and bill length", side=3, adj=0, line=2.5, cex=1.2)
mtext("Dimensions for Adelie, Chinstrap, and Gentoo Penguins at Palmer Station LTER", side=3, adj=0, line=1.2, cex=1)

############################################################################

# now let's try to simplify the above by making use of tinyplot functionality

# generally the syntax of tinyplot() (or plt() for short) is like the plot() syntax
plt(bill_length_mm ~ flipper_length_mm, data=dat, pch=21, bg="gray",
    xlab="Flipper length (mm)", ylab="Bill length (mm)")

# however, there are some subtle differences; bty="l" and las=1 do not work
# within the plt() call as above; we can get around this by setting the 'las'
# value first with par(), suppressing the box altogether with frame=FALSE, and
# if we like adding the L box back with box()
par(las=1)
plt(bill_length_mm ~ flipper_length_mm, data=dat, pch=21, bg="gray",
    xlab="Flipper length (mm)", ylab="Bill length (mm)", frame=FALSE)
box(bty="l")

# tinyplot supports specifying a grouping variable as part of the formula
plt(bill_length_mm ~ flipper_length_mm | species, data=dat)

# using themes, we can also change the overall look of the plot, with other
# defaults (e.g., the minimal theme uses horizontal axis labels by default)
#
# see here for the look of the built-in themes:
# https://grantmcdermott.com/tinyplot/vignettes/themes.html

# recreate the same figure as above using tinyplot
tinytheme("classic", palette.qualitative=c("darkorange","purple","cyan4"))
plt(bill_length_mm ~ flipper_length_mm | species, data=dat,
    xlab="Flipper length (mm)", ylab="Bill length (mm)",
    cex=1.2, alpha=0.7, pch=c(19,17,15), grid=TRUE,
    legend=legend("bottomright", title="Penguin species", lwd=5),
    main="Flipper and bill length",
    sub="Dimensions for Adelie, Chinstrap, and Gentoo Penguins at Palmer Station LTER")
plt_add(type="lm", se=FALSE, lwd=5, alpha=1)

# much shorter / easier code

# reset the theme to the default
tinytheme()

############################################################################

# if the variable after | is a quantitative variable, then it will be used as
# a gradient for coloring the points
plt(bill_length_mm ~ flipper_length_mm | body_mass_g, data=dat, pch=19)

# if we specify a facet variable, then we get a plot per facet level
plt(bill_length_mm ~ flipper_length_mm, data=dat, pch=19, facet = ~ species)

# make this look nicer and add regression lines per group
tinytheme("clean")
plt(bill_length_mm ~ flipper_length_mm, data=dat, facet = ~ species,
    xlab="Flipper length (mm)", ylab="Bill length (mm)", pch=21, bg="gray",
    main="Flipper and bill length",
    sub="Dimensions for Adelie, Chinstrap, and Gentoo Penguins at Palmer Station LTER")
plt_add(type="lm", se=FALSE, lwd=5, col="gray40")

# also distinguish the male and female penguins
plt(bill_length_mm ~ flipper_length_mm | sex, data=dat, facet = ~ species,
    xlab="Flipper length (mm)", ylab="Bill length (mm)", pch=19,
    main="Flipper and bill length",
    sub="Dimensions for Adelie, Chinstrap, and Gentoo Penguins at Palmer Station LTER",
    legend=legend("topleft", bty="o"))
plt_add(type="lm", se=FALSE, lwd=5)

############################################################################

# let's work a bit with the airquality dataset
dat <- airquality

# inspect the first 6 rows
head(dat)

# turn the Month variable into a factor with proper (shortened) month names
dat$Month <- factor(month.abb[dat$Month], levels=month.abb[5:9])
head(dat)

# line plot of temperature over days for the different months (note: we can
# also make adjustments to the theme defaults via tinytheme())
tinytheme("clean", bty="l", grid=FALSE)
plt(Temp ~ Day | Month, data=dat, type="l", lwd=2)

# instead of using colors for the months, we can use different line types
plt(Temp ~ Day | Month, data=dat, type="l", lwd=2,
    col="black", lty=c("solid","dashed","dotted","dotdash","longdash"))

# but specifying the different line types this way is tedious; tinyplot
# provides a special keyword ("by") for selecting different line types per group
plt(Temp ~ Day | Month, data=dat, type="l", lwd=2, col="black", lty="by")

# sidenote: the "by" keyword also works for other arguments
plt(Temp ~ Day | Month, data=dat, type="l", lwd=2)
plt_add(type="p", pch=21, col="black", bg="by", lwd=1)

# in principle, this should also work, but the points in the plot are not
# filled in (maybe this is a small bug?!?)
plt(Temp ~ Day | Month, data=dat, type="o", lwd=2, pch=21, fill="by")

# but this works (without the black outline of the points)
plt(Temp ~ Day | Month, data=dat, type="o", lwd=2, pch=19)

# in principle, if the grouping variable is an ordered factor, then a
# sequential ("viridis") palette should be used automatically, but this does
# not seem to work correctly at the moment
plt(Temp ~ Day | ordered(Month), data=dat, pch=19)

# we could work around this here by manually specifying the desired palette
plt(Temp ~ Day | Month, data=dat, type="o", lwd=2, pch=19, palette="viridis")

############################################################################

# let's explore some more plot types

# box plots for different months
plt(Temp ~ Month | Month, data=dat, type="boxplot", legend=FALSE)

# plot kernel density estimates for the different months
plt(~ Temp | Month, data=dat, type="density", fill="by", legend="topright")

# ridgeline plot of the densities
tinytheme("ridge2")
plt(Month ~ Temp | Month, data=dat, type="ridge", legend=FALSE)
plt(Month ~ Temp, data=dat, type="ridge", gradient=TRUE, scale=0.8,
    palette=hcl.colors(n=100, palette="temps", rev=TRUE))

# plot smoothed (local regression) curves
tinytheme("clean", bty="l")
plt(Temp ~ Day | Month, data=dat, type="loess", lwd=2)

# if we don't want the CI regions, we can suppress them with se=FALSE
plt(Temp ~ Day | Month, data=dat, type="loess", lwd=3, se=FALSE)

############################################################################

# let's try a plot with many 'small multiples'

library(nlme)

dat <- Orthodont

# plot distance as a function of age for the different subjects
tinytheme("clean")
plt(distance ~ age, data=dat, pch=19, facet = ~ Subject, frame=FALSE, ylim=c(15,35))

# sidenote: the lattice package is a nice alternative for creating such plots
# that uses the space even more effectively
library(lattice)
xyplot(distance ~ age | Subject, data=dat, pch=19, ylim=c(10,40),
       panel = function(x, y, ...) {
         panel.grid(h = -1, v = -1, lty="dotted")
         panel.xyplot(x, y, ...)
       })

############################################################################

# an attempt to recreate the plot above using base R graphics ... :/

tinytheme("default")

par(mfrow=c(5,6), mar=c(0,0,0,0), oma=c(3,3,3,3), las=1)

subs <- unique(as.character(dat$Subject))

k <- 0
for (i in 1:5) {
   for (j in 1:6) {
      k <- k + 1
      if (k <= length(subs)) {
         plot(NA, xlab="", ylab="", xaxt="n", yaxt="n",
              xlim=range(dat$age), ylim=c(15,38))
         grid()
         points(distance ~ age, data=dat, subset=Subject == subs[k],
                pch=19, cex=1.2, col="dodgerblue3")
         usr <- par("usr")
         rect(min(dat$age)-1, 35, max(dat$age)+1, 35+5, col="lightgray", border="black")
         text((min(dat$age)+max(dat$age))/2, 37, subs[k], font=2, cex=1.3)
         box()
         if (i %% 2 == 0 && j == 6)
            axis(side=4, lwd=0, lwd.ticks=1)
         if (i %% 2 != 0 && j == 1)
            axis(side=2, lwd=0, lwd.ticks=1)
         if (j %% 2 == 0 && i == 1)
            axis(side=3, lwd=0, lwd.ticks=1, outer=TRUE)
         if (j %% 2 != 0 && i == 5)
            axis(side=1, lwd=0, lwd.ticks=1)
      }
   }
}

graphics.off()

############################################################################

# fit a regression model per subject
res <- lmList(distance ~ age | Subject, data=dat, pool=FALSE)

# extract the slope estimates and corresponding 95% CI bounds
tab <- data.frame(coef(res)[,"age",drop=FALSE], do.call(rbind, confint(res, parm="age")))
names(tab) <- c("coef", "ci.lb", "ci.ub")
tab$subject <- rownames(tab)
tab$sex <- ifelse(startsWith(tab$subject, "M"), "male", "female")
tab <- sort_by(tab, ~ subject)
tab

# plot the coefficients with CI bounds for the male and female subjects
tinytheme("clean", las=2)
with(tab, plt(x=subject, y=coef, facet=sex, ymin=ci.lb, ymax=ci.ub,
              type="pointrange", xlab="", ylab="", facet.args=list(free=TRUE)))

# or we can flip the axes around (need to increase the left margin a bit)
tinytheme("clean", fmar=c(1,3,1,1))
with(tab, plt(x=subject, y=coef, facet=sex, ymin=ci.lb, ymax=ci.ub,
              type="pointrange", xlab="", ylab="", facet.args=list(free=TRUE),
              flip=TRUE))

############################################################################

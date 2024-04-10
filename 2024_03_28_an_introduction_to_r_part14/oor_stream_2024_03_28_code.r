############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-03-28
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 12.4 - 12.7
#
# last updated: 2024-04-10

############################################################################

## 12.4: Using graphics parameters

# copy the mtcars dataset to dat and inspect the dataset
dat <- mtcars
dat

# scatterplot with default settings
plot(mpg ~ hp, data=dat)

# change the plotting symbol to a filled circle, use gray as the background
# color of the circle, use only an L-shape around the plot, use horizontal
# axis labels, and add proper x-axis and y-axis labels
plot(mpg ~ hp, data=dat, pch=21, bg="gray", bty="l", las=1,
     xlab="Horsepower", ylab="Miles per Gallon")

# by setting the graphics parameters within the plot() function call, we only
# temporarily change the defaults, so when we create a new graph, it will
# revert to the defaults
plot(mpg ~ wt, data=dat, xlab="Weight", ylab="Miles per Gallon")

# 12.4.1: Permanent changes: The par() function

# list all graphics parameters (with their current values)
par()

# list only some selected graphics parameters
par(c("col", "lty"))

# set some graphics parameters
par(bty="l", las=1, pch=21)

# redraw the two graphs from above
plot(mpg ~ hp, data=dat, bg="gray", xlab="Horsepower", ylab="Miles per Gallon")
plot(mpg ~ wt, data=dat, bg="gray", xlab="Weight", ylab="Miles per Gallon")

# why don't we also use 'bg' above with par() to set the background color of
# the points? it turns out that the graphical parameter 'bg' is for setting
# the background color of the plotting device and that plot() *also* has a
# special argument called 'bg' for setting the background color of points (so
# these two things - the graphical parameter and the argument of plot() - are
# different things)

# to illustrate this distinction
par(bty="l", las=1, pch=21, bg="gray")
plot(mpg ~ hp, data=dat, bg="dodgerblue", xlab="Horsepower", ylab="Miles per Gallon")

# see help(par) for more details on all of the graphical parameters

# as noted there, some graphical parameters can only be set via par() and not
# within the call to the plotting function; a good example of this is 'mfrow'
# for splitting up the plotting device into multiple rows and/or columns
par(mfrow=c(1,2))
plot(mpg ~ hp, data=dat, bg="dodgerblue", xlab="Horsepower", ylab="Miles per Gallon")
plot(mpg ~ wt, data=dat, bg="dodgerblue", xlab="Weight", ylab="Miles per Gallon")

# note that the adjusted graphics parameters stay in effect for additional
# plots drawn on the same plotting device
plot(mpg ~ factor(cyl), data=dat)

# if we did not want two subplots, we set mfrow back to a single row/column
par(mfrow=c(1,1))
plot(mpg ~ factor(cyl), data=dat)

# the easiest way to reset the graphics parameters to their defaults is to
# close the plotting device (i.e., close the window) or via dev.off()
dev.off()

# now if we call a high-level plotting function that automatically opens a new
# plotting device (if none is already open), then the defaults are used again
plot(mpg ~ factor(cyl), data=dat)

# in RStudio, we can also click on the broom symbol above the plot region to
# close the plotting device and hence reset adjusted graphical parameters

# because one can easily reset the graphics parameters via dev.off(), the
# workflow using oldpar <- par(...) and then par(oldpar) is not something that
# is really needed in interactive analysis sessions (this can be relevant when
# writing plotting functions that adjust graphical parameters to reset things
# back when the function finishes doing its job)

# 12.4.2: Temporary changes: Arguments to graphics functions

# changing graphics parameters directly within the function call was
# illustrated already above; generally, this is also what I do in my workflow
# (except of course for graphics parameters that can only be set via par())

# as noted above, there can be inconsistencies when doing so, as we also saw
# above with 'bg' (with par(), it sets the background color of the plotting
# device, within plot(), it sets the background color of filled plotting
# symbols)

############################################################################

## 12.5: Graphics parameters list

# 12.5.1: Graphical elements

# illustrate the pch argument
plot(1, 1, pch="+")

# not sure why the manual says that the symbol will be 'slightly above or
# below the appropriate position'; in this example, the + is exactly at the
# intersection of (1,1)
abline(h=1, lty="dotted")
abline(v=1, lty="dotted")

# can also use numbers for pch
plot(1, 1, pch=19)

# see help(points) for details on what the numbers mean
help(points)

# with the filled symbols, we can specify the background color
plot(1, 1, pch=21, bg="gray")

# and the border color
plot(1, 1, pch=21, bg="gray", col="dodgerblue")

# illustrate different line types
abline(h=0.8, lty=1) # solid line (default)
abline(h=0.9, lty=2) # dashed line
abline(h=1.1, lty=3) # dotted line
abline(h=1.2, lty=4) # dotdash line

# can also write out the line types ("solid", "dashed", "dotted", "dotdash")
# and there are even more types -- see again help(par)

# illustrate differences in line width (lwd does not have to be an integer)
plot(1, 1, type="n")
abline(h=0.8, lwd=1)
abline(h=0.9, lwd=2)
abline(h=1.0, lwd=3)
abline(h=1.1, lwd=4)
abline(h=1.2, lwd=5.5)

# illustrate different colors
plot(1, 1, pch=19, col="red")
points(1.2, 1.2, pch=19, col="blue")

# different colors for the axes and axes labels
plot(1, 1, pch=19, xlab="x-Axis Label", ylab="y-Axis Label", main="Title",
     col.axis="dodgerblue", col.lab="gray", col.main="red")

# illustrate the font argument
text(1, 0.8, "Some text", font=1) # normal font (default)
text(1, 0.9, "Some text", font=2) # bold font
text(1, 1.1, "Some text", font=3) # italic font
text(1, 1.2, "Some text", font=4) # bold and italic font

# can also adjust the font type for the axes and title
plot(1, 1, pch=19, xlab="x-Axis Label", ylab="y-Axis Label", main="Title",
     font.lab=2, font.axis=3, font.main=4)

# instead of 'adj', illustrate the 'pos argument
plot(1, 1, pch=19)
text(1, 1, "on top")
plot(1, 1, pch=19)
text(1, 1, "to the bottom", pos=1)
text(1, 1, "to the left",   pos=2)
text(1, 1, "to the top",    pos=3)
text(1, 1, "to the right",  pos=4)

# illustrate the cex argument
plot(1, 1, pch=19)
points(1, 1.1, pch=19, cex=2)
points(1, 1.2, pch=19, cex=0.5)

# note that cex affects the height/width of the symbol and not area; for
# example, the black square with cex=10 has 4 times the area as the smaller
# red square with cex=5, even though cex=10 is twice as large as cex=5
plot(1, 1, pch=15, cex=10)
points(1, 1, pch=15, cex=5, col="red")

# this is relevant when we want the size of points to reflect how often
# certain combinations of values occurred; for example, say we plot the number
# of gears of the cars versus the number of carburetors
plot(dat$gear, dat$carb, pch=19)

# for certain points, there may be multiple cars with the same combination of
# x and y values; we can use xyTable() to determine this
res <- xyTable(dat$gear, dat$carb)
res

# this gives all unique combinations of x and y values that occurred and for
# each how often it occurred in the dataset; we can use these values to create
# a scatterplot where the point sizes reflect the frequency of how often the
# various combinations occurred in the data
plot(res$x, res$y, pch=19, cex=res$number)

# however, the point sizes are not scaled in their area to the frequency when
# we do this; if we want the area of the points to reflect the frequency, we
# have to take the square root of the frequencies
plot(res$x, res$y, pch=19, cex=sqrt(res$number))

# illustrate cex.axis, cex.lab, and cex.main
plot(1, 1, pch=19, xlab="x-Axis Label", ylab="y-Axis Label", main="Title",
     cex.axis=1.2, cex.lab=1.4, cex.main=1.8)

# 12.5.2: Axes and tick marks

# illustrate the lab argument for increasing/decreasing the number of tick
# marks on the x- and y-axis (note that these are suggestions; the function
# still chooses an appropriate number to create nice axis annotations; also
# note that the third value does not have any effect -- see help(par))
plot(mpg ~ hp, data=dat)
plot(mpg ~ hp, data=dat, lab=c(3, 10, 10))

# illustrate the las argument
plot(mpg ~ hp, data=dat)
plot(mpg ~ hp, data=dat, las=1)
plot(mpg ~ hp, data=dat, las=2)
plot(mpg ~ hp, data=dat, las=3)

# illustrate the mgp argument
plot(mpg ~ hp, data=dat)
plot(mpg ~ hp, data=dat, mgp=c(2.5,1,0))   # move the axis labels closer to the axis
plot(mpg ~ hp, data=dat, mgp=c(2.5,0.8,0)) # move the annotations closer to the axis

# illustrate the tck argument
plot(mpg ~ hp, data=dat)
plot(mpg ~ hp, data=dat, tck=-0.008)
plot(mpg ~ hp, data=dat, tck=0.008, mgp=c(2.5,0.4,0))

# illustrate the xlim and ylim arguments
plot(mpg ~ hp, data=dat)
plot(mpg ~ hp, data=dat, xlim=c(0,350), ylim=c(0,40))

# illustrate the xaxs and yaxs arguments (note: by default a bit of space is
# added to the x- and y-axis limits; we can suppress this with xaxs="i" and
# yaxs="i")
plot(mpg ~ hp, data=dat, xlim=c(0,350), ylim=c(0,40))
plot(mpg ~ hp, data=dat, xlim=c(0,350), ylim=c(0,40), xaxs="i", yaxs="i")

# 12.5.3: Figure margins

# use the default margin sizes
plot(mpg ~ hp, data=dat)

# check what the defaults are
par("mar")

# adjust the margin sizes and redraw the plot
par(mar=c(5,4,1,1))
plot(mpg ~ hp, data=dat)

# note: mar is one of those graphical parameters that you can only change via
# par() and not within the call to the plotting function

# close the plotting device to reset 'mar' to the defaults
dev.off()

# 12.5.4: Multiple figure environment

# illustrate the use of 'mfrow'
par(mfrow=c(2,2))
plot(mpg ~ hp, data=dat)
plot(mpg ~ wt, data=dat)
plot(mpg ~ factor(cyl), data=dat)
plot(mpg ~ factor(gear), data=dat)

dev.off()

# adding subfigures within figures via the 'fig' graphical parameter
plot(mpg ~ hp, data=dat)
par(fig=c(50,98,50,98)/100, new=TRUE, cex=0.8, mgp=c(1.8,0.7,0))
plot(mpg ~ wt, data=dat)

dev.off()

############################################################################

## 12.6: Device drivers

# when working with R interactively, we typically want to immediately see the
# plot we are creating (as we did above), but in the end we often want to save
# the plot to a file (like a pdf or tiff)

# the following will save the plot to my_plot.tiff; note the dev.off() at the
# end; this closes the plotting device and actually saves the plot

tiff("my_plot.tiff", width=2000, height=2000, pointsize=36)

plot(mpg ~ hp, data=dat, pch=21, bg="gray", bty="l", las=1,
     xlab="Horsepower", ylab="Miles per Gallon")

dev.off()

# 12.6.1: PostScript diagrams for typeset documents

# postscript() is just another plotting device that can be used as shown above

# 12.6.2: Multiple graphics devices

# skipping this because it is rare that one uses multiple devices at the same time

############################################################################

## 12.7: Dynamic graphics

# an illustration of a 3d scatterplot

# install the rgl package and load it
#install.packages("rgl")
library(rgl)

# can rotate this 3d scatterplot with the mouse (click and drag)
plot3d(dat$hp, dat$wt, dat$mpg, size=10,
       xlab="Horsepower", ylab="Weight", zlab="Miles per Gallon")

# for dynamic graphics, the plotly package is also really nice

# install the plotly package and load it
#install.packages("plotly")
library(plotly)

plot_ly(x=dat$hp, y=dat$wt, z=dat$mpg, type="scatter3d", mode="markers")

# fit a regression model predicting mpg from wt and hp and also use quadratic
# terms for each predictor, then compute the predicted mpg value for
# combinations of wt and hp (within the range of the observed data)
res <- lm(mpg ~ wt + I(wt^2) + hp + I(hp^2), data=dat)
wts <- seq(min(dat$wt), max(dat$wt), length=100)
hps <- seq(min(dat$hp), max(dat$hp), length=100)
pred <- outer(wts, hps, function(x, y) {
   coef(res)[1] + coef(res)[2]*x + coef(res)[3]*x^2 + coef(res)[4]*y + coef(res)[5]*y^2
})

add_surface(plot_ly(x=wts, y=hps, z=pred))

############################################################################

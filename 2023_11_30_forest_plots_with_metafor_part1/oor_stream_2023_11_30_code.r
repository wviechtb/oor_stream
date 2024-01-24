############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2023-11-30 / 2024-01-18
#
# Topic(s):
# - Drawing forest plots with the metafor package
# - https://www.metafor-project.org
#
# last updated: 2024-01-24

# note: the same code was used in the stream on 2024-01-18

############################################################################

# install the metafor package
#install.packages("metafor")

# load the metafor package
library(metafor)

############################################################################

# copy the BCG dataset to 'dat' and examine the data
dat <- dat.bcg
dat

# tpos, tneg, cpos, cneg are the variables corresponding to the 2x2 tables
#
#           TB+    TB-
#         +------+------+
# treated | tpos | tneg |
#         +------+------+
# control | cpos | cneg |
#         +------+------+

# calculate log risk ratios and corresponding sampling variances
dat <- escalc(measure="RR", ai=tpos, bi=tneg,
                            ci=cpos, di=cneg, data=dat,
                            slab=paste0(author, ", ", year))
dat

# yi = the log risk ratios
# vi = the corresponding sampling variances

# random-effects model
res <- rma(yi, vi, data=dat)
res

# estimated average risk ratio with 95% confidence and prediction interval
predict(res, transf=exp, digits=2)

# look at the documentation of the forest() function
help(forest)

# note: forest() is a 'generic function' with specific 'method functions' that
# are called depending on what type of object is passed to the function

# when passing a model object to forest(), the forest.rma() function is used
help(forest.rma)

# create a very simple forest plot based on the results from the model
forest(res)

# notes:
#
# - do not call the method function directly with forest.rma(res); while this
#   would work, let R handle the 'dispatching' to the appropriate method
#   function for you
# - the study labels are shown in the plot; these were added to the object
#   created with escalc() further above (via the 'slab' argument)

# one can also specify study labels directly when using the forest() function
forest(res, slab=paste0(author, " (", year, ")"))

# but generally, it is better to specify the study labels when using escalc()
# or when fitting the model (so, in rma(), there is also the possibility to
# specify study labels via the 'slab' argument)

# add a study header and a header for the annotations
forest(res, header=TRUE)

# suppress the annotations on the right-hand side
forest(res, header=TRUE, annotate=FALSE)

# suppress the summary estimate (the polygon/diamond) on the bottom
forest(res, header=TRUE, addfit=FALSE)

# show the prediction interval around the summary polygon as a dotted line
forest(res, header=TRUE, addpred=TRUE)

# show the weights
forest(res, header=TRUE, showweights=TRUE)

# the header argument can also be a string or a two-element character vector
forest(res, header="Author, Year")
forest(res, header=c("Author, Year", "Log Risk Ratio [95% CI]"))

# suppress the reference line at 0
forest(res, header=TRUE, refline=NA)

# put the reference line at the pooled estimate
forest(res, header=TRUE, refline=coef(res))

# add multiple reference lines by specifying a vector for 'refline'
forest(res, header=TRUE, refline=c(0, coef(res)))

# adjust the x-axis label
forest(res, header=TRUE, xlab="Log Relative Risk")

# label the endpoints of the x-axis limits (or also the center)
forest(res, header=TRUE, xlab=c("(favors treatment)", "(favors control)"))
forest(res, header=TRUE, xlab=c("(favors treatment)", "Log Risk Ratio", "(favors control)"))

# adjust the label given to the summary estimate
forest(res, header=TRUE, mlab="Summary")

# change the symbol for the observed outcomes to circles
forest(res, header=TRUE, pch=19)

# note: see help(points) for more information about the possible values for
# the 'pch' argument

# specify the background color of 'open' plot symbols with the bg argument
forest(res, header=TRUE, pch=21, bg="gray")

# change the color for the observed outcomes to blue
forest(res, header=TRUE, colout="blue")

# colout can also be a vector, giving a color to each individual study; for
# example, we could use a different color for studies where the estimate is
# significantly different from 0 versus those where it is not; we can get the
# p-values for testing H0: log(RR) = 0 for each study with summary()
summary(dat)

# extract the p-values and compare them against alpha=0.05
summary(dat)$pval <= 0.05

# based on this logical vector, create a color vector
ifelse(summary(dat)$pval <= 0.05, "red", "black")

# use this as input to the 'colout' argument
forest(res, header=TRUE, colout=ifelse(summary(dat)$pval <= 0.05, "red", "black"))

# use red for the more recent studies
forest(res, header=TRUE, colout=ifelse(year > 1970, "red", "black"))

# something very colorful
forest(res, header=TRUE, colout=rainbow(13))

# change the color of the summary polygon and/or its border color
forest(res, header=TRUE, col="gray")
forest(res, header=TRUE, col="gray", border="darkgray")

# change the line type for the confidence intervals to dashed lines
forest(res, header=TRUE, lty="dashed")

# note: see help(par) for the line type options

# adjust the number of digits to which the annotations and x-axis tick labels
# are rounded (the default is 2L, where L declares 2 to be an integer)
forest(res, header=TRUE, digits=3L)

# when specifying an integer for digits, trailing zeros on the x-axis tick
# labels are dropped; when not explicitly declaring the number to be an
# integer, trailing zeros are not dropped
forest(res, header=TRUE, digits=3)

# specify a different number of digits for the annotations and x-axis labels
forest(res, header=TRUE, digits=c(3,1))
forest(res, header=TRUE, digits=c(3L,1L))

# change the vertical expansion factor for the CI limits and the summary polygon
forest(res, header=TRUE, efac=1.5)
forest(res, header=TRUE, efac=0.5)
forest(res, header=TRUE, efac=c(0,1))

# make all point sizes equal
forest(res, header=TRUE, psize=1.2)

# there is some more advanced functionality that is available in connection
# with the point sizes, making use of the plim argument; see the documentation
# for further details

# shade the rows of the forest plot zebra-style
forest(res, header=TRUE, shade=TRUE)
forest(res, header=TRUE, shade="zebra")

# zebra-style shading starting with the second study
forest(res, header=TRUE, shade="zebra2")

# shade all rows
forest(res, header=TRUE, shade="all")

# can adjust the color for the shaded rows
forest(res, header=TRUE, shade=TRUE, colshade="lightblue")

# shade rows where the estimate is significantly different from 0
forest(res, header=TRUE, shade=summary(dat)$pval <= 0.05)

# shade rows 1, 5, and 10 (note: row 1 is the study at the bottom of the plot)
forest(res, header=TRUE, shade=c(1,5,10))

# order the studies by the size of the observed outcomes
forest(res, header=TRUE, order="obs")

# order the studies by their precision
forest(res, header=TRUE, order="prec")

# can also specify a variable for 'order' based on which the studies are sorted
forest(res, header=TRUE, order=year)

# adjust the size of the elements in the plot
forest(res, header=TRUE, cex=1.5)

# for further control over the size of the x-axis title and the x-axis tick
# mark labels, can use the cex.lab and cex.axis arguments

# adjust the x-axis limits to -3 and 3
forest(res, header=TRUE, alim=c(-3,3))

# also adjust the number of x-axis tick marks to 7
forest(res, header=TRUE, alim=c(-3,3), steps=7)

# with the 'at' argument, we can specify the exact position of the tick marks
forest(res, header=TRUE, at=c(-3,-1,0,1,3))

# note: if a CI bound falls outside of the range of the x-axis, this is
# indicated with an arrow symbol
forest(res, header=TRUE, alim=c(-3,1))

# do the back-transformation via an x-axis transformation
forest(res, header=TRUE, atransf=exp)

# so the values from the x-axis (-3, -2, -1, 0, 1, 2) are exponentiated; the
# values given then reflect risk ratios, where for example exp(1) is of the
# same magnitude as exp(-1), but of course in different directions; the x-axis
# is now said to be on a 'log scale'

# but the positions of the back-transformed tick marks are not ideal; instead,
# we want to use more meaningful values, such as twice the risk (2) or half
# the risk (0.5), so we specify the log risk ratio tick mark positions, which
# are then exponentiated via the axis transformation
forest(res, header=TRUE, atransf=exp, digits=c(2L,4L),
       at=log(c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4)))

# note that some tick mark labels may not show up because there is not enough
# space to show them without labels overlapping; we will deal with this later

# alternatively, we can transform all values in the plot directly
forest(res, header=TRUE, transf=exp)

# this does not look so nice; we can make this look a bit nicer if we specify
# narrower x-axis limits (i.e., closer to 1)
forest(res, header=TRUE, transf=exp, alim=c(0,2))

# move the reference line to 1
forest(res, header=TRUE, transf=exp, alim=c(0,2), refline=1)

# note: because exp() is a non-linear transformation, the CIs are not
# symmetric around the point estimates

# look at the defaults chosen by forest() for the plot region limits (xlim)
print(forest(res, header=TRUE))

# decrease the left-hand side limit of the plot region; as a result, the
# forest part of the plot is moving to the left
forest(res, header=TRUE, xlim=c(-6,5))

# by decreasing the right-hand side limit, we can reduce the wasted space
# before the annotations
forest(res, header=TRUE, xlim=c(-6,4))

# coming back to the axis transformation, we can make use of xlim in this way
# to create more space for the forest part of the plot and avoid that x-axis
# tick marks labels are suppressed because of overlap; compare these plots
forest(res, header=TRUE, atransf=exp, digits=c(2L,4L),
       at=log(c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4)))
forest(res, header=TRUE, xlim=c(-6,4), atransf=exp, digits=c(2L,4L),
       at=log(c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4)))
forest(res, header=TRUE, xlim=c(-5,3), atransf=exp, digits=c(2L,4L),
       at=log(c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4)))

# this can also be used to create space for further annotations in the plot
forest(res, header=TRUE, xlim=c(-16,6))

# with ilab, one can add additional variables to the plot; one also has to
# specify where to put these additional variables with the ilab.xpos argument
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9)
text(c(-9.5,-8,-6,-4.5), 15, c("TB+", "TB-", "TB+", "TB-"), cex=0.9, font=2)
text(c(-8.75,-5.25), 16, c("Vaccinated", "Control"), cex=0.9, font=2)

# with ilab.pos, can change the alignment of the variables that are added; for
# example, we can use ilab.pos=2 to right-align them (have to adjust the
# position of the text headers that are added to make things line up again)
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), ilab.pos=2, cex=0.9)
text(c(-9.5,-8,-6,-4.5), 15, c("TB+", "TB-", "TB+", "TB-"), cex=0.9, font=2, pos=2)
text(c(-8,-4.5), 16, c("Vaccinated", "Control"), cex=0.9, font=2, pos=2)

# save the plot as a png file

png("forest_plot.png", width=2500, height=1800, pointsize=10, res=300)

forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9)
text(c(-9.5,-8,-6,-4.5), 15, c("TB+", "TB-", "TB+", "TB-"), cex=0.9, font=2)
text(c(-8.75,-5.25), 16, c("Vaccinated", "Control"), cex=0.9, font=2)

dev.off()

############################################################################

# as noted earlier, we can use the 'mlab' argument to adjust the text for the
# row that includes the summary polygon
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9, mlab="Summary")

# often, we want to add additional information in this row, such as statistics
# about the amount of heterogeneity, like the Q-test, I^2, and tau^2; this
# information we can find in the model output
res

# so we could in principle manually add this information via 'mlab'
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9,
       mlab="RE Model (Q = 152.23, df = 12, p < .001; I^2 = 92.2%, tau^2 = 0.31)")

# to plot math equations, see help(plotmath); can do this in various ways
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9,
       mlab=expression("RE Model (Q = 152.23, df = 12, p < .001;" ~ I^2 == 92.2*"%," ~ tau^2 == 0.31 * ")"))
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9,
       mlab=expression(paste("RE Model (Q = 152.23, df = 12, p < .001; ", I^2 == 92.2, "%, ", tau^2 == 0.31, ")")))

# instead of copy-pasting values manually from the output, we can automate the
# extraction of the relevant pieces of information from the model object (the
# fmtx() and fmtp() functions from the metafor package are useful for nicely
# formatting statistics and p-values)
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9,
       mlab=paste0("RE Model (Q = ", fmtx(res$QE, digits=2), ", df = ", res$k-1, ", ",
                   fmtp(res$QEp, digits=3, pname="p", sep=TRUE),
                   "; I^2 = ", fmtx(res$I2, digits=1), "%",
                   ", tau^2 = ", fmtx(res$tau2, digits=2), ")"))

# and now we want to combine this with a math expression; for this,
# bquote(paste(...)) is very convenient; within paste(), we just add the
# elements that we want to show, consisting of text (in quotes), code to
# evaluate (like this: .(<code>)), and plotmath syntax
forest(res, header=TRUE, xlim=c(-16,6), ilab=cbind(tpos, tneg, cpos, cneg),
       ilab.xpos=c(-9.5,-8,-6,-4.5), cex=0.9,
       mlab=bquote(paste("RE Model (Q = ", .(fmtx(res$QE, digits=2)), ", df = ", .(res$k-1), ", ",
                         .(fmtp(res$QEp, digits=3, pname="p", sep=TRUE)),
                         "; ", I^2, " = ", .(fmtx(res$I2, digits=1)), "%",
                         ", ", tau^2, " = ", .(fmtx(res$tau2, digits=2)), ")")))
text(c(-9.5,-8,-6,-4.5), 15, c("TB+", "TB-", "TB+", "TB-"), cex=0.9, font=2)
text(c(-8.75,-5.25), 16, c("Vaccinated", "Control"), cex=0.9, font=2)

############################################################################

# further topics to be discussed at the next session:

# - arguments ylim and rows
# - argument width
# - the alignment of the annotations (and the fonts argument)
# - how to show studies with missings
# - the difference between the different forest functions
# - forest plots for models with moderators
# - use of forest plots outside of meta-analysis

############################################################################

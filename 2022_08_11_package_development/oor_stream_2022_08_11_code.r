############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-08-11
#
# Topic(s):
# - R Package Development
#
# last updated: 2022-08-12

############################################################################

# The goal is to create an R package for making ridgeline charts/plots/figures
# (https://r-graph-gallery.com/ridgeline-plot.html) using 'base R graphics'
# (there is already an R package for creating such figures using ggplot2,
# namely the ggridges package: https://cran.r-project.org/package=ggridges,
# but no such package for creating ridgeline charts using base R graphics).
#
# The goal of this stream is to cover the steps for creating a basic R package
# and to work on the functionality of the package itself. Note that 'writing R
# packages' was actually covered already in a previous stream (on 2020-11-12),
# so quite a bit of what's below is a repeat of the same notes.
#
# Notes:
#
# 1) I don't have access to a computer with macOS and hence cannot test out
#    the required steps here (but we can try to work this out together during
#    the stream).
#
# 2) We will *not* make use packages like devtools [1] or roxygen2 [2]. These
#    packages can make certain steps a bit easier and we can explore their
#    functionality in the future, but for now I want to focus on how package
#    creation works without their use.
#
#    [1] https://cran.r-project.org/package=devtools
#    [2] https://cran.r-project.org/package=roxygen2
#
# 3) I will assume you have R version 4.2.1 (i.e., the current version of R)
#    installed. If you have an older version installed, you need to change
#    things accordingly (but you really should upgrade anyway).
#
# Before we can start, we need to add R to the system 'path', so we can use R
# from the command line (the 'command prompt' as it is sometimes called under
# Windows). For macOS and Linux, this should already be the case. For Windows,
# we have to take some extra steps.
#
# In principle, you can skip this step if you are going to use the command
# line ('Terminal') under RStudio (which adds R to the path automatically).
# However, if you don't want this dependency, then we can go through the
# following steps.
#
# First, we need to figure out where R is installed (in RStudio, you can check
# this under Tools, Global Options). By default, it should be at:
#
# C:\Program Files\R\R-4.2.1
#
# If so, then we need to add C:\Program Files\R\R-4.2.1\bin\ to the path (note
# the 'bin' at the end).
#
# Click on the Start menu, type 'env', click on 'Edit the system environment
# variables'. Then click on 'Environmental Variables', and under the 'System
# variables' click on Path and Edit. Click on New and add:
#
# C:\Program Files\R\R-4.2.1\bin\
#
# Click Ok. Then click on the Start menu, type 'cmd' and click on 'Command
# Prompt'. Then type 'path', enter, and check that what you added above now
# shows up. Just typing 'R' and enter should then run R on the command line.
# Exit R with 'q(save="no")'.

############################################################################

# Now we are ready to start creating the package.
#
# 1) Create a directory/folder on your computer for creating the package. I
#    would suggest some directory under 'Documents'. Let's call this directory
#    'ridgelines'.
#
# 2) Within this directory, create a DESCRIPTION file (no extension!) and add
#    the contents as discussed during the stream. The directory where the
#    DESCRIPTION file is located is the 'package root'.
#
# 3) Within this directory, create directories called 'man' and 'R'.
#
# 4) Inside the 'R' directory, create a ridgelines.R file. This is where we
#    will put the function for creating ridgeline charts.
#
# 5) Inside the 'man' directory, create a ridgelines.Rd file and add the
#    contents as discussed during the stream.
#
# So the structure should now look like this:
#
# ridgelines
#   DESCRIPTION
#     - R
#       ridgelines.R
#     - man
#       ridgelines.Rd
#
# Done! You now have everything in place for creating your first R package
# (yes, the basic steps are that simple).
#
# On the command line, navigate to the directory *just above* the package root
# directory (you can do this with the 'cd' command). In RStudio, you can also
# use the 'Files' pane, navigate to the directory, click on 'More', and select
# 'Open New Terminal Here'. With the 'dir' command, check the contents of the
# directory and make sure that the 'ridgelines' directory shows up.
#
# Then type:
#
# R CMD build ridgelines
#
# This will 'build' the package.
#
# You should now find a .tar.gz file in the directory. Now we can 'check' that
# everything is okay with the package with:
#
# R CMD check --no-manual ridgelines_0.9-1.tar.gz
#
# As long as you see 'Status: OK' at the end, everything is fine.
#
# Note: There is also a stricter version of the check we can run with:
#
# R CMD check --no-manual --as-cran ridgelines_0.9-1.tar.gz
#
# Finally, we can 'install' the package with:
#
# R CMD INSTALL ridgelines_0.9-1.tar.gz
#
# Now start R/RStudio and try loading the package with:

library(ridgelines)

############################################################################

# Eventually, you also need to start reading the 'Writing R Extensions' manual
# (which is the definite manual for creating R packages), but be warned that
# parts of it are quite technical (and a lot of it may not be relevant when
# creating basic packages).
#
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html
#
# For a very basic package, sections 1.1, 1.3, and 2 are most important.

############################################################################

# The GitHub repo for the package created can be found here:
#
# https://github.com/wviechtb/ridgelines

############################################################################

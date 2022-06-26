############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2020-11-12
#
# Topic(s):
# - writing R packages
#
# last updated: 2020-11-12

############################################################################

# restart the R session (Menu 'Session' - Restart R)

############################################################################

# function to compute the moving average of a (numeric) vector (note that this
# is a slightly modified version compared to the one we created last week)

movavg <- function(x, window=1, na.rm=FALSE) {

   if (!is.numeric(x))
      stop("Need to supply a numeric vector via argument 'x'.")

   n <- length(x)

   mx <- rep(NA, n)

   for (i in 1:n) {
      pos <- seq(i-window, i+window)
      pos <- pos[pos >= 1 & pos <= n]
      mx[i] <- mean(x[pos], na.rm=na.rm)
   }

   z <- list(mx=mx, window=window, n=n)
   class(z) <- "movavg"
   return(z)

}

# print() method

print.movavg <- function(x) {

   cat("\nMoving Average\n")
   cat("Window Size:", x$window, "\n\n")
   print(x$mx)
   cat("\n")

}

# plot() method

plot.movavg <- function(x, ...) {

   plot(1:x$n, x$mx, type="o", ...)

}

# let's try this out

x <- 1:50
y <- 2 + log(x) + rnorm(50, 0, 0.2)
res <- movavg(y)
res
plot(x, y, pch=19)
lines(x, res$mx)

res <- movavg(y, window=3)
res
plot(x, y, pch=19)
lines(x, res$mx, lwd=4, col="red")

plot(res, lwd=2, pch=19)

############################################################################

# We will now create an R package that contains these functions.
#
# Notes:
#
# 1) I don't have access to a computer with Mac OS X and hence cannot test out
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
# 3) I will assume you have R version 4.0.3 (i.e., the current version of R)
#    installed. If you have an older version installed, you need to change
#    things accordingly (but you really should upgrade anyway).
#
# Before we can start, we need to add R to the system 'path', so we can use R
# from the command line (the 'command prompt' as it is sometimes called under
# Windows). For Mac OS X and Linux, this should already be the case. For
# Windows, we have to take some extra steps.
#
# In principle, you can skip this step if you are going to use the command
# line ('Terminal') under RStudio (which adds R to the path automatically).
# However, if you don't want this dependency, then we can go through the
# following steps.
#
# First, we need to figure out where R is installed (in RStudio, you can check
# this under Tools, Global Options). By default, it should be at:
#
# C:\Program Files\R\R-4.0.3
#
# If so, then we need to add C:\Program Files\R\R-4.0.3\bin\ to the path (note
# the 'bin' at the end).
#
# Click on the Start menu, type 'env', click on 'Edit the system environment
# variables'. Then click on 'Environmental Variables', and under the 'System
# variables' click on Path and Edit. In Windows 7, add the following to the
# end of what is shown under 'Variable value':
#
# ;C:\Program Files\R\R-4.0.3\bin\
#
# (note the semicolon). Under Windows 10, click on New and add the path above
# but without the semicolon at the beginning.
#
# Then click on the Start menu, type 'cmd' and click on 'Command Prompt'. Then
# type 'path', enter, and check that what you added above now shows up. Just
# typing 'R' and enter should then run R on the command line. Exit R with
# 'q(save="no")'.

############################################################################

# Now we are ready to start creating the package.
#
# 1) Create a directory/folder on your computer for creating the package. I
#    would suggest some directory under 'Documents'.
#
# 2) Within this directory, create a DESCRIPTION file (no extension!) and add
#    the contents as discussed during the stream. The directory where the
#    DESCRIPTION file is located is the 'package root'.
#
# 3) Within this directory, create directories called 'man' and 'R'.
#
# 4) Inside the 'R' directory, create a movavg.R file and put the code for the
#    movavg() function (see above) inside this file.
#
# 5) Inside the 'man' directory, create a movavg.Rd file and add the contents
#    as discussed during the stream.
#
# Done! You now have everything in place for creating your first R package
# (yes, the basic steps are that simple).
#
# On the command line, navigate to the directory *just above* the package root
# directory (you can do this with the 'cd' command). In RStudio, you can also
# use the 'Files' pane, navigate to the directory, click on 'More', and select
# 'Open New Terminal Here'. With the 'dir' command, check the contents of the
# directory and make sure that the directory you created in step 1 shows up.
# Then type:
#
# R CMD build <name of the package root directory you created in step 1>
#
# (without the <>). This will 'build' the package.
#
# You should now find a .tar.gz file in the directory. Now we can 'check' that
# everything is okay with the package with:
#
# R CMD check --no-manual movpack_0.1-0.tar.gz
#
# As long as you see 'Status: OK' at the end, everything is fine.
#
# Finally, we can 'install' the package with:
#
# R CMD INSTALL movpack_0.1-0.tar.gz
#
# Now start R/RStudio and try loading the package with:

library(movpack)

# And try running an example:

x <- 1:50
y <- 2 + log(x) + rnorm(50, 0, 0.2)
res <- movavg(y)
res
plot(x, y, pch=19)
lines(x, res$mx)

# Also check the documentation of the function with:

help(movpack)

############################################################################

# Now we will add the method functions, print() and plot(), to the package.
#
# 1) Add the two functions to the movavg.R file (or create separate .R files)
#    inside of the 'R' directory.
#
# 2) Create a NAMESPACE file (no extension!) in the package root. Add the
#    following to this file (without the # at the beginning):
#
# exportPattern("^[^\\.]")
#
# S3method(print, movavg)
# S3method(plot, movavg)
#
# 3) As above, build the package, check it (there will be WARNINGS), and then
#    install the package. We will deal with the warnings in a moment.
#
# Now restart R/RStudio and test that it works:

library(movpack)

x <- 1:50
y <- 2 + log(x) + rnorm(50, 0, 0.2)
res <- movavg(y)
res
plot(res)

############################################################################

# We need to fix the warnings (which I will explain during the stream).
#
# 1) Change the print function by adding '...' to the arguments:

print.movavg <- function(x, ...) {

   cat("\nMoving Average\n")
   cat("Window Size:", x$window, "\n\n")
   print(x$mx)
   cat("\n")

}

# 2) We need to add documentation for the print() and plot() methods. Inside
#    the 'man' directory, create a movavg_methods.Rd file and add the contents
#    as discussed during the stream.
#
# Now build the package and re-check it. The warnings should be resolved.

############################################################################

# To be able to create the pdf manual, you need to install LaTeX. A popular
# implementation of this for Windows is:
#
# https://miktex.org
#
# Download the installer and run it.
#
# For Linux, I would suggest to install 'texlive' (which you should be able to
# find in the package repo of your distro; you may also need to install
# texlive-latex-extra and texlive-fonts-extra). I think MacTeX is often used
# in combination with Mac OS X (https://www.tug.org/mactex/).
#
# Once you have LaTeX installed, try running:
#
# R CMD check movpack_0.1-0.tar.gz
#
# (you may need to reopen the command prompt / terminal first). The first time
# you do this under Windows, you may get prompts to install additional LaTeX
# packages. Also, I had to open the MikTeX Console, and under 'Tasks' run
# 'Refresh file name database' and 'Refresh font map database' once to get it
# to work.

############################################################################

# If your package doesn't contain any 'compiled' code, then the above is
# sufficient for creating packages. Ultimately, you may also need to install
# additional tools (compilers, some utilities), which you can do under Windows
# by installing Rtools:
#
# 1) Go to CRAN (https://cran.r-project.org/), Download R for Windows, Rtools,
#    download rtools40-x86_64.exe, and install Rtools40.
#
# 2) You then need to add the \usr\bin directory of Rtools to your path (yes,
#    that part again). I would just go through the steps described earlier and
#    add
#
#    C:\rtools40\usr\bin\
#
#    to the path (at least this is the default installation location). Or you
#    can start R/RStudio and run:

writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

# Now you should be able to create/install 'source' packages even when they
# include code that needs compilation (e.g., C/C++/Fortran code).
#
# Eventually, you also need to start reading the 'Writing R Extensions' manual
# (which is the definite manual for creating R packages), but be warned that
# parts of it are quite technical (and a lot of it may not be relevant when
# creating basic packages).
#
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html

############################################################################

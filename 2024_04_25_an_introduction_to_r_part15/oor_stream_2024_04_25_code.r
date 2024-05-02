############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2024-04-25
#
# Topic(s):
# - An Introduction to R
#   https://cran.r-project.org/doc/manuals/r-release/R-intro.html
# - Section(s): 13.1 - 14.4
#
# last updated: 2024-05-02

############################################################################

### 13: Packages

# see which packages are installed
library()

# terminology:
# - package = book
# - library = place where you store books
# (don't say you are using the 'xyz library' for your analysis!)

# typically, there are (at least) two libraries:

# 1) the 'system library' into which the packages are installed that come with
#    R by default (e.g., the 'base', 'stats', 'utils' packages)
# 2) the 'user library' where additional packages are installed to when they are
#    installed by the user
# 3) there may be additional 'site libraries' where additional packages may be
#    installed to by the administrator of the computer (which can be useful when
#    there are multiple users of a computer)

# to see the locations of the libraries
.libPaths()

# to get more information about installed packages (but this outputs a large
# matrix with many columns that is hard to read)
installed.packages()

# only show where packages are installed to, their version, and the 'priority'
installed.packages()[,c("LibPath", "Version", "Priority")]

# the 'base' and 'recommended' packages (see 'Priority' column) are installed
# with R automatically; it is also possible to install an updated version of
# recommended packages (if there is an update)

# list loaded packages (have to explicitly use print())
print(.packages())

# can also use search(), but strictly speaking, this is showing the 'search
# path', that is, the locations where R is looking for an object (and this
# includes also things like the 'global environment'
search()

# for example, when we type
mtcars

# then R is going through the search path to find an object with that name
# (which happens to be part of the 'datasets' package)

# install the 'lme4' package (https://cran.r-project.org/package=lme4)
install.packages("lme4")

# if possible, R will try to install a package from the 'binary version', which
# avoids having to compile C/C++/Fortan code (if the package makes use of such
# code); this will typically be the case under Windows and macOS; alternatively,
# if one has the appropriate tools installed, one can also install packages from
# the 'source version', but this is more time-consuming

# load the 'lme4' package
library(lme4)

# to get a list of help topics of an installed package, one can do as described
# in the manual using help.start(), but more directly, we can do the following
help(package="lme4")

# updating packages
update.packages()

# to avoid getting the prompt whether to update each package
update.packages(ask=FALSE)

############################################################################

## 13.1: Standard packages

# as discussed above, these are the packages that come with R

############################################################################

## 13.2: Contributed packages and CRAN

# the 'recommended' packages also come with R; everything else are additional
# packages installed by the user

############################################################################

## 13.3: Namespaces

# install the 'psych' and 'lavaan' packages
install.packages("psych")
install.packages("lavaan")

# a note about 'masking' (or name clashes): different packages may contain
# functions that have the same name; for example:

library(psych)
library(lavaan)

# - note that it says that function 'cor2cov' has been masked
# - what has happened is that both packages have a function called 'cor2cov'
# - so when you now use the cor2cov function, the one from the lavaan package
#   will be used (i.e., always the one from the package loaded last)
# - but what if you want to use the 'cor2cov' function from the psych package?
# - then you can use psych::cor2cov() to explicitly tell R to use the cor2cov
#   function from the psych package
# - note that the two functions essentially do the same thing in this example
#   (although the argument names are different), but this may not be true in
#   other cases (for example, stats::filter and dplyr::filter have entirely
#   different purposes)
# - the more packages you load, the more likely it is that two packages will
#   contain functions with the same name and hence that masking will occur
# - to avoid the headaches that this can potentially create, only load packages
#   at the beginning of your script that you really need

############################################################################

### 14: OS facilities

## 14.1: Files and directories

# file.create() - create an empty file
# dir.create()  - create a directory (folder)
# file.remove() - remove a file
# unlink()      - remove a file or directory
# list.files()  - list the files in a particular directory
# list.dirs()   - list the directories
# file.exists() - check if a file exists
# file.copy()   - copy a file
# file.path()   - construct file paths in a platform independent way

## 14.2: Filepaths

# a couple recommendations:
# - use relative (not absolute) file paths
# - when specifying a file path, use forward slashes
# - assume file names are case-sensitive

## 14.3: System commands

# it is good to know about the possibility to run system commands from within
# R with system() and system2(), but these are really more advanced use cases,
# so we won't get into specifics here

## 14.4: Compression and Archives

# as described, R comes with a whole bunch of functionality for dealing with
# archives (such as .gzip, .tar, and .zip files)

############################################################################

### B.4 Scripting with R

# sometimes, we want to run an entire R script from the command line and capture
# all output that is generated in an output file; this can be done with 'R CMD
# BATCH input.r output.txt' (note that the input file cannot contain any spaces
# in the file name)

############################################################################

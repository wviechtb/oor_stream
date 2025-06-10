############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-05-29
#
# Topic(s):
# - syntax style guides and syntax formatting
# - packages styler, lintr, and formatR
# - the R formatter Air
#
# last updated: 2025-06-10

############################################################################

# there is no 'official' R style guide, at least none can be found on the R
# website (https://www.r-project.org) or on CRAN (https://cran.r-project.org)
#
# but various guides have been written; some well-known syntax style guides:
# - https://style.tidyverse.org
# - https://google.github.io/styleguide/Rguide.html
# - https://contributions.bioconductor.org/r-code.html#r-code
#
# sometimes labs / research groups have their own style guide, for example:
# - https://jef.works/R-style-guide/
#
# (during the stream, we went through https://style.tidyverse.org in detail)

############################################################################

# there is also software that can help with formatting your code; this is
# sometimes called a linter: https://en.wikipedia.org/wiki/Lint_(software)
#
# there are several R packages that help with this:
# - https://cran.r-project.org/package=styler
# - https://cran.r-project.org/package=lintr
# - https://cran.r-project.org/package=formatR
#
# and a separate piece of software:
# - https://posit-dev.github.io/air/

############################################################################

# install the styler package and load it
#install.packages("styler")
library(styler)

# can style a file like the script from the last session (assuming it is in
# the current working directory); note: this changes the file, so make a
# backup if you want to keep the original
style_file("oor_stream_2025_05_22_code.r")

# in RStudio, there is an add-in that you can access via the toolbar (e.g.,
# Addins -> Style active file)

############################################################################

# install the lintr package and load it
#install.packages("lintr")
library(lintr)

# lint the same file; this shows all potential style issues, but does not
# actually change the file (not sure whether that is possible)
lint("oor_stream_2025_05_22_code.r")

############################################################################

# install the formatR package and load it
#install.packages("formatR")
library(formatR)

# tidy the script and save into a separate file (I prefer 3 spaces for indentation)
tidy_source("oor_stream_2025_05_22_code.r", indent=3,
            file="oor_stream_2025_05_22_code_formated.r")

# or can tidy a script and save into the same file
tidy_file("oor_stream_2025_05_22_code.r", indent=3)

############################################################################

# Air is a separate piece of software to format R code
#
# https://posit-dev.github.io/air/
#
# you can follow the installation directions given there or just download a
# pre-compiled binary from: https://github.com/posit-dev/air (add the path to
# the binary to your path if you want to call air from the command line)
#
# the syntax for the command line is this:
#
# air format oor_stream_2025_05_22_code.r
#
# note that this changes the file, so make a backup before trying this out
#
# Air is under active development, so things may change

############################################################################

# some final thoughts:
#
# - do pay attention to the formatting of your code
# - if you are working with other people on a project, stick to the same
#   formatting style
# - linters can help with / automate formatting, but your preferences may
#   differ from those implemented in the linter (with some effort it may be
#   possible to tweak settings so the linter adheres to your preferences)

############################################################################

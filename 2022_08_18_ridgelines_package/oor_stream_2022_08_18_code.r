############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2022-08-18
#
# Topic(s):
# - continue work on the 'ridgelines' package
#
# last updated: 2022-08-23

############################################################################

# In this stream, we continued to work on the 'ridgeslines' package. The
# code for the package is hosted on GitHub and can be found here:
#
# https://github.com/wviechtb/ridgelines
#
# Things that were implemented during this stream:
#
# - added the 'alpha' argument for easily adding alpha blending (transparency)
#   to the colors specified via the 'col' argument
# - more flexible handling of the 'col' argument (by recycling the color(s)
#   specified as often as needed)
# - better y-axis scaling by using the maximum y value across all groups
#   (yaxis="maxall"), but can still scale to the maximum within each group (with
#   yaxis="pergrp")
# - added 'bw' argument to compute the mean (or some other function) of the
#   bandwidths of the various kernel density estimates to use across all groups
#   (can also specify a single value for the 'bw' argument or one per group)
# - allow passing additional arguments to the density() function via the ...
#   argument (although might change how this work in the future by using an
#   argument to collect all such arguments)

############################################################################

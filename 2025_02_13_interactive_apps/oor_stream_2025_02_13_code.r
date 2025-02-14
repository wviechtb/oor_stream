############################################################################

# Open Online R Stream (https://www.wvbauer.com/doku.php/live_streams)
#
# By:   Wolfgang Viechtbauer (https://www.wvbauer.com)
# Date: 2025-02-13
#
# Topic(s):
# - the 'chesstrainer' package
# - interactive apps (without Shiny)
#
# last updated: 2025-02-14

############################################################################

### the chesstrainer package

# GitHub repo: https://github.com/wviechtb/chesstrainer
# docs: https://wviechtb.github.io/chesstrainer/

# install the package
install.packages("remotes")
remotes::install_github("wviechtb/chesstrainer")

# load the package
library(chesstrainer)

# start playing
play()

# details on how to work with the trainer can be found here:
# https://wviechtb.github.io/chesstrainer/reference/chesstrainer-package.html

# the package is a nice illustration of what is possible with R in terms of
# writing interactive applications using base R/graphics without using
# additional toolkits, programming languages, or packages (like Tcl/Tk,
# JavaScript, shiny, etc.)

############################################################################

# the main function that allows for this kind of interactive behavior with a
# plot window is getGraphicsEvent(); see the documentation for details
help(getGraphicsEvent)

# note: getGraphicsEvent() only works for the x11() and windows() plotting
# devices, not for the quartz() device (that is the default under macOS) or
# the RStudio plotting device; under macOS(), one can try to first open up a
# x11() device before creating a plot with the x11() function (not sure if
# this works on all macOS systems); under RStudio, one can try the same under
# Linux and macOS or windows() under Windows

# let's try out a very simple example; create a simple plot
plot(1)

# run this and then either click on the plot somewhere or hit a key
out <- getGraphicsEvent(onMouseDown = function(button, x, y) return(c(x=x, y=y, button=button)),
                        onKeybd = function(key) return(key))
print(out)

# out will either be the coordinates of the mouse click (x and y) and the
# button clicked (0 = left mouse button, 1 = middle, 2 = right) or the key
# that was hit; note that x and y are given in normalized device coordinates,
# where (x=0, y=0) is the very lower left and (x=1, y=1) the top right

# getGraphicsEvent() stops when one of the function returns a non-NULL value;
# so in the following example, clicking will print the c(x, y, button) vector
# to the console but any keystroke will exit stop getGraphicsEvent()

fun.mousedown <- function(button, x, y) {
   print(c(x=x, y=y, button=button))
   return(NULL)
}

fun.keyboard <- function(key) return(key)

out <- getGraphicsEvent(onMouseDown = fun.mousedown, onKeybd = fun.keyboard)

# besides mouse clicks (onMouseDown) and keyboard strokes (onKeybd), there are
# additional events that are handled by getGraphicsEvent(), including moving
# the mouse (onMouseMove) and releasing a mouse button (onMouseUp); let's try
# a more comprehensive example

############################################################################

# function that can be used to draw on a plot (e.g., to create annotations)
# click and drag to draw with a pencil, keys 1, 2, 3, and 4 for selecting
# colors (black, red, green, or blue), r to draw rectangles (click, drag, and
# release), p to switch to pencil mode, and e to switch to eraser mode

draw <- function() {

   # if no plotting device is open, exit
   if (dev.cur() == 1) return(invisible())

   # set some defaults
   cols <- c("black", "red", "green", "blue")
   col  <- "black"
   mode <- "pencil"

   fun.mousedown <- function(button,x,y) {
      pressed <<- TRUE
      # convert normalized device coordinates to user coordinates
      x <- grconvertX(x, from="ndc", to="user")
      y <- grconvertY(y, from="ndc", to="user")
      x.last <<- x
      y.last <<- y
      x.rect.start <<- x
      y.rect.start <<- y
      return(NULL)
   }

   fun.mousemove <- function(button,x,y) {
      if (pressed) {
         x <- grconvertX(x, from="ndc", to="user")
         y <- grconvertY(y, from="ndc", to="user")
         if (mode == "pencil")
            segments(x.last, y.last, x, y, lwd=4, col=col)
         if (mode == "eraser")
            segments(x.last, y.last, x, y, lwd=30, col="white")
         x.last <<- x
         y.last <<- y
      }
      return(NULL)
   }

   fun.mouseup <- function(button,x,y) {
      pressed <<- FALSE
      if (mode == "rect")
         rect(x.rect.start, y.rect.start, x.last, y.last, lwd=5, border=col)
      return(NULL)
   }

   fun.key <- function(key) {

      if (is.element(key, c("1","2","3","4")))
         col <<- cols[as.numeric(key)]

      if (key == "e")
         mode <<- "eraser"

      if (key == "p")
         mode <<- "pencil"

      if (key == "r")
         mode <<- "rect"

      if (key == "q")
         return(invisible(1))

      return(NULL)

   }

   pressed <- FALSE
   x.last <- NA_real_
   y.last <- NA_real_
   x.rect.start <- NA_real_
   y.rect.start <- NA_real_

   getGraphicsEvent(prompt="", onMouseDown=fun.mousedown,
                    onMouseMove=fun.mousemove, onMouseUp=fun.mouseup,
                    onKeybd=fun.key)

}

dat <- mtcars
plot(dat$hp, dat$mpg, pch=21, bg="gray", cex=1.5)
draw()

############################################################################

# another event handler is onIdle, which is a function to call when no other
# events are triggered; this can be used for animations and interactive apps;
# note: onIdle only works for the x11() device, not windows() -- apologies to
# people using Windows ... but you should switch to Linux anyway ;)

# let's try out using onIdle to create a simple snake-like game for two
# players, where the goal is to eat up the blue squares without touching the
# border of the playing area; player 1 controls their snake with the cursor
# keys, player 2 controls their snake with the 'wasd' keys

snake <- function() {

   # set up the plot
   par(mar=c(4,4,4,4), xpd=NA, xaxs="i", yaxs="i")
   plot(NA, xlim=c(0,1), ylim=c(0,1), xlab="", ylab="", xaxt="n", yaxt="n")
   box(lwd=5)

   # initial size of the snakes
   size1 <- 4
   size2 <- 4

   # initial position and direction for snake 1
   x1.pos.old <- rep(0.25, size1)
   y1.pos.old <- rep(0.25, size1)
   x1.pos.new <- x1.pos.old
   y1.pos.new <- y1.pos.old
   direction1 <- 3

   # initial position and direction for snake 2
   x2.pos.old <- rep(0.75, size2)
   y2.pos.old <- rep(0.75, size2)
   x2.pos.new <- x2.pos.old
   y2.pos.new <- y2.pos.old
   direction2 <- 1

   # controls how quickly the snakes move
   movesize <- 0.01

   # draw the starting positions of the two snakes
   points(x1.pos.new[1], y1.pos.new[1], pch=15, cex=1.5, col="black")
   points(x2.pos.new[1], y2.pos.new[1], pch=15, cex=1.5, col="red")

   # sleep for 2 seconds for players to get ready
   Sys.sleep(2)

   # add text to the top about the size of each snake
   text(0.25, 1.05, paste("Size: ", size1), cex=1.5, col="black")
   text(0.75, 1.05, paste("Size: ", size2), cex=1.5, col="red")

   # position of the first block
   x.block <- runif(1, 0.05, 0.95)
   y.block <- runif(1, 0.05, 0.95)

   idlefun <- function() {

      # draw the block
      points(x.block, y.block, pch=15, cex=1.5, col="blue")

      # overdraw with white the last element of the position vector of each snake
      points(x1.pos.old[size1], y1.pos.old[size1], pch=15, col="white", cex=1.6)
      points(x2.pos.old[size2], y2.pos.old[size2], pch=15, col="white", cex=1.6)

      # adjust the first element of the position of each snake based on their current direction
      if (direction1 == 1)
         y1.pos.new[1] <<- y1.pos.old[1] - movesize
      if (direction1 == 2)
         x1.pos.new[1] <<- x1.pos.old[1] - movesize
      if (direction1 == 3)
         y1.pos.new[1] <<- y1.pos.old[1] + movesize
      if (direction1 == 4)
         x1.pos.new[1] <<- x1.pos.old[1] + movesize
      if (direction2 == 1)
         y2.pos.new[1] <<- y2.pos.old[1] - movesize
      if (direction2 == 2)
         x2.pos.new[1] <<- x2.pos.old[1] - movesize
      if (direction2 == 3)
         y2.pos.new[1] <<- y2.pos.old[1] + movesize
      if (direction2 == 4)
         x2.pos.new[1] <<- x2.pos.old[1] + movesize

      # check if the first snake has touched the border of the playing area
      if (y1.pos.new[1] <= 0 || y1.pos.new[1] >= 1 || x1.pos.new[1] <= 0 || x1.pos.new[1] >= 1) {
         text(0.5, 0.5, "Player 1 has died", cex=1.5)
         return(invisible(1))
      }

      # check if the second snake has touched the border of the playing area
      if (y2.pos.new[1] <= 0 || y2.pos.new[1] >= 1 || x2.pos.new[1] <= 0 || x2.pos.new[1] >= 1) {
         text(0.5, 0.5, "Player 2 has died", cex=1.5)
         return(invisible(1))
      }

      # draw the snakes at their current position
      points(x1.pos.new[1], y1.pos.new[1], pch=15, cex=1.5, col="black")
      points(x2.pos.new[1], y2.pos.new[1], pch=15, cex=1.5, col="red")

      # check if snake 1 is close enough to the block to eat it up
      if (abs(x1.pos.new[1] - x.block) <= 0.01 && abs(y1.pos.new[1] - y.block) <= 0.01) {
         x1.pos.new <<- c(x1.pos.new, x1.pos.new[size1])
         y1.pos.new <<- c(y1.pos.new, y1.pos.new[size1])
         size1 <<- size1 + 1
         points(x.block, y.block, pch=15, col="white", cex=1.6)
         x.block <<- runif(1, 0.05, 0.95)
         y.block <<- runif(1, 0.05, 0.95)
         rect(0.05, 1.02, 0.45, 1.1, col="white", border=NA)
         text(0.25, 1.05, paste("Size: ", size1), cex=1.5, col="black")
      }

      # check if snake 2 is close enough to the block to eat it up
      if (abs(x2.pos.new[1] - x.block) <= 0.01 && abs(y2.pos.new[1] - y.block) <= 0.01) {
         x2.pos.new <<- c(x2.pos.new, x2.pos.new[size2])
         y2.pos.new <<- c(y2.pos.new, y2.pos.new[size2])
         size2 <<- size2 + 1
         points(x.block, y.block, pch=15, col="white", cex=1.6)
         x.block <<- runif(1, 0.05, 0.95)
         y.block <<- runif(1, 0.05, 0.95)
         rect(0.55, 1.02, 0.95, 1.1, col="white", border=NA)
         text(0.75, 1.05, paste("Size: ", size2), cex=1.5, col="red")
      }

      # update the position vector for each snake
      x1.pos.old <<- c(x1.pos.new[1], x1.pos.old[1:size1-1])
      y1.pos.old <<- c(y1.pos.new[1], y1.pos.old[1:size1-1])
      x2.pos.old <<- c(x2.pos.new[1], x2.pos.old[1:size2-1])
      y2.pos.old <<- c(y2.pos.new[1], y2.pos.old[1:size2-1])

      return(NULL)

   }

   keyfun <- function(key) {

      if (identical(key, "Down"))
         direction1 <<- 1
      if (identical(key, "Left"))
         direction1 <<- 2
      if (identical(key, "Up"))
         direction1 <<- 3
      if (identical(key, "Right"))
         direction1 <<- 4

      if (identical(key, "s"))
         direction2 <<- 1
      if (identical(key, "a"))
         direction2 <<- 2
      if (identical(key, "w"))
         direction2 <<- 3
      if (identical(key, "d"))
         direction2 <<- 4

      if (identical(key, "q"))
         return(0)

      return(NULL)

   }

   getGraphicsEvent(prompt="", onKeybd=keyfun, onIdle=idlefun)

}

snake()

############################################################################

# this is just a proof-of-concept, illustrating that writing small interactive
# games is possible using the same approach

############################################################################

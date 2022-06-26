# print() method

print.movavg <- function(x, ...) {

   cat("\nMoving Average\n")
   cat("Window Size:", x$window, "\n\n")
   print(x$mx)
   cat("\n")

}

# plot() method

plot.movavg <- function(x, ...) {

   plot(1:x$n, x$mx, type="o", ...)

}

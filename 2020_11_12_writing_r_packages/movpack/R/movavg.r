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
